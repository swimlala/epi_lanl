CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-13T17:02:04Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Ap   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Kp   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        Up   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gp   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ip   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        {p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �L        �LArgo profile    3.1 1.2 19500101000000  20170813170204  20181025093509  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @��a��1   @�}'�@:����S��cMV�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD�3D�,�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(�
B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Ck��Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D*�>D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DY�>DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da
Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D�
D��D�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A��A�"�A�$�A�&�A�{A�"�A�VAڰ!A��yAٗ�A�S�A�JA���Aש�A�p�A�+A־wA�JA�^5A���AϋDA�A�%A�=qA�33A�O�A���A�5?A��yA��A��A���A���A��yA�z�A��A�x�A�`BA���A��A�|�A�
=A�t�A�1A�A�A�A�\)A�33A�ffA��A�?}A�VA��
A�z�A�=qA�|�A�r�A���A�+A�M�A�ZA�dZA�K�A�|�A��
A��HA��A�5?A���A��A�z�A�A�|�A� �A��FA���A���A�A���A�JA�+A��A���A���A���A��HA�5?A��A�hsA���A�dZA�l�A�A�S�A��A�jA�v�A���A�JAC�A}�A|�uA|n�A{K�Ay�mAx-AwXAv �Au��AuG�Atz�As�FAr��Aq/Ap��Ap(�Ao�Am�PAl��Ak�FAj��Ai�Ai&�Ag��AfĜAf1Ae�PAe+Ad9XAcAb-AaA_��A_A^�jA^ĜA]�7A[O�AY��AW��AV{ATM�AR(�AO\)AM33AKhsAJ �AH��AG�AF=qAEC�AD �AC�
AB�RAA7LA@I�A?
=A>ĜA>��A>=qA=��A=�-A=�A< �A;l�A:v�A9�A9l�A7��A7&�A7A6�uA5A5�A5�7A4ZA3�A2�A1�A/�mA.�\A-x�A-�A,v�A+x�A*z�A)+A(�A'dZA&VA%?}A$v�A#S�A"��A ��A -A7LA��AoA��AJA|�A�+A�+AK�A��AVA��A�A-Ap�AA�Al�A��A�uA��A�`AjA�A�/A��AXA
v�A	�
A	33A^5A�AVA�9A��A�A�;A�A~�A|�A (�@��y@��@�\)@���@���@�ff@��T@��7@���@�@�V@��m@��H@�7L@�r�@�P@�^@�(�@��@���@�I�@�|�@◍@��/@�@ݡ�@��/@�\)@�ff@ش9@ץ�@�~�@Ցh@ԛ�@�~�@��@Η�@��@ʸR@ɡ�@���@�(�@�@ř�@ēu@�1@�@�@�7L@��j@�1@��w@�
=@�v�@��T@���@�hs@��/@�ƨ@��@�M�@�G�@��D@��@�Q�@��@�E�@�O�@�j@� �@���@�v�@��7@���@�I�@�l�@���@���@�~�@���@���@�bN@��w@�n�@�X@��m@�K�@�"�@���@���@��@��m@��F@�o@�@�&�@�Ĝ@��D@�dZ@�ff@�p�@���@�z�@�1'@�\)@��\@��@���@�hs@�X@�7L@�Ĝ@��@�Q�@��;@���@�+@��+@�@�x�@��@��`@���@��D@�Q�@�(�@��;@��
@���@�ƨ@�|�@���@��@�@��h@�%@��/@�Ĝ@���@�1'@�b@���@���@���@��@�|�@�dZ@�S�@�"�@��H@�ff@�-@���@���@���@�Q�@�1@��m@��@���@�C�@�@��R@�ff@�M�@��#@�O�@�&�@���@�Ĝ@��D@�1'@��F@�;d@�"�@��y@��!@���@�v�@�^5@�J@�O�@�%@���@��@��@�Z@�A�@�(�@�b@��@l�@~�+@}��@}O�@|��@|�D@|I�@{��@{�
@{ƨ@{�F@{dZ@z�!@yx�@yG�@y7L@x��@x1'@w��@w|�@w+@w
=@vȴ@v��@vE�@u�@u��@up�@u`B@uO�@uV@t�/@tz�@s�F@s@r�!@rn�@rM�@q��@q�7@qG�@q%@pĜ@p��@p�9@q%@qx�@rJ@qX@p�9@q�7@qX@p��@o�@m��@m��@l�@lj@k��@j�!@jM�@i��@ix�@i&�@i&�@i7L@i&�@i�@h��@iG�@i�7@i7L@iX@i7L@hr�@g�@g�@hQ�@h�u@hA�@f��@fff@e@e�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�{A��A�"�A�$�A�&�A�{A�"�A�VAڰ!A��yAٗ�A�S�A�JA���Aש�A�p�A�+A־wA�JA�^5A���AϋDA�A�%A�=qA�33A�O�A���A�5?A��yA��A��A���A���A��yA�z�A��A�x�A�`BA���A��A�|�A�
=A�t�A�1A�A�A�A�\)A�33A�ffA��A�?}A�VA��
A�z�A�=qA�|�A�r�A���A�+A�M�A�ZA�dZA�K�A�|�A��
A��HA��A�5?A���A��A�z�A�A�|�A� �A��FA���A���A�A���A�JA�+A��A���A���A���A��HA�5?A��A�hsA���A�dZA�l�A�A�S�A��A�jA�v�A���A�JAC�A}�A|�uA|n�A{K�Ay�mAx-AwXAv �Au��AuG�Atz�As�FAr��Aq/Ap��Ap(�Ao�Am�PAl��Ak�FAj��Ai�Ai&�Ag��AfĜAf1Ae�PAe+Ad9XAcAb-AaA_��A_A^�jA^ĜA]�7A[O�AY��AW��AV{ATM�AR(�AO\)AM33AKhsAJ �AH��AG�AF=qAEC�AD �AC�
AB�RAA7LA@I�A?
=A>ĜA>��A>=qA=��A=�-A=�A< �A;l�A:v�A9�A9l�A7��A7&�A7A6�uA5A5�A5�7A4ZA3�A2�A1�A/�mA.�\A-x�A-�A,v�A+x�A*z�A)+A(�A'dZA&VA%?}A$v�A#S�A"��A ��A -A7LA��AoA��AJA|�A�+A�+AK�A��AVA��A�A-Ap�AA�Al�A��A�uA��A�`AjA�A�/A��AXA
v�A	�
A	33A^5A�AVA�9A��A�A�;A�A~�A|�A (�@��y@��@�\)@���@���@�ff@��T@��7@���@�@�V@��m@��H@�7L@�r�@�P@�^@�(�@��@���@�I�@�|�@◍@��/@�@ݡ�@��/@�\)@�ff@ش9@ץ�@�~�@Ցh@ԛ�@�~�@��@Η�@��@ʸR@ɡ�@���@�(�@�@ř�@ēu@�1@�@�@�7L@��j@�1@��w@�
=@�v�@��T@���@�hs@��/@�ƨ@��@�M�@�G�@��D@��@�Q�@��@�E�@�O�@�j@� �@���@�v�@��7@���@�I�@�l�@���@���@�~�@���@���@�bN@��w@�n�@�X@��m@�K�@�"�@���@���@��@��m@��F@�o@�@�&�@�Ĝ@��D@�dZ@�ff@�p�@���@�z�@�1'@�\)@��\@��@���@�hs@�X@�7L@�Ĝ@��@�Q�@��;@���@�+@��+@�@�x�@��@��`@���@��D@�Q�@�(�@��;@��
@���@�ƨ@�|�@���@��@�@��h@�%@��/@�Ĝ@���@�1'@�b@���@���@���@��@�|�@�dZ@�S�@�"�@��H@�ff@�-@���@���@���@�Q�@�1@��m@��@���@�C�@�@��R@�ff@�M�@��#@�O�@�&�@���@�Ĝ@��D@�1'@��F@�;d@�"�@��y@��!@���@�v�@�^5@�J@�O�@�%@���@��@��@�Z@�A�@�(�@�b@��@l�@~�+@}��@}O�@|��@|�D@|I�@{��@{�
@{ƨ@{�F@{dZ@z�!@yx�@yG�@y7L@x��@x1'@w��@w|�@w+@w
=@vȴ@v��@vE�@u�@u��@up�@u`B@uO�@uV@t�/@tz�@s�F@s@r�!@rn�@rM�@q��@q�7@qG�@q%@pĜ@p��@p�9@q%@qx�@rJ@qX@p�9@q�7@qX@p��@o�@m��@m��@l�@lj@k��@j�!@jM�@i��@ix�@i&�@i&�@i7L@i&�@i�@h��@iG�@i�7@i7L@iX@i7L@hr�@g�@g�@hQ�@h�u@hA�@f��@fff@e@e�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B�B�HB��BPBVB1B	7B\B\BoBPB+BB��B�B�HB�B��B��B�B�B��B��B��BǮBB�dB�FB�-B��B��B��B��B�bB�B� B{�Bw�Br�Bl�BcTB[#BS�BJ�BE�B@�B=qB:^B/B!�B�BuBJBB��B�B��B^5B/BoB
�B
�BB
�B+B1'B(�B�B�BA�Bv�BiyBD�B2-B>wBJ�BL�BP�BR�BQ�BE�B8RB+B�B+B	7B
��B
�B
�fB
�5B
��B
��B
�?B
��B
��B
��B
�=B
�+B
}�B
r�B
dZB
\)B
R�B
M�B
I�B
A�B
:^B
2-B
'�B
"�B
�B
�B
DB
B	��B	��B	�B	�fB	�)B	��B	��B	��B	ǮB	ŢB	��B	�dB	�!B	��B	��B	�!B	�LB	�B	��B	�VB	}�B	m�B	^5B	J�B	33B	$�B	�B	PB	B��B��B��B�B�B�B�`B�NB�BB�BB�;B�HB�;B�/B�B��B��B��BǮBŢB��B�wB�qB�^B�XB��BȴBŢBÖB�qB�?B�B��B��B��B��B��B�uB�PB�+B�B}�Bx�Bw�Bu�Br�Bk�BgmBdZB`BB`BB_;B^5B\)B[#BXBT�BS�BR�BR�BQ�BN�BL�BI�BG�BE�BC�BA�B?}B>wB;dB9XB6FB49B5?B5?B33B2-B5?B49B33B1'B-B(�B%�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B{B�B{BhBhBhBbBVBPBJBDB
=B	7B+B%B1B	7B
=B
=BDB
=B	7B	7B1B%BBBBBBBBBB+B%B%B1B
=BJBJBDBJBPBPBPBPBPB\BbBhB{B�B�B�B�B�B�B �B!�B �B#�B%�B&�B%�B'�B(�B)�B)�B-B/B/B0!B49B6FB:^B<jB<jB<jBA�BE�BH�BH�BJ�BO�BP�BQ�BVBYB^5BbNBgmBhsBiyBm�Bq�Bs�Bv�Bx�B}�B�B�B�+B�7B�PB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�RB�dB�dB�qB�}B��B��BŢBƨBǮBȴB��B��B��B��B��B��B��B��B�B�B�#B�/B�HB�ZB�`B�fB�sB�B�B�B�B�B��B��B��B��B��B	  B	B	1B	DB	PB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	&�B	(�B	(�B	)�B	,B	.B	1'B	33B	49B	6FB	6FB	8RB	9XB	9XB	:^B	<jB	>wB	D�B	D�B	D�B	F�B	H�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	P�B	R�B	R�B	T�B	VB	W
B	W
B	XB	[#B	^5B	`BB	aHB	bNB	dZB	gmB	iyB	jB	k�B	n�B	r�B	t�B	v�B	{�B	|�B	� B	�%B	�1B	�DB	�DB	�1B	�1B	�7B	�7B	�=B	�DB	�JB	�PB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B�B��B�RB��B��B��BB>B�B
:BBQB�B�B�BGB�oB�B�WB��BשBٗB�rB��B��B�PB��B��B��B��B�}B��B�B��B��B��B�B�XB��B~5B{�Bx�Bt�Bh�B_;BZ	BNoBI=BBfB>�B>�B5�B%�B[B�B�B�B�_BܑB�lBd�B3JB-B
�VB
��B
�B+BB3zB+wB;B�B=DB|RBqwBI�B1�B=�BK�BN�BTcBT�BV*BI B<B0�BkB�B2B
�MB
��B
�B
�B
�"B
�CB
�zB
�UB
��B
�IB
��B
��B
�B
w�B
f�B
_�B
T�B
OB
L]B
DB
=�B
6�B
)�B
$pB
"RB
�B
VB
�B	�gB	�B	�HB	�B	��B	�B	�NB	��B	�bB	�.B	��B	��B	��B	��B	��B	�/B	��B	�qB	��B	��B	��B	q�B	b�B	P[B	6�B	(VB	B	nB	B	 �B�B�OB�[B�:B�3B�B�WB��B�B�:B��B�B��B��B��BԢB�EB�KB�8B�?B��B��B��B��B��B�B��B�RB�IB�~B��B��B��B�mB�WB�*B��B��B��B��B��Bz�Bz�BwBw BmBi�Bg�BbBawB`�B_�B^�B`zB[yBV�BUBT�BU`BTZBQ%BPQBLQBI�BF�BF�BC�BAB@/B>�B<CB8�B7#B7eB7zB6	B5B6�B5�B6*B3�B1]B,:B'�B'^B'lB#;B"�B EB�B�ByBhB;B�BxB�BKB"BB�B�B�BMBBeB�B-BRB0B	0B�B	B
�BaB(B�B�B
mB
�BB	[BCB`BB�BB{B�BeB�B3B�B9B-BBzB�B}B]BWB�B�BmBQB�B�BGB�B�BB�BTB~B dB!WB"�B#bB%�B':B'�B'rB(�B)_B*�B+|B.�B/�B00B21B5�B8"B;*B<�B<�B>BB�BF�BH�BI�BLHBP�BQZBR3BWBY�B_BcBg�Bh�BjMBnbBr\BtBwBx�B~B��B�oB�mB��B��B��B�KB�&B�QB�B��B��B�$B�!B�B�OB�B�B�B��B�B�'B��B��B�IB��B��B��B�,B��B��B��B�B��B��B��B��B�$B�IBѦB�\B֑B�sB�XB��B�B�B�B�B��B��B��B�B��B�DB�}B�B�B�4B�CB	 vB	�B	�B	cB	�B	�B	�B	�B	�B	B	�B	B	�B	 �B	" B	$B	&B	'B	)B	)1B	*CB	,�B	.�B	1`B	3oB	4�B	6yB	6B	8kB	9dB	9gB	:�B	<�B	?_B	D�B	D�B	D�B	G$B	IB	I�B	KB	K�B	MB	M�B	OB	P"B	Q(B	SB	R�B	UB	V<B	W6B	W`B	X�B	[�B	^yB	`uB	a\B	b�B	d�B	g�B	i�B	j�B	k�B	n�B	rxB	tlB	vhB	|aB	}UB	qB	�GB	�yB	��B	��B	�8B	��B	��B	��B	��B	��B	��B	��B	��B	�[B	�bB	��B	��B	��B	�bB	��B	��B	��B	��B	�`B	�5B	��B	��B	��B	�:B	��B	�oB	�pB	�=B	�511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352012018102313520120181023135201  AO  ARCAADJP                                                                    20170813170204    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170813170204  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170813170204  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135201  QC  PRES            @�ffD�,�G�O�                PM  ARSQCTM V1.1                                                                20181023135201  QC  PSAL            @�ffD�,�G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093509  IP                  G�O�G�O�G�O�                