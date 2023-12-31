CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-27T19:18:19Z AOML 3.0 creation; 2016-08-07T21:36:48Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160627191819  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ~A   AO  5286_8897_126                   2C  D   APEX                            6531                            072314                          846 @׷'|���1   @׷(���@5�$�/��c9���l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ~A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy�fD��D�I�D�vfD�ɚD� D�<�D�i�D��fD��D�C3D�|�D�|�D���D�L�Dڙ�D��3D�  D�C3D�p D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A��HA��HA�{A�{B 
=B
=B
=B
=B p�B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�8RB�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C)C)C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct)Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Ds�
Dy�
D�D�I�D�v�D���D�RD�=D�i�D�ָD�D�C�D�}D�}D���D�MDڙ�D�ӅD� RD�C�D�pRD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA�ffA�hsA�jA�jA�l�A�jA�jA�l�A�n�A�n�A�l�A�n�A�n�A�n�A�l�A�l�A�l�A�n�A�l�A�ffA�`BA�XA�C�A�1'A�-A�-A�+A��A���A�ĜȂhA�^5A�ĜA�x�Aɟ�A�M�A�VAƝ�A��yA�ffA��A�r�A��#A�ĜA���AÑhA�S�A�I�A�A�A��A��A�ƨA�|�A��A�^5A��7A�7LA�I�A���A�(�A�r�A���A�n�A���A�bA�/A�n�A�ȴA�hsA��FA���A�oA�(�A�x�A�VA�`BA�S�A�jA�^5A�?}A��HA��/A�l�A��-A�XA��A��`A�A�M�A�&�A���A�?}A�I�A��hA�9XA�\)A���A�JA���A�x�A���A��\A�K�A���A���A�I�A��A�l�A�-A��\A��PA�$�A��A��HA�I�A��FA��RA�%A�(�A�|�A�t�A~��A|ZAzbAu&�Ar{Aq7LAot�Am%AihsAe��Ab�9Aa/A`�yA`��A^JAZbAY/AV$�AS�wAP�HAO�hAK��AIAFz�AD��ACC�AAoA?��A=�wA;��A8E�A5x�A49XA1�A.��A-�PA-;dA,�`A,�A,z�A,��A+33A)�TA(�yA(A�A'�TA'"�A%�hA#�A"9XA!"�A 5?A��AI�A?}A��AbA��AS�A��A�A?}A��A"�AffA`BA
=AdZA��A-A�
AdZA�A
�/A
��A	dZA{A��A"�A
=A��AȴA;dA|�A�7Ax�A��A�`AK�A(�A&�A�\A~�An�A^5AbA�AG�A��A5?A�#A%A I�@��@�&�@�o@�  @���@�1@�dZ@@�|�@�G�@���@�b@��@�@��`@�@�%@�S�@�{@ݲ-@ݑh@�7L@܋D@�  @ڸR@�V@���@٩�@ّh@��`@���@ؓu@�j@�I�@�  @�dZ@�K�@�@ָR@֏\@�M�@�J@�p�@�z�@��@Ӯ@�"�@җ�@�M�@��T@���@�@Ѻ^@�x�@��@У�@�I�@�1@϶F@�\)@��@�=q@��@���@�@͡�@�/@̬@˶F@ʸR@�5?@ɺ^@ɑh@�O�@���@�I�@�S�@ƸR@���@Ĭ@�ȴ@��@���@�E�@�E�@�{@��@�@��T@��h@���@�X@���@��m@���@��!@�E�@�I�@�ff@��@��m@�t�@�E�@���@��T@���@���@�$�@���@� �@��9@��`@���@���@���@�%@��@�X@���@��T@��#@�?}@�bN@��@���@�"�@���@�5?@�@��@���@�hs@��j@� �@��F@��@�|�@��@��R@�-@��@��^@�p�@��@��`@��u@���@��D@�Q�@�1@��
@���@�S�@���@�$�@��T@��@�7L@��`@�Ĝ@��u@�bN@��
@��P@�+@���@��+@�v�@�5?@��#@�O�@��`@�Z@�1'@��@�K�@�ff@��@�{@��-@�&�@��@���@�bN@��@��
@���@�;d@�o@��y@��@���@�-@�@��@��@���@���@��@���@��j@��u@�bN@�1'@��F@�t�@�"�@��@�~�@�ff@�E�@��7@�/@�%@���@��D@��D@��@��@�j@�1@��@�t�@��y@���@�v�@�M�@���@��@��/@�r�@�A�@�1@��
@���@��@�5?@��h@�7L@��@���@��9@�j@�A�@�1'@�(�@��m@��m@���@��@��m@��w@��@�\)@�K�@�;d@�@�ff@�5?@��@���@���@���@��@|�j@s"�@jM�@a�@YG�@R��@M�@E�h@?l�@:��@333@.��@*M�@%��@ 1'@t�@v�@&�@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�ffA�ffA�hsA�jA�jA�l�A�jA�jA�l�A�n�A�n�A�l�A�n�A�n�A�n�A�l�A�l�A�l�A�n�A�l�A�ffA�`BA�XA�C�A�1'A�-A�-A�+A��A���A�ĜȂhA�^5A�ĜA�x�Aɟ�A�M�A�VAƝ�A��yA�ffA��A�r�A��#A�ĜA���AÑhA�S�A�I�A�A�A��A��A�ƨA�|�A��A�^5A��7A�7LA�I�A���A�(�A�r�A���A�n�A���A�bA�/A�n�A�ȴA�hsA��FA���A�oA�(�A�x�A�VA�`BA�S�A�jA�^5A�?}A��HA��/A�l�A��-A�XA��A��`A�A�M�A�&�A���A�?}A�I�A��hA�9XA�\)A���A�JA���A�x�A���A��\A�K�A���A���A�I�A��A�l�A�-A��\A��PA�$�A��A��HA�I�A��FA��RA�%A�(�A�|�A�t�A~��A|ZAzbAu&�Ar{Aq7LAot�Am%AihsAe��Ab�9Aa/A`�yA`��A^JAZbAY/AV$�AS�wAP�HAO�hAK��AIAFz�AD��ACC�AAoA?��A=�wA;��A8E�A5x�A49XA1�A.��A-�PA-;dA,�`A,�A,z�A,��A+33A)�TA(�yA(A�A'�TA'"�A%�hA#�A"9XA!"�A 5?A��AI�A?}A��AbA��AS�A��A�A?}A��A"�AffA`BA
=AdZA��A-A�
AdZA�A
�/A
��A	dZA{A��A"�A
=A��AȴA;dA|�A�7Ax�A��A�`AK�A(�A&�A�\A~�An�A^5AbA�AG�A��A5?A�#A%A I�@��@�&�@�o@�  @���@�1@�dZ@@�|�@�G�@���@�b@��@�@��`@�@�%@�S�@�{@ݲ-@ݑh@�7L@܋D@�  @ڸR@�V@���@٩�@ّh@��`@���@ؓu@�j@�I�@�  @�dZ@�K�@�@ָR@֏\@�M�@�J@�p�@�z�@��@Ӯ@�"�@җ�@�M�@��T@���@�@Ѻ^@�x�@��@У�@�I�@�1@϶F@�\)@��@�=q@��@���@�@͡�@�/@̬@˶F@ʸR@�5?@ɺ^@ɑh@�O�@���@�I�@�S�@ƸR@���@Ĭ@�ȴ@��@���@�E�@�E�@�{@��@�@��T@��h@���@�X@���@��m@���@��!@�E�@�I�@�ff@��@��m@�t�@�E�@���@��T@���@���@�$�@���@� �@��9@��`@���@���@���@�%@��@�X@���@��T@��#@�?}@�bN@��@���@�"�@���@�5?@�@��@���@�hs@��j@� �@��F@��@�|�@��@��R@�-@��@��^@�p�@��@��`@��u@���@��D@�Q�@�1@��
@���@�S�@���@�$�@��T@��@�7L@��`@�Ĝ@��u@�bN@��
@��P@�+@���@��+@�v�@�5?@��#@�O�@��`@�Z@�1'@��@�K�@�ff@��@�{@��-@�&�@��@���@�bN@��@��
@���@�;d@�o@��y@��@���@�-@�@��@��@���@���@��@���@��j@��u@�bN@�1'@��F@�t�@�"�@��@�~�@�ff@�E�@��7@�/@�%@���@��D@��D@��@��@�j@�1@��@�t�@��y@���@�v�@�M�@���@��@��/@�r�@�A�@�1@��
@���@��@�5?@��h@�7L@��@���@��9@�j@�A�@�1'@�(�@��m@��m@���@��@��m@��w@��@�\)@�K�@�;d@�@�ff@�5?@��@���G�O�@���@��@|�j@s"�@jM�@a�@YG�@R��@M�@E�h@?l�@:��@333@.��@*M�@%��@ 1'@t�@v�@&�@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
1B
	7B
B
  B	��B	�B	�`B	�B
 �B
0!B
I�B
u�B
�{B
��B
��B
��B
=B�B,B:^B?}BA�BD�BVB~�B��B�qB�?B�B��B�VB��B�+Bv�B�VB��B��B��B��B��B�B�B��BoBA�BffBe`BcTBZBiyBw�B�Bx�BcTBYBO�BD�B��B�jB�uB�LB�B�}B��B�3B��B�XB�B��B��BffB.BJB
�sB
�#B
ȴB
��B
7LB
�B
B	��B
�B
��B
��B
�^B
�'B
��B
��B
��B
�PB
k�B
VB
I�B
0!B
uB	��B	�sB	��B	��B	��B	�DB	v�B	aHB	K�B	9XB	/B	,B	&�B	�B��B��B�sB�5B��BɺB�9B��B�hB�+B�Bt�BjBe`BXB:^B49B5?B6FB:^BA�BC�BE�BF�BH�BVB|�B~�B{�Bz�B� B�B�7B�Bz�Bs�Bm�BcTB^5BZBVBT�BVBVBT�BS�BT�BXB\)BiyBr�Bq�B{�B}�B}�B}�B}�B}�B|�B{�B|�B� B� B� B� B�=B�JB�B�B�B�7B�DB�oB��B�!BB�`B�B�B�B�B�B�B�B�B�B�`B�B��BȴBÖB�wB�XB�FB�FB�?B�B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�B�!B�FB�XB�XBB��B��B��B��B�5B�TB�B�B�B�B�B�B��B��B��B��B��B	B	B	B	+B	+B		7B	JB	\B	hB	uB	{B	�B	�B	�B	�B	(�B	.B	1'B	33B	49B	5?B	6FB	9XB	;dB	;dB	;dB	<jB	@�B	B�B	A�B	?}B	=qB	9XB	5?B	0!B	49B	7LB	:^B	>wB	@�B	C�B	F�B	J�B	M�B	O�B	N�B	K�B	K�B	M�B	K�B	E�B	=qB	2-B	0!B	.B	/B	1'B	6FB	:^B	D�B	Q�B	W
B	\)B	^5B	_;B	aHB	bNB	bNB	dZB	hsB	jB	m�B	l�B	l�B	k�B	k�B	l�B	m�B	n�B	o�B	o�B	o�B	o�B	o�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	z�B	}�B	�B	�B	�+B	�=B	�=B	�PB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�LB	�XB	�^B	�jB	�qB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�NB	�NB	�NB	�HB	�BB	�BB	�HB	�NB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
+B
bB
�B
�B
(�B
/B
5?B
9XB
A�B
A�B
J�B
P�B
T�B
XB
\)B
aHB
e`B
jB
p�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
:B
	>B
&B
 
B	��B	�B	�gB	�B
 �B
0'B
I�B
u�B
�|B
��B
��B
��B
?B�B,B:]B?BA�BD�BVB~�B��B�rB�?B�B��B�TB��B�,Bv�B�UB��B��B��B��B��B�B�B��BoBA�BfhBe]BcSBZBiuBw�B�"Bx�BcSBYBO�BD�B��B�jB�vB�LB�B�|B��B�2B��B�WB�B��B��BfdB.BJB
�vB
�"B
ȷB
��B
7PB
�B
B	��B
�B
��B
��B
�^B
�)B
��B
��B
��B
�SB
k�B
VB
I�B
0)B
}B	�B	�~B	��B	��B	��B	�LB	v�B	aTB	K�B	9dB	/*B	,B	&�B	�B�B��B�B�HB�B��B�MB��B�~B�CB�'Bt�Bj�BevBX)B:vB4:B5YB6DB:xBA�BC�BE�BF�BH�BVB}BB{�Bz�B�B�(B�NB�'Bz�Bs�Bm�BchB^KBZ2BVBUBVBVBUBTBUBX'B\>Bi�Br�Bq�B{�B~B~	B~B~B~B}B{�B}B�B�B�B�B�QB�^B�&B�B�4B�LB�WB��B��B�1B¡B�mB��B�B�B�B�B��B�B�B�B�oB�'B��B��BæB��B�gB�WB�XB�QB�(B�B�B�
B�B�&B�	B��B��B��B��B��B��B��B��B��B��B�B�3B�WB�iB�hBB��B��B��B�B�FB�cB�B�B�B�B�B�B��B��B��B��B�B	B	#B	-B	9B	9B		EB	UB	lB	wB	�B	�B	�B	�B	�B	�B	)B	. B	14B	3=B	4DB	5KB	6RB	9cB	;qB	;rB	;qB	<vB	@�B	B�B	A�B	?�B	=}B	9bB	5KB	0.B	4DB	7VB	:iB	>�B	@�B	C�B	F�B	J�B	M�B	O�B	N�B	K�B	K�B	M�B	K�B	E�B	=|B	27B	0,B	. B	/&B	11B	6PB	:iB	D�B	Q�B	WB	\6B	^@B	_GB	aQB	bVB	bXB	ddB	h~B	j�B	m�B	l�B	l�B	k�B	k�B	l�B	m�B	n�B	o�B	o�B	o�B	o�B	o�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	z�B	}�B	�B	�(B	�4B	�FB	�EB	�ZB	�_B	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�"B	�#B	�#B	�(B	�5B	�>B	�DB	�SB	�^B	�gB	�qB	�xB	��B	��B	��B	B	ÛB	ÜB	ÝB	ĤB	ŪB	ǸB	ȽB	ȺB	ȽB	ɿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�%B	�#B	�)B	�2B	�0B	�.B	�.B	�.B	�/B	�.B	�/B	�0B	�4B	�:B	�?B	�HB	�MB	�QB	�YB	�ZB	�SB	�TB	�TB	�MB	�KB	�JB	�MB	�SB	�MB	�SB	�^B	�_B	�fB	�jB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��G�O�B	��B
1B
gB
�B
�B
(�B
/!B
5DB
9[B
A�B
A�B
J�B
P�B
UB
XB
\,B
aHB
eaB
j�B
p�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.01 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436482016080714364820160807143648  AO  ARCAADJP                                                                    20160627191819    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160627191819  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160627191819  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143648  IP                  G�O�G�O�G�O�                