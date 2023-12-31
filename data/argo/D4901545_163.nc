CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-22T17:02:22Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ވ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �d        �dArgo profile    3.1 1.2 19500101000000  20171022170222  20181025093510  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�/��qC1   @�/�[��@:�\)�c"�+J1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
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
=B�B�B�B�B�B�8RB���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aϕ�Aϗ�Aϙ�Aϗ�Aϗ�AϓuAϓuAϏ\AϏ\AρA�jA�Q�A�G�A�?}A�5?A�-A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA��A�jA��jA�
=A�33A�A�A���A���A�7LA�1'A��-A��DA��HA�A�{A�z�A�(�A�+A��A�1'A��A��A��A���A��A�z�A���A�C�A�bNA���A���A�33A�x�A�  A�x�A���A�JA��hA�1'A��A��wA�jA���A��uA��A�(�A���A��PA��A��mA�  A���A�
=A�^5A��A���A���A�{A��#A�oA���A~�jA|�jAz��Ay�#AxffAw�AvZAu�;At�9Ar�Ar=qAq��Ap�Am�PAl�DAlQ�Ak%Ah�RAfM�Ad�9Ad-Aa+A^�+A]�PA[�AZ�/AZ1AY�AX�`AX(�AWhsAW�AV�AV��AU�wAR��AP�APbAO�PAO33AO��AO��AO;dAMoAJ�/AH{AE�mADz�AC�A?�FA=�#A<5?A;��A:��A:  A9C�A8n�A7��A6�A5G�A4�uA4�A3?}A21'A1+A0�jA0E�A/p�A.��A-|�A,�uA,JA+7LA*�A)�A'�TA'ƨA'�-A'p�A'%A&r�A%ƨA%�wA%/A$�A#��A"z�A!��A!��A!S�A��A�hAO�A��A��AVA�/A1AAQ�A�A+A�HA$�A��Av�AjAp�AoAA+AA�A��A7LA�A�A"�A�9A�TA7LA��A�9A1'A�A�
A	��A�HAr�A�AĜA=qAO�A��A9XAO�AȴA��AVA?}A �+@���@�@�I�@�o@�O�@�dZ@�$�@�Ĝ@�ƨ@�V@��
@��y@��T@��/@�I�@�1'@��y@�7L@�C�@旍@���@�l�@��H@�^@�A�@ޗ�@ݺ^@���@ۍP@�{@�r�@���@֏\@Ԭ@��@�`B@�@�X@��m@�C�@�$�@�j@�-@��T@��@�p�@�Ĝ@�Z@��
@�o@�E�@��-@�(�@��F@�o@�v�@��7@��D@�Q�@���@�K�@��@�@�?}@�Z@��P@��\@���@���@�ƨ@��!@�$�@�x�@���@��@�n�@��@���@�/@�r�@��@�ƨ@���@�V@��^@�V@��@�I�@�9X@� �@��@�@�^5@��@�@�Ĝ@���@�t�@�l�@�\)@��y@�V@��@�/@��/@��9@� �@�t�@�=q@��@��/@��j@��u@�bN@��@��@���@���@��!@��\@�{@��h@���@��@��@�r�@�I�@�(�@��;@�\)@��y@�n�@�{@���@�%@��@�j@�(�@�1@��
@�|�@�@��@��\@�V@�J@���@�p�@��/@� �@��@��y@���@�^5@�@���@�Ĝ@�K�@�v�@�M�@��-@���@��u@�Q�@�9X@��m@�|�@�"�@��@�n�@��@��#@���@�/@�7L@�7L@��@�Ĝ@�z�@�bN@�Z@�1'@�1@��F@�dZ@�\)@�K�@�S�@�o@��H@��H@���@��R@��+@�$�@��#@���@���@�x�@�V@��u@�Z@� �@�P@;d@~��@~��@~E�@~{@}�T@}@}��@}/@|1@{C�@z��@zJ@y�7@x��@x�u@x�u@x�@x  @w+@v��@vE�@v$�@u@u`B@uV@t�/@tZ@t�@s��@s�m@sƨ@st�@s"�@r�@r��@r~�@r�@q�^@q��@q��@q�#@q��@q�#@qx�@qx�@qhs@qx�@q�7@q�7@q�7@qx�@qx�@q�7@q7L@q�@q7L@q7L@p��@p�9@p�@pbN@p1'@o��@o��@o|�@n��@nv�@n$�@m�@m�h@m�@l�j@lj@l�@k�@j�H@jn�@j�@i��@i&�@hĜ@h��@h�u@h�@hA�@g��@g;d@f�y@fff@f{@e�-@d��@d��@d9X@c�
@c��@c��@c��@b��@a��@a��@aG�@`��@`Ĝ@_�@_�P@_K�@_�@^v�@^5?@^$�@]p�@\(�@[�
@[�F@[t�@[S�@Z�H@Z~�@ZM�@Z-@ZJ@Y�@Y�7@X�9@X��@X�@X1'@Wl�@W�@V��@Vff@VE�@V@U�h@U`B@U�@T�@TZ@TI�@T(�@T1@S�
@S��@S@R�H@R�H@R��@R�\@Rn�@R-@Q�@Q��@QX@P�u@P1'@Pb@O��@O�@O|�@Ol�@O\)@O;d@N�@N�@N��@N$�@N{@N@M�@N@N@M�T@M��@M��@M��@Mp�@M?}@L�/@L��@L9X@L�@K�m@K�
@K��@KS�@J�H@J��@J��@J��@J=q@I�#@IX@H��@HA�@Hb@G�@G��@G�P@G;d@F�y@F�+@FV@F@E@E`B@E�@D��@Dj@DI�@D9X@D(�@D(�@D�@C�m@Cƨ@CdZ@C"�@B�!@Bn�@B^5@A��@A��@A��@A�7@Ahs@A%@@�@@  @?l�@?�@>�y@>�R@>v�@>$�@=�@=@=��@=O�@=V@<�/@<�@<j@<I�@<(�@<�@<1@;��@;33@:�H@:��@:�!@:�\@:M�@9�@9X@9&�@9%@8��@8�u@8A�@81'@81'@8 �@8b@7��@7�@7\)@6ȴ@6��@65?@5�@5@5�h@5�@4��@4z�@4�@3S�@3"�@3@2�@2��@2��@2�\@2J@1��@1hs@0��@0��@0bN@0A�@01'@/�;@/l�@/+@/�@/�@/
=@.�@.�@.�y@.�y@.��@.��@/�@/�@.�y@.�R@.��@.�+@.@-�@-�T@-�T@-@-?}@,��@,��@,I�@+�F@+t�@+dZ@+C�@+33@+"�@+o@*��@*��@*�!@*�\@*M�@)�@)��@)��@)��@)��@)��@)�@(�`@(Ĝ@(�9@(bN@(b@(  @'�@'\)@'
=@&�R@&��@&ff@&E�@&$�@&@%�T@%@%O�@%?}@%V@$��@$�/@$��@$�@$�D@$j@$9X@#�m@#��@#o@#o@#@"�H@"�!@"~�@"=q@"J@"J@!��@!��@!��@!x�@!X@!�@ �u@ bN@ 1'@  �@  �@ b@ b@�@��@;d@��@�@��@v�@E�@5?@{@�@��@�-@�-@��@�h@`B@/@V@�/@j@�
@�F@S�@�H@��@��@��@��@�#@x�@��@r�@A�@b@�;@��@|�@\)@K�@+@��@�@��@ff@E�@5?@{@�@��@��@I�@�
@ƨ@�@C�@o@�H@��@�!@��@~�@~�@n�@-@��@��@�7@�7@hs@X@&�@%@�`@��@Ĝ@�9@�@1'@ �@  @��@�@l�@;d@+@
=@�y@��@v�@E�@{@�@�@�T@�T@�T@��@�-@p�@?}@/@V@�@�j@9X@��@��@"�@
�@
�H@
�\@
^5@
�@	��@	�@	�#@	�7@	G�@	�@��@��@��@Ĝ@��@�u@bN@A�@A�@1'@1'@b@�@��@l�@\)@\)@K�@K�@+@ȴ@��@v�@ff@5?@�T@�-@�-@��@�h@�@`B@V@��@�j@�@�D@z�@j@j@j@j@�@ƨ@�F@��@��@dZ@C�@"�@@�@��@��@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aϕ�Aϗ�Aϙ�Aϗ�Aϗ�AϓuAϓuAϏ\AϏ\AρA�jA�Q�A�G�A�?}A�5?A�-A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA��A�jA��jA�
=A�33A�A�A���A���A�7LA�1'A��-A��DA��HA�A�{A�z�A�(�A�+A��A�1'A��A��A��A���A��A�z�A���A�C�A�bNA���A���A�33A�x�A�  A�x�A���A�JA��hA�1'A��A��wA�jA���A��uA��A�(�A���A��PA��A��mA�  A���A�
=A�^5A��A���A���A�{A��#A�oA���A~�jA|�jAz��Ay�#AxffAw�AvZAu�;At�9Ar�Ar=qAq��Ap�Am�PAl�DAlQ�Ak%Ah�RAfM�Ad�9Ad-Aa+A^�+A]�PA[�AZ�/AZ1AY�AX�`AX(�AWhsAW�AV�AV��AU�wAR��AP�APbAO�PAO33AO��AO��AO;dAMoAJ�/AH{AE�mADz�AC�A?�FA=�#A<5?A;��A:��A:  A9C�A8n�A7��A6�A5G�A4�uA4�A3?}A21'A1+A0�jA0E�A/p�A.��A-|�A,�uA,JA+7LA*�A)�A'�TA'ƨA'�-A'p�A'%A&r�A%ƨA%�wA%/A$�A#��A"z�A!��A!��A!S�A��A�hAO�A��A��AVA�/A1AAQ�A�A+A�HA$�A��Av�AjAp�AoAA+AA�A��A7LA�A�A"�A�9A�TA7LA��A�9A1'A�A�
A	��A�HAr�A�AĜA=qAO�A��A9XAO�AȴA��AVA?}A �+@���@�@�I�@�o@�O�@�dZ@�$�@�Ĝ@�ƨ@�V@��
@��y@��T@��/@�I�@�1'@��y@�7L@�C�@旍@���@�l�@��H@�^@�A�@ޗ�@ݺ^@���@ۍP@�{@�r�@���@֏\@Ԭ@��@�`B@�@�X@��m@�C�@�$�@�j@�-@��T@��@�p�@�Ĝ@�Z@��
@�o@�E�@��-@�(�@��F@�o@�v�@��7@��D@�Q�@���@�K�@��@�@�?}@�Z@��P@��\@���@���@�ƨ@��!@�$�@�x�@���@��@�n�@��@���@�/@�r�@��@�ƨ@���@�V@��^@�V@��@�I�@�9X@� �@��@�@�^5@��@�@�Ĝ@���@�t�@�l�@�\)@��y@�V@��@�/@��/@��9@� �@�t�@�=q@��@��/@��j@��u@�bN@��@��@���@���@��!@��\@�{@��h@���@��@��@�r�@�I�@�(�@��;@�\)@��y@�n�@�{@���@�%@��@�j@�(�@�1@��
@�|�@�@��@��\@�V@�J@���@�p�@��/@� �@��@��y@���@�^5@�@���@�Ĝ@�K�@�v�@�M�@��-@���@��u@�Q�@�9X@��m@�|�@�"�@��@�n�@��@��#@���@�/@�7L@�7L@��@�Ĝ@�z�@�bN@�Z@�1'@�1@��F@�dZ@�\)@�K�@�S�@�o@��H@��H@���@��R@��+@�$�@��#@���@���@�x�@�V@��u@�Z@� �@�P@;d@~��@~��@~E�@~{@}�T@}@}��@}/@|1@{C�@z��@zJ@y�7@x��@x�u@x�u@x�@x  @w+@v��@vE�@v$�@u@u`B@uV@t�/@tZ@t�@s��@s�m@sƨ@st�@s"�@r�@r��@r~�@r�@q�^@q��@q��@q�#@q��@q�#@qx�@qx�@qhs@qx�@q�7@q�7@q�7@qx�@qx�@q�7@q7L@q�@q7L@q7L@p��@p�9@p�@pbN@p1'@o��@o��@o|�@n��@nv�@n$�@m�@m�h@m�@l�j@lj@l�@k�@j�H@jn�@j�@i��@i&�@hĜ@h��@h�u@h�@hA�@g��@g;d@f�y@fff@f{@e�-@d��@d��@d9X@c�
@c��@c��@c��@b��@a��@a��@aG�@`��@`Ĝ@_�@_�P@_K�@_�@^v�@^5?@^$�@]p�@\(�@[�
@[�F@[t�@[S�@Z�H@Z~�@ZM�@Z-@ZJ@Y�@Y�7@X�9@X��@X�@X1'@Wl�@W�@V��@Vff@VE�@V@U�h@U`B@U�@T�@TZ@TI�@T(�@T1@S�
@S��@S@R�H@R�H@R��@R�\@Rn�@R-@Q�@Q��@QX@P�u@P1'@Pb@O��@O�@O|�@Ol�@O\)@O;d@N�@N�@N��@N$�@N{@N@M�@N@N@M�T@M��@M��@M��@Mp�@M?}@L�/@L��@L9X@L�@K�m@K�
@K��@KS�@J�H@J��@J��@J��@J=q@I�#@IX@H��@HA�@Hb@G�@G��@G�P@G;d@F�y@F�+@FV@F@E@E`B@E�@D��@Dj@DI�@D9X@D(�@D(�@D�@C�m@Cƨ@CdZ@C"�@B�!@Bn�@B^5@A��@A��@A��@A�7@Ahs@A%@@�@@  @?l�@?�@>�y@>�R@>v�@>$�@=�@=@=��@=O�@=V@<�/@<�@<j@<I�@<(�@<�@<1@;��@;33@:�H@:��@:�!@:�\@:M�@9�@9X@9&�@9%@8��@8�u@8A�@81'@81'@8 �@8b@7��@7�@7\)@6ȴ@6��@65?@5�@5@5�h@5�@4��@4z�@4�@3S�@3"�@3@2�@2��@2��@2�\@2J@1��@1hs@0��@0��@0bN@0A�@01'@/�;@/l�@/+@/�@/�@/
=@.�@.�@.�y@.�y@.��@.��@/�@/�@.�y@.�R@.��@.�+@.@-�@-�T@-�T@-@-?}@,��@,��@,I�@+�F@+t�@+dZ@+C�@+33@+"�@+o@*��@*��@*�!@*�\@*M�@)�@)��@)��@)��@)��@)��@)�@(�`@(Ĝ@(�9@(bN@(b@(  @'�@'\)@'
=@&�R@&��@&ff@&E�@&$�@&@%�T@%@%O�@%?}@%V@$��@$�/@$��@$�@$�D@$j@$9X@#�m@#��@#o@#o@#@"�H@"�!@"~�@"=q@"J@"J@!��@!��@!��@!x�@!X@!�@ �u@ bN@ 1'@  �@  �@ b@ b@�@��@;d@��@�@��@v�@E�@5?@{@�@��@�-@�-@��@�h@`B@/@V@�/@j@�
@�F@S�@�H@��@��@��@��@�#@x�@��@r�@A�@b@�;@��@|�@\)@K�@+@��@�@��@ff@E�@5?@{@�@��@��@I�@�
@ƨ@�@C�@o@�H@��@�!@��@~�@~�@n�@-@��@��@�7@�7@hs@X@&�@%@�`@��@Ĝ@�9@�@1'@ �@  @��@�@l�@;d@+@
=@�y@��@v�@E�@{@�@�@�T@�T@�T@��@�-@p�@?}@/@V@�@�j@9X@��@��@"�@
�@
�H@
�\@
^5@
�@	��@	�@	�#@	�7@	G�@	�@��@��@��@Ĝ@��@�u@bN@A�@A�@1'@1'@b@�@��@l�@\)@\)@K�@K�@+@ȴ@��@v�@ff@5?@�T@�-@�-@��@�h@�@`B@V@��@�j@�@�D@z�@j@j@j@j@�@ƨ@�F@��@��@dZ@C�@"�@@�@��@��@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B�B�B�B�#B�)B�/B�/B�5B�5B�;B�;B�BB�BB�HB�HB�HB�HB�HB�HB�HB�NB�NB�TB�TB�TB�NB�BB��B��BɺB�3B��B��B��B��B��B�VBv�BP�BF�B;dB33B,B)�B)�B'�B�BPB��B�B�`B�BǮB�jB�wB�qB�3B��B��B��B�PB�Bu�BdZBcTBjBffBaHBYBG�B>wB2-B'�B�B	7B
��B
�`B
��B
B
ǮB
��B
�B
ɺB
�dB
��B
��B
�PB
y�B
iyB
XB
N�B
B�B
:^B
2-B
8RB
5?B
)�B
%�B
�B
�B
B	��B	�B	�`B	��B	�}B	�'B	�B	��B	|�B	w�B	iyB	ffB	iyB	m�B	m�B	hsB	cTB	^5B	\)B	XB	L�B	2-B	�B	�B	#�B	(�B	@�B	B�B	?}B	2-B	�B	\B��B�B�ZB��B�B��B��B��B��B��B��B�hB��B��B��B�{B�uB�bB�VB�JB�=B�1B�B�B�B�B� B�B�B~�B}�B|�Bz�Bx�Bz�B|�B|�B{�Bz�Bx�Bw�Bw�Bw�Bv�Bs�Bq�Bo�Bl�BgmBffBgmBgmBdZBdZBdZBffBdZBcTB]/B[#B\)BYBVBQ�BL�BI�BF�BE�BD�BB�B?}B=qB;dB;dB:^B9XB8RB6FB49B/B,B+B(�B'�B%�B$�B#�B"�B!�B �B�B�B�B�B�B�B�B�B{BuBoBuBoBhBbB\BVBVBVBPBJBDB1B+B1B	7B
=B
=B	7BDBDBDBDB
=B1B1B1B+BBBBBB1B	7BDB	7BbB{B�B�B�B�B�B�B�B�B�B�B �B"�B$�B$�B%�B'�B'�B)�B+B+B,B/B1'B33B6FB:^B;dB=qB@�BC�BE�BG�BH�BI�BK�BL�BM�BP�BS�BVBYB[#B]/B]/B]/B_;BaHBdZBffBffBk�Bq�Br�Br�Br�Bt�Bv�By�Bz�B{�B{�B{�B}�B�B�1B�7B�7B�=B�DB�JB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�FB�RB�^B�dB�qB�}B��B��BÖBǮBȴB��B��B��B��B��B��B��B�B�)B�5B�5B�/B�/B�/B�;B�HB�TB�TB�`B�fB�sB�B�B�B�B��B��B��B��B��B	B	%B	+B	1B	JB	VB	VB	\B	bB	hB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	(�B	)�B	+B	.B	2-B	49B	5?B	9XB	:^B	<jB	=qB	>wB	?}B	@�B	B�B	E�B	G�B	J�B	M�B	N�B	P�B	S�B	ZB	[#B	[#B	\)B	_;B	bNB	cTB	ffB	gmB	hsB	jB	l�B	m�B	p�B	r�B	r�B	s�B	s�B	v�B	w�B	x�B	y�B	z�B	}�B	� B	�B	�B	�7B	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�qB	�wB	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
JB
JB
PB
VB
\B
bB
bB
bB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
[#B
[#B
[#B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B�0B�kB�{B�GB�GB�UB�XB�rB�=B�3B�:B�:B�6B�BB�CB�LB�HB�CB�?B�HB�HB�MB�MB�\B�\B�tB�B��B��BԗB�!B�dB�:B��B�B��B��B�B~�BW4BM|BE�B8�B4�B0�B0�B+bB#QB|B�+B�B�B�B͵B�oB�kB��B��B��B�tB��B��B�B{Be�Bc�Bk�Bg�BcBB_1BJ�BA�B4�B,�BEB�B
��B
�B
�MB
�vB
�B
��B
ޘB
�KB
��B
�{B
��B
��B
~mB
nB
Z[B
RLB
D�B
=B
3SB
;,B
9bB
+�B
'NB
!�B
OB
gB	�mB	�B	��B	׸B	�OB	��B	��B	��B	�B	|�B	kAB	h�B	j�B	o:B	o�B	j�B	dB	^�B	]:B	Z�B	T�B	7hB	�B	B	$�B	'�B	@�B	C�B	E`B	7�B	&;B	�B��B�nB�
BƙB�B�B��B��B�B�?B�B�B��B�B��B�tB��B��B�WB�nB�DB�EB��B�@B�lB�!B��B��B�BAB~-B}�B|BzaB|�B}B~lB}<B|�B|�BynBw�By BzMBt�Br\Bq2Bo�Bh�Bf�Bi�BjBf.Be|BfoBg@BfkBf�B^B[MB^"BY�BX?BS�BN�BK/BGBFvBFbBD�B@�B?yB=B;�B;B:B8�B6�B8�B1�B-&B,�B+B).B(B&`B$�B$�B"�B!;B jB!#BzB^B�BRB�B�B�B�BB�B8B�B�B�B�BB~B�BwB�B	B	iB	�B	�B�B:B�BiBFBB,BoB
 B�B
�B	�BBIBRB�B�B	�B�B5B	�B^B"B�B.BaB�B�B�B �B hB �B �B"B$>B%1B%]B&�B(]B(�B*�B+�B+�B-B/�B2B4�B7|B:�B<.B>kBBCBDfBF<BHBI9BJ�BL?BM=BN�BQ�BT�BV�BY�B[�B]EB]XB]�B`6Bb0Bd�Bf�Bg�Bm)Bq�Br�Br�BsYBu�Bw�BzQB{VB|)B|�B|�B�B��B��B�eB�qB��B��B�B�!B��B��B��B�3B�HB�zB�3B�B��B�B�B�\B��B��B��B��B��B�3B��B��B��B��B��B�B�EB��B�B�B�B�:B�B�vBѳBԁB��B�oBދBޞBݬB�)B�B�EB�{B�B�EB��B�B�B��B�B�#B�B�YB��B��B�:B��B	B	"B	UB	�B	�B	vB	bB	�B	�B	�B	�B	{B	�B	~B	�B	�B	�B	�B	�B	 	B	"WB	&CB	(B	)'B	*GB	+�B	.�B	2~B	4�B	5�B	9�B	:�B	<�B	=�B	>�B	?�B	@�B	B�B	E�B	H�B	KOB	NEB	OEB	QAB	T�B	Z%B	[!B	[3B	\�B	_�B	brB	c�B	f}B	g�B	h�B	j�B	l�B	m�B	p�B	r�B	r�B	s�B	s�B	v�B	w�B	x�B	y�B	{B	~+B	�B	�B	��B	�#B	�`B	��B	�mB	��B	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�$B	�"B	�,B	�PB	�4B	�4B	�zB	��B	�iB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ìB	âB	äB	��B	�B	��B	��B	�B	��B	� B	�?B	�B	�B	�B	��B	��B	��B	ΓB	�yB	�	B	�9B	�&B	�B	��B	�B	�&B	�B	�sB	�1B	�B	֓B	�B	�^B	�AB	�]B	�FB	܂B	�tB	�RB	�MB	�KB	�SB	�yB	�B	�`B	�rB	�B	��B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	�'B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�kB	�5B
 B
 +B
 B
#B
B
B
"B
QB
B
:B
{B
7B
3B
=B
	)B
	4B

VB
MB
CB
cB
fB
nB
�B
}B
�B
sB
�B
mB
�B
�B
�B
�B
rB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 �B
"#B
#B
"�B
#B
#�B
#�B
#�B
$�B
%,B
&GB
'VB
'XB
)2B
)B
)B
*1B
*<B
* B
+'B
+!B
,IB
,;B
-7B
-5B
-<B
.-B
..B
.B
.&B
.oB
/lB
0]B
0/B
0;B
09B
0YB
0sB
0�B
1MB
1AB
1NB
2gB
2hB
3?B
4<B
4FB
4FB
4hB
4NB
4pB
5�B
5[B
5{B
6oB
6cB
6eB
7�B
7B
8�B
8�B
9�B
9wB
:wB
:hB
:qB
:hB
:~B
:�B
;�B
;�B
;�B
;�B
<�B
<B
<uB
<�B
=�B
=�B
=wB
=lB
=|B
=�B
>wB
?rB
?{B
?pB
?{B
@kB
@�B
A�B
A�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
NB
NB
N'B
N�B
N�B
N�B
N�B
N�B
OB
N�B
O�B
N�B
OB
O�B
O�B
O�B
PB
P<B
Q	B
QB
P�B
Q�B
Q�B
Q�B
RB
R)B
R0B
S!B
SB
SB
SB
TB
TB
TB
TB
UB
UB
T�B
UB
UB
U#B
V+B
VB
V%B
V\B
WqB
X+B
XZB
XcB
Y+B
YB
Y0B
YB
Y�B
YeB
[�B
[eB
[GB
\PB
\RB
]`B
]HB
^MB
^@B
^MB
^VB
_XB
_`B
_lB
_TB
_JB
`ZB
`]B
`�B
`�B
a�B
b�B
caB
c�B
c�B
d�B
d�B
deB
dpB
dgB
dtB
d[B
enB
e�B
e�B
e�B
ekB
ffB
fB
frB
f�B
g�B
g�B
g~B
gxB
g|B
g�B
h�B
h}B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�[�<#�
<G�_<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<;x�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352092018102313520920181023135209  AO  ARCAADJP                                                                    20171022170222    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171022170222  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171022170222  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135209  QC  PRES            @�  D���G�O�                PM  ARSQCTM V1.1                                                                20181023135209  QC  PSAL            @�  D���G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093510  IP                  G�O�G�O�G�O�                