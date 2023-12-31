CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:47:49Z creation;2022-06-04T17:47:49Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174749  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��/���1   @��0Â�@.'-�c���Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi��Bo��Bx  B�  B�  B�  B�  B���B���B�  B���B�33B�  B�  B�  B�  B�  B�  B�  B�  BÙ�B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C33C  C�C�fC�fC  C�fC�fC"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=q@��@�Q�A (�A!A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=Bp�B
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
=Bi��Bo��Bx
=B�B�B�B�B���B���B�B���B�8RB�B�B�B�B�B�B�B�BÞ�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�C �C�C�C�C)C
�C�C�C�C5�C�C)C��C��C�C��C��C"�C$�C&�C(�C*�C,�C.)C0�C2�C4�C6�C8�C:�C;��C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd)Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D �
D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN�
DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�N�A�FtA�JXA�N�A�R A�S&A�R�A�T,A�S�A�U2A�V9A�U�A�W?A�MA�@OA�<�A�&A��A��SA���A�<jA�VA��WA���A��RAڸ�Aڟ�A�8�A��A٩�A�(�A؋xA���A�wfA�W�A�v+AЇ+A�9XA���A�poA�oiA�B�A��A��A���AA�Q�A��A���A��A�,A�NpA���A�v�A��A�+6A��-A�B�A�SA�+�A��[A��MA�K)A�S[A��A�|PA�MA�1'A�ŢA��*A���A��A�֡A� �A��A��&A���A�w�A��RA���A���A~b�A{g8Av�eAp�"Al��Aj�YAg�AeX�AbW�Aa8�A`�A[1'AVu�AT�-AS�AN�NAK�AH�OAGA�AB�mA>�zA<�+A;��A:f�A8�A6HA4g�A3~(A1�A/�jA.��A.�A,��A*��A*�A&�A%�A#�gA"�A#0UA$d�A$�oA#�A"یA"g8A"CA!�+A!�qA"FA#�6A$y�A$�2A$�A#��A#VA!�zA �;A�^Ac�AE�Ar�AFtA��A�A^�A�oA6A��A��AffA�A#�A2�A0UA�A��A��AMjA \A�Ae�A�IA!�AY�A=�A�ZAK^A��A�HA�A4AoA�A�9A��A�wAbA�$A%AA��A
��A
��A
QA	y>A	#:A	A��A�kAB[A�7A �A�KA��A�bA%�A�@A��ARTA�AsAOA�rA��Am�A&A{A�xA�A ֡A �@�s�@�@@���@��z@��|@�%�@��A@�U�@�~�@�
�@�l�@���@�~�@��a@�xl@�c�@�q�@��Z@���@�\@�Dg@�?�@�o @�.@���@��@�Z�@미@�Ɇ@�xl@�/�@�:�@�{@��@��>@�.I@�V@㕁@�v�@�M@�\)@��@��Y@�@�s@�=@���@�D�@ݠ�@�V@�~(@�M�@��@�s@�5�@ڔF@��d@�F�@��@�ff@��@�Y�@֧�@���@�%F@�<�@��#@ӛ=@�x�@�-w@���@��A@�IR@Є�@�1@��T@Ϯ@�c�@ι�@�8�@�ԕ@�e,@��@��H@̟�@�a|@���@���@��)@�&�@�Ta@���@�9�@Ƣ4@�A�@�ԕ@�/@đ�@�M@Ô�@��@£�@�Q�@�J@���@�/�@��F@�*�@���@���@�'�@���@�M@�)�@��@���@�b�@�A�@�Ĝ@���@�4�@��@�8�@��N@�S�@��/@�z�@�Ta@�6@��)@���@��@���@�?�@�(�@�خ@���@��	@�t�@�.I@��M@���@��@���@�A�@��@��@���@�{�@�7�@��@��w@��$@���@�Y�@�@���@�6@�G@��@��0@�|�@�=@��@��,@�B[@���@�&@�	l@���@���@�YK@�	@��9@��'@�]�@��@��[@���@�~�@�:�@�~@�ݘ@���@�&�@��@�:*@���@�,�@���@���@���@���@�Q�@��Z@��n@�v`@�:�@��O@�4n@��@� �@��@�0�@��@��@�Ov@���@���@�[W@�"�@��@��@�GE@��@���@�iD@�5�@��@�	l@���@��@�:*@��g@�S&@�'�@���@���@��L@��u@�p;@�m�@�oi@�j@�Ft@� �@��@���@���@�IR@��@��@�u%@�bN@�^5@��@��K@���@���@���@��7@�w2@��@���@�u%@�4n@� �@��t@��M@�O@��	@�Ĝ@�z�@��@���@���@�@�I�@�ԕ@��[@���@�j@��@��/@�Ĝ@���@���@�e�@�^5@��r@��0@�~�@�+�@��@��@�ں@��s@��}@�[�@��@�خ@���@���@�f�@��@��v@��<@��}@��\@�Ov@�:*@�	@��@�|�@��@��@��R@��@�e�@�$�@�	@��F@��X@�g8@�Ov@�-�@�@���@��g@��@@���@�U�@�7L@�@�ی@���@���@�Z@�;�@��@��m@��n@�p�@�X@�A�@�#�@���@��O@�� @�xl@��@�x�@�7L@��P@�֡@��$@���@���@�w�@�]d@�=q@��@���@�m]@�+�@��@���@��r@�;�@���@�]�@�(@��`@���@��@�_�@�9X@�w@~��@~�1@~($@}|@}4@|��@|�O@|��@{�r@{�	@{&@z�X@y��@yG�@xH@w��@v�"@ve@u��@t�f@t�[@te�@s��@s>�@r��@r�<@r��@rs�@r!�@q��@qIR@p��@p �@oƨ@o�P@o&@nn�@n+k@m�@mT�@l��@lr�@k�@kW?@kP�@k8@k�@k�@j��@jxl@j
�@i�@h��@h�p@h�@h�$@hI�@g+@f� @fh
@e��@eA @d�p@c��@cMj@b�@b��@bOv@a��@a�@`��@`�o@`bN@_�w@_@^~�@^Z�@^3�@]�@]�@\�P@\��@\'R@[a@[�@Z��@Z��@Z�@Zz@Z{@Yԕ@Y��@Yf�@YA @Y+@X��@X1'@W��@WC�@W@V�m@VC�@U�t@U�h@Uc@UY�@T�|@T�o@T,=@S�@S�;@S��@SZ�@R�h@R@�@Q��@Q�z@Q�t@Q�n@Qs�@Q@P��@Pm�@P>B@O�$@Oo@N�@NR�@M�d@Mu�@MDg@M�@M@@L��@L��@Lq@L-�@K�@K+@J{�@J8�@J�@I��@I�@H�@H[�@HN�@H%�@HM@G�@G�:@G�@F��@F �@Ef�@D�@Dl"@D<�@D@C�;@C�@@C=@B��@B
�@A�@A8�@A�@@��@?x@>��@>��@>_�@>kQ@>0U@=��@=�@<��@<ѷ@<��@<e�@;��@;C@:��@:M�@9��@9k�@95�@8�P@8y>@8:�@8�@7��@7��@7�F@7�	@7�@6�L@6ff@6:*@6($@6@5��@5IR@5�@4��@4�)@4�e@4�D@4`�@4<�@4$@4�@3��@3U�@3>�@3�@2҉@2��@2��@2Ta@2:*@24@1��@1��@1�@1��@1[W@1A @0�)@0tT@01'@0x@/ݘ@/y�@/�@.��@.�r@.($@-�>@-��@-(�@,�?@,c�@,*�@+��@+�[@+t�@+E9@+�@*�6@*YK@*i�@*1�@*-@*&�@)��@)zx@(��@(��@(bN@( �@'�A@'�@'��@'�q@'�	@'_p@&��@&6�@&)�@&&�@&�@%��@%��@%��@%zx@%@$�/@$�z@$m�@$?�@$M@#�W@#�F@#��@#6z@"��@"ff@"GE@"+k@"�@!��@!�'@!��@!��@!u�@!Q�@ �@ �@ H@ �@��@�Q@�g@�@��@v`@e�@!-@�@�L@a|@)�@�@�^@@��@��@V�@�@��@S�@�@�6@a|@	@��@�=@a�@@��@�@��@�@�o@u�@tT@h�@j@1'@�@�@RT@o@�2@��@B[@.�@)�@�@�@�o@�d@�t@��@Y�@+�@��@��@tT@D�@��@�@O@�@��@��@�s@��@}V@Ta@8�@$�@�@ �@�@�@�@��@Y�@hs@c�@B�@@@��@�O@�e@��@�D@!@��@�@�g@"�@�@�y@�2@ں@�@�6@�1@�r@a|@=q@{@�@��@�h@�~@��@c@o @Q�@<6@#�@@@�@��@��@�_@>B@�W@�@�q@��@Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A�N�A�FtA�JXA�N�A�R A�S&A�R�A�T,A�S�A�U2A�V9A�U�A�W?A�MA�@OA�<�A�&A��A��SA���A�<jA�VA��WA���A��RAڸ�Aڟ�A�8�A��A٩�A�(�A؋xA���A�wfA�W�A�v+AЇ+A�9XA���A�poA�oiA�B�A��A��A���AA�Q�A��A���A��A�,A�NpA���A�v�A��A�+6A��-A�B�A�SA�+�A��[A��MA�K)A�S[A��A�|PA�MA�1'A�ŢA��*A���A��A�֡A� �A��A��&A���A�w�A��RA���A���A~b�A{g8Av�eAp�"Al��Aj�YAg�AeX�AbW�Aa8�A`�A[1'AVu�AT�-AS�AN�NAK�AH�OAGA�AB�mA>�zA<�+A;��A:f�A8�A6HA4g�A3~(A1�A/�jA.��A.�A,��A*��A*�A&�A%�A#�gA"�A#0UA$d�A$�oA#�A"یA"g8A"CA!�+A!�qA"FA#�6A$y�A$�2A$�A#��A#VA!�zA �;A�^Ac�AE�Ar�AFtA��A�A^�A�oA6A��A��AffA�A#�A2�A0UA�A��A��AMjA \A�Ae�A�IA!�AY�A=�A�ZAK^A��A�HA�A4AoA�A�9A��A�wAbA�$A%AA��A
��A
��A
QA	y>A	#:A	A��A�kAB[A�7A �A�KA��A�bA%�A�@A��ARTA�AsAOA�rA��Am�A&A{A�xA�A ֡A �@�s�@�@@���@��z@��|@�%�@��A@�U�@�~�@�
�@�l�@���@�~�@��a@�xl@�c�@�q�@��Z@���@�\@�Dg@�?�@�o @�.@���@��@�Z�@미@�Ɇ@�xl@�/�@�:�@�{@��@��>@�.I@�V@㕁@�v�@�M@�\)@��@��Y@�@�s@�=@���@�D�@ݠ�@�V@�~(@�M�@��@�s@�5�@ڔF@��d@�F�@��@�ff@��@�Y�@֧�@���@�%F@�<�@��#@ӛ=@�x�@�-w@���@��A@�IR@Є�@�1@��T@Ϯ@�c�@ι�@�8�@�ԕ@�e,@��@��H@̟�@�a|@���@���@��)@�&�@�Ta@���@�9�@Ƣ4@�A�@�ԕ@�/@đ�@�M@Ô�@��@£�@�Q�@�J@���@�/�@��F@�*�@���@���@�'�@���@�M@�)�@��@���@�b�@�A�@�Ĝ@���@�4�@��@�8�@��N@�S�@��/@�z�@�Ta@�6@��)@���@��@���@�?�@�(�@�خ@���@��	@�t�@�.I@��M@���@��@���@�A�@��@��@���@�{�@�7�@��@��w@��$@���@�Y�@�@���@�6@�G@��@��0@�|�@�=@��@��,@�B[@���@�&@�	l@���@���@�YK@�	@��9@��'@�]�@��@��[@���@�~�@�:�@�~@�ݘ@���@�&�@��@�:*@���@�,�@���@���@���@���@�Q�@��Z@��n@�v`@�:�@��O@�4n@��@� �@��@�0�@��@��@�Ov@���@���@�[W@�"�@��@��@�GE@��@���@�iD@�5�@��@�	l@���@��@�:*@��g@�S&@�'�@���@���@��L@��u@�p;@�m�@�oi@�j@�Ft@� �@��@���@���@�IR@��@��@�u%@�bN@�^5@��@��K@���@���@���@��7@�w2@��@���@�u%@�4n@� �@��t@��M@�O@��	@�Ĝ@�z�@��@���@���@�@�I�@�ԕ@��[@���@�j@��@��/@�Ĝ@���@���@�e�@�^5@��r@��0@�~�@�+�@��@��@�ں@��s@��}@�[�@��@�خ@���@���@�f�@��@��v@��<@��}@��\@�Ov@�:*@�	@��@�|�@��@��@��R@��@�e�@�$�@�	@��F@��X@�g8@�Ov@�-�@�@���@��g@��@@���@�U�@�7L@�@�ی@���@���@�Z@�;�@��@��m@��n@�p�@�X@�A�@�#�@���@��O@�� @�xl@��@�x�@�7L@��P@�֡@��$@���@���@�w�@�]d@�=q@��@���@�m]@�+�@��@���@��r@�;�@���@�]�@�(@��`@���@��@�_�@�9X@�w@~��@~�1@~($@}|@}4@|��@|�O@|��@{�r@{�	@{&@z�X@y��@yG�@xH@w��@v�"@ve@u��@t�f@t�[@te�@s��@s>�@r��@r�<@r��@rs�@r!�@q��@qIR@p��@p �@oƨ@o�P@o&@nn�@n+k@m�@mT�@l��@lr�@k�@kW?@kP�@k8@k�@k�@j��@jxl@j
�@i�@h��@h�p@h�@h�$@hI�@g+@f� @fh
@e��@eA @d�p@c��@cMj@b�@b��@bOv@a��@a�@`��@`�o@`bN@_�w@_@^~�@^Z�@^3�@]�@]�@\�P@\��@\'R@[a@[�@Z��@Z��@Z�@Zz@Z{@Yԕ@Y��@Yf�@YA @Y+@X��@X1'@W��@WC�@W@V�m@VC�@U�t@U�h@Uc@UY�@T�|@T�o@T,=@S�@S�;@S��@SZ�@R�h@R@�@Q��@Q�z@Q�t@Q�n@Qs�@Q@P��@Pm�@P>B@O�$@Oo@N�@NR�@M�d@Mu�@MDg@M�@M@@L��@L��@Lq@L-�@K�@K+@J{�@J8�@J�@I��@I�@H�@H[�@HN�@H%�@HM@G�@G�:@G�@F��@F �@Ef�@D�@Dl"@D<�@D@C�;@C�@@C=@B��@B
�@A�@A8�@A�@@��@?x@>��@>��@>_�@>kQ@>0U@=��@=�@<��@<ѷ@<��@<e�@;��@;C@:��@:M�@9��@9k�@95�@8�P@8y>@8:�@8�@7��@7��@7�F@7�	@7�@6�L@6ff@6:*@6($@6@5��@5IR@5�@4��@4�)@4�e@4�D@4`�@4<�@4$@4�@3��@3U�@3>�@3�@2҉@2��@2��@2Ta@2:*@24@1��@1��@1�@1��@1[W@1A @0�)@0tT@01'@0x@/ݘ@/y�@/�@.��@.�r@.($@-�>@-��@-(�@,�?@,c�@,*�@+��@+�[@+t�@+E9@+�@*�6@*YK@*i�@*1�@*-@*&�@)��@)zx@(��@(��@(bN@( �@'�A@'�@'��@'�q@'�	@'_p@&��@&6�@&)�@&&�@&�@%��@%��@%��@%zx@%@$�/@$�z@$m�@$?�@$M@#�W@#�F@#��@#6z@"��@"ff@"GE@"+k@"�@!��@!�'@!��@!��@!u�@!Q�@ �@ �@ H@ �@��@�Q@�g@�@��@v`@e�@!-@�@�L@a|@)�@�@�^@@��@��@V�@�@��@S�@�@�6@a|@	@��@�=@a�@@��@�@��@�@�o@u�@tT@h�@j@1'@�@�@RT@o@�2@��@B[@.�@)�@�@�@�o@�d@�t@��@Y�@+�@��@��@tT@D�@��@�@O@�@��@��@�s@��@}V@Ta@8�@$�@�@ �@�@�@�@��@Y�@hs@c�@B�@@@��@�O@�e@��@�D@!@��@�@�g@"�@�@�y@�2@ں@�@�6@�1@�r@a|@=q@{@�@��@�h@�~@��@c@o @Q�@<6@#�@@@�@��@��@�_@>B@�W@�@�q@��@Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	 B	 B	 iB	 �B	 iB	 OB	 OB	 4B	 4B	 OB	 OB	 OB	 �B	 �B	;B	'B	[B	�B	�B	�B	SB	jB	)�B	TFB	e�B	nIB	q�B	m)B	]IB	VB	SB	OB	K�B	I�B	;�B	!�B	 �B	K)B	B	��B	��B	ΊB	�vB	��B	��B
KB
W�B
ZB
]�B
��B
��B
��B  B!B[=BHB�B�+B�	B�pB�B�B��B�WB�MB�B�"B�B��B�Bh�B+B�BBSB
��B
p�B
'RB	��B	��B	��B	�1B	utB	ncB	|jB	c�B	NVB	EmB	A�B	:xB	0UB	+�B	%,B	(B�0B��B��B�B��B��B��B��B��B�DB��B��B�2B��B��B��B��B��B��B��B��B�xB��B�fB��B��B��B�BϑB�&B�QB��B��B��B	�B	�B	(sB	[#B	tTB	�;B	��B	��B	�B	�gB	��B	��B	��B	��B	��B	�[B	��B	�gB	�bB	��B	�B	�+B	�7B	�XB	�B	�KB	�7B	�jB	��B	��B	��B	��B	�6B	��B	�HB	��B	�B	�<B	�B	�<B	��B	�qB	��B	��B	�iB	��B	�AB	�B	��B	�uB	�B	�[B	ĜB	��B	�B	�wB	�(B	�B	��B	B	��B	�B	��B	��B	�B	ƎB	��B	ƨB	��B	��B	ʌB	�PB	�"B	ΊB	��B	�B	�HB	бB	��B	�4B	�:B	��B	ѝB	�B	бB	��B	�(B	�(B	�\B	��B	��B	�\B	��B	�NB	�NB	ѝB	��B	��B	өB	��B	�B	ѝB	҉B	�oB	�uB	�B	ҽB	҉B	��B	�hB	�4B	�B	�4B	��B	��B	�B	өB	��B	�B	өB	ӏB	�,B	��B	��B	�&B	�uB	өB	��B	�aB	��B	ӏB	өB	��B	�uB	�B	�B	ӏB	өB	�aB	�mB	�
B	׍B	��B	��B	��B	׍B	��B	�_B	�eB	ٴB	��B	�_B	�+B	��B	�7B	��B	��B	��B	�1B	ٚB	�eB	�eB	ٚB	�7B	ڠB	��B	�=B	�WB	یB	��B	ܬB	�B	��B	�B	�B	ݘB	�5B	��B	ބB	�pB	�B	߾B	߾B	�'B	�vB	��B	�bB	�4B	�B	��B	�nB	�B	�ZB	��B	��B	�`B	�fB	�B	�B	��B	��B	�XB	�yB	��B	�B	�DB	�_B	�B	�B	�B	�kB	��B	��B	��B	�=B	�B	��B	�cB	�B	��B	�5B	�iB	�iB	�5B	�OB	�B	�B	��B	�[B	��B	��B	�GB	�B	��B	��B	��B	�B	��B	�ZB	��B	��B	��B	�tB	�tB	��B	�rB	��B	��B	��B	�$B	��B	�2B	�B	�fB	�8B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�0B	�0B	��B	�B	�PB	�qB	�qB	��B	��B	��B	�]B	��B	�cB	��B
;B
B
�B
GB
�B
MB
�B
3B
3B
�B
�B
%B
�B
�B
B
�B
EB
zB
�B
1B
�B
�B
	B
	RB
	�B
	�B
	�B
	�B

#B

�B

�B

�B

�B
DB
xB
^B
B
JB
0B
B
�B
0B
JB
JB
0B
JB
dB
�B
B
�B
�B
jB
"B
pB
�B
�B
�B
�B
�B
�B
\B
�B
B
HB
�B
�B
B
�B
�B
:B
TB
�B
�B
aB
2B
�B
MB
B
�B
�B
B
B
�B
B
9B
B

B
YB
�B
B
�B
�B
B
�B
+B
�B
KB
B
QB
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
=B
#B
WB
�B
�B
�B
B
5B
jB
�B
!B
VB
�B
 'B
 \B
 �B
!B
!�B
!�B
!�B
!�B
"NB
"4B
"4B
"NB
"�B
#B
#:B
#TB
#TB
#�B
#�B
#�B
#�B
$tB
%�B
&LB
&�B
'8B
'�B
'�B
'�B
'�B
'�B
($B
(XB
(�B
)DB
)�B
*KB
*eB
*B
*�B
+�B
+�B
,"B
,"B
,�B
,�B
,�B
,�B
-�B
./B
./B
.IB
.�B
/ B
/�B
/�B
0;B
1[B
1�B
1�B
2�B
3�B
3�B
3�B
49B
4B
4nB
4�B
5%B
5B
5B
5?B
5tB
5tB
5�B
6FB
72B
7�B
7fB
72B
7�B
8RB
8�B
8�B
9	B
9$B
8�B
8�B
9�B
9�B
9�B
:DB
:�B
:�B
:�B
:�B
;B
<B
<B
<6B
=�B
=�B
=�B
=�B
=qB
=�B
>�B
>�B
>�B
?B
>�B
?.B
?HB
?}B
?�B
@ B
@4B
?�B
?�B
@B
@B
@B
@�B
A�B
A�B
A�B
A�B
B[B
B[B
B'B
BAB
B�B
CB
C-B
C-B
CGB
CaB
CaB
C�B
C�B
DB
DgB
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
HB
H�B
IRB
I7B
IB
IB
IB
IB
I7B
IlB
I�B
J	B
J	B
J�B
KB
K)B
LB
L0B
L~B
L�B
L�B
MB
MPB
M�B
N"B
N�B
OBB
N�B
N�B
NVB
N�B
O(B
OBB
OvB
O�B
O�B
P}B
Q�B
R�B
RoB
RB
Q�B
Q�B
R:B
SuB
S@B
S[B
S�B
S�B
S�B
T�B
T�B
TaB
TFB
T�B
UMB
VB
V9B
U�B
U�B
U�B
V9B
V�B
WYB
WYB
W$B
W�B
W�B
W�B
X_B
X�B
X�B
X�B
X�B
Y1B
YKB
YKB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
[=B
[qB
[qB
[WB
[WB
\B
\�B
]/B
]IB
]�B
^B
^B
^B
^B
^OB
^�B
]�B
]�B
^OB
^�B
^�B
^�B
_B
_!B
_!B
_VB
_VB
_VB
_VB
_;B
_;B
_�B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
a�B
b�B
b�B
c:B
cTB
c:B
c:B
c�B
c�B
d@B
e,B
e�B
e`B
eFB
e�B
e`B
e�B
fB
fB
fB
e�B
e�B
e�B
fB
fB
fB
f�B
gB
gB
gB
gB
g8B
gRB
gmB
gRB
g�B
h
B
h
B
h>B
hXB
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
j0B
jB
j�B
j�B
j�B
j�B
j�B
kB
kB
kkB
k�B
k�B
k�B
k�B
lB
lB
l=B
l"B
lqB
l�B
l�B
mB
m)B
m]B
mwB
nB
n}B
n�B
n�B
n�B
o5B
o�B
o�B
p;B
pUB
p�B
p�B
qB
qAB
qvB
q�B
q�B
q�B
q�B
rB
rB
rB
rB
q�B
rGB
raB
r�B
r�B
sB
sMB
s�B
tB
tB
tB
t9B
t9B
tnB
t�B
t�B
t�B
u%B
u?B
u?B
u�B
u�B
vB
vzB
v�B
v�B
wLB
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y	B
y	B
x�B
x�B
y>B
yrB
z�B
{dB
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|jB
|B
|B
|B
|B
|B
|jB
|�B
}�B
~B
~B
}�B
~B
~BB
~�B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�4B
�4B
��B
�B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	 B	 B	 iB	 �B	 iB	 OB	 OB	 4B	 4B	 OB	 OB	 OB	 �B	 �B	;B	'B	[B	�B	�B	�B	SB	jB	)�B	TFB	e�B	nIB	q�B	m)B	]IB	VB	SB	OB	K�B	I�B	;�B	!�B	 �B	K)B	B	��B	��B	ΊB	�vB	��B	��B
KB
W�B
ZB
]�B
��B
��B
��B  B!B[=BHB�B�+B�	B�pB�B�B��B�WB�MB�B�"B�B��B�Bh�B+B�BBSB
��B
p�B
'RB	��B	��B	��B	�1B	utB	ncB	|jB	c�B	NVB	EmB	A�B	:xB	0UB	+�B	%,B	(B�0B��B��B�B��B��B��B��B��B�DB��B��B�2B��B��B��B��B��B��B��B��B�xB��B�fB��B��B��B�BϑB�&B�QB��B��B��B	�B	�B	(sB	[#B	tTB	�;B	��B	��B	�B	�gB	��B	��B	��B	��B	��B	�[B	��B	�gB	�bB	��B	�B	�+B	�7B	�XB	�B	�KB	�7B	�jB	��B	��B	��B	��B	�6B	��B	�HB	��B	�B	�<B	�B	�<B	��B	�qB	��B	��B	�iB	��B	�AB	�B	��B	�uB	�B	�[B	ĜB	��B	�B	�wB	�(B	�B	��B	B	��B	�B	��B	��B	�B	ƎB	��B	ƨB	��B	��B	ʌB	�PB	�"B	ΊB	��B	�B	�HB	бB	��B	�4B	�:B	��B	ѝB	�B	бB	��B	�(B	�(B	�\B	��B	��B	�\B	��B	�NB	�NB	ѝB	��B	��B	өB	��B	�B	ѝB	҉B	�oB	�uB	�B	ҽB	҉B	��B	�hB	�4B	�B	�4B	��B	��B	�B	өB	��B	�B	өB	ӏB	�,B	��B	��B	�&B	�uB	өB	��B	�aB	��B	ӏB	өB	��B	�uB	�B	�B	ӏB	өB	�aB	�mB	�
B	׍B	��B	��B	��B	׍B	��B	�_B	�eB	ٴB	��B	�_B	�+B	��B	�7B	��B	��B	��B	�1B	ٚB	�eB	�eB	ٚB	�7B	ڠB	��B	�=B	�WB	یB	��B	ܬB	�B	��B	�B	�B	ݘB	�5B	��B	ބB	�pB	�B	߾B	߾B	�'B	�vB	��B	�bB	�4B	�B	��B	�nB	�B	�ZB	��B	��B	�`B	�fB	�B	�B	��B	��B	�XB	�yB	��B	�B	�DB	�_B	�B	�B	�B	�kB	��B	��B	��B	�=B	�B	��B	�cB	�B	��B	�5B	�iB	�iB	�5B	�OB	�B	�B	��B	�[B	��B	��B	�GB	�B	��B	��B	��B	�B	��B	�ZB	��B	��B	��B	�tB	�tB	��B	�rB	��B	��B	��B	�$B	��B	�2B	�B	�fB	�8B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�0B	�0B	��B	�B	�PB	�qB	�qB	��B	��B	��B	�]B	��B	�cB	��B
;B
B
�B
GB
�B
MB
�B
3B
3B
�B
�B
%B
�B
�B
B
�B
EB
zB
�B
1B
�B
�B
	B
	RB
	�B
	�B
	�B
	�B

#B

�B

�B

�B

�B
DB
xB
^B
B
JB
0B
B
�B
0B
JB
JB
0B
JB
dB
�B
B
�B
�B
jB
"B
pB
�B
�B
�B
�B
�B
�B
\B
�B
B
HB
�B
�B
B
�B
�B
:B
TB
�B
�B
aB
2B
�B
MB
B
�B
�B
B
B
�B
B
9B
B

B
YB
�B
B
�B
�B
B
�B
+B
�B
KB
B
QB
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
=B
#B
WB
�B
�B
�B
B
5B
jB
�B
!B
VB
�B
 'B
 \B
 �B
!B
!�B
!�B
!�B
!�B
"NB
"4B
"4B
"NB
"�B
#B
#:B
#TB
#TB
#�B
#�B
#�B
#�B
$tB
%�B
&LB
&�B
'8B
'�B
'�B
'�B
'�B
'�B
($B
(XB
(�B
)DB
)�B
*KB
*eB
*B
*�B
+�B
+�B
,"B
,"B
,�B
,�B
,�B
,�B
-�B
./B
./B
.IB
.�B
/ B
/�B
/�B
0;B
1[B
1�B
1�B
2�B
3�B
3�B
3�B
49B
4B
4nB
4�B
5%B
5B
5B
5?B
5tB
5tB
5�B
6FB
72B
7�B
7fB
72B
7�B
8RB
8�B
8�B
9	B
9$B
8�B
8�B
9�B
9�B
9�B
:DB
:�B
:�B
:�B
:�B
;B
<B
<B
<6B
=�B
=�B
=�B
=�B
=qB
=�B
>�B
>�B
>�B
?B
>�B
?.B
?HB
?}B
?�B
@ B
@4B
?�B
?�B
@B
@B
@B
@�B
A�B
A�B
A�B
A�B
B[B
B[B
B'B
BAB
B�B
CB
C-B
C-B
CGB
CaB
CaB
C�B
C�B
DB
DgB
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
HB
H�B
IRB
I7B
IB
IB
IB
IB
I7B
IlB
I�B
J	B
J	B
J�B
KB
K)B
LB
L0B
L~B
L�B
L�B
MB
MPB
M�B
N"B
N�B
OBB
N�B
N�B
NVB
N�B
O(B
OBB
OvB
O�B
O�B
P}B
Q�B
R�B
RoB
RB
Q�B
Q�B
R:B
SuB
S@B
S[B
S�B
S�B
S�B
T�B
T�B
TaB
TFB
T�B
UMB
VB
V9B
U�B
U�B
U�B
V9B
V�B
WYB
WYB
W$B
W�B
W�B
W�B
X_B
X�B
X�B
X�B
X�B
Y1B
YKB
YKB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
[=B
[qB
[qB
[WB
[WB
\B
\�B
]/B
]IB
]�B
^B
^B
^B
^B
^OB
^�B
]�B
]�B
^OB
^�B
^�B
^�B
_B
_!B
_!B
_VB
_VB
_VB
_VB
_;B
_;B
_�B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
a�B
b�B
b�B
c:B
cTB
c:B
c:B
c�B
c�B
d@B
e,B
e�B
e`B
eFB
e�B
e`B
e�B
fB
fB
fB
e�B
e�B
e�B
fB
fB
fB
f�B
gB
gB
gB
gB
g8B
gRB
gmB
gRB
g�B
h
B
h
B
h>B
hXB
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
j0B
jB
j�B
j�B
j�B
j�B
j�B
kB
kB
kkB
k�B
k�B
k�B
k�B
lB
lB
l=B
l"B
lqB
l�B
l�B
mB
m)B
m]B
mwB
nB
n}B
n�B
n�B
n�B
o5B
o�B
o�B
p;B
pUB
p�B
p�B
qB
qAB
qvB
q�B
q�B
q�B
q�B
rB
rB
rB
rB
q�B
rGB
raB
r�B
r�B
sB
sMB
s�B
tB
tB
tB
t9B
t9B
tnB
t�B
t�B
t�B
u%B
u?B
u?B
u�B
u�B
vB
vzB
v�B
v�B
wLB
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y	B
y	B
x�B
x�B
y>B
yrB
z�B
{dB
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|jB
|B
|B
|B
|B
|B
|jB
|�B
}�B
~B
~B
}�B
~B
~BB
~�B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�4B
�4B
��B
�B
��B
��B
��B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104942  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174749  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174749  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174749                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024757  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024757  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                