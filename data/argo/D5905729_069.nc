CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-03-17T09:00:49Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20200317090049  20210209100527  5905729 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               EA   AO  7160                            2C  D   NAVIS_A                         0838                            170425                          863 @�
�ĥe+1   @�
�`�f@*(r� Ĝ�c_;dZ�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      EA   A   A   @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX��B_��Bg��Bp  Bx  B�  B�  B�  B�  B�33B�  B�33B�33B���B�  B�33B�33B�33B�33B���B�ffB�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C �C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�3D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A (�A>�\A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BHp�BPp�BX�
B_��Bg��Bp
=Bx
=B�B�B�B�B�8RB�B�8RB�8RB���B�B�8RB�8RB�8RB�8RB���B�k�B�B�B�B�B�B�8RB�B���B�B�B�B�B�B�B�B�C )C)C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV)CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D�>D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD��D�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aҗ�Aҝ�Aҡ�Aң�Aҡ�Aҡ�AҬAҩ�Aң�Aҧ�AҰ!AҰ!AҰ!AҰ!AҲ-AҲ-AҲ-AҲ-AҴ9AҶFAҶFAҴ9AҶFAҺ^AҸRAҸRAҶFAҸRAҼjA���A���AҾwAҾwA���A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA���A�v�A�1A���A�ĜA��`A��A��TA�S�A���A�
=A�S�A�ZA�S�A�%A��RA��7A�A�A��mA�ȴAzjAsoAn�Ai��Ad  A^��AZffAV�/AT�AP�yAO?}AN�ALr�AJ��AH�HAHA�AGhsAFM�AEx�AD�9AC�
ABbNAA�^AA7LA>��A<�DA;�
A;`BA:�A9`BA8M�A7hsA77LA7+A733A7&�A6~�A5S�A4bNA4ZA4�uA5+A5l�A4��A4JA3��A4{A3ƨA3C�A2��A2v�A1�
A1l�A0�RA01'A/�A/��A/;dA/A.��A.��A.jA.(�A-�;A-��A-\)A-oA,��A,��A,ffA,9XA+�A+�7A+&�A*�A*ĜA*��A*ffA*9XA*{A)�mA)�^A)x�A)+A(�yA(^5A(�A'��A&��A&�uA%�#A%�hA%`BA%XA%G�A%A$ȴA$��A$�A#��A$$�A$=qA$M�A$ZA$1A#��A#l�A#dZA#`BA#XA#O�A#;dA#A"z�A"-A!t�A �A�A��A`BA%A�DAbAC�A�HA��AQ�A��AA\)A�A~�A�AƨAhsA?}AĜAM�A�TA�^AXA�HA�A�A5?A��A��A��A��A�FA�A&�A�/A
=A�AoA�DA��A��A
=AoA��A=qAJA�mA��A��A�A�A��A�RA�9A��A�DA�mA��A�A~�AI�A(�AA�-A�A�\A�wAO�A&�AA
�yA
�9A
r�A
�A	�-A	O�A	�A�RA�DAM�A�A��Ap�AXA�A�AjA�A�^AhsA7LA�\A�AS�A��Ar�AE�A(�AA�wA33A �`A ��A �DA I�A �@��m@�33@��y@��+@���@��w@��@��@��\@��@��@�/@�Z@���@�+@��+@���@�X@��@�~�@�=q@��@�x�@���@�1'@�t�@�E�@�D@�F@�t�@�\)@�o@��@�5?@�V@��/@��@�  @�\)@��@�~�@�J@�@�/@���@�9@�9X@�;d@⟾@�^5@��T@�-@�p�@���@�(�@�;d@��H@�n�@�5?@ݩ�@�V@���@�bN@��m@ۍP@�K�@��H@ڏ\@��T@ج@�1@���@׮@�\)@���@�V@��#@�?}@��`@�Z@�l�@Ұ!@җ�@�v�@���@��T@ѩ�@��@�z�@�  @���@��@�7L@��`@̛�@̋D@�Q�@���@�33@ʏ\@�E�@��@ɩ�@�&�@ȓu@�1'@ǍP@�C�@��@���@�%@ģ�@�j@���@�ȴ@�J@�%@�r�@� �@��w@�
=@���@�V@��#@��7@�X@���@�9X@��
@�l�@�o@��+@��-@���@�b@��@�t�@�dZ@�+@��@���@��@���@�G�@�%@��j@��D@�A�@�ƨ@��@�ȴ@�^5@�5?@��@�X@�%@��D@�  @��w@��@�dZ@���@���@���@���@�-@�p�@���@�1'@��m@�S�@��@�v�@���@�/@�(�@���@�t�@�K�@���@�v�@�J@���@��h@�%@��@�Z@�b@��
@���@�t�@�;d@�@���@��@�X@�Ĝ@�bN@���@���@��@��@�C�@��y@��!@�V@�@�p�@�V@���@��D@���@���@�;d@�+@���@�n�@�=q@��@��^@�x�@�x�@�&�@��/@�I�@��@��m@��P@�33@�ȴ@�n�@��#@�p�@���@��9@�z�@�A�@�(�@�ƨ@���@�t�@�dZ@�C�@�@�~�@�$�@�J@���@��7@�/@��j@�A�@��
@�|�@�S�@�+@���@���@��!@�n�@�@�p�@��/@��@�bN@�9X@�  @��F@�\)@�o@��@���@�5?@�{@��@�@���@���@��@�/@��`@��@�z�@�9X@���@�t�@�dZ@�33@�
=@���@�ff@�{@���@��-@�X@�%@��@�9X@���@��@�K�@�
=@���@��@��y@��!@�~�@�ff@�M�@��^@�%@���@�r�@�9X@�b@�@�;@��@��@|�@\)@
=@~�+@~5?@}�h@|�@|j@|9X@|1@{ƨ@{C�@z��@z~�@z^5@z^5@y��@y�7@yX@x��@x�9@x  @wl�@w+@v��@v�R@vV@v@u@uO�@s�
@sdZ@s"�@r�@r��@r^5@r=q@q�@q7L@p��@pbN@o�;@oK�@o�@n��@nE�@n@m��@m�h@mO�@mV@l��@lj@k��@kdZ@k@j��@jJ@i�^@i��@ix�@h��@h�9@h��@h�@hr�@hbN@h �@g�w@g��@g|�@g\)@f�@f��@fff@fE�@fE�@f$�@f@e��@e`B@eV@d�@d��@dI�@c��@c"�@b��@b~�@b-@a�@aG�@a%@`r�@_�;@_��@_�w@_l�@_
=@^ȴ@^v�@^5?@]�T@]�@]O�@\��@\��@\1@[ƨ@[t�@Z�@Z��@Z��@Z��@Z�\@Z~�@Z=q@ZJ@Y�^@Y�7@X��@X�u@XbN@X  @W�P@WK�@V�@VE�@U@Up�@U/@U�@T��@Tj@TI�@T1@S�@St�@SdZ@S@R�!@R~�@R^5@RM�@Q�@Q��@Q��@Qx�@QX@Q%@P�@Pb@O�;@O��@O\)@O+@Nȴ@Nv�@N$�@M��@MO�@M�@L��@K�
@K��@K�@KS�@Ko@J��@J��@J^5@JJ@I�^@IX@I&�@H��@H�`@H��@H��@HQ�@H1'@G�;@G�w@G��@Gl�@G�@F�@FV@E�@E��@Ep�@D�/@DZ@C�F@Ct�@CS�@B��@B�@A�^@Ahs@A�@@��@@�u@@r�@@Q�@@1'@?�@?��@?�@>ȴ@>�+@>$�@=��@=�@<��@<Z@<1@;��@;��@;�@;t�@;o@:��@:-@9��@9X@97L@9�@8Ĝ@8�@81'@7�@7��@6��@6��@6V@6E�@6$�@6@5�h@5V@4�/@4��@49X@4(�@4�@3ƨ@3C�@3o@2�!@2^5@2M�@2-@2-@2J@1��@1�^@1x�@1�@1%@0��@0�u@0Q�@0  @/��@/�@/|�@.��@.�R@.�+@.ff@-�T@-�@-�@-V@,�/@,z�@,9X@+��@+ƨ@+�F@+��@+33@+o@+@*�@*�H@*��@*M�@)�#@)��@)��@)��@)�7@)�7@)hs@)7L@(�`@(�u@(A�@'�@'�w@'�P@'\)@'
=@&��@&�@&ff@&V@&5?@%�@%p�@%?}@$�@$�@$j@$9X@#�m@#�F@#��@#��@#�@#�@#dZ@#o@"��@"��@"~�@"=q@"J@!�@!�@!�#@!�^@!hs@!&�@ ��@ Q�@ 1'@ b@   @�@�;@��@�@K�@+@�@�y@�@ȴ@�R@��@V@�T@�@p�@`B@/@�@V@�/@�@(�@�
@�F@�@S�@�H@��@��@��@�\@n�@J@��@hs@��@Ĝ@Q�@1'@ �@b@  @�@�;@�;@�w@�P@|�@l�@;d@��@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aҗ�Aҝ�Aҡ�Aң�Aҡ�Aҡ�AҬAҩ�Aң�Aҧ�AҰ!AҰ!AҰ!AҰ!AҲ-AҲ-AҲ-AҲ-AҴ9AҶFAҶFAҴ9AҶFAҺ^AҸRAҸRAҶFAҸRAҼjA���A���AҾwAҾwA���A�A�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA���A�v�A�1A���A�ĜA��`A��A��TA�S�A���A�
=A�S�A�ZA�S�A�%A��RA��7A�A�A��mA�ȴAzjAsoAn�Ai��Ad  A^��AZffAV�/AT�AP�yAO?}AN�ALr�AJ��AH�HAHA�AGhsAFM�AEx�AD�9AC�
ABbNAA�^AA7LA>��A<�DA;�
A;`BA:�A9`BA8M�A7hsA77LA7+A733A7&�A6~�A5S�A4bNA4ZA4�uA5+A5l�A4��A4JA3��A4{A3ƨA3C�A2��A2v�A1�
A1l�A0�RA01'A/�A/��A/;dA/A.��A.��A.jA.(�A-�;A-��A-\)A-oA,��A,��A,ffA,9XA+�A+�7A+&�A*�A*ĜA*��A*ffA*9XA*{A)�mA)�^A)x�A)+A(�yA(^5A(�A'��A&��A&�uA%�#A%�hA%`BA%XA%G�A%A$ȴA$��A$�A#��A$$�A$=qA$M�A$ZA$1A#��A#l�A#dZA#`BA#XA#O�A#;dA#A"z�A"-A!t�A �A�A��A`BA%A�DAbAC�A�HA��AQ�A��AA\)A�A~�A�AƨAhsA?}AĜAM�A�TA�^AXA�HA�A�A5?A��A��A��A��A�FA�A&�A�/A
=A�AoA�DA��A��A
=AoA��A=qAJA�mA��A��A�A�A��A�RA�9A��A�DA�mA��A�A~�AI�A(�AA�-A�A�\A�wAO�A&�AA
�yA
�9A
r�A
�A	�-A	O�A	�A�RA�DAM�A�A��Ap�AXA�A�AjA�A�^AhsA7LA�\A�AS�A��Ar�AE�A(�AA�wA33A �`A ��A �DA I�A �@��m@�33@��y@��+@���@��w@��@��@��\@��@��@�/@�Z@���@�+@��+@���@�X@��@�~�@�=q@��@�x�@���@�1'@�t�@�E�@�D@�F@�t�@�\)@�o@��@�5?@�V@��/@��@�  @�\)@��@�~�@�J@�@�/@���@�9@�9X@�;d@⟾@�^5@��T@�-@�p�@���@�(�@�;d@��H@�n�@�5?@ݩ�@�V@���@�bN@��m@ۍP@�K�@��H@ڏ\@��T@ج@�1@���@׮@�\)@���@�V@��#@�?}@��`@�Z@�l�@Ұ!@җ�@�v�@���@��T@ѩ�@��@�z�@�  @���@��@�7L@��`@̛�@̋D@�Q�@���@�33@ʏ\@�E�@��@ɩ�@�&�@ȓu@�1'@ǍP@�C�@��@���@�%@ģ�@�j@���@�ȴ@�J@�%@�r�@� �@��w@�
=@���@�V@��#@��7@�X@���@�9X@��
@�l�@�o@��+@��-@���@�b@��@�t�@�dZ@�+@��@���@��@���@�G�@�%@��j@��D@�A�@�ƨ@��@�ȴ@�^5@�5?@��@�X@�%@��D@�  @��w@��@�dZ@���@���@���@���@�-@�p�@���@�1'@��m@�S�@��@�v�@���@�/@�(�@���@�t�@�K�@���@�v�@�J@���@��h@�%@��@�Z@�b@��
@���@�t�@�;d@�@���@��@�X@�Ĝ@�bN@���@���@��@��@�C�@��y@��!@�V@�@�p�@�V@���@��D@���@���@�;d@�+@���@�n�@�=q@��@��^@�x�@�x�@�&�@��/@�I�@��@��m@��P@�33@�ȴ@�n�@��#@�p�@���@��9@�z�@�A�@�(�@�ƨ@���@�t�@�dZ@�C�@�@�~�@�$�@�J@���@��7@�/@��j@�A�@��
@�|�@�S�@�+@���@���@��!@�n�@�@�p�@��/@��@�bN@�9X@�  @��F@�\)@�o@��@���@�5?@�{@��@�@���@���@��@�/@��`@��@�z�@�9X@���@�t�@�dZ@�33@�
=@���@�ff@�{@���@��-@�X@�%@��@�9X@���@��@�K�@�
=@���@��@��y@��!@�~�@�ff@�M�@��^@�%@���@�r�@�9X@�b@�@�;@��@��@|�@\)@
=@~�+@~5?@}�h@|�@|j@|9X@|1@{ƨ@{C�@z��@z~�@z^5@z^5@y��@y�7@yX@x��@x�9@x  @wl�@w+@v��@v�R@vV@v@u@uO�@s�
@sdZ@s"�@r�@r��@r^5@r=q@q�@q7L@p��@pbN@o�;@oK�@o�@n��@nE�@n@m��@m�h@mO�@mV@l��@lj@k��@kdZ@k@j��@jJ@i�^@i��@ix�@h��@h�9@h��@h�@hr�@hbN@h �@g�w@g��@g|�@g\)@f�@f��@fff@fE�@fE�@f$�@f@e��@e`B@eV@d�@d��@dI�@c��@c"�@b��@b~�@b-@a�@aG�@a%@`r�@_�;@_��@_�w@_l�@_
=@^ȴ@^v�@^5?@]�T@]�@]O�@\��@\��@\1@[ƨ@[t�@Z�@Z��@Z��@Z��@Z�\@Z~�@Z=q@ZJ@Y�^@Y�7@X��@X�u@XbN@X  @W�P@WK�@V�@VE�@U@Up�@U/@U�@T��@Tj@TI�@T1@S�@St�@SdZ@S@R�!@R~�@R^5@RM�@Q�@Q��@Q��@Qx�@QX@Q%@P�@Pb@O�;@O��@O\)@O+@Nȴ@Nv�@N$�@M��@MO�@M�@L��@K�
@K��@K�@KS�@Ko@J��@J��@J^5@JJ@I�^@IX@I&�@H��@H�`@H��@H��@HQ�@H1'@G�;@G�w@G��@Gl�@G�@F�@FV@E�@E��@Ep�@D�/@DZ@C�F@Ct�@CS�@B��@B�@A�^@Ahs@A�@@��@@�u@@r�@@Q�@@1'@?�@?��@?�@>ȴ@>�+@>$�@=��@=�@<��@<Z@<1@;��@;��@;�@;t�@;o@:��@:-@9��@9X@97L@9�@8Ĝ@8�@81'@7�@7��@6��@6��@6V@6E�@6$�@6@5�h@5V@4�/@4��@49X@4(�@4�@3ƨ@3C�@3o@2�!@2^5@2M�@2-@2-@2J@1��@1�^@1x�@1�@1%@0��@0�u@0Q�@0  @/��@/�@/|�@.��@.�R@.�+@.ff@-�T@-�@-�@-V@,�/@,z�@,9X@+��@+ƨ@+�F@+��@+33@+o@+@*�@*�H@*��@*M�@)�#@)��@)��@)��@)�7@)�7@)hs@)7L@(�`@(�u@(A�@'�@'�w@'�P@'\)@'
=@&��@&�@&ff@&V@&5?@%�@%p�@%?}@$�@$�@$j@$9X@#�m@#�F@#��@#��@#�@#�@#dZ@#o@"��@"��@"~�@"=q@"J@!�@!�@!�#@!�^@!hs@!&�@ ��@ Q�@ 1'@ b@   @�@�;@��@�@K�@+@�@�y@�@ȴ@�R@��@V@�T@�@p�@`B@/@�@V@�/@�@(�@�
@�F@�@S�@�H@��@��@��@�\@n�@J@��@hs@��@Ĝ@Q�@1'@ �@b@  @�@�;@�;@�w@�P@|�@l�@;d@��@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	B�BBuB+BB
�B
��B
}�B
L�B
%�B
�B
�B

=B	��B	�HB	�}B	�?B	�B	��B	��B	��B	��B	�9B	��B	��B
%�B
C�B
z�B
�hB
��B
�3B
ŢB
��B
�B
�;B
�ZB
�mB
�sB
�B
�B
�TB
�BB
ɺB
�'B
��B
�'B
�'B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
�-B
�jB
ɺB
�B
��B
��B
�B
�ZB
�`B
�mB
�yB
�B
�yB
�sB
�fB
�mB
�mB
�mB
�`B
�fB
�`B
�ZB
�ZB
�TB
�NB
�NB
�NB
�HB
�NB
�HB
�BB
�;B
�;B
�5B
�5B
�/B
�/B
�/B
�/B
�)B
�#B
�#B
�B
�B
�B
�B
�B
��B
��B
��B
��B
ɺB
ȴB
ǮB
ǮB
ɺB
ȴB
ƨB
ƨB
ƨB
ÖB
ƨB
ǮB
ȴB
ɺB
��B
ɺB
ȴB
ȴB
ȴB
ǮB
ǮB
ǮB
ƨB
ĜB
B
�wB
�XB
�'B
�B
�B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
��B
�oB
�\B
�PB
�DB
�=B
�1B
�%B
�B
�%B
�B
�B
~�B
}�B
|�B
z�B
{�B
{�B
|�B
}�B
�B
�B
~�B
�B
�%B
�1B
�+B
�B
�B
~�B
� B
}�B
}�B
� B
~�B
� B
�B
�B
� B
~�B
~�B
~�B
}�B
}�B
|�B
z�B
y�B
v�B
v�B
u�B
t�B
s�B
q�B
o�B
m�B
k�B
k�B
jB
iyB
iyB
hsB
gmB
e`B
dZB
cTB
cTB
bNB
aHB
`BB
_;B
^5B
^5B
]/B
\)B
[#B
ZB
YB
XB
W
B
VB
S�B
R�B
Q�B
P�B
O�B
O�B
N�B
N�B
M�B
L�B
L�B
L�B
L�B
K�B
K�B
I�B
H�B
G�B
F�B
D�B
D�B
D�B
C�B
C�B
B�B
B�B
A�B
@�B
?}B
=qB
<jB
;dB
<jB
9XB
9XB
8RB
7LB
7LB
6FB
5?B
33B
0!B
.B
.B
.B
.B
.B
-B
-B
,B
,B
,B
+B
+B
+B
)�B
)�B
)�B
(�B
(�B
(�B
'�B
&�B
&�B
%�B
%�B
$�B
$�B
$�B
#�B
#�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
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
�B
�B
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
"�B
"�B
"�B
"�B
!�B
#�B
#�B
!�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
"�B
#�B
"�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
,B
+B
+B
,B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
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
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q�B
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
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
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
iyB
iyB
iyB
iyB
iyB
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
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
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
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
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
w�B
w�B
x�B
x�B
x�B
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
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�pB�B�B�B�wB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B	StBBB�B�B
�uB
��B
��B
W�B
)rB
#�B
B
vB
 �B	�QB	�|B	�`B	��B	��B	��B	�dB	��B	�SB	��B
B
)�B
K|B
~�B
��B
��B
��B
�4B
��B
�vB
�oB
�B
��B
�rB
�cB
��B
�B
�B
ТB
��B
��B
��B
��B
��B
�VB
�B
��B
��B
�?B
�3B
��B
��B
��B
�{B
��B
�)B
׾B
�B
�B
��B
��B
�qB
�
B
�B
��B
�[B
��B
�B
�B
�B
�SB
��B
��B
��B
��B
��B
�B
��B
�B
�B
� B
�B
�B
��B
�B
�9B
�,B
޽B
ݢB
ݍB
��B
ݢB
܋B
ےB
ۘB
��B
��B
��B
كB
ֽB
�2B
ռB
�B
ϽB
ʊB
�;B
��B
��B
�B
�LB
��B
��B
��B
�9B
�qB
ǉB
ȜB
ʀB
˦B
�-B
��B
ȿB
��B
��B
��B
�5B
��B
�LB
�7B
�EB
�	B
��B
��B
��B
�B
�B
��B
�~B
�B
�?B
�SB
�B
�B
��B
��B
�nB
�,B
�1B
��B
�sB
�NB
�6B
��B
�B
�EB
��B
iB
~�B
~OB
z�B
{�B
{�B
|�B
~B
�iB
��B
~�B
��B
�<B
��B
��B
� B
�yB
~�B
�@B
~�B
~uB
�`B
0B
�B
��B
�B
��B
,B
B
!B
~@B
�B
}�B
|UB
{dB
wLB
wB
v$B
u�B
uB
sB
q�B
n�B
k�B
k�B
j�B
i�B
jB
iXB
hcB
fYB
d�B
d6B
c�B
b�B
b-B
aB
_�B
^vB
_-B
]�B
\�B
[�B
[B
Y�B
X�B
X�B
W�B
UvB
T0B
R�B
QQB
P'B
P=B
O�B
P:B
N�B
MeB
MB
M�B
M@B
L5B
L�B
JB
IHB
JB
HB
D�B
EoB
E#B
D:B
C�B
C�B
C�B
BaB
AuB
@nB
>MB
=nB
=�B
>�B
9�B
:6B
8�B
8B
8�B
7wB
7?B
5�B
1eB
.sB
.AB
.�B
.sB
/B
.�B
-XB
,dB
,�B
,�B
+�B
+�B
+�B
*�B
*VB
*=B
)>B
)�B
*B
(�B
'6B
'tB
&B
&1B
%�B
%�B
%�B
$FB
$dB
#B
#�B
#�B
"B
"`B
"hB
"7B
!B
!KB
 .B
 �B
 KB
�B
�B
�B
B
2B
�B
]B
�B
&B
nB
�B
�B
�B
�B
aB
�B
B
�B
�B
^B
 lB
�B
�B
B
B
�B
�B
5B
�B
�B
B
'B
$B
oB
~B
AB
�B
B
/B
GB
�B
?B
B
�B
'B
�B
$B
�B
2B
JB
�B
cB
B
oB
1B
B
�B
DB
PB
YB
?B
�B
�B
 B
�B
CB
�B
�B
B
$B
B
�B
-B
<B
B
.B
�B
-B
zB
�B
/B
IB
�B
(B
�B
+B
jB
wB
B
�B
$B
KB
%B
�B
 �B
"rB
#�B
#�B
#�B
#"B
#sB
#\B
#@B
"�B
$�B
%B
"nB
 �B
 �B
!UB
"6B
"GB
"B
"B
"wB
"qB
#B
$3B
$&B
$B
$B
$ B
#B
$7B
#�B
%�B
%�B
%fB
%pB
&B
&B
&B
&<B
'kB
';B
'gB
'�B
'\B
(xB
(OB
(NB
(�B
(}B
(rB
)B
)�B
*lB
*HB
+zB
,PB
+_B
+B
,~B
-~B
.�B
/dB
/dB
/�B
0�B
0�B
0�B
1B
1�B
1�B
2�B
2�B
2~B
3]B
3�B
3xB
3_B
3LB
3gB
3�B
5B
4�B
5eB
4�B
5�B
5�B
5�B
7B
6�B
6�B
7�B
7�B
7�B
7�B
7B
7�B
7�B
99B
92B
9�B
9�B
9�B
9�B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
=}B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
?�B
@�B
@�B
@�B
A�B
CB
B�B
C2B
B�B
CB
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
GqB
G�B
H/B
G�B
IB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
K<B
K9B
L)B
L�B
L�B
L�B
M3B
M2B
L�B
L�B
M�B
N#B
N*B
M�B
N B
OB
OdB
OUB
PB
P
B
PB
P1B
P"B
PB
PKB
QB
QCB
R!B
RB
RB
R8B
RB
R#B
R^B
SMB
SB
SBB
SKB
SB
TKB
T%B
T B
TB
T B
TB
TB
U*B
UDB
UtB
U)B
U:B
VCB
VdB
V4B
V
B
V B
V[B
W6B
WB
WB
WB
WB
W2B
WMB
WB
WB
W#B
W`B
W'B
X?B
X"B
XB
X%B
X%B
X0B
X`B
XFB
X'B
X'B
XnB
X�B
YoB
YQB
YPB
YPB
YIB
Y�B
ZLB
Z�B
Z�B
[/B
[4B
[aB
[kB
[RB
[_B
[UB
\fB
\oB
\PB
\bB
\yB
\�B
]bB
]lB
]�B
^MB
^AB
^OB
^@B
^DB
^mB
^\B
^tB
^dB
^�B
_oB
_fB
_�B
`�B
`zB
`�B
`�B
a�B
a�B
auB
aYB
a�B
a�B
bjB
b�B
b�B
bWB
b^B
b�B
b�B
c}B
cnB
cdB
c�B
c�B
cdB
cqB
coB
c�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
g/B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
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
j�B
j�B
k�B
k�B
l B
l�B
l�B
l�B
mB
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
pB
qB
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r B
rB
sB
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
vB
u�B
u�B
vB
v�B
v�B
v�B
w"B
v�B
wB
xB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
yB
y	B
yB
x�B
x�B
x�B
y2B
zB
y�B
y�B
z?B
z'B
z!B
z�B
{B
{.B
{B
{B
{B
{�B
| B
|7B
|B
{�B
{�B
{�B
|B
|FB
}KB
}B
|�B
|�B
|�B
|�B
}
B
}B
}3B
~:B
~:B
~;B
&B
$B
)B
=B

B
B
\B
�B
�B
�;B
�iB
�+B
�EB
�>B
�?B
�2B
�HB
�3B
�B
�B
�B
�B
�-B
�QB
�BB
�B
�DB
�JB
�;B
�+B
�B
� B
�/B
�ZB
�SB
��B
�ZB
�5B
�3B
�'B
�(B
�&B
�(B
�:B
�fB
�<B
�.B
�FB
�.B
�*B
�*B
�-B
�UB
�jB
�cB
�.B
�0B
�=B
�1B
�/B
�CB
�BB
�}B
�ZB
�EB
�PB
�OB
�wB
�XB
�4B
�7B
�@B
�JB
�sB
�aB
�vB
��B
�[B
��B
�RB
�FB
�GB
�IB
�IB
�BB
�@B
�RB
�[B
�CB
�JB
�^B
�bB
�dB
�G11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�9�<~ �<K/<#�
<Ix&<#�
<1@�<^'�<><#�
<#�
<#�
<#�
<2��<~ �<#�
<D �<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  202102081350262021020813502620210208135026  AO  ARCAADJP                                                                    20200317090049    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200317090049  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200317090049  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20210208135026  QC  PRES            @�33D�� G�O�                PM  ARSQCTM V1.1                                                                20210208135026  QC  PSAL            @�33D�� G�O�                PM  ARSQCOWGV1.1CTD_2018v2 + Argo_2018v01                                       20210209100527  IP                  G�O�G�O�G�O�                