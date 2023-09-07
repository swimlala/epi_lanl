CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-04-30T17:02:10Z creation      
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
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20180430170210  20230721230916  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�_ߒ�Q1   @�_6�'n@<��O�;�c͉7Kƨ1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C)C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4�
D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�C�D�pR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A���A���A���A���A���A��/A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�A��wA��wA��RA���A��uA��A�7LA���A�\)A�A��A���A��uA��A��A�v�A�S�A�;dA�+A��A��A��wA��uA�jA�S�A�5?A���A��/A���A�A���A���A��A�`BA��A���A���A�%A�A�dZA�oA�|�A�A���A�M�A���A�z�A�A���A��A�I�A���A���A���A��
A�A��A��A�r�A�&�A�/A���A�"�A��
A��wA�A��9A�33A��;A�+A�1'A��9A���A��A��A��^A�=qA�dZA��jA�5?A�A|�A{��Ax��Au��AuƨAu��Aux�Au\)At9XAq�#Ao��An�Al�jAkO�Ai��Ahv�Ae�Ad��Ab��AaK�A`(�A_��A_XA^�A]/A[AYx�AX5?AWXAU�AT��AR�HAR  API�AO�ANffAN9XAMx�ALJAKt�AJ1'AH��AH��AH1'AG|�AG7LAFv�AES�ADZAC��AB��AA��A@��A@5?A?C�A>�jA=��A<ffA;��A;+A:E�A9�
A9�A8�A8ffA8A6�A6E�A5VA333A2�\A1�A1�A133A1oA0�`A.z�A+��A*z�A*A)�mA)�;A)�#A)��A)A)�FA)�A)�hA);dA(�A(�A'/A$~�A$�A"I�A"�A��A��AM�AA�PA�TAG�A��A�AS�A��A�;A��AVA�\AA|�AO�AA1'AXA��A=qAXA��AE�A�TAXA�uAA?}An�AJA
��A	��A=qA�^At�A+A&�A�A$�A�#A��AXA�A=qA��AVA ^5@���@�K�@��@�/@� �@���@�  @���@���@�S�@�J@�@�?}@�bN@�I�@�|�@�-@�(�@��m@���@�F@��y@��@旍@�{@��@�x�@��@�A�@�$�@�O�@�9X@�dZ@�C�@ޗ�@݉7@ܛ�@��@���@�\)@�@ڗ�@��@��T@١�@�/@�Z@ָR@���@��@���@җ�@��@��@�n�@�G�@��@���@��@ɡ�@�%@�Q�@�5?@�I�@��H@�$�@���@���@��@��F@�"�@�v�@��^@���@�K�@���@�E�@���@�Z@��@�|�@�S�@��@��@��@�1@�\)@���@�@��@�z�@��@��\@���@�X@�  @�dZ@�v�@�1'@��@�l�@���@�{@�%@��@�@�=q@�r�@��
@��
@��w@��@���@��@��H@���@�J@�p�@��
@��7@���@�(�@��;@��F@��@��H@�O�@���@�r�@�bN@�I�@�1'@� �@�|�@�=q@�`B@��@�  @�\)@�K�@�l�@�C�@�;d@�"�@�o@��y@�v�@��@�x�@�X@�/@��@�z�@��@�t�@�@��!@�5?@���@��h@��@�x�@�x�@�p�@�p�@��@�+@�@��@��@���@��\@�E�@��@���@���@��^@��-@���@���@��7@�x�@�X@�7L@���@��@�|�@��@��\@��@�O�@�V@��@��/@�Ĝ@��j@��9@��@���@�bN@�@K�@~�R@}�-@}`B@}�@|��@|j@|(�@{�m@{�@{"�@z�@yhs@y%@x�`@xr�@w;d@v�R@v�+@vv�@vv�@vv�@vv�@vV@v@u�-@u`B@u?}@u/@u�@uV@t��@t�@t�/@t�@tj@tj@t1@s��@sS�@r�H@r^5@r=q@q��@qx�@p�9@pA�@o�P@o+@o
=@o
=@o
=@n�y@n5?@m�T@m�T@m�-@mp�@mO�@l�@l(�@k33@ko@j~�@j^5@j�@j�@jJ@i��@i��@i��@i��@iX@h��@h�`@hĜ@g�w@f�R@fV@f5?@f{@f@e�T@ep�@d�/@d9X@d�@d1@c��@c�m@cC�@b��@bM�@a�#@a7L@`��@`bN@` �@`  @_�w@_��@^ȴ@^v�@^V@]�T@]O�@]/@]�@\�@\�/@\��@\�@\j@\Z@\(�@\�@\1@\1@[��@\1@[��@[�
@[�F@[dZ@[@Z��@Z��@Z��@Zn�@Z�@Y��@X��@X��@XQ�@X �@X  @X  @X  @W�;@W�w@W�@WK�@V�@V��@V5?@V@U�-@U��@U�h@Up�@UO�@UV@T�@Tz�@Tj@TZ@TI�@T9X@S��@S�
@S�@Rn�@QG�@Q�@Q�@Q%@P��@P�@P �@P  @O�;@O�;@O�;@O�;@O�w@O�@O��@O;d@Nȴ@N@Mp�@M/@MV@L�@Lz�@Lz�@Lj@Lj@LZ@LZ@LI�@L(�@K�
@KC�@K@J��@J�@I��@I��@I��@I��@I��@Ihs@I%@H��@HĜ@H��@Hr�@G|�@G�@Fȴ@F�R@F��@F{@E�@E/@D�j@DI�@C��@C��@CS�@C"�@B~�@BJ@AG�@@�9@@bN@@A�@@ �@?��@?|�@?+@>ff@=�@=�h@=O�@=?}@<��@<�@<��@<�D@<�D@<z�@<Z@;ƨ@;S�@:�!@:~�@:=q@9�@9�@9��@9&�@9&�@9�@9%@8�9@8Q�@8 �@8  @7��@7l�@7K�@7+@6��@6�@6E�@5�T@5�-@5p�@5V@4�@4�@4Z@4(�@3��@3��@3�F@3t�@333@3@3@2��@2M�@1�#@1��@1x�@1X@1�@0�`@0��@01'@/�@/�@/|�@/K�@/;d@/
=@.�y@.�+@.V@-��@-�h@-`B@-O�@-?}@-?}@-?}@-?}@-?}@-V@,�j@,z�@,Z@,I�@+�m@+�F@+��@+��@+�@+t�@+S�@+S�@+S�@+33@+"�@+"�@+o@+@+@+@+@+@+@+@*�H@*��@*��@*^5@*-@)��@)��@)G�@(��@(�`@(�`@(�`@(Ĝ@(r�@(b@'�w@'�P@'l�@';d@&ȴ@&��@&v�@&ff@&V@&E�@&5?@&{@%�@%��@%�-@%�h@%p�@%�@$Z@#�@"�@"��@"�!@"��@"=q@!��@!�@!�^@!hs@ ��@   @l�@+@�@
=@�y@�y@�y@�y@�y@�y@�y@�@�@�@ȴ@�+@@��@/@Z@1@��@C�@@��@�!@~�@�@��@7L@Ĝ@bN@Q�@A�@ �@�@l�@+@
=@�R@E�@5?@$�@�@@��@�@`B@`B@?}@V@�@z�@Z@Z@Z@I�@(�@�@��@�m@�
@��@�@dZ@dZ@33@�H@��@~�@n�@-@�#@�#@�#@��@��@�7@x�@X@7L@�`@Ĝ@�@A�@b@b@b@ �@ �@b@�@�w@��@�P@\)@\)@;d@+@
=@ȴ@ȴ@ȴ@�R@��@��@ff@$�@{@@@�@�T@�-@�h@/@�@�/@�@I�@9X@(�@�@�@1@1@1@��@ƨ@t�@C�@"�@@
��@
��@
��@
�!@
M�@
J@	�@	��@	hs@	&�@	&�@	�@	�@Ĝ@�u@�@r�@Q�@Q�@Q�@Q�@A�@b@�@�w@�w@�@�@|�@�@ȴ@�R@�R@�R@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A���A���A���A���A���A��/A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�A��wA��wA��RA���A��uA��A�7LA���A�\)A�A��A���A��uA��A��A�v�A�S�A�;dA�+A��A��A��wA��uA�jA�S�A�5?A���A��/A���A�A���A���A��A�`BA��A���A���A�%A�A�dZA�oA�|�A�A���A�M�A���A�z�A�A���A��A�I�A���A���A���A��
A�A��A��A�r�A�&�A�/A���A�"�A��
A��wA�A��9A�33A��;A�+A�1'A��9A���A��A��A��^A�=qA�dZA��jA�5?A�A|�A{��Ax��Au��AuƨAu��Aux�Au\)At9XAq�#Ao��An�Al�jAkO�Ai��Ahv�Ae�Ad��Ab��AaK�A`(�A_��A_XA^�A]/A[AYx�AX5?AWXAU�AT��AR�HAR  API�AO�ANffAN9XAMx�ALJAKt�AJ1'AH��AH��AH1'AG|�AG7LAFv�AES�ADZAC��AB��AA��A@��A@5?A?C�A>�jA=��A<ffA;��A;+A:E�A9�
A9�A8�A8ffA8A6�A6E�A5VA333A2�\A1�A1�A133A1oA0�`A.z�A+��A*z�A*A)�mA)�;A)�#A)��A)A)�FA)�A)�hA);dA(�A(�A'/A$~�A$�A"I�A"�A��A��AM�AA�PA�TAG�A��A�AS�A��A�;A��AVA�\AA|�AO�AA1'AXA��A=qAXA��AE�A�TAXA�uAA?}An�AJA
��A	��A=qA�^At�A+A&�A�A$�A�#A��AXA�A=qA��AVA ^5@���@�K�@��@�/@� �@���@�  @���@���@�S�@�J@�@�?}@�bN@�I�@�|�@�-@�(�@��m@���@�F@��y@��@旍@�{@��@�x�@��@�A�@�$�@�O�@�9X@�dZ@�C�@ޗ�@݉7@ܛ�@��@���@�\)@�@ڗ�@��@��T@١�@�/@�Z@ָR@���@��@���@җ�@��@��@�n�@�G�@��@���@��@ɡ�@�%@�Q�@�5?@�I�@��H@�$�@���@���@��@��F@�"�@�v�@��^@���@�K�@���@�E�@���@�Z@��@�|�@�S�@��@��@��@�1@�\)@���@�@��@�z�@��@��\@���@�X@�  @�dZ@�v�@�1'@��@�l�@���@�{@�%@��@�@�=q@�r�@��
@��
@��w@��@���@��@��H@���@�J@�p�@��
@��7@���@�(�@��;@��F@��@��H@�O�@���@�r�@�bN@�I�@�1'@� �@�|�@�=q@�`B@��@�  @�\)@�K�@�l�@�C�@�;d@�"�@�o@��y@�v�@��@�x�@�X@�/@��@�z�@��@�t�@�@��!@�5?@���@��h@��@�x�@�x�@�p�@�p�@��@�+@�@��@��@���@��\@�E�@��@���@���@��^@��-@���@���@��7@�x�@�X@�7L@���@��@�|�@��@��\@��@�O�@�V@��@��/@�Ĝ@��j@��9@��@���@�bN@�@K�@~�R@}�-@}`B@}�@|��@|j@|(�@{�m@{�@{"�@z�@yhs@y%@x�`@xr�@w;d@v�R@v�+@vv�@vv�@vv�@vv�@vV@v@u�-@u`B@u?}@u/@u�@uV@t��@t�@t�/@t�@tj@tj@t1@s��@sS�@r�H@r^5@r=q@q��@qx�@p�9@pA�@o�P@o+@o
=@o
=@o
=@n�y@n5?@m�T@m�T@m�-@mp�@mO�@l�@l(�@k33@ko@j~�@j^5@j�@j�@jJ@i��@i��@i��@i��@iX@h��@h�`@hĜ@g�w@f�R@fV@f5?@f{@f@e�T@ep�@d�/@d9X@d�@d1@c��@c�m@cC�@b��@bM�@a�#@a7L@`��@`bN@` �@`  @_�w@_��@^ȴ@^v�@^V@]�T@]O�@]/@]�@\�@\�/@\��@\�@\j@\Z@\(�@\�@\1@\1@[��@\1@[��@[�
@[�F@[dZ@[@Z��@Z��@Z��@Zn�@Z�@Y��@X��@X��@XQ�@X �@X  @X  @X  @W�;@W�w@W�@WK�@V�@V��@V5?@V@U�-@U��@U�h@Up�@UO�@UV@T�@Tz�@Tj@TZ@TI�@T9X@S��@S�
@S�@Rn�@QG�@Q�@Q�@Q%@P��@P�@P �@P  @O�;@O�;@O�;@O�;@O�w@O�@O��@O;d@Nȴ@N@Mp�@M/@MV@L�@Lz�@Lz�@Lj@Lj@LZ@LZ@LI�@L(�@K�
@KC�@K@J��@J�@I��@I��@I��@I��@I��@Ihs@I%@H��@HĜ@H��@Hr�@G|�@G�@Fȴ@F�R@F��@F{@E�@E/@D�j@DI�@C��@C��@CS�@C"�@B~�@BJ@AG�@@�9@@bN@@A�@@ �@?��@?|�@?+@>ff@=�@=�h@=O�@=?}@<��@<�@<��@<�D@<�D@<z�@<Z@;ƨ@;S�@:�!@:~�@:=q@9�@9�@9��@9&�@9&�@9�@9%@8�9@8Q�@8 �@8  @7��@7l�@7K�@7+@6��@6�@6E�@5�T@5�-@5p�@5V@4�@4�@4Z@4(�@3��@3��@3�F@3t�@333@3@3@2��@2M�@1�#@1��@1x�@1X@1�@0�`@0��@01'@/�@/�@/|�@/K�@/;d@/
=@.�y@.�+@.V@-��@-�h@-`B@-O�@-?}@-?}@-?}@-?}@-?}@-V@,�j@,z�@,Z@,I�@+�m@+�F@+��@+��@+�@+t�@+S�@+S�@+S�@+33@+"�@+"�@+o@+@+@+@+@+@+@+@*�H@*��@*��@*^5@*-@)��@)��@)G�@(��@(�`@(�`@(�`@(Ĝ@(r�@(b@'�w@'�P@'l�@';d@&ȴ@&��@&v�@&ff@&V@&E�@&5?@&{@%�@%��@%�-@%�h@%p�@%�@$Z@#�@"�@"��@"�!@"��@"=q@!��@!�@!�^@!hs@ ��@   @l�@+@�@
=@�y@�y@�y@�y@�y@�y@�y@�@�@�@ȴ@�+@@��@/@Z@1@��@C�@@��@�!@~�@�@��@7L@Ĝ@bN@Q�@A�@ �@�@l�@+@
=@�R@E�@5?@$�@�@@��@�@`B@`B@?}@V@�@z�@Z@Z@Z@I�@(�@�@��@�m@�
@��@�@dZ@dZ@33@�H@��@~�@n�@-@�#@�#@�#@��@��@�7@x�@X@7L@�`@Ĝ@�@A�@b@b@b@ �@ �@b@�@�w@��@�P@\)@\)@;d@+@
=@ȴ@ȴ@ȴ@�R@��@��@ff@$�@{@@@�@�T@�-@�h@/@�@�/@�@I�@9X@(�@�@�@1@1@1@��@ƨ@t�@C�@"�@@
��@
��@
��@
�!@
M�@
J@	�@	��@	hs@	&�@	&�@	�@	�@Ĝ@�u@�@r�@Q�@Q�@Q�@Q�@A�@b@�@�w@�w@�@�@|�@�@ȴ@�R@�R@�R@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�B��B��B��B��B�hB�=B�B� B}�Bx�Bs�BffBVBR�BN�BB�B2-B�B+B�B�HB�
BB�B��B��B�BgmBR�BH�BC�B;dB49B(�B�B
��B
�yB
��B
��B
�dB
�FB
�B
��B
��B
�%B
q�B
ffB
S�B
D�B
B�B
A�B
@�B
>wB
6FB
%�B
�B
PB
B	��B	�B	�mB	�B	��B	ĜB	�LB	�B	�B	��B	��B	��B	�bB	�B	{�B	t�B	iyB	aHB	S�B	L�B	C�B	B�B	B�B	B�B	?}B	;dB	:^B	6FB	1'B	0!B	-B	)�B	(�B	$�B	�B	�B	�B	bB	1B	B��B��B�B�B�B�B�B�mB�fB�ZB�BB�;B�BB�)B�
B��BĜBB��B�wB�qB�jB�XB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�1B�B~�Bv�Bt�Bs�Br�Bp�Bn�Bm�Bk�BjBiyBhsBgmBffBffBe`Be`BdZBcTBcTBbNBaHB`BB_;B_;B^5B^5B^5B]/B\)BZBZBYBXBVBT�BR�BQ�BP�BO�BM�BM�BK�BK�BJ�BJ�BI�BI�BH�BG�BE�BD�BD�BD�BC�BC�BA�B@�B?}B?}B?}B?}B?}B>wB>wB>wB=qB<jB;dB:^B:^B9XB8RB7LB8RB8RB8RB8RB7LB7LB7LB7LB7LB7LB8RB7LB7LB8RB8RB9XB9XB9XB9XB9XB9XB9XB9XB8RB9XB;dB<jB;dB9XB:^B<jB<jB<jB>wB@�BB�BB�BB�BB�BA�BD�BF�BH�BI�BJ�BK�BK�BK�BL�BM�BN�BQ�BS�BT�BT�BT�BW
BYBYBZBZBZB^5BaHBbNBdZBffBk�Bm�Bo�Br�Bu�Bv�Bx�Bx�By�B~�B� B� B�B�B�1B�JB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�3B�9B�^B�jB�qB�qB�qB�qB�jB�wB��BĜBɺB��B��B��B�B�5B�BB�NB�NB�NB�ZB�`B�fB�fB�fB�`B�`B�mB�B�B�B�B�B�B�B�B�B�B�B��B	B	B	B	%B	+B		7B	PB	VB	oB	oB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	&�B	)�B	,B	-B	-B	-B	.B	.B	/B	0!B	49B	8RB	:^B	<jB	?}B	@�B	A�B	C�B	E�B	G�B	I�B	K�B	L�B	P�B	S�B	T�B	T�B	W
B	\)B	`BB	aHB	bNB	bNB	bNB	bNB	cTB	e`B	ffB	gmB	hsB	hsB	iyB	iyB	iyB	jB	jB	k�B	l�B	l�B	m�B	n�B	p�B	q�B	s�B	t�B	u�B	w�B	|�B	~�B	�B	�%B	�%B	�%B	�%B	�+B	�PB	�\B	�\B	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�-B	�9B	�LB	�RB	�RB	�RB	�XB	�dB	�qB	�wB	��B	ÖB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B

=B
DB
JB
JB
JB
JB
JB
PB
VB
VB
VB
\B
\B
bB
hB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
,B
,B
-B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
aHB
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
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
iyB
iyB
jB
iyB
jB
jB
jB
jB
jB
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
l�B
l�B
l�B
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
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
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
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B�4B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�4B�_B�;B�B�5B��B�aB�7B�4B��B��B��B��B�bB��B��B��B�)B�NB��B�XB��B��B��B��B�HB�[B�B��B�5B�~B��B�sB��B��Bz�Bw�BktBW�BTQBR�BGSB7UB"B�B��B�tB�hB��B��B�$B�YB�BkqBU�BI�BE�B<�B7B-B{B�B
�B
��B
�QB
��B
�B
��B
�*B
��B
��B
t�B
mGB
ZB
EB
B�B
A�B
@�B
A�B
<;B
+\B
UB
�B
�B	��B	�%B	�nB	�yB	�B	��B	� B	�B	�0B	�(B	��B	�0B	�{B	�UB	~2B	x�B	l�B	euB	U�B	PB	E�B	C�B	B�B	C�B	B;B	<�B	<�B	8�B	1�B	1DB	.�B	*�B	*�B	'XB	�B	�B	\B	IB		�B	�B	 B�1B�vB�aB�TB��B�B�B�8B��B�B�BB��B�B�QB�>B�)B�B��B�0B��B�B��B��B�B��B��B��B��B��B��B��B��B��B�dB�<B�9B��B��B�,B�yB�9Bw$Bu�Bt�Bs_Bq�Br�BoBl�Bl|BktBjYBiVBg*Bg�Bf�Bf�Be�Bc�Bd8Bd�Bc�BaTBaQBa�B`B_RB_WB^�B^`B\kB[�B[`BYEBYBX�BV�BSMBQ�BP�BS>BO�BL�BL�BKiBKzBJ�BK(BJBH�BGBE�BEBEBEqBD�BC.BCXBBB@8BAYB@�B?�B?B?}B>�B>|B>!B=�B:�B:�B9�B9�B;�B9�B9B8�B8�B7�B8�B:xB8�B8�B8�B8�B8_B8�B9�B9B9�B:B9�B:	B:B9�B9�B:"B9�B;�B<�B=�B=gB;GB<�B>B>�B>7B@UBB{BC�BCBCoBC�BD�BGPBH�BI�BJnBK�BL�BLOBL�BM�BN�BPWBS�BT�BU�BU�BV�BW�BY[BYSBZ�BZTB\uB_�Bb(BcBerBg�Bl'Bn�Bq$Bs�BvrBx�By�Bz?B|�B�B�_B��B�B��B��B��B��B��B�kB��B��B��B��B��B�TB��B�bB�zB��B��B��B�B�zB�[B�iB��B�B�.B��B��B��B��B��B�TB�B½B�BB��B˟B��B��B�VB�BB�fB�kB�B�B�%B�B�B�B�B�YB�3B�)B�.B�B�YB�qB��B��B��B�B��B��B��B�KB	EB	.B	@B	BB	aB		�B	~B	�B	zB	�B	~B	�B	�B	�B	�B	�B	�B	B	�B	?B	@B	!�B	$�B	'�B	*RB	,.B	- B	-,B	-B	.B	.B	/4B	0mB	4�B	8�B	:�B	=B	?�B	@�B	A�B	C�B	E�B	G�B	JB	LB	M�B	Q_B	T?B	UB	UTB	W�B	\�B	`eB	aTB	bMB	bOB	bOB	bfB	c�B	e�B	f�B	g�B	h}B	h}B	i�B	i�B	i�B	j�B	j�B	k�B	l�B	l�B	m�B	n�B	p�B	rB	s�B	t�B	v-B	x_B	}HB	�B	�_B	�<B	�&B	�&B	�EB	��B	��B	�_B	��B	��B	��B	��B	�B	�CB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�oB	��B	�FB	�-B	�.B	�*B	�4B	�fB	��B	��B	�`B	�[B	�]B	�aB	��B	��B	��B	��B	��B	��B	��B	��B	ǽB	��B	��B	�@B	�B	��B	�B	�7B	��B	��B	�B	��B	��B	�	B	�B	�B	�B	��B	�B	��B	�B	��B	�B	�B	�B	�CB	�RB	�9B	�B	�"B	�XB	�]B	ݔB	ޱB	�B	�B	�sB	�hB	�SB	�VB	�pB	�xB	�rB	�B	��B	�B	�B	�B	�B	�~B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�*B	��B	��B	��B	��B	��B	�B	��B	��B	�HB	�QB
 �B
}B
NB
4B
lB
KB
&B
,B
#B
1B
)B
6B
FB
rB
�B
	lB
	cB

�B
�B
fB
SB
GB
IB
hB
�B
rB
bB
kB
zB
�B
�B
�B
wB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
B
�B
�B
�B
�B
�B
�B
�B
 =B
#B
$B
%B
%�B
&,B
&�B
&�B
&�B
&�B
&�B
&�B
(QB
)=B
*dB
,(B
,0B
-@B
-B
-BB
-ZB
.B
.B
.B
/MB
0bB
0DB
1;B
1IB
1iB
2EB
2EB
2NB
3IB
3�B
4}B
4]B
4gB
5�B
5XB
5kB
6�B
6iB
6eB
6DB
7uB
7{B
7}B
8sB
8UB
8�B
8�B
8�B
8bB
9�B
9pB
9�B
9}B
9fB
:�B
<�B
<�B
<�B
<�B
=wB
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?|B
?yB
?{B
?|B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
GB
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J	B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
MUB
NeB
O<B
PB
O�B
O�B
PB
PB
P�B
QB
QB
QHB
RwB
SRB
T"B
UB
UB
UB
UB
T�B
T�B
U B
T�B
T�B
UB
T�B
T�B
U	B
U,B
UWB
UAB
VUB
V�B
XHB
X]B
YNB
YCB
Z=B
Z6B
Z>B
Z]B
[pB
[tB
\wB
\oB
]<B
]9B
]GB
^�B
^eB
^fB
_VB
_yB
_�B
`OB
`OB
`gB
`iB
`[B
a_B
a^B
aHB
aaB
amB
bhB
b�B
ckB
cRB
cTB
cbB
ckB
c_B
ckB
c`B
caB
cxB
dpB
dmB
dZB
dvB
d�B
e�B
euB
eiB
e�B
f�B
ffB
feB
frB
f{B
fzB
guB
g}B
g�B
g�B
g�B
h�B
h�B
h�B
izB
ixB
ioB
ixB
hzB
h�B
i�B
i�B
i�B
i�B
j~B
i�B
j�B
j�B
j�B
j~B
jB
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
l�B
l�B
l�B
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
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
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
s�B
s�B
s�B
s�B
u8B
u�B
u�B
u�B
u�B
u�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810310930162018103109301620181031093016  AO  ARCAADJP                                                                    20180430170210    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180430170210  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180430170210  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181031093016  QC  PRES            @�  D�p G�O�                PM  ARSQCTM V1.1                                                                20181031093016  QC  PSAL            @�  D�p G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230916  IP                  G�O�G�O�G�O�                