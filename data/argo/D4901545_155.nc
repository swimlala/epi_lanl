CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-03T17:04:10Z creation      
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
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20170803170410  20181025093509  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @����=�1   @���/vl@:�     �cH���S�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B p�Bp�B
=B
=B��B(
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �Dz>D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�C�DրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�}D��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�r�A�hsA�Q�A�l�A�`BA��A��A��mAվwAՑhAՃA�l�A�S�A�C�A�1'A�VA��A�7LA���A�ƨA�~�A�$�A�oA�1'A�"�A�v�A�jA�JA�S�A�S�A�ZA��;A�z�A�jA���A���A�n�A� �A�O�A��`A��-A� �A�`BA��A���A��A��mA�|�A�A���A�bNA���A�S�A���A�bNA�=qA��jA��;A�-A�^5A��A��uA� �A��9A���A��TA��7A��9A���A���A��wA�5?A���A�"�A�5?A���A�bA���A�=qA��A�VA�I�A��!A���A�33A��jA�M�A���A�z�A�&�A���A���A�XA��mA�dZA���A�`BA��RA�I�A��wA�9XA��yA�n�A���A�S�A�A��A~�A~I�A}|�A|bAz5?Ax�Avn�AuAt5?Ar�\Aq"�An�yAl�`Ak�#Ak33Ai\)AeC�Ac�#Ab�!A`M�A_hsA^n�A]`BA\v�A\�A[�hAZv�AX�/AV��AUAT��ARbNAQS�AP�!API�AO;dAN~�AMoAJ�`AI�AI�AG�wAF�AFJAE�PAE/AD��ADbAC��AC7LAB��AA�AA;dA@��A?�FA>VA=��A=VA<��A<(�A<�A;�7A9�wA8�A8VA7��A7��A7�7A7/A5��A5K�A4M�A3��A2�\A1��A1+A0�!A/�A.M�A-;dA,��A+��A*��A*z�A*�A(ĜA';dA&M�A%hsA$��A$��A$ffA#��A#"�A"�jA"1A!�-A!|�A!�A �DA�A�Az�A-A��A33A�!AbNAE�A�wA�RA�FA/A�DA��A&�AM�A?}A��A��A��A��Ar�A�7A��A�PA/AȴA�A�hA`BA��A�+A�-A
�jA
ZA	��A	+Az�A|�A%AZAC�A�!A��A-Ap�AS�A �!@���@�G�@��@���@���@�~�@���@�  @��@���@��@�t�@홚@�u@�S�@�`B@�9X@�"�@��#@�?}@�9@�F@�=q@���@��@�J@��@ܼj@۝�@�ff@�hs@�1'@���@�X@��@�~�@љ�@Ͼw@�ff@�G�@��;@�$�@�&�@��m@Ə\@��@ċD@��@�p�@�b@�
=@��@��@�ff@��@�|�@�`B@��F@�@�X@��@��P@�M�@��@��w@�|�@��@��@�ff@���@�b@��w@��@��+@���@���@���@��@�1'@��w@��H@�ff@�@���@�O�@�V@���@�r�@��@�\)@�^5@��@��-@��7@��@��@�I�@��@�^5@��^@�&�@���@�b@�l�@�+@���@��#@�p�@�7L@�Z@� �@��F@�S�@��H@�^5@�@��^@�7L@���@�z�@��@���@��@��P@�l�@�"�@���@�ff@��@���@���@��7@�?}@�%@��/@�Ĝ@��@�Z@�b@��@��P@�;d@�
=@���@�v�@�E�@�@���@�&�@��u@�I�@��;@��@�l�@�"�@�E�@���@��@��T@���@�hs@��@���@��D@�j@� �@�1@��@��;@��
@���@���@���@��@�;d@��H@��\@�-@��-@�x�@�G�@�7L@�V@���@��`@���@��@�(�@�@~ȴ@~��@~@}O�@|�/@|�@{t�@z�\@y��@x��@xA�@w��@w��@w;d@v�+@v5?@v$�@v{@u�@u��@u`B@t�D@s��@s�
@sƨ@s�F@s��@st�@s33@s@r��@rJ@q�@q�#@r~�@sƨ@s�
@s�m@s��@r�H@r��@r�@q7L@pĜ@p �@p  @p�9@qX@qhs@q�@p�`@q�7@qx�@qx�@qhs@pĜ@p�u@p�@p  @nv�@n@lj@l9X@k�
@kt�@k33@j�H@j=q@i��@i&�@h�`@hb@g�w@g��@h�9@h�9@h�@hbN@hA�@g��@fV@e�h@eO�@e�@eV@d�@dz�@c��@c��@ct�@ct�@c�@cdZ@cdZ@dI�@d9X@d1@c�m@c��@c"�@b��@b��@b~�@b�!@b��@b��@b^5@a�7@a%@`�`@`�`@`��@`�u@`b@_�@_��@_\)@_
=@^ff@^@^{@]�@]O�@\��@]?}@\��@\�@\(�@[�
@[��@[t�@[33@Z�H@Z��@Z��@Z-@Z�@Z�H@Y�@Y�#@ZM�@Z�H@[o@Z�H@Z~�@Z-@ZM�@ZM�@ZJ@Y�7@X�`@X�u@XA�@Wl�@V�+@UO�@U��@U�T@U�h@U`B@U`B@U?}@T��@TZ@T9X@T9X@T9X@S�m@S�F@S�@SS�@S@R�!@R~�@RM�@Q��@Q�#@Q��@Qhs@Q&�@P��@P �@O��@O+@O�@O�@N��@N�@N�R@N�+@N$�@M@M�h@M?}@L�/@L�j@L��@L9X@K��@KC�@J�H@J��@Jn�@J=q@I�#@Ihs@I%@H��@HbN@G��@G��@Gl�@G+@G�@F��@F�R@F��@Fff@FE�@F$�@F@E�-@E`B@EO�@E/@D��@D(�@C�F@C�@Ct�@C33@B��@B�\@A��@A��@A�7@Ax�@AG�@A%@@�9@@�@?�;@?�P@?|�@?l�@?K�@?;d@>�@>��@>5?@>@=��@=`B@<��@<j@<j@<j@<I�@;ƨ@;�@;33@;@:��@:n�@:J@9hs@9%@8��@8�9@8�u@8�@8�@8�@8r�@8Q�@81'@7�@7�P@6�@6v�@6$�@5@5�@4�j@4Z@4(�@4�@41@3�F@3t�@3@2�H@2�\@2�@1��@1��@1�@1��@1hs@1�@0�9@0�u@0�@0r�@0Q�@/�;@/�@/l�@/;d@/�@.��@.ȴ@.�+@.V@-��@-�h@-�@,�/@,�j@,I�@,�@,�@,1@,1@+��@+�m@+ƨ@+�F@+dZ@+C�@+o@*�H@*�!@*=q@)�#@)��@)��@)x�@)7L@)%@(�u@(bN@(A�@'�@'�@'l�@'K�@';d@'+@&��@&v�@&5?@&{@%��@%�h@$��@$�D@$Z@$1@#��@#dZ@#C�@"��@"��@"~�@"n�@"^5@"-@!�7@!&�@!�@!%@ Ĝ@ r�@ Q�@  �@ b@�@��@��@K�@�@
=@��@ȴ@��@�+@�+@V@$�@@��@�-@�h@O�@/@�/@z�@1@�
@ƨ@�@S�@o@�@��@��@~�@^5@M�@-@J@��@��@�^@��@�7@x�@X@&�@�@%@��@�9@�@bN@A�@ �@  @�@�;@�;@��@|�@K�@
=@�y@v�@E�@@@�-@��@��@�h@p�@p�@`B@O�@�@�/@��@�j@�j@�j@�@�D@j@9X@(�@�m@dZ@o@�@�H@��@~�@M�@-@�@�@�@�^@��@�7@7L@�@�@�`@�9@��@�u@�@r�@A�@ �@��@�P@|�@\)@\)@K�@;d@�@�R@��@5?@@@�@?}@�@�/@��@�@Z@9X@��@�
@�
@ƨ@��@�@dZ@"�@
��@
��@
~�@
n�@
M�@
M�@
=q@
-@
�@	��@	�@	�@	�@	�@	��@	�7@	G�@	�@��@�9@�@�@r�@r�@bN@A�@  @�;@�@K�@K�@;d@�@
=@��@�y@�@��@5?@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�r�A�r�A�hsA�Q�A�l�A�`BA��A��A��mAվwAՑhAՃA�l�A�S�A�C�A�1'A�VA��A�7LA���A�ƨA�~�A�$�A�oA�1'A�"�A�v�A�jA�JA�S�A�S�A�ZA��;A�z�A�jA���A���A�n�A� �A�O�A��`A��-A� �A�`BA��A���A��A��mA�|�A�A���A�bNA���A�S�A���A�bNA�=qA��jA��;A�-A�^5A��A��uA� �A��9A���A��TA��7A��9A���A���A��wA�5?A���A�"�A�5?A���A�bA���A�=qA��A�VA�I�A��!A���A�33A��jA�M�A���A�z�A�&�A���A���A�XA��mA�dZA���A�`BA��RA�I�A��wA�9XA��yA�n�A���A�S�A�A��A~�A~I�A}|�A|bAz5?Ax�Avn�AuAt5?Ar�\Aq"�An�yAl�`Ak�#Ak33Ai\)AeC�Ac�#Ab�!A`M�A_hsA^n�A]`BA\v�A\�A[�hAZv�AX�/AV��AUAT��ARbNAQS�AP�!API�AO;dAN~�AMoAJ�`AI�AI�AG�wAF�AFJAE�PAE/AD��ADbAC��AC7LAB��AA�AA;dA@��A?�FA>VA=��A=VA<��A<(�A<�A;�7A9�wA8�A8VA7��A7��A7�7A7/A5��A5K�A4M�A3��A2�\A1��A1+A0�!A/�A.M�A-;dA,��A+��A*��A*z�A*�A(ĜA';dA&M�A%hsA$��A$��A$ffA#��A#"�A"�jA"1A!�-A!|�A!�A �DA�A�Az�A-A��A33A�!AbNAE�A�wA�RA�FA/A�DA��A&�AM�A?}A��A��A��A��Ar�A�7A��A�PA/AȴA�A�hA`BA��A�+A�-A
�jA
ZA	��A	+Az�A|�A%AZAC�A�!A��A-Ap�AS�A �!@���@�G�@��@���@���@�~�@���@�  @��@���@��@�t�@홚@�u@�S�@�`B@�9X@�"�@��#@�?}@�9@�F@�=q@���@��@�J@��@ܼj@۝�@�ff@�hs@�1'@���@�X@��@�~�@љ�@Ͼw@�ff@�G�@��;@�$�@�&�@��m@Ə\@��@ċD@��@�p�@�b@�
=@��@��@�ff@��@�|�@�`B@��F@�@�X@��@��P@�M�@��@��w@�|�@��@��@�ff@���@�b@��w@��@��+@���@���@���@��@�1'@��w@��H@�ff@�@���@�O�@�V@���@�r�@��@�\)@�^5@��@��-@��7@��@��@�I�@��@�^5@��^@�&�@���@�b@�l�@�+@���@��#@�p�@�7L@�Z@� �@��F@�S�@��H@�^5@�@��^@�7L@���@�z�@��@���@��@��P@�l�@�"�@���@�ff@��@���@���@��7@�?}@�%@��/@�Ĝ@��@�Z@�b@��@��P@�;d@�
=@���@�v�@�E�@�@���@�&�@��u@�I�@��;@��@�l�@�"�@�E�@���@��@��T@���@�hs@��@���@��D@�j@� �@�1@��@��;@��
@���@���@���@��@�;d@��H@��\@�-@��-@�x�@�G�@�7L@�V@���@��`@���@��@�(�@�@~ȴ@~��@~@}O�@|�/@|�@{t�@z�\@y��@x��@xA�@w��@w��@w;d@v�+@v5?@v$�@v{@u�@u��@u`B@t�D@s��@s�
@sƨ@s�F@s��@st�@s33@s@r��@rJ@q�@q�#@r~�@sƨ@s�
@s�m@s��@r�H@r��@r�@q7L@pĜ@p �@p  @p�9@qX@qhs@q�@p�`@q�7@qx�@qx�@qhs@pĜ@p�u@p�@p  @nv�@n@lj@l9X@k�
@kt�@k33@j�H@j=q@i��@i&�@h�`@hb@g�w@g��@h�9@h�9@h�@hbN@hA�@g��@fV@e�h@eO�@e�@eV@d�@dz�@c��@c��@ct�@ct�@c�@cdZ@cdZ@dI�@d9X@d1@c�m@c��@c"�@b��@b��@b~�@b�!@b��@b��@b^5@a�7@a%@`�`@`�`@`��@`�u@`b@_�@_��@_\)@_
=@^ff@^@^{@]�@]O�@\��@]?}@\��@\�@\(�@[�
@[��@[t�@[33@Z�H@Z��@Z��@Z-@Z�@Z�H@Y�@Y�#@ZM�@Z�H@[o@Z�H@Z~�@Z-@ZM�@ZM�@ZJ@Y�7@X�`@X�u@XA�@Wl�@V�+@UO�@U��@U�T@U�h@U`B@U`B@U?}@T��@TZ@T9X@T9X@T9X@S�m@S�F@S�@SS�@S@R�!@R~�@RM�@Q��@Q�#@Q��@Qhs@Q&�@P��@P �@O��@O+@O�@O�@N��@N�@N�R@N�+@N$�@M@M�h@M?}@L�/@L�j@L��@L9X@K��@KC�@J�H@J��@Jn�@J=q@I�#@Ihs@I%@H��@HbN@G��@G��@Gl�@G+@G�@F��@F�R@F��@Fff@FE�@F$�@F@E�-@E`B@EO�@E/@D��@D(�@C�F@C�@Ct�@C33@B��@B�\@A��@A��@A�7@Ax�@AG�@A%@@�9@@�@?�;@?�P@?|�@?l�@?K�@?;d@>�@>��@>5?@>@=��@=`B@<��@<j@<j@<j@<I�@;ƨ@;�@;33@;@:��@:n�@:J@9hs@9%@8��@8�9@8�u@8�@8�@8�@8r�@8Q�@81'@7�@7�P@6�@6v�@6$�@5@5�@4�j@4Z@4(�@4�@41@3�F@3t�@3@2�H@2�\@2�@1��@1��@1�@1��@1hs@1�@0�9@0�u@0�@0r�@0Q�@/�;@/�@/l�@/;d@/�@.��@.ȴ@.�+@.V@-��@-�h@-�@,�/@,�j@,I�@,�@,�@,1@,1@+��@+�m@+ƨ@+�F@+dZ@+C�@+o@*�H@*�!@*=q@)�#@)��@)��@)x�@)7L@)%@(�u@(bN@(A�@'�@'�@'l�@'K�@';d@'+@&��@&v�@&5?@&{@%��@%�h@$��@$�D@$Z@$1@#��@#dZ@#C�@"��@"��@"~�@"n�@"^5@"-@!�7@!&�@!�@!%@ Ĝ@ r�@ Q�@  �@ b@�@��@��@K�@�@
=@��@ȴ@��@�+@�+@V@$�@@��@�-@�h@O�@/@�/@z�@1@�
@ƨ@�@S�@o@�@��@��@~�@^5@M�@-@J@��@��@�^@��@�7@x�@X@&�@�@%@��@�9@�@bN@A�@ �@  @�@�;@�;@��@|�@K�@
=@�y@v�@E�@@@�-@��@��@�h@p�@p�@`B@O�@�@�/@��@�j@�j@�j@�@�D@j@9X@(�@�m@dZ@o@�@�H@��@~�@M�@-@�@�@�@�^@��@�7@7L@�@�@�`@�9@��@�u@�@r�@A�@ �@��@�P@|�@\)@\)@K�@;d@�@�R@��@5?@@@�@?}@�@�/@��@�@Z@9X@��@�
@�
@ƨ@��@�@dZ@"�@
��@
��@
~�@
n�@
M�@
M�@
=q@
-@
�@	��@	�@	�@	�@	�@	��@	�7@	G�@	�@��@�9@�@�@r�@r�@bN@A�@  @�;@�@K�@K�@;d@�@
=@��@�y@�@��@5?@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB?}B?}B<jB'�B�BDBDBPBoBuBhBoB{B�B�B�B�B�B%�B�uB{�BaHBu�B�B�B|�By�Bv�B� B�JB�JB�}BȴBɺB��B�BhB�B\B+B  B��B�B�#B�'B��B�\B��B��B��B�oB{�Bu�BffB[#BK�B?}B?}B>wB=qB=qB=qB:^B+BuBB��B�B�;B��B��B�FB�RB�FB�B��B��B�VB�1B�B�B~�B|�Bu�BhsB\)BS�B_;BT�BT�BN�BC�B8RB-B#�B�B\B+B
��B
�B
�fB
�)B
�B
��B
ÖB
�LB
�3B
�B
��B
��B
��B
�1B
u�B
_;B
P�B
D�B
<jB
.B
�B
PB	��B	�B	�B	�B	�9B	��B	��B	�=B	�B	|�B	v�B	p�B	n�B	jB	bNB	YB	O�B	I�B	D�B	7LB	5?B	5?B	6FB	1'B	,B	#�B	�B	DB	B	  B��B��B��B��B��B�B�B�B�sB�ZB�HB�;B�)B�
B��B��B��B��B��BƨB�XB�-B�!B�3B�XB�^B�XB�dB�jB�^B�XB�LB�9B�-B�'B�B�B��B��B��B��B��B��B�oB�7B�B�B�B�B� B�B~�B|�B{�By�Bx�Bu�Bs�Br�Bp�Bo�Bn�Bn�Bn�Bn�Bl�Bk�BiyBffBcTBaHB_;B\)BYBVBT�BT�BT�BS�BQ�BN�BJ�BF�B@�B=qB=qB<jB=qB=qB<jB;dB:^B9XB9XB8RB6FB49B33B2-B0!B.B-B)�B'�B&�B%�B#�B"�B �B�B�B�B�B�B�B�B�B{BoBhB\BPBDB	7B	7B1B+B%B%B%B%BB%BBBBBBBBBBB  BBBB  B  B  BBBBBBBBB+B
=BPB\BhBhBuB�B�B�B�B�B!�B#�B$�B%�B%�B%�B)�B,B,B,B/B1'B1'B49B6FB6FB8RB;dB>wB?}B@�BA�BB�BC�BE�BG�BJ�BO�BP�BQ�BR�BVBW
BXBYB_;BbNBffBiyBq�Br�Bs�Bt�B{�B}�B~�B�B�B�1B�DB�VB�oB�{B��B��B��B��B��B��B��B��B��B�B�3B�9B�LB�^B�dB�qB��BBÖBĜBŢBǮBɺB��B��B��B��B��B�B�
B�B�)B�;B�TB�ZB�fB�sB�yB�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	%B	%B	+B	
=B	DB	VB	hB	oB	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	#�B	&�B	'�B	+B	.B	0!B	33B	49B	7LB	:^B	>wB	B�B	E�B	G�B	I�B	J�B	K�B	N�B	O�B	P�B	P�B	P�B	Q�B	R�B	XB	[#B	\)B	\)B	\)B	]/B	^5B	`BB	aHB	cTB	ffB	gmB	hsB	m�B	s�B	s�B	t�B	u�B	w�B	x�B	y�B	z�B	z�B	|�B	� B	�B	�7B	�7B	�7B	�7B	�PB	�VB	�VB	�\B	�bB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�FB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�TB	�ZB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
PB
PB
VB
\B
\B
\B
hB
hB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
.B
/B
/B
0!B
0!B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
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
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
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
D�B
E�B
E�B
E�B
E�B
F�B
F�B
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
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
XB
XB
YB
YB
YB
ZB
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
\)B
\)B
]/B
]/B
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
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
aHB
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
dZB
dZB
e`B
e`B
e`B
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
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
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
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B?yB?�B?�B+4BPBDB�BxBB)B�B�B�B�B�B�B�BB/�B��B��Bj�BzWB�LB�+B�\B~�B|�B�B��B��BĺB��B͎B��B�B�BB�BB�B��B��B�DB��B��B��B��B�KB��B�B�B|\Bn4Bb�BQ�BA�BCBA�B@�B@�BA$B?�B0�B�B�B.B�B�oB�<B��B�NB�B��B�	B�eB��B�_B��B��B��B�B�BzBkkB^�BVBa}BV�BV�BQ�BF�B;~B/&B&GBVBHB
 B
��B
�
B
�B
݋B
�B
��B
�fB
��B
�-B
��B
�AB
��B
�0B
�B
z�B
coB
T�B
F�B
@�B
20B
%�B
�B	��B	��B	�cB	�6B	�^B	�OB	��B	�.B	�1B	1B	x�B	qbB	o�B	l�B	e�B	]DB	R�B	K�B	I�B	9{B	6�B	6%B	8{B	2�B	/wB	(�B	�B	OB	HB	�B�tB�B��B�NB�JB��B�B�(B�qB�/B��B��B��B�kB�B��B�!B�B�uB�uB�nB��B��B�IB��B�QB�6B� B��B��B� B�uB�:B�MB��B� B��B�iB�\B��B��B��B�#B�{B��B�xB�ZB�B��B��B�B�YBB}Bz�Bz2Bw�Bu�BuzBr�Bp�BprBp0BpVBo�BmBm�BmBjBe?Bc�BbZB]�B\/BY�BV�BU�BWBV�BS�BQ�BM�BI�BA�B>�B?�B>B>B?B=OB=�B<�B:iB:~B:QB7�B6VB4+B3�B2pB/\B.�B-�B)�B'.B'eB&�B$�B!�B!(B�B&B�B�B4B;B�B!BB�BABfB�B
�B%B	/BB�BhB;B�BAB�B�B�B�B�B�B�B�B�BXB�B�B�B�BBeB\B�B�B�B�BB�BdBB�B	9B
�B|B�BVBB+BIBHB�BB�B"�B$'B%RB&AB&�B('B*�B,yB,hB-�B0/B1yB2B4�B6�B7B9�B<)B?B@BABA�BB�BD5BFvBH�BLKBP{BQLBR0BS�BV�BWdBX�B[kB`6Bc-Bf�Bj�Br~Br�BtBu�B|XB~5B�B�QB��B��B��B��B��B��B�#B��B�AB�.B�%B�B�B�B�VB��B��B��B�yB��B��B��B��B��BöB��B�B�B��B�OB�BB�.B�sB�BB�JB�kB٣B��B�B�B��B�B��B��B�B�B��B��B� B�B�IB�?B�PB�*B	 iB	4B	>B	4B	&B	2B	(B	eB	
pB	�B	�B	�B	B	2B	�B	�B	�B	�B	�B	 �B	 �B	">B	$TB	'eB	(�B	+&B	.}B	0�B	3�B	4�B	7�B	;B	?)B	C!B	FB	G�B	I�B	J�B	L0B	OB	O�B	P�B	P�B	P�B	R2B	SzB	XlB	[5B	\2B	\0B	\=B	]HB	^_B	`cB	a�B	c�B	f~B	guB	hB	l�B	s�B	s�B	t�B	vEB	w�B	y0B	z{B	{4B	{UB	|�B	{B	��B	�-B	�pB	�SB	��B	�[B	�SB	�eB	��B	��B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	� B	��B	�IB	��B	��B	�B	��B	�B	��B	��B	�DB	�B	�zB	�B	�B	�B	�B	�OB	�\B	�B	�eB	�B	�B	�5B	�B	�B	�SB	�vB	�qB	��B	��B	��B	��B	��B	�aB	ÃB	ĕB	��B	�FB	��B	źB	ǭB	��B	��B	�B	��B	��B	�B	�B	�>B	�B	��B	�B	�YB	�/B	��B	�8B	�HB	�kB	�WB	�OB	�TB	�\B	�kB	�QB	�]B	�B	�YB	��B	�B	�uB	�)B	�B	�pB	��B	��B	��B	�B	�B	��B	� B	�B	��B	��B	�DB	�SB	�B	�wB	��B	��B	��B	��B	��B	�.B	�*B	��B	��B	��B	� B
 #B
  B
(B
?B
DB
5B
6B
SB
1B
CB
QB
QB
bB
�B
�B
	�B

FB

<B

YB

VB

VB

cB
�B
�B
zB
�B
�B
pB
tB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 B
 �B
 �B
!�B
"B
"�B
#*B
#�B
%B
$�B
$�B
%B
&B
&B
'TB
'B
'�B
'�B
(B
'�B
(4B
)#B
)8B
* B
+*B
+UB
+fB
,JB
,B
,B
,#B
,bB
-=B
-IB
.9B
/@B
/eB
0oB
0�B
2wB
29B
2\B
3IB
3;B
48B
4:B
4FB
4QB
4UB
4lB
5�B
5�B
6�B
6�B
7�B
7�B
7�B
8�B
9B
9`B
9eB
9�B
9�B
:�B
;B
;�B
;�B
<�B
<hB
<xB
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
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
D�B
E�B
E�B
E�B
E�B
GB
F�B
G�B
G�B
G�B
G�B
H�B
IB
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K(B
K�B
L�B
L�B
L�B
M(B
NB
M�B
O
B
OB
O�B
O�B
P)B
Q B
P�B
P�B
P�B
QB
QEB
R#B
R�B
R�B
SB
S#B
TB
TB
TB
TB
TB
TB
U0B
UB
UB
UB
U!B
VB
VB
VB
V(B
V%B
VB
W&B
WB
WB
W3B
WB
WCB
XSB
X`B
Y6B
Y#B
YDB
Z>B
ZJB
Z4B
Z=B
[<B
[:B
[9B
[0B
[;B
[;B
[-B
\MB
\5B
\@B
\5B
\5B
\EB
\JB
\3B
];B
]IB
]AB
]JB
]BB
^IB
^HB
^HB
^=B
^?B
^4B
^^B
^IB
_]B
_bB
_SB
_�B
`dB
`mB
`lB
`PB
`NB
aEB
`LB
`[B
`CB
`NB
`NB
`dB
`qB
`NB
aQB
bNB
bMB
bZB
bfB
biB
brB
aUB
a}B
a�B
b�B
bfB
b]B
bhB
b�B
cyB
cnB
caB
cVB
cyB
cwB
deB
dtB
e�B
exB
edB
e�B
e�B
eoB
eiB
frB
fvB
f�B
fB
f�B
f�B
gzB
g�B
glB
h~B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
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
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1��<���<L��<+�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<S��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352002018102313520020181023135200  AO  ARCAADJP                                                                    20170803170410    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170803170410  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170803170410  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135200  QC  PRES            @�33D�y�G�O�                PM  ARSQCTM V1.1                                                                20181023135200  QC  PSAL            @�33D�y�G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093509  IP                  G�O�G�O�G�O�                