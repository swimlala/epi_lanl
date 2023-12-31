CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2016-04-19T15:16:32Z creation; 2016-04-19T15:16:32Z updated; 2016-09-02T17:52:34Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7$   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8    	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8(   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8H   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8h   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8l   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8t   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8x   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  `4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �     TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ެ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �X  �XArgo profile    3.1 1.2 19500101000000  20160419151632  20181027140152  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               4A   AO  5006_0305_052                   2C  D   NAVIS_A                         0305                            082713                          863 @�d#���1   @�d$l�p@7;�l�C��c�x���1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      4A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C��C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D�>D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$�
D%
D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D-
D-�
D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj
Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD��D�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�z�A�z�A�|�AځAڃAڃAڅAڋDAڍPAڏ\AڍPAړuAړuAڑhAڑhAڅA��Aײ-A�(�A��A��A�bA�hsA�9XA�bA�dZA���A��yA�bNA���A�JAƮA�A�ZA���A��A�S�A�oA�bA���A�I�A���A��A���A���A�A��A�x�A�"�A�dZA�p�A��PA�1A��A�9XA��A�bNA�;dA��A���A�Q�A�hsA�%A��A�ffA�l�A���A�  A��A��hA�M�A�Q�A��hA���A��A��^A�1A��7A���A�+A�33A�x�A���A��A���A�p�A��A���A��7A��A���A�?}A�1'A���A��7A���A�E�A�E�A�x�A�ƨA��A�1'A���A�A�A�dZA�7LA��DA��A�ĜA�G�A��\A�"�A33A}��A{G�Ay�^AxM�Aul�As�Aq�PApAm�Ak�Ai
=Ahr�Ah1'Ah1Ag�AgK�AfE�Ae�7Ab�A`E�A]|�AYAXbNAV��AU�AT  AR�RAQAP��AP-APAO`BAN��AJ��AE�AB=qA@��A>ffA=\)A;x�A:��A8�DA7�A6Q�A5�A533A4jA4bA3��A3�A2�RA0-A/��A.��A-��A-"�A,ĜA,bA*�A)A(��A'p�A&��A$�A#��A#p�A#�A"VAAbAbA��A�+A��A��A�A�A�\A��A`BA&�A%A�!A(�A�A�A�AE�AS�A�;AbNA��A+A
~�A	�hA��A�AVAG�A  A33A�A�9AffA��At�A33@���@�E�@�hs@�C�@���@���@�@�hs@�Ĝ@��@�S�@��@�@��@�@�  @�@�V@��T@�hs@�r�@�w@��@�O�@���@���@�z�@���@�^@�7@���@�l�@ޟ�@���@܃@��m@��#@��`@ם�@�C�@�-@�/@�  @��#@�bN@�;d@�M�@ͩ�@̣�@�Q�@�S�@�=q@�@��@�x�@�%@���@�=q@��@�bN@� �@���@�M�@�@�7L@�bN@��@�o@�+@��@�
=@��@�ff@�-@�{@���@��@��@�r�@��@��y@���@�=q@��-@���@�b@�|�@�dZ@�33@���@�5?@���@�j@�1@���@�o@�ȴ@�$�@���@��F@�l�@�33@���@��T@�O�@�&�@���@��@�ƨ@�K�@�v�@���@��`@��@��@�r�@��F@�l�@�ƨ@�C�@�ff@�{@�@��@�hs@�S�@�K�@���@�dZ@��P@�ȴ@��D@�(�@��@�bN@�ff@��@��^@��@� �@��+@�-@���@�  @��
@���@��
@�A�@�1@�l�@�|�@��9@�  @��@�l�@��@��y@�"�@�+@��@�=q@��@�@���@��@��T@�hs@�`B@�X@�G�@��@� �@��@��@���@��R@�{@��#@��@��j@�&�@��^@���@��9@� �@�r�@��F@�;d@�
=@���@�n�@���@�p�@�^5@�v�@�&�@�Ĝ@�I�@�Q�@�r�@�A�@��;@���@�dZ@��m@��m@���@���@�|�@�l�@�\)@�"�@�ȴ@�n�@�$�@��@��7@�hs@�hs@�X@��j@�1@�1'@�1'@� �@��@��@�33@��y@���@���@��\@�~�@�M�@���@���@�x�@�x�@��/@�Ĝ@���@��/@���@��j@���@���@��@�  @�+@�C�@��@��@��@��\@���@�J@�@�p�@�G�@�/@�%@���@��/@��`@���@��D@�z�@���@�1'@l�@~��@~v�@}@}/@|��@|��@|��@|j@|j@|(�@|j@|j@|�@{��@{��@{"�@z=q@yX@y��@y7L@y%@x �@wl�@w�@v�R@vff@v5?@u��@up�@u/@t�@tj@t�@s�m@sdZ@r�@r�\@r-@r�@q��@q7L@o�@o�P@o\)@o
=@n��@nV@m��@m�@m?}@mV@l�D@l�@k�m@k�@kS�@k@jn�@i�@i�7@iG�@h��@h�u@hQ�@hb@g��@g\)@g+@g|�@gK�@fV@e�T@e�T@e�T@f{@e�T@e�h@e/@d��@dZ@c��@c��@cdZ@co@b�H@b�\@b-@a�@aG�@`�9@`�@`bN@`Q�@`bN@`��@_K�@^�R@^��@^E�@^$�@^@]p�@]��@^{@]O�@[ƨ@Z��@Y��@Y�^@Y��@Y7L@XĜ@X�9@X��@Xr�@X �@W�@Wl�@V��@V��@V�+@VE�@V$�@U��@U�@T�@T��@T�D@T(�@S�
@SS�@R�H@R��@Rn�@RJ@Q��@Q�7@QG�@P��@P�`@PĜ@PQ�@Pb@O��@O�P@N�@N{@N@M�T@M�@M�@M�T@M�-@Mp�@M`B@M?}@M/@L��@L1@K�m@K��@K@J��@Jn�@JJ@I��@I�7@I��@Ihs@H��@H��@Hr�@H �@G��@G�P@Gl�@G;d@G
=@F�@F�+@F5?@F{@E��@Ep�@EO�@E?}@EV@D�@D�j@DZ@C��@Cƨ@C�@CC�@C"�@Co@C��@C�
@CdZ@C"�@Co@Co@Co@B�@B�\@B^5@B^5@B=q@B-@B��@C"�@Co@Co@B��@B��@Bn�@B-@A��@A�7@A�@@1'@?�P@?|�@>�y@>�R@>E�@>{@>5?@>��@>v�@>E�@>$�@>@=�T@=@=�-@=�@<��@<��@<z�@<9X@<1@;�
@;ƨ@;��@;S�@:�@:M�@:-@:-@97L@8bN@8 �@8  @7�;@7�@7\)@6�@6E�@5��@5?}@4�j@4��@4I�@4�@4I�@41@4(�@49X@4�@3ƨ@3�F@2��@2~�@2M�@2=q@1��@1��@1X@1&�@1�@0��@0�9@0Q�@/�@/K�@.��@.��@.{@-��@-��@-O�@,z�@,1@+�
@+t�@+S�@+C�@+o@*��@*�@)�#@)��@)��@)x�@)%@(bN@( �@'�@'�P@'�P@'l�@'\)@'�@&ȴ@&�R@&��@&ff@&E�@&E�@&{@%�-@%O�@%�@%V@$�/@$�j@$�D@$z�@$I�@$(�@$(�@$1@$1@#��@#�
@#�F@#��@#�@#S�@#33@#@"�H@"�!@"M�@!��@!�^@!��@!�7@!x�@!x�@!7L@ ��@ �u@ bN@ 1'@  �@  �@ b@   @�@��@�@|�@\)@K�@;d@+@
=@�y@�y@ȴ@��@ff@$�@��@@�-@��@�h@�h@p�@�@�/@��@�D@I�@�@��@ƨ@t�@o@@�!@~�@^5@M�@J@��@�7@G�@�@Ĝ@Q�@Q�@ �@��@�@|�@|�@l�@
=@�y@ȴ@{@�-@�@p�@O�@��@Z@��@�@�@t�@t�@S�@"�@�H@�!@~�@M�@M�@=q@-@�#@��@x�@hs@hs@�@�`@��@�9@bN@��@�@�;@�@�P@\)@�@��@�R@��@v�@5?@��@�@�@��@z�@I�@9X@(�@��@�F@��@C�@@
�H@
��@
��@
��@
�\@
^5@	��@	�#@	�7@	hs@	G�@	�@�`@��@Ĝ@�9@�`@	X@�`@1'@  @|�@l�@K�@;d@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�z�A�z�A�|�AځAڃAڃAڅAڋDAڍPAڏ\AڍPAړuAړuAڑhAڑhAڅA��Aײ-A�(�A��A��A�bA�hsA�9XA�bA�dZA���A��yA�bNA���A�JAƮA�A�ZA���A��A�S�A�oA�bA���A�I�A���A��A���A���A�A��A�x�A�"�A�dZA�p�A��PA�1A��A�9XA��A�bNA�;dA��A���A�Q�A�hsA�%A��A�ffA�l�A���A�  A��A��hA�M�A�Q�A��hA���A��A��^A�1A��7A���A�+A�33A�x�A���A��A���A�p�A��A���A��7A��A���A�?}A�1'A���A��7A���A�E�A�E�A�x�A�ƨA��A�1'A���A�A�A�dZA�7LA��DA��A�ĜA�G�A��\A�"�A33A}��A{G�Ay�^AxM�Aul�As�Aq�PApAm�Ak�Ai
=Ahr�Ah1'Ah1Ag�AgK�AfE�Ae�7Ab�A`E�A]|�AYAXbNAV��AU�AT  AR�RAQAP��AP-APAO`BAN��AJ��AE�AB=qA@��A>ffA=\)A;x�A:��A8�DA7�A6Q�A5�A533A4jA4bA3��A3�A2�RA0-A/��A.��A-��A-"�A,ĜA,bA*�A)A(��A'p�A&��A$�A#��A#p�A#�A"VAAbAbA��A�+A��A��A�A�A�\A��A`BA&�A%A�!A(�A�A�A�AE�AS�A�;AbNA��A+A
~�A	�hA��A�AVAG�A  A33A�A�9AffA��At�A33@���@�E�@�hs@�C�@���@���@�@�hs@�Ĝ@��@�S�@��@�@��@�@�  @�@�V@��T@�hs@�r�@�w@��@�O�@���@���@�z�@���@�^@�7@���@�l�@ޟ�@���@܃@��m@��#@��`@ם�@�C�@�-@�/@�  @��#@�bN@�;d@�M�@ͩ�@̣�@�Q�@�S�@�=q@�@��@�x�@�%@���@�=q@��@�bN@� �@���@�M�@�@�7L@�bN@��@�o@�+@��@�
=@��@�ff@�-@�{@���@��@��@�r�@��@��y@���@�=q@��-@���@�b@�|�@�dZ@�33@���@�5?@���@�j@�1@���@�o@�ȴ@�$�@���@��F@�l�@�33@���@��T@�O�@�&�@���@��@�ƨ@�K�@�v�@���@��`@��@��@�r�@��F@�l�@�ƨ@�C�@�ff@�{@�@��@�hs@�S�@�K�@���@�dZ@��P@�ȴ@��D@�(�@��@�bN@�ff@��@��^@��@� �@��+@�-@���@�  @��
@���@��
@�A�@�1@�l�@�|�@��9@�  @��@�l�@��@��y@�"�@�+@��@�=q@��@�@���@��@��T@�hs@�`B@�X@�G�@��@� �@��@��@���@��R@�{@��#@��@��j@�&�@��^@���@��9@� �@�r�@��F@�;d@�
=@���@�n�@���@�p�@�^5@�v�@�&�@�Ĝ@�I�@�Q�@�r�@�A�@��;@���@�dZ@��m@��m@���@���@�|�@�l�@�\)@�"�@�ȴ@�n�@�$�@��@��7@�hs@�hs@�X@��j@�1@�1'@�1'@� �@��@��@�33@��y@���@���@��\@�~�@�M�@���@���@�x�@�x�@��/@�Ĝ@���@��/@���@��j@���@���@��@�  @�+@�C�@��@��@��@��\@���@�J@�@�p�@�G�@�/@�%@���@��/@��`@���@��D@�z�@���@�1'@l�@~��@~v�@}@}/@|��@|��@|��@|j@|j@|(�@|j@|j@|�@{��@{��@{"�@z=q@yX@y��@y7L@y%@x �@wl�@w�@v�R@vff@v5?@u��@up�@u/@t�@tj@t�@s�m@sdZ@r�@r�\@r-@r�@q��@q7L@o�@o�P@o\)@o
=@n��@nV@m��@m�@m?}@mV@l�D@l�@k�m@k�@kS�@k@jn�@i�@i�7@iG�@h��@h�u@hQ�@hb@g��@g\)@g+@g|�@gK�@fV@e�T@e�T@e�T@f{@e�T@e�h@e/@d��@dZ@c��@c��@cdZ@co@b�H@b�\@b-@a�@aG�@`�9@`�@`bN@`Q�@`bN@`��@_K�@^�R@^��@^E�@^$�@^@]p�@]��@^{@]O�@[ƨ@Z��@Y��@Y�^@Y��@Y7L@XĜ@X�9@X��@Xr�@X �@W�@Wl�@V��@V��@V�+@VE�@V$�@U��@U�@T�@T��@T�D@T(�@S�
@SS�@R�H@R��@Rn�@RJ@Q��@Q�7@QG�@P��@P�`@PĜ@PQ�@Pb@O��@O�P@N�@N{@N@M�T@M�@M�@M�T@M�-@Mp�@M`B@M?}@M/@L��@L1@K�m@K��@K@J��@Jn�@JJ@I��@I�7@I��@Ihs@H��@H��@Hr�@H �@G��@G�P@Gl�@G;d@G
=@F�@F�+@F5?@F{@E��@Ep�@EO�@E?}@EV@D�@D�j@DZ@C��@Cƨ@C�@CC�@C"�@Co@C��@C�
@CdZ@C"�@Co@Co@Co@B�@B�\@B^5@B^5@B=q@B-@B��@C"�@Co@Co@B��@B��@Bn�@B-@A��@A�7@A�@@1'@?�P@?|�@>�y@>�R@>E�@>{@>5?@>��@>v�@>E�@>$�@>@=�T@=@=�-@=�@<��@<��@<z�@<9X@<1@;�
@;ƨ@;��@;S�@:�@:M�@:-@:-@97L@8bN@8 �@8  @7�;@7�@7\)@6�@6E�@5��@5?}@4�j@4��@4I�@4�@4I�@41@4(�@49X@4�@3ƨ@3�F@2��@2~�@2M�@2=q@1��@1��@1X@1&�@1�@0��@0�9@0Q�@/�@/K�@.��@.��@.{@-��@-��@-O�@,z�@,1@+�
@+t�@+S�@+C�@+o@*��@*�@)�#@)��@)��@)x�@)%@(bN@( �@'�@'�P@'�P@'l�@'\)@'�@&ȴ@&�R@&��@&ff@&E�@&E�@&{@%�-@%O�@%�@%V@$�/@$�j@$�D@$z�@$I�@$(�@$(�@$1@$1@#��@#�
@#�F@#��@#�@#S�@#33@#@"�H@"�!@"M�@!��@!�^@!��@!�7@!x�@!x�@!7L@ ��@ �u@ bN@ 1'@  �@  �@ b@   @�@��@�@|�@\)@K�@;d@+@
=@�y@�y@ȴ@��@ff@$�@��@@�-@��@�h@�h@p�@�@�/@��@�D@I�@�@��@ƨ@t�@o@@�!@~�@^5@M�@J@��@�7@G�@�@Ĝ@Q�@Q�@ �@��@�@|�@|�@l�@
=@�y@ȴ@{@�-@�@p�@O�@��@Z@��@�@�@t�@t�@S�@"�@�H@�!@~�@M�@M�@=q@-@�#@��@x�@hs@hs@�@�`@��@�9@bN@��@�@�;@�@�P@\)@�@��@�R@��@v�@5?@��@�@�@��@z�@I�@9X@(�@��@�F@��@C�@@
�H@
��@
��@
��@
�\@
^5@	��@	�#@	�7@	hs@	G�@	�@�`@��@Ĝ@�9@�`@	X@�`@1'@  @|�@l�@K�@;d@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-BȴB+BJB�B+B?}BYB[#B[#BcTB�B�LBĜB�B�B+BoBoB�B�B$�B-B(�B%�B$�B%�B$�B �B&�B&�B�B�B�B�B�B�B�B�B�BuBhBVBDBoB�BhB1B\B{B�B�B�BoB	7B��B��B�B�mB�/B��B��BĜB�'B�VBu�BaHBA�B!�BDB��B�B�sB�B��B��B�1Bv�Bn�BdZBXBS�BG�B;dB1'B(�B�BbB+B
��B
��B
�^B
��B
�PB
w�B
o�B
hsB
_;B
R�B
>wB
0!B
$�B
hB
B	��B	�sB	�B	ŢB	�^B	�?B	�3B	�'B	�!B	�B	��B	��B	�+B	iyB	K�B	33B	'�B	�B	 �B	"�B	�B	�B	{B	\B	PB	B��B�5B�qB�-B�B��B��B��B��B��B�{B�uB�hB�\B�PB�JB�DB�=B�+B�B�B� B~�B}�B|�Bz�Bx�Bv�Bs�Bq�Bo�Bn�Bl�Bk�BjBffBbNB^5BZBXBVBS�BS�BQ�BO�BN�BM�BM�BL�BL�BK�BJ�BJ�BI�BH�BG�BF�BE�BE�BD�BD�BC�BA�BA�BA�B@�B@�BA�BA�BA�BA�B?}B?}B?}B?}B@�B@�B?}B>wB>wB?}BB�BC�BF�BH�BI�BI�BH�BK�BL�BK�BK�BL�BM�BP�BS�BS�BXB^5B`BB^5B_;B_;B^5B^5B^5B`BBdZBdZBcTBcTBbNBdZBe`BdZBcTBcTBcTBe`BhsBk�Bm�Bp�Br�Br�Bt�Bv�Bx�Bx�Bx�By�B{�B}�B}�B}�B}�B�B�B�B�B�%B�=B�\B�hB�uB�uB�uB��B��B��B��B��B��B��B��B��B�B�B�!B�?B�LB�^B�^B�dB�jB�}BBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�)B�NB�HB�BB�mB�B�B�B��B��B��B	B	B	PB	bB	oB	uB	oB	�B	!�B	#�B	%�B	#�B	�B	 �B	,B	2-B	49B	6FB	;dB	A�B	>wB	:^B	9XB	6FB	6FB	;dB	=qB	@�B	E�B	G�B	F�B	H�B	T�B	T�B	S�B	S�B	R�B	T�B	W
B	YB	ZB	ZB	^5B	`BB	aHB	aHB	aHB	bNB	e`B	ffB	ffB	gmB	cTB	cTB	iyB	k�B	l�B	l�B	k�B	k�B	k�B	n�B	s�B	t�B	s�B	s�B	w�B	u�B	t�B	x�B	z�B	|�B	~�B	� B	�7B	�=B	�=B	�DB	�=B	�JB	�\B	�\B	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�9B	�?B	�?B	�LB	�LB	�LB	�RB	�^B	�qB	�}B	��B	��B	�}B	�}B	��B	ÖB	B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�#B	�/B	�;B	�BB	�BB	�BB	�NB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�`B	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
+B
1B
1B

=B

=B

=B
DB
DB
JB
JB
PB
PB
\B
bB
bB
bB
hB
hB
hB
oB
{B
uB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
uB
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
5?B
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
;dB
>wB
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
@�B
?}B
A�B
A�B
@�B
@�B
@�B
A�B
B�B
C�B
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
G�B
G�B
G�B
G�B
G�B
F�B
F�B
G�B
G�B
F�B
F�B
E�B
E�B
E�B
D�B
D�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
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
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
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
W
B
W
B
VB
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
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
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
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
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
l�B
l�B
l�B
l�B
m�B
l�B
l�B
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
m�B
m�B
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
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
q�B
s�B
r�B
q�B
q�B
q�B
q�B
q�B
q�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B͐BB�BHB-�BA^BY�B[�B_�Bg�B��B��B��B��B�B�BKB�B'B!�B*B1WB*�B'CB'vB(dB*MB)�B/�B*�B!�BBB�B�B�BB6BB�BB#B�B�B!�B9B
zB�B�B"YB�B�BB"B�B�JB�6B�9B�hB�)B�.BȰB�0B��By5Bh+BHB&B;B�QB�B�B��B��B��B��Bx�Br�Bg�BY�BW�BJ�B>DB4B-B�BB
�B �B
ڣB
��B
�*B
�rB
z�B
q|B
j�B
b�B
W�B
BB
3B
+�B
�B
�B	�yB	�B	�'B	��B	��B	��B	��B	�xB	��B	�~B	�B	�sB	��B	q
B	UB	6�B	,wB	!�B	$�B	%�B	!B	�B	B	�B	�B	>B	�B�B�B�YB�iB��B��B��B��B��B��B��B�hB�yB�4B�3B��B��B�YB�.B�B�TB��B~�B~lB}�Bz�Bx�Bv�Bs�Bs�Bp�Bm�BlVBlJBllBf�Bc)B_�B[KBW�BVBU&BSBS�BQ)BN�BNdBM&BM�BM%BLBLIBJuBJMBJBJ�BIkBGBFSBFfBFBFWBDgBB�BCsBC�BC�BBDBB,BBgBAB@�B@SBCrBBqBA�BB�B@�BA�BBBCBD�BG�BI�BJ|BJ0BJ7BM_BM�BMFBL�BM~BN�BRLBUBVlBYB`"BcDB_�Ba1B`NB^kB^�B_�BaBe>Be�Bc�Be�BcRBe�Be�Be�BdvBd�Be�Bg3Bi�Bl�BnhBq�BsBs�BvBwBx�ByyByzB{QB~SB�B~�B~_B�B��B�uB�8B�HB��B�DB�8B��B��B��B�FB��B��B�B�B�IB��B��B�TB�xB��B��B��B�B�B��B��B��B�dB�tB�NB�HB�fB̟B�<B��B��B�'B�LB�9BъB�#B��B�8B��B� B�^B��B�B�sB�GB�gB�B�	B�B�&B�lB��B	BB	�B	iB	�B	9B	ZB	rB	B	"B	#�B	&�B	&/B	$B	�B	,�B	4�B	4�B	6~B	;+B	C�B	@yB	:�B	:�B	7�B	6�B	;�B	=&B	?�B	E�B	H�B	F}B	GB	VB	U�B	T+B	T�B	R�B	T�B	V�B	YCB	[qB	ZMB	^YB	`RB	aVB	ajB	bB	b[B	enB	f�B	f�B	iB	eKB	c-B	irB	lB	m�B	l�B	l�B	lB	j�B	m�B	s�B	vFB	t�B	sFB	x�B	v�B	uB	yqB	{DB	}�B	�B	~�B	�.B	�JB	��B	�B	�+B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�,B	�dB	�oB	�\B	�SB	��B	�:B	�B	�0B	��B	��B	��B	�9B	�OB	�wB	��B	��B	��B	��B	�]B	�yB	��B	��B	�B	��B	��B	��B	�LB	¯B	ĐB	ǖB	��B	��B	ʭB	�B	�B	ДB	��B	̨B	�OB	ҷB	�:B	�JB	��B	��B	مB	ٌB	�VB	�CB	�]B	�<B	�AB	�"B	�aB	�B	�VB	�B	��B	��B	�B	�B	��B	�B	�nB	�oB	�rB	�nB	�TB	�B	�.B	�tB	�B	�B	�B	�*B	�:B	�2B	�cB	��B	��B	�NB	�(B	��B	��B	��B	��B	��B	�
B	��B	��B	�$B	�B	��B	�-B	�4B	�;B	�=B	�	B
 =B
 kB
 �B	�;B	�B	�/B	�BB
 ?B
 ^B
 @B
<B
6B
wB
hB
CB
iB
NB
qB
�B
�B

�B

rB

|B
�B
rB
rB
sB
�B
iB
+B
�B
�B
�B
fB
fB
GB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
kB
�B
B
�B
�B
�B
�B
B
�B
^B
MB
�B
nB

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
 B
�B
B
 B
�B
�B
 B
!B
 �B
 �B
#B
#�B
#�B
$0B
$B
$B
$B
$kB
$lB
$�B
#�B
$�B
%�B
%�B
&B
'B
&�B
(B
(B
(eB
(`B
)B
)AB
)gB
)+B
*<B
*LB
+PB
+B
*�B
++B
+sB
,0B
,0B
,GB
,IB
,=B
,"B
,-B
,3B
-4B
-PB
-MB
-)B
/lB
/=B
04B
0*B
0@B
04B
0@B
0_B
1_B
1FB
1LB
1OB
2AB
25B
2�B
5"B
7�B
7sB
8]B
8OB
8QB
8eB
8�B
8rB
8RB
9kB
9aB
;B
>5B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B%B
@�B
?�B
A�B
A�B
@�B
@�B
@kB
AFB
B�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H B
F�B
F�B
HoB
HJB
F�B
F�B
E�B
E�B
E�B
EB
EB
DB
C�B
C�B
C�B
C�B
C�B
DvB
E�B
E�B
E�B
F�B
F�B
F�B
F,B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
H�B
H�B
H�B
IB
H�B
H�B
H�B
I>B
JB
I�B
I�B
J�B
J�B
J�B
KB
KB
J�B
J�B
J�B
J�B
KB
K3B
K�B
LB
L�B
M�B
M�B
M�B
M�B
OB
N�B
O�B
PB
O�B
O�B
PB
P(B
Q+B
RB
Q�B
RB
RB
RB
R�B
SB
SB
S�B
TB
S�B
TB
TB
TB
TB
TB
TB
TB
U$B
UB
U(B
UMB
U@B
V8B
V"B
VB
WB
WB
V8B
W{B
WB
W4B
X3B
XB
YB
Y"B
Y&B
Y"B
Y1B
Y5B
ZBB
Z6B
Z(B
Z'B
[2B
[=B
[>B
\,B
\HB
]YB
]YB
]fB
]mB
^@B
^BB
_GB
_FB
_;B
_UB
_zB
_gB
_IB
`kB
`gB
`^B
`TB
`[B
`qB
`xB
aPB
ayB
afB
bcB
b[B
bxB
byB
c{B
c|B
cxB
c�B
c�B
dXB
d|B
d�B
etB
dwB
dXB
ejB
e�B
etB
eyB
e�B
f�B
f�B
fnB
f~B
f�B
f�B
f�B
g�B
goB
guB
gmB
g�B
g�B
g�B
h�B
h�B
i�B
ivB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
l�B
llB
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
m�B
m�B
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
o�B
o�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
pxB
qXB
tB
s<B
q�B
r
B
q�B
q�B
q�B
q�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�*<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<% �<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201609291651292016092916512920160929165129  AO  ARCAADJP                                                                    20160419151632    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160419151632  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160419151632  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160929165129  QC  PRES            @�  D�� G�O�                PM  ARSQCTM V1.1                                                                20160929165129  QC  PSAL            @�  D�� G�O�                PM  ARSQOWGUV1.0                                                                20181027140152  IP                  G�O�G�O�G�O�                