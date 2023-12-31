CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2016-06-05T19:15:45Z creation; 2016-06-05T19:15:45Z updated; 2016-09-02T17:52:40Z converted from 3.0   
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pp   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��  ��Argo profile    3.1 1.2 19500101000000  20160605191545  20181027140158  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               SA   AO  5006                            2C  D   NAVIS_A                         0305                            082713                          863 @ױ�韺�1   @ױ�����@7�I�^5�c��$�/1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      SA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�8RB�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$z>D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+�
D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D7�>D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD�ÅD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�MD�`R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A��Aˡ�A��AʾwAʑhA��yAȝ�A�;dA�
=A�ȴA�^5A�(�A�+A��A�ƨA�ZA�n�A��A��Aě�A�l�A���Aħ�A�+A�K�A¾wA�jA�=qA�1A�l�A�VA���A���A�v�A�Q�A�^5A���A��A�A��/A�ȴA��TA�ƨA���A�ffA��A�-A� �A�M�A��A��;A�\)A� �A��A�bA��A���A���A��+A�ȴA���A�x�A��^A��-A��\A�Q�A�&�A�r�A�"�A�%A��TA�|�A���A��A��A�K�A�ƨA�A�A��A�E�A���A�bA��+A��A�r�A�=qA���A�&�A���A��`A��7A��A�VA���A�I�A�A�n�A��\A�=qA���A�\)A�bNA���A�-A�x�A�E�A��A��A���A�n�A���A�7LA�{A~ �A};dA{O�Ay33Av�`At��Aq�^Ao�Aox�An��Al^5Ak��AkVAi�wAg�-Ac�AbE�Aap�A`��A_�wA^~�A]ƨA\�`AY�TAYVAX��AV�/AVbAS�AP�uAMS�AL9XAJ�+AG��AC��ABv�ABbAAA?��A>n�A=��A=�wA=VA<E�A<A�A;��A:�RA9�A81A6I�A5��A4�A4r�A3��A2��A1��A1
=A0�A/dZA-?}A+��A+�wA+|�A*$�A(��A'��A&��A%XA$�A �A33A-AA|�A7LA�jA{AO�AbA�A�RA-A�FAp�AK�A��AbA�HA��AG�A��A��A-AG�AjA�mAȴA�TA�Az�A�AhsA
jA	A��A��A�7A"�AQ�A�AO�A��Al�AQ�Ap�A �D@���@��j@�
=@�@�Z@���@��/@�M�@�\)@�V@���@��`@��@�C�@��/@�+@���@��@�|�@�=q@�h@��`@�z�@���@ߝ�@��@݉7@��m@��y@ם�@Ձ@�/@��`@ԋD@��T@�bN@ϝ�@���@�Z@˕�@��@ʗ�@�E�@�{@�@�V@Ȭ@�Q�@Ǯ@ƸR@�@���@� �@�dZ@�+@��@�?}@��@�
=@�ff@�5?@�@�x�@��@�o@�@�O�@�O�@��@��F@��R@���@�&�@�I�@��@��!@���@�I�@���@�+@��H@�v�@�M�@�5?@���@�/@�Z@�dZ@���@�v�@���@�&�@�r�@��@��@�=q@��T@�hs@��@�9X@��F@�@�5?@��^@��/@���@�A�@�l�@��@��@�S�@��@��#@�%@���@��
@���@�l�@�"�@���@���@��\@�~�@�M�@��@�hs@���@��@��m@�C�@��H@��T@��7@�hs@��@��`@��`@���@��@�Q�@�I�@�bN@�r�@�(�@���@�  @�(�@�(�@�A�@��@�S�@��R@�M�@���@�&�@�Ĝ@�A�@��u@���@��`@��@��@���@��u@���@�A�@��@���@�\)@�@��@���@�ff@�E�@�J@�X@�&�@��@�%@��@���@��@�Z@��m@��@�t�@���@��@��@�dZ@��@�
=@��@�C�@�@��R@�~�@�-@���@��h@�X@�/@�%@��/@��j@�r�@�A�@�(�@�Q�@���@�V@�G�@���@���@��u@��@�@���@��@��@�\)@��@��-@���@��#@��#@���@���@��u@�Q�@��@�7L@�?}@��@��9@��D@�z�@��@�1'@~ȴ@}p�@{��@{�@|Z@|�/@~�R@~V@}�T@}�@|z�@|j@{@z=q@y7L@x��@xĜ@x�`@x�`@x��@x��@x��@y%@x�@xQ�@x1'@xA�@xQ�@xQ�@xQ�@x1'@x �@w�;@w|�@vȴ@vE�@u�T@u�@up�@u`B@u�@t�@tZ@sƨ@sdZ@so@r�@r�!@rn�@rM�@r�@q��@q�^@q��@qhs@p��@pr�@o��@ol�@n�R@m�T@m�-@m?}@l(�@k�
@k��@kt�@k"�@jJ@i�@hr�@g�@g�w@g�@g��@g��@g��@g|�@g;d@fȴ@fV@e�@d��@d�/@d�@d�D@dZ@d(�@c��@c��@c"�@b�\@b^5@b-@bJ@a�@a��@ax�@a%@_�w@_;d@_
=@^��@^V@\�D@[��@[C�@[33@Z�!@ZM�@ZJ@Y�7@Y&�@X��@X��@XĜ@Xr�@XA�@X  @Wl�@W
=@V��@Vff@Vff@VV@VE�@VE�@V@U`B@T�/@T�j@T�D@TZ@S�F@SS�@So@S@S@R�H@R��@R�\@R~�@R=q@Q��@Q�7@Qx�@QG�@P�`@P��@P�9@Pr�@PQ�@P �@Pb@O��@N��@Nv�@M�@M�@M/@L��@L�@L�j@L��@L��@L�@L�@L��@Lz�@L�@Kƨ@K��@K�@K��@K�@Ko@J��@J^5@J=q@JJ@I�#@I�7@Ix�@Ix�@IG�@I&�@H��@H1'@H �@H  @G��@Gl�@G�@F��@F��@F�R@Fff@F5?@F{@E�h@E?}@D��@D9X@C�@CdZ@CC�@Co@B�@B�\@BJ@Ahs@A7L@@Ĝ@@�@@ �@?�;@?\)@>��@>�@>��@>v�@>V@>V@>ff@>ff@>V@>5?@>{@=�h@<�j@<9X@<(�@;��@;��@;t�@;C�@:��@:�@9�^@9�7@9X@9&�@8Ĝ@8��@8�@8bN@81'@7�@7l�@7�@6�@6��@6$�@6@5�T@5�h@5O�@5V@4�@4��@4j@3�m@3ƨ@3��@3��@3t�@3o@2��@2~�@2^5@2�@1��@1x�@1X@17L@0�`@0�9@0��@0bN@0 �@0  @0  @/�@/�w@/�P@/\)@.��@.ȴ@.ff@.E�@.{@-�@-`B@-O�@-p�@-?}@-�@,�@,��@,��@,�@+�
@+�F@+�@+33@*�H@*��@*��@*�!@*�!@*-@*n�@)��@)�7@)��@)hs@)7L@)7L@)�@)%@(�`@(�9@(�u@(�@(r�@(Q�@(1'@(  @'�@'\)@&ȴ@&$�@%��@%�@%O�@%�@$�@$�@$9X@$�@#��@#ƨ@#��@#��@#��@#dZ@#o@"�@"��@"��@"��@"~�@"=q@!x�@!%@ Ĝ@ �u@ bN@ A�@�@�;@�;@�w@�@�@�P@�P@|�@|�@l�@
=@��@ff@@�T@@�@/@�/@��@�@��@�@�@�D@j@Z@I�@9X@��@�F@��@t�@"�@�@��@��@M�@�^@��@hs@G�@�`@�u@Q�@ �@b@��@�P@|�@|�@+@��@ȴ@V@$�@�@�T@�h@O�@��@�@�j@��@�D@j@Z@9X@�@��@�m@ƨ@��@dZ@33@o@�@��@~�@=q@-@J@��@��@x�@7L@&�@��@Ĝ@�u@r�@Q�@ �@�;@�w@��@�P@\)@+@�y@ȴ@�R@��@ff@5?@$�@$�@{@@�T@��@��@��@��@�-@�@`B@?}@�@�/@�/@��@��@j@Z@Z@I�@�@1@ƨ@t�@33@o@@
�@
�@
�H@
�\@
M�@
�@	�#@	��@	�7@	X@	G�@	�@��@��@�@Q�@  @�;@�@�P@l�@+@�y@�@ȴ@��@��@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A��Aˡ�A��AʾwAʑhA��yAȝ�A�;dA�
=A�ȴA�^5A�(�A�+A��A�ƨA�ZA�n�A��A��Aě�A�l�A���Aħ�A�+A�K�A¾wA�jA�=qA�1A�l�A�VA���A���A�v�A�Q�A�^5A���A��A�A��/A�ȴA��TA�ƨA���A�ffA��A�-A� �A�M�A��A��;A�\)A� �A��A�bA��A���A���A��+A�ȴA���A�x�A��^A��-A��\A�Q�A�&�A�r�A�"�A�%A��TA�|�A���A��A��A�K�A�ƨA�A�A��A�E�A���A�bA��+A��A�r�A�=qA���A�&�A���A��`A��7A��A�VA���A�I�A�A�n�A��\A�=qA���A�\)A�bNA���A�-A�x�A�E�A��A��A���A�n�A���A�7LA�{A~ �A};dA{O�Ay33Av�`At��Aq�^Ao�Aox�An��Al^5Ak��AkVAi�wAg�-Ac�AbE�Aap�A`��A_�wA^~�A]ƨA\�`AY�TAYVAX��AV�/AVbAS�AP�uAMS�AL9XAJ�+AG��AC��ABv�ABbAAA?��A>n�A=��A=�wA=VA<E�A<A�A;��A:�RA9�A81A6I�A5��A4�A4r�A3��A2��A1��A1
=A0�A/dZA-?}A+��A+�wA+|�A*$�A(��A'��A&��A%XA$�A �A33A-AA|�A7LA�jA{AO�AbA�A�RA-A�FAp�AK�A��AbA�HA��AG�A��A��A-AG�AjA�mAȴA�TA�Az�A�AhsA
jA	A��A��A�7A"�AQ�A�AO�A��Al�AQ�Ap�A �D@���@��j@�
=@�@�Z@���@��/@�M�@�\)@�V@���@��`@��@�C�@��/@�+@���@��@�|�@�=q@�h@��`@�z�@���@ߝ�@��@݉7@��m@��y@ם�@Ձ@�/@��`@ԋD@��T@�bN@ϝ�@���@�Z@˕�@��@ʗ�@�E�@�{@�@�V@Ȭ@�Q�@Ǯ@ƸR@�@���@� �@�dZ@�+@��@�?}@��@�
=@�ff@�5?@�@�x�@��@�o@�@�O�@�O�@��@��F@��R@���@�&�@�I�@��@��!@���@�I�@���@�+@��H@�v�@�M�@�5?@���@�/@�Z@�dZ@���@�v�@���@�&�@�r�@��@��@�=q@��T@�hs@��@�9X@��F@�@�5?@��^@��/@���@�A�@�l�@��@��@�S�@��@��#@�%@���@��
@���@�l�@�"�@���@���@��\@�~�@�M�@��@�hs@���@��@��m@�C�@��H@��T@��7@�hs@��@��`@��`@���@��@�Q�@�I�@�bN@�r�@�(�@���@�  @�(�@�(�@�A�@��@�S�@��R@�M�@���@�&�@�Ĝ@�A�@��u@���@��`@��@��@���@��u@���@�A�@��@���@�\)@�@��@���@�ff@�E�@�J@�X@�&�@��@�%@��@���@��@�Z@��m@��@�t�@���@��@��@�dZ@��@�
=@��@�C�@�@��R@�~�@�-@���@��h@�X@�/@�%@��/@��j@�r�@�A�@�(�@�Q�@���@�V@�G�@���@���@��u@��@�@���@��@��@�\)@��@��-@���@��#@��#@���@���@��u@�Q�@��@�7L@�?}@��@��9@��D@�z�@��@�1'@~ȴ@}p�@{��@{�@|Z@|�/@~�R@~V@}�T@}�@|z�@|j@{@z=q@y7L@x��@xĜ@x�`@x�`@x��@x��@x��@y%@x�@xQ�@x1'@xA�@xQ�@xQ�@xQ�@x1'@x �@w�;@w|�@vȴ@vE�@u�T@u�@up�@u`B@u�@t�@tZ@sƨ@sdZ@so@r�@r�!@rn�@rM�@r�@q��@q�^@q��@qhs@p��@pr�@o��@ol�@n�R@m�T@m�-@m?}@l(�@k�
@k��@kt�@k"�@jJ@i�@hr�@g�@g�w@g�@g��@g��@g��@g|�@g;d@fȴ@fV@e�@d��@d�/@d�@d�D@dZ@d(�@c��@c��@c"�@b�\@b^5@b-@bJ@a�@a��@ax�@a%@_�w@_;d@_
=@^��@^V@\�D@[��@[C�@[33@Z�!@ZM�@ZJ@Y�7@Y&�@X��@X��@XĜ@Xr�@XA�@X  @Wl�@W
=@V��@Vff@Vff@VV@VE�@VE�@V@U`B@T�/@T�j@T�D@TZ@S�F@SS�@So@S@S@R�H@R��@R�\@R~�@R=q@Q��@Q�7@Qx�@QG�@P�`@P��@P�9@Pr�@PQ�@P �@Pb@O��@N��@Nv�@M�@M�@M/@L��@L�@L�j@L��@L��@L�@L�@L��@Lz�@L�@Kƨ@K��@K�@K��@K�@Ko@J��@J^5@J=q@JJ@I�#@I�7@Ix�@Ix�@IG�@I&�@H��@H1'@H �@H  @G��@Gl�@G�@F��@F��@F�R@Fff@F5?@F{@E�h@E?}@D��@D9X@C�@CdZ@CC�@Co@B�@B�\@BJ@Ahs@A7L@@Ĝ@@�@@ �@?�;@?\)@>��@>�@>��@>v�@>V@>V@>ff@>ff@>V@>5?@>{@=�h@<�j@<9X@<(�@;��@;��@;t�@;C�@:��@:�@9�^@9�7@9X@9&�@8Ĝ@8��@8�@8bN@81'@7�@7l�@7�@6�@6��@6$�@6@5�T@5�h@5O�@5V@4�@4��@4j@3�m@3ƨ@3��@3��@3t�@3o@2��@2~�@2^5@2�@1��@1x�@1X@17L@0�`@0�9@0��@0bN@0 �@0  @0  @/�@/�w@/�P@/\)@.��@.ȴ@.ff@.E�@.{@-�@-`B@-O�@-p�@-?}@-�@,�@,��@,��@,�@+�
@+�F@+�@+33@*�H@*��@*��@*�!@*�!@*-@*n�@)��@)�7@)��@)hs@)7L@)7L@)�@)%@(�`@(�9@(�u@(�@(r�@(Q�@(1'@(  @'�@'\)@&ȴ@&$�@%��@%�@%O�@%�@$�@$�@$9X@$�@#��@#ƨ@#��@#��@#��@#dZ@#o@"�@"��@"��@"��@"~�@"=q@!x�@!%@ Ĝ@ �u@ bN@ A�@�@�;@�;@�w@�@�@�P@�P@|�@|�@l�@
=@��@ff@@�T@@�@/@�/@��@�@��@�@�@�D@j@Z@I�@9X@��@�F@��@t�@"�@�@��@��@M�@�^@��@hs@G�@�`@�u@Q�@ �@b@��@�P@|�@|�@+@��@ȴ@V@$�@�@�T@�h@O�@��@�@�j@��@�D@j@Z@9X@�@��@�m@ƨ@��@dZ@33@o@�@��@~�@=q@-@J@��@��@x�@7L@&�@��@Ĝ@�u@r�@Q�@ �@�;@�w@��@�P@\)@+@�y@ȴ@�R@��@ff@5?@$�@$�@{@@�T@��@��@��@��@�-@�@`B@?}@�@�/@�/@��@��@j@Z@Z@I�@�@1@ƨ@t�@33@o@@
�@
�@
�H@
�\@
M�@
�@	�#@	��@	�7@	X@	G�@	�@��@��@�@Q�@  @�;@�@�P@l�@+@�y@�@ȴ@��@��@�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBBVB)�B0!B?}BYBO�B`BBgmBo�B|�B�+B�\B��B��B��B��B��B��B�'B��B�B)�B5?BW
Bn�Bt�Bx�Bt�B�1B�VB�hB�uB�\B��B��B��B��BȴB�B��BB�BBVB!�B'�B&�B-B:^Bm�B�+B�^BPB�B�B�B�BPB1B��B�yB�sB�yB�NB�BB�;B��B��B��BƨB��B��B��B��B�oB�JB�B}�Bs�Bn�BjBbNBQ�B?}B8RB49B�B�BJB��B�B�B�NB�B�jB��B�\B�=B�B{�Be`BVBN�BG�B<jB8RB33B%�BbB
��B
�#B
�dB
��B
t�B
gmB
VB
G�B
9XB
+B
�B
1B
B	��B	�B	�B	�ZB	�)B	��B	�jB	�9B	�'B	�B	��B	��B	��B	��B	�B	z�B	t�B	ffB	]/B	E�B	$�B	%B��B�B�B��B��B��B��BǮBȴB��B�/B�5B�#B�HB�fB�`B�;B�
B�B�
B��B��B��B��B��BƨBÖB�wB�9B�B�B��B��B��B��B�{B�JB�%B}�By�Bw�Bv�Bt�Bs�Br�Bp�Bm�BjBjBhsBffBffBffBffBdZB`BB\)BZBW
BVBT�BR�BP�BN�BL�BI�BE�BA�B?}B?}B>wB;dB8RB6FB49B5?B6FB5?B49B33B33B0!B-B)�B)�B&�B%�B$�B$�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B$�B&�B'�B,B-B.B.B.B.B.B.B.B.B2-B5?B6FB8RB9XB:^B:^B9XB:^B;dB?}B@�BA�BA�BA�BF�BK�BO�BR�BZBe`Bk�Bm�Bp�Bp�Bq�Bq�Bw�By�Bz�Bz�Bz�Bz�B{�B|�B|�B}�B~�B�B�B�+B�7B�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�LB�LB�RB�dB�}B��BĜBŢBƨBƨBǮB��B��B��B�B�B�HB�ZB�`B�fB�sB�yB�B�B�B�B�B��B��B	B	+B	
=B	DB	PB	VB	uB	�B	�B	�B	�B	 �B	(�B	1'B	49B	5?B	6FB	8RB	>wB	F�B	F�B	I�B	N�B	O�B	Q�B	S�B	T�B	W
B	[#B	_;B	aHB	dZB	e`B	gmB	iyB	k�B	m�B	o�B	p�B	q�B	u�B	w�B	z�B	|�B	|�B	|�B	}�B	~�B	�B	�B	�+B	�1B	�7B	�DB	�JB	�PB	�PB	�PB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�3B	�'B	�!B	�B	�B	�B	�3B	�?B	�RB	�jB	�dB	�dB	�qB	B	ŢB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	ɺB	ȴB	��B	��B	��B	�#B	�)B	�#B	�B	�B	�#B	�#B	�#B	�#B	�)B	�5B	�BB	�BB	�BB	�HB	�HB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
VB
\B
bB
oB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
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
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
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
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
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
K�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
Q�B
R�B
R�B
S�B
T�B
T�B
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
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
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
\)B
]/B
]/B
\)B
]/B
]/B
]/B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
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
l�B
l�B
l�B
l�B
l�B
l�B
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
n�B
o�B
o�B
o�B
o�B
o�B
o�B
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
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBB�BBB�BBBBB2BWBB*�B0�BB4B]�BQtB`�BhyBqDB}�B�#B��B��B��B�B�HB��B�kB��B�FBhB,�B9�BY�Bp)Bu�By�Bw	B��B��B�B��B�]B��B�#B��B�GB�FB�yB�;BI[BzB�B#]B)�B'B/xB>�Bm�B�dB��BB�B�BB�BBHB B�B�kB��B��B�iB��B�B�&B�FB�~B�B��B�pB�6B�OB��B��B��Bw�Bq�Bm:Be8BX�BB�B9�B<%B:B�B�B��B�B��B�uB�0B�1B��B��B��B��B�QBjNBX�BR/BKB=uB9hB8�B-�B�BjB
�B
��B
�IB
w^B
l�B
[�B
NB
?=B
3qB
MB
	�B
rB
OB	�B	��B	�B	��B	��B	��B	�B	��B	�eB	��B	�lB	��B	��B	��B	{�B	x{B	hMB	b�B	LB	+�B	�B��B�/B�BҌB��B��B�BʲB��BωB��B�DB�>B��B�B��B�WBۈB�B��B�SB�B�GBПB�VB��B��B·B��B��B��B��B��B�B�;B�B�9B��B��B|<Bx�BwfBueBt�Bt@Br}Bp�Bk�BlxBi�Bg�BgBf�Bg�Bf6BcPB^�B[�BW�BV�BVBU,BS	BP*BO�BLBG�BCB@mBA(B@�B=B:�B8hB4�B6>B8PB6�B5(B4�B6KB2�B/dB,;B+�B*�B(B&�B&�B&B#QB#-B"�BB�B�BB�B�B�B�B�B�BBAB8B�BB�BZB�BUB�B4B!BBB!9B%�B$�B%�B)B*B,�B-�B.�B.~B.TB.�B.�B.�B.�B.�B3cB6�B7TB9�B:]B:�B:�B;�B;jB=JB@PB@�BA�BB=BC]BG�BMfBP`BR�BZqBgBl�Bn�BqkBq�BrBs=By3B{�B{�B{xB{AB{jB|B}B}sB~�B�B�dB��B��B�/B�aB�fB��B��B�hB�3B�yB�B�QB��B��B�B��B�+B�DB��B�7B�UB��B��B��B��B�SB��B�iB��B��B��B�(B��B��BĹB��B�MB�xB�^BˈB��B��BدBۊB�B�~B��B�B�sB�B�B��B�B�B�B�B�VB	�B	�B	
:B	+B	=B	�B	7B	3B	B	�B	 =B	!eB	(�B	0�B	4"B	53B	6GB	8�B	>�B	F�B	G4B	J�B	OB	P/B	RmB	T4B	UEB	WiB	[UB	_�B	bUB	d�B	ezB	g�B	icB	k�B	n
B	p B	q]B	rBB	u�B	w�B	z�B	|�B	}gB	}aB	~
B	~�B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�SB	�1B	�B	�wB	�_B	��B	�FB	�oB	��B	�MB	�oB	�bB	��B	��B	�jB	��B	�&B	�<B	�bB	��B	��B	��B	�jB	�nB	ŚB	��B	�4B	��B	��B	��B	�HB	��B	̰B	ʪB	��B	�2B	ҒB	��B	�gB	�vB	۪B	چB	�+B	�B	۪B	��B	ۅB	�B	�B	�?B	�6B	�CB	�GB	�>B	�B	�B	�B	�mB	�|B	�B	�B	�B	�B	�B	��B	�B	� B	��B	��B	��B	��B	��B	�	B	��B	�#B	�B	��B	��B	��B	� B	��B	��B	��B	�B	��B	�B	�5B	�KB	��B	�!B	��B	��B	� B	�XB	��B
 9B
 %B
+B
MB
�B
�B
�B
�B
FB
3B
1B
*B
.B
CB
]B
�B
�B
�B
	�B

WB

dB
^B
iB
iB
lB
�B
�B
�B
uB
sB
oB
mB
}B
sB
�B
B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
&B
B
�B
�B
�B
 2B
!B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
# B
$+B
$B
#�B
%B
%&B
%�B
%�B
&B
%�B
&B
&�B
'EB
'nB
(VB
)[B
)PB
*BB
*!B
*B
+%B
+B
+B
*�B
*�B
+B
+!B
+QB
+AB
,*B
,B
+�B
,B
,iB
,^B
-AB
-%B
-7B
-7B
-JB
.B
.B
.;B
.2B
.XB
.�B
/%B
/8B
/iB
/FB
/\B
08B
0%B
0UB
0\B
0CB
1<B
0oB
1\B
1kB
1�B
2�B
3CB
3FB
3NB
4RB
4vB
4�B
4�B
5^B
5�B
5jB
5}B
6tB
6�B
6�B
7aB
7iB
7iB
7cB
7HB
7;B
7LB
7WB
7aB
7`B
7�B
8�B
8�B
9eB
9vB
9�B
:B
:�B
:�B
;�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
JB
J�B
KB
J�B
J�B
J�B
L>B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
M!B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
O&B
N�B
P'B
P&B
P�B
R.B
SB
R�B
T
B
UB
UB
V#B
VB
VB
WB
WB
WB
W&B
W>B
XGB
XoB
X{B
XEB
XEB
X0B
X3B
X/B
YAB
ZhB
[9B
[8B
[GB
[9B
[#B
[-B
[CB
[^B
[9B
[9B
[ B
[.B
[NB
[QB
[�B
[sB
[QB
[EB
[CB
\BB
\cB
\5B
\*B
\?B
\5B
\*B
\?B
]+B
\6B
]+B
]9B
\pB
]sB
]aB
]tB
^OB
^PB
^eB
^rB
_wB
_IB
_TB
`LB
`5B
`AB
`YB
`XB
`MB
`MB
aVB
awB
avB
a_B
bjB
b�B
buB
cwB
cpB
c�B
c�B
clB
czB
cmB
c�B
c�B
c�B
dB
dkB
d�B
emB
emB
e`B
e�B
e�B
e�B
f�B
f�B
f�B
g{B
g�B
g�B
g�B
h|B
h�B
h�B
h}B
h�B
hB
h�B
h�B
h�B
h|B
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
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
n�B
o�B
o�B
o�B
o�B
o�B
o�B
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
v�B
v�B
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<C5V<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-*N<#�
<#�
<<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =-0.01 dbar                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201609291651422016092916514220160929165142  AO  ARCAADJP                                                                    20160605191545    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160605191545  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160605191545  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160929165142  QC  PRES            @�33D�` G�O�                PM  ARSQCTM V1.1                                                                20160929165142  QC  PSAL            @�33D�` G�O�                PM  ARSQOWGUV1.0                                                                20181027140158  IP                  G�O�G�O�G�O�                