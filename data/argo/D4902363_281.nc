CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-17T00:35:14Z creation;2018-09-17T00:35:19Z conversion to V3.1;2019-12-19T07:32:32Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180917003514  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_281                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؁��\� 1   @؁�b� @9�ڹ�Y��dL�{���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`��Bg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;fD;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�<�Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�3D�C3D΀ D�� D�  D�@ Dσ3D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D���D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�G�A�{A�{A�{B 
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
=BXp�B`�
Bg��Bo��Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D�=D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D;
D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRDŽD� RD�@RDƀRD��RD� RD�=DǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD��D�C�D΀RD��RD� RD�@RDσ�D��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD��D�C�DڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD��D�@RD�RD��RD��D�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1A�%A�1A�%A�A���A���A�  A�  A�  A�A�A�A�A�A�A�A�%A�%A�%A�%A�1A�1A�
=A�
=A�
=A�%AӉ7A�n�A�O�A�ZA�bA�5?A��+A�`BA���A�=qA��DA���A�t�A�%A��uA�(�A��A�  A��wA�^5A���A�(�A�XA�oA��+A�9XA��A���A�&�A��+A��A���A�bNA���A��;A�%A��A�A�A�C�A�E�A��HA�x�A�+A�|�A�dZA�x�A�v�A���A��A��RA�A�9XA�Q�A�|�A���A�I�A���A�ĜA�$�A��!A�ƨA��!A���A��A�ZA��A�JA��A��A���A�x�A���A�-A���A�ZA��`A���A�v�A��A�A�A��A��uAA/A~�yA~I�A}�PA|bAx��Aw�#Aw�7Av�/At{As
=Ap��AoG�Am�hAl�`Alv�Ak��Ah�RAf�`Aet�Ad=qAcl�AcAat�A_&�A^�RA]|�A\z�A[7LAZ~�AYC�AX�AX1AWK�AV�\AU��AS�-AR�HARv�ARQ�AQK�APbAN�RAMhsAL1AK"�AI�AI�-AIdZAH��AHr�AGƨAF��AE�-AE�ADffACp�ABbAA��A@�/A?�A>��A=�TA<��A;�7A;�A:�DA8��A7��A6=qA5�^A5hsA5VA3�hA2E�A0��A0(�A-��A-+A,�A,I�A,1A+�;A+��A+C�A*�jA*JA)7LA(��A'��A'"�A$bNA#�FA"�RA!|�A ��A ��A �A (�A\)AĜA �AO�A��AbA�
A�AO�A��AȴA�+AjAjA1'AC�A�
A��Az�A��A=qA�A�wA
=A��A
=A�A5?A�PA�yA �A�
A&�A
^5A	33A��A��A1'A��A��A�`A5?Al�A%A�A�
A��AZAA�A ^5@���@�33@�  @�hs@�j@� �@�\@�/@�@���@���@�=q@�&�@�1@�
=@�ȴ@�M�@�$�@��@�h@���@�bN@��m@�h@�(�@�\)@�ȴ@��@�G�@� �@�C�@ّh@֟�@��/@Ӯ@ҸR@�~�@�x�@ϥ�@���@�J@ˍP@�v�@��@ɺ^@���@�A�@��;@�+@ř�@�"�@���@���@�ȴ@���@�V@���@��@�Z@��@��@��!@�$�@��@���@�X@�bN@�K�@�^5@�hs@�Z@�ƨ@�+@��+@�ff@��7@���@�o@��T@�%@��u@���@�33@�{@�O�@��@��@���@��R@�^5@��@�?}@���@�b@��
@�l�@��@�=q@��h@�j@�ȴ@���@��@�p�@�/@��`@��/@���@�Q�@���@�V@�E�@�@���@�%@��@�9X@��@��@�"�@���@���@��\@�ff@��#@�%@���@�@��H@��R@�M�@��@��7@��@��j@�Q�@��@���@�C�@���@�E�@��7@�O�@���@�z�@�S�@�
=@�^5@�x�@��@��@���@���@�j@�  @�|�@�"�@�~�@�{@�p�@�7L@��@��@��@���@� �@�b@��m@�ƨ@��@�\)@�;d@�o@��@���@���@�~�@�^5@�=q@�{@�@�J@�J@�$�@�=q@��@�{@���@��-@�p�@�?}@��@��@�j@�9X@�@~�y@~�+@}�@}��@}�@{��@{dZ@{t�@{�F@{�@{t�@{dZ@{S�@{@z�\@z~�@zJ@y��@yX@xQ�@w|�@w\)@w;d@w+@w�@w�@v��@v��@v@u�h@u/@t��@t�j@tZ@s�m@s�@sC�@r�@r��@r�\@r^5@r^5@r^5@r�@q�#@qhs@p�u@p1'@p  @o��@o�w@o�@o|�@oK�@n��@n�R@n�+@nE�@n@m@m�h@m`B@m?}@mV@l�/@l��@lI�@k��@j�H@jn�@j�@i��@i%@h�`@h��@hQ�@g�@gl�@g�@f�@fE�@e�h@d��@d��@d�@d�D@dj@cƨ@cS�@c33@c"�@b�H@b�@a7L@_�@_�w@_��@_\)@_
=@^V@]�@]�-@]O�@]/@\�@\��@\�j@\Z@[�m@[�@[o@Z��@Z��@Z~�@ZJ@Y��@Yx�@YX@XĜ@XQ�@W�@W�@W�P@Wl�@W+@V�y@Vȴ@V�R@Vff@V5?@U@UO�@T�@T�D@T9X@T1@Sƨ@S33@R^5@RM�@RM�@R=q@R=q@RJ@Q��@Q7L@PĜ@P�9@P�@O�@O��@O|�@OK�@OK�@O�@N�@Nv�@M��@MV@L�@Lj@L9X@L1@K�@Ko@J�H@J�\@J-@I��@I�#@I��@IX@I%@H��@HQ�@H �@G�@G�@Fȴ@F�+@Fv�@FV@E�T@E?}@D�j@Dj@DI�@C��@CS�@Co@C@C@C@B�@B~�@B=q@B-@BJ@A�#@A��@AX@A&�@A&�@A�@@��@@Ĝ@@�9@@�@?�@?�;@?�@?�P@?l�@?+@>ȴ@>v�@>ff@>E�@>{@>@>@>@>{@=�T@=p�@=O�@=�@<��@<�D@<Z@<9X@;��@;�m@;��@;t�@;t�@;dZ@;dZ@;S�@;33@:��@:��@:-@:-@9��@8��@8�9@8�u@8�@8�@8r�@8bN@8 �@8 �@8 �@7��@7�@7|�@7\)@7+@6�R@6ff@6E�@6{@6@5�T@5V@4��@4�D@4Z@4�@3�F@3�@333@333@333@333@3@2��@2�!@2��@2�\@2~�@2-@1�@1�^@1��@1G�@1%@0Ĝ@0��@0�u@0r�@01'@/��@/�P@/|�@/\)@/;d@/�@/
=@.�y@.ȴ@.v�@-��@,��@,�D@,j@,(�@+�m@+t�@+o@*��@*-@)��@*J@)��@)G�@(�9@(1'@(b@'�;@'l�@&�y@&V@%�@%@%�h@%?}@%V@$�/@$�@$j@#��@#C�@#"�@"�\@"-@!��@!��@!hs@!�@ �`@ �`@ ��@ Ĝ@ A�@�;@�w@\)@;d@�@�@�@
=@��@��@�+@V@$�@�T@��@��@p�@`B@O�@/@�@V@�/@��@�D@9X@1@�m@�F@��@C�@o@�H@M�@M�@-@��@�^@�@r�@bN@bN@A�@�;@+@�y@��@v�@5?@@��@�-@��@?}@V@�@��@z�@Z@1@�
@��@�@t�@S�@"�@@@�@��@�!@��@�\@~�@n�@M�@-@�#@��@�7@X@G�@7L@�@%@��@��@Ĝ@1'@�;@�w@�@�P@;d@+@�@
=@�y@�@��@ff@5?@�@�T@��@?}@�@��@��@�@9X@(�@(�@1@�
@��@�@dZ@"�@
�@
��@
~�@
M�@
�@
J@
J@	��@	��@	��@	��@	�#@	�#@	�#@	�#@	��@	��@	�7@	G�@	G�@	7L@��@��@�u@�u@��@��@��@��@��@��@bN@b@�;@�;@��@�w@��@l�@\)@K�@;d@+@�@
=@�y@ȴ@��@v�@5?@�@�@�T@��@@�-@�-@�-@`B@/@�@V@�@�D@j@Z@9X@�m@�@t�@dZ@33@o@@��@��@�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1A�%A�1A�%A�A���A���A�  A�  A�  A�A�A�A�A�A�A�A�%A�%A�%A�%A�1A�1A�
=A�
=A�
=A�%AӉ7A�n�A�O�A�ZA�bA�5?A��+A�`BA���A�=qA��DA���A�t�A�%A��uA�(�A��A�  A��wA�^5A���A�(�A�XA�oA��+A�9XA��A���A�&�A��+A��A���A�bNA���A��;A�%A��A�A�A�C�A�E�A��HA�x�A�+A�|�A�dZA�x�A�v�A���A��A��RA�A�9XA�Q�A�|�A���A�I�A���A�ĜA�$�A��!A�ƨA��!A���A��A�ZA��A�JA��A��A���A�x�A���A�-A���A�ZA��`A���A�v�A��A�A�A��A��uAA/A~�yA~I�A}�PA|bAx��Aw�#Aw�7Av�/At{As
=Ap��AoG�Am�hAl�`Alv�Ak��Ah�RAf�`Aet�Ad=qAcl�AcAat�A_&�A^�RA]|�A\z�A[7LAZ~�AYC�AX�AX1AWK�AV�\AU��AS�-AR�HARv�ARQ�AQK�APbAN�RAMhsAL1AK"�AI�AI�-AIdZAH��AHr�AGƨAF��AE�-AE�ADffACp�ABbAA��A@�/A?�A>��A=�TA<��A;�7A;�A:�DA8��A7��A6=qA5�^A5hsA5VA3�hA2E�A0��A0(�A-��A-+A,�A,I�A,1A+�;A+��A+C�A*�jA*JA)7LA(��A'��A'"�A$bNA#�FA"�RA!|�A ��A ��A �A (�A\)AĜA �AO�A��AbA�
A�AO�A��AȴA�+AjAjA1'AC�A�
A��Az�A��A=qA�A�wA
=A��A
=A�A5?A�PA�yA �A�
A&�A
^5A	33A��A��A1'A��A��A�`A5?Al�A%A�A�
A��AZAA�A ^5@���@�33@�  @�hs@�j@� �@�\@�/@�@���@���@�=q@�&�@�1@�
=@�ȴ@�M�@�$�@��@�h@���@�bN@��m@�h@�(�@�\)@�ȴ@��@�G�@� �@�C�@ّh@֟�@��/@Ӯ@ҸR@�~�@�x�@ϥ�@���@�J@ˍP@�v�@��@ɺ^@���@�A�@��;@�+@ř�@�"�@���@���@�ȴ@���@�V@���@��@�Z@��@��@��!@�$�@��@���@�X@�bN@�K�@�^5@�hs@�Z@�ƨ@�+@��+@�ff@��7@���@�o@��T@�%@��u@���@�33@�{@�O�@��@��@���@��R@�^5@��@�?}@���@�b@��
@�l�@��@�=q@��h@�j@�ȴ@���@��@�p�@�/@��`@��/@���@�Q�@���@�V@�E�@�@���@�%@��@�9X@��@��@�"�@���@���@��\@�ff@��#@�%@���@�@��H@��R@�M�@��@��7@��@��j@�Q�@��@���@�C�@���@�E�@��7@�O�@���@�z�@�S�@�
=@�^5@�x�@��@��@���@���@�j@�  @�|�@�"�@�~�@�{@�p�@�7L@��@��@��@���@� �@�b@��m@�ƨ@��@�\)@�;d@�o@��@���@���@�~�@�^5@�=q@�{@�@�J@�J@�$�@�=q@��@�{@���@��-@�p�@�?}@��@��@�j@�9X@�@~�y@~�+@}�@}��@}�@{��@{dZ@{t�@{�F@{�@{t�@{dZ@{S�@{@z�\@z~�@zJ@y��@yX@xQ�@w|�@w\)@w;d@w+@w�@w�@v��@v��@v@u�h@u/@t��@t�j@tZ@s�m@s�@sC�@r�@r��@r�\@r^5@r^5@r^5@r�@q�#@qhs@p�u@p1'@p  @o��@o�w@o�@o|�@oK�@n��@n�R@n�+@nE�@n@m@m�h@m`B@m?}@mV@l�/@l��@lI�@k��@j�H@jn�@j�@i��@i%@h�`@h��@hQ�@g�@gl�@g�@f�@fE�@e�h@d��@d��@d�@d�D@dj@cƨ@cS�@c33@c"�@b�H@b�@a7L@_�@_�w@_��@_\)@_
=@^V@]�@]�-@]O�@]/@\�@\��@\�j@\Z@[�m@[�@[o@Z��@Z��@Z~�@ZJ@Y��@Yx�@YX@XĜ@XQ�@W�@W�@W�P@Wl�@W+@V�y@Vȴ@V�R@Vff@V5?@U@UO�@T�@T�D@T9X@T1@Sƨ@S33@R^5@RM�@RM�@R=q@R=q@RJ@Q��@Q7L@PĜ@P�9@P�@O�@O��@O|�@OK�@OK�@O�@N�@Nv�@M��@MV@L�@Lj@L9X@L1@K�@Ko@J�H@J�\@J-@I��@I�#@I��@IX@I%@H��@HQ�@H �@G�@G�@Fȴ@F�+@Fv�@FV@E�T@E?}@D�j@Dj@DI�@C��@CS�@Co@C@C@C@B�@B~�@B=q@B-@BJ@A�#@A��@AX@A&�@A&�@A�@@��@@Ĝ@@�9@@�@?�@?�;@?�@?�P@?l�@?+@>ȴ@>v�@>ff@>E�@>{@>@>@>@>{@=�T@=p�@=O�@=�@<��@<�D@<Z@<9X@;��@;�m@;��@;t�@;t�@;dZ@;dZ@;S�@;33@:��@:��@:-@:-@9��@8��@8�9@8�u@8�@8�@8r�@8bN@8 �@8 �@8 �@7��@7�@7|�@7\)@7+@6�R@6ff@6E�@6{@6@5�T@5V@4��@4�D@4Z@4�@3�F@3�@333@333@333@333@3@2��@2�!@2��@2�\@2~�@2-@1�@1�^@1��@1G�@1%@0Ĝ@0��@0�u@0r�@01'@/��@/�P@/|�@/\)@/;d@/�@/
=@.�y@.ȴ@.v�@-��@,��@,�D@,j@,(�@+�m@+t�@+o@*��@*-@)��@*J@)��@)G�@(�9@(1'@(b@'�;@'l�@&�y@&V@%�@%@%�h@%?}@%V@$�/@$�@$j@#��@#C�@#"�@"�\@"-@!��@!��@!hs@!�@ �`@ �`@ ��@ Ĝ@ A�@�;@�w@\)@;d@�@�@�@
=@��@��@�+@V@$�@�T@��@��@p�@`B@O�@/@�@V@�/@��@�D@9X@1@�m@�F@��@C�@o@�H@M�@M�@-@��@�^@�@r�@bN@bN@A�@�;@+@�y@��@v�@5?@@��@�-@��@?}@V@�@��@z�@Z@1@�
@��@�@t�@S�@"�@@@�@��@�!@��@�\@~�@n�@M�@-@�#@��@�7@X@G�@7L@�@%@��@��@Ĝ@1'@�;@�w@�@�P@;d@+@�@
=@�y@�@��@ff@5?@�@�T@��@?}@�@��@��@�@9X@(�@(�@1@�
@��@�@dZ@"�@
�@
��@
~�@
M�@
�@
J@
J@	��@	��@	��@	��@	�#@	�#@	�#@	�#@	��@	��@	�7@	G�@	G�@	7L@��@��@�u@�u@��@��@��@��@��@��@bN@b@�;@�;@��@�w@��@l�@\)@K�@;d@+@�@
=@�y@ȴ@��@v�@5?@�@�@�T@��@@�-@�-@�-@`B@/@�@V@�@�D@j@Z@9X@�m@�@t�@dZ@33@o@@��@��@�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BŢBŢBƨBƨBƨBƨBƨBƨBǮBǮBƨBŢBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBƨBƨBŢBB�!B�^Bq�B��B��B��B�BƨB�jB�LB�!B�?B�dB�'B��B��B�FB�9B��B��B�3B��B��B�hB�JB��B��B��B�bB�7B�B�B�Bu�BiyBffB[#B\)BN�BK�B;dBG�BE�B;dB+B&�B�B{BPB��B��B��B�wB�B��B�\B�PB�JB�%B�Bp�B\)BT�BI�BH�BD�B-B�B\BB  B
��B
�B
�mB
�5B
��B
�FB
��B
��B
��B
��B
�uB
�+B
�%B
�B
|�B
r�B
bNB
B�B
D�B
D�B
7LB
�B
�B
	7B	��B	�B	��B	�B	�`B	��B	�}B	�}B	�}B	�jB	�^B	�B	��B	��B	��B	��B	�JB	�+B	�B	|�B	x�B	n�B	gmB	`BB	K�B	M�B	M�B	K�B	@�B	1'B	+B	$�B	�B	�B	�B	�B	�B	�B	�B	hB	VB	B	B	B	1B	+B	VB	+B	1B	  B	B��B�B��B�B�ZB�BB�B�BB�)B�BƨBƨB�qB�}B��B�RB�^B�LB�XB�XB�LB�9B�B�B��B��B��B�hB}�B�DB�1B�B�DB�VB�DB�B|�B|�Bz�Bt�Bu�Bu�Bx�Bw�Bv�Bu�Bu�Bs�Bs�Bq�BjB^5BR�BM�BA�BD�BT�BXBS�BM�BD�BK�BO�BK�BI�BH�BE�BJ�BE�BB�B>wBF�BG�BD�BB�BC�B=qB:^B:^B<jB<jB6FB1'B:^B9XB6FB1'B+B#�B�B%�B+B/B&�B$�B�B�B�B%�B"�B"�B$�B+B)�B+B)�B'�B$�B#�B!�B�B�B"�B"�B �B�B�B�B\B
=B�B�B �B$�B�B�B �B �B�B �B+B(�B%�B&�B'�B#�B�B�B!�B'�B"�B33B33B1'B1'B49B6FB5?B1'B7LB:^B:^B8RB49B2-B8RB8RB7LB@�BA�BA�BE�B>wB8RBC�BC�BG�BN�BL�BO�BL�BR�BXB[#B_;B]/BbNBcTBcTBe`BhsBk�BjBhsBiyBjBgmBk�Bt�B�B�B�B�B�+B�%B�B~�B�PB�oB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�3B�XB�XB�XB�jB�wB��BĜBǮBɺB��B��B��B��B��B�B�B�B�
B�TB�NB�fB�B��B��B��B��B��B��B��B��B	B	B	JB	VB	\B	VB	DB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	%�B	(�B	+B	.B	0!B	2-B	49B	49B	7LB	7LB	7LB	9XB	<jB	;dB	A�B	C�B	B�B	C�B	F�B	H�B	J�B	N�B	N�B	P�B	W
B	\)B	_;B	_;B	aHB	aHB	aHB	`BB	aHB	cTB	bNB	e`B	iyB	hsB	m�B	r�B	r�B	r�B	s�B	s�B	r�B	r�B	r�B	v�B	x�B	z�B	{�B	{�B	|�B	�B	�B	�B	�%B	�+B	�1B	�=B	�7B	�1B	�7B	�7B	�DB	�bB	�oB	�uB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�FB	�LB	�RB	�jB	�wB	�wB	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�5B	�/B	�/B	�/B	�;B	�BB	�BB	�NB	�NB	�TB	�TB	�TB	�NB	�ZB	�ZB	�fB	�mB	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
  B
  B	��B
B
B
B
%B
B
B
%B
1B
1B
1B

=B
DB

=B

=B

=B
DB

=B
JB
JB
JB
\B
bB
hB
bB
\B
\B
hB
oB
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
$�B
%�B
&�B
&�B
%�B
$�B
#�B
%�B
%�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
(�B
'�B
)�B
)�B
+B
,B
(�B
-B
/B
0!B
0!B
1'B
2-B
2-B
33B
33B
2-B
2-B
1'B
2-B
1'B
1'B
1'B
2-B
2-B
33B
2-B
0!B
33B
49B
5?B
49B
49B
5?B
5?B
7LB
7LB
7LB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
;dB
:^B
;dB
;dB
<jB
=qB
<jB
<jB
<jB
>wB
?}B
?}B
?}B
?}B
?}B
>wB
>wB
=qB
;dB
;dB
=qB
?}B
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
?}B
?}B
?}B
>wB
@�B
?}B
>wB
=qB
=qB
>wB
@�B
?}B
@�B
@�B
A�B
@�B
@�B
?}B
B�B
C�B
B�B
C�B
D�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
H�B
J�B
I�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
M�B
M�B
N�B
M�B
N�B
O�B
N�B
O�B
N�B
O�B
P�B
O�B
R�B
S�B
S�B
R�B
Q�B
R�B
T�B
VB
T�B
S�B
Q�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
W
B
XB
YB
XB
YB
YB
YB
YB
ZB
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
]/B
]/B
]/B
\)B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
\)B
^5B
`BB
`BB
`BB
_;B
aHB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
aHB
bNB
aHB
aHB
cTB
cTB
bNB
cTB
bNB
dZB
e`B
dZB
dZB
dZB
e`B
e`B
dZB
e`B
e`B
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
k�B
k�B
l�B
l�B
k�B
k�B
k�B
jB
jB
k�B
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
m�B
m�B
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
m�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BŢBŢBƨBƨBƨBƨBƨBƨBǮBǮBƨBŢBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBƨB��B�%B�B��BĜBwB��B�B�pB��BɠB��B��B�MB��B�jB�B�kB��B��B��B��B�\B�nB��B��B�FB�B�/B�B��B��B��B�YB��B��BwfBk�BhXB]dB^OBQNBNB>BBH�BF�B=<B-�B)B�B�BVB��B׍B��B��B�!B��B�:B�B�"B��B�uBr�B^�BWYBK�BI�BE�B/iB \B�B?BUB
�LB
��B
�sB
ߊB
өB
��B
��B
�QB
��B
��B
�FB
��B
��B
��B
}�B
tB
dtB
E�B
E�B
E9B
8�B
�B
$B
�B	��B	��B	��B	�B	�B	�B	��B	�;B	��B	�qB	�dB	�B	�EB	��B	�#B	��B	��B	�KB	�[B	}�B	y�B	o�B	h�B	a�B	M�B	N�B	NpB	LdB	A�B	2�B	,�B	&�B	CB	�B	�B	 B	5B	CB	YB	oB	vB	�B	B	?B		�B	�B	B	fB		RB	�B	GB�VB�ZB��B��B�fB��B��B��B��B��B��B�fB�HB��B��B��B��B�B��B��B��B��B��B�B��B��B��B�B�;B�JB��B�{B��B�pB��B�3B~B}�B{�Bu�Bv�BvzBy>BxRBw2BvFBvBtBs�Bq�Bk6B_�BT�BO�BDMBF�BU�BXyBT�BOBFtBL�BP}BL�BJ�BI�BF�BKDBF�BC�B?�BGEBHBESBC-BD3B>wB;dB;dB="B=<B7�B2�B:�B:DB7fB2|B,�B&LB!�B'mB+�B/�B($B%�B!-B)B�B&�B#�B#�B%zB+QB*eB+6B*KB(XB%`B$tB"hBB�B#nB#TB!bBjB�BB�B0B�B�B!|B%,B�B�B!|B!�BEB!|B+QB)_B&�B'mB(sB$�BBeB"�B(�B$&B3MB3�B1�B1�B4�B6�B5�B1�B7�B:�B:�B8�B5B3B9	B9	B8B@�BBBA�BFB?HB9�BD3BD�BHKBOBBM�BP}BM�BS�BX�B[�B_�B]�Bb�Bc�Bc�Be�Bh�Bk�Bj�Bh�BjBkBhsBl�ButB�3B�9B�MB�gB�EB�?B��B� B��B��B��B��B��B��B��B��B��B�B��B� B��B�&B�HB�hB��B��B��B��B��B��B��B��B��B��B�	B�0B�"B�<B�bB�oB�QBچBٚB��B�B��B�B��B��B�B�B�	B�8B�>B�6B�qB	aB	�B	~B	pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&B	)B	+B	.B	0!B	2-B	49B	4nB	7fB	7�B	7�B	9�B	<�B	;�B	A�B	C�B	B�B	C�B	F�B	H�B	J�B	N�B	O(B	QNB	W$B	\)B	_;B	_VB	aHB	abB	aHB	`vB	a|B	cnB	b�B	e�B	i�B	h�B	m�B	r�B	r�B	r�B	s�B	s�B	r�B	r�B	r�B	v�B	y	B	z�B	|B	|B	}<B	�;B	�-B	�9B	�?B	�+B	�KB	�=B	�RB	�KB	�lB	�lB	��B	�}B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�LB	�0B	�"B	�OB	�[B	�TB	�ZB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�,B	�@B	�aB	�gB	�/B	�OB	�IB	�dB	�dB	�VB	�\B	�\B	�hB	�hB	�TB	�TB	�B	�hB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�	B	�	B	�B	��B	�B	�B	�B
 B
B
B
 B
 4B
 4B	�.B
;B
B
3B
?B
9B
9B
YB
KB
KB
fB

XB
^B

XB

rB

XB
^B

rB
dB
~B
~B
vB
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
yB
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
#B
#�B
$�B
%�B
$�B
%�B
&�B
&�B
%�B
$�B
$B
%�B
%�B
%B
%�B
%�B
'B
&�B
(
B
(
B
(�B
)�B
)�B
)�B
*B
)B
($B
*B
*0B
+B
,=B
)_B
-)B
/OB
0!B
0!B
1'B
2GB
2GB
33B
33B
2GB
2GB
1AB
2GB
1AB
1[B
1AB
2GB
2GB
3MB
2GB
0oB
3MB
4TB
5ZB
4TB
4TB
5?B
5tB
7LB
7LB
7LB
6`B
7fB
8RB
8RB
8lB
8lB
8lB
9rB
9�B
;B
:xB
;dB
;B
<�B
=qB
<�B
<jB
<�B
>�B
?}B
?�B
?�B
?�B
?�B
>�B
>�B
=�B
;�B
;�B
=�B
?�B
>�B
>wB
>�B
>�B
>�B
>�B
?�B
@�B
?�B
?�B
?�B
>�B
@�B
?}B
>�B
=�B
=�B
>�B
@�B
?�B
@�B
@�B
A�B
@�B
@�B
?�B
B�B
C�B
B�B
C�B
D�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
G�B
H�B
J�B
I�B
J�B
K�B
K�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
M�B
M�B
N�B
M�B
N�B
O�B
OB
O�B
N�B
O�B
Q B
PB
R�B
TB
TB
SB
RTB
S&B
T�B
VB
UB
T,B
R B
VB
VB
W$B
W$B
W$B
X+B
X+B
X+B
W$B
X+B
YKB
X+B
Y1B
Y1B
YB
Y1B
Z7B
[#B
[#B
[=B
[=B
[=B
\)B
\)B
\CB
\CB
\)B
]/B
]B
]/B
\CB
\CB
\CB
\CB
^5B
^OB
^5B
^5B
^OB
^5B
^5B
^OB
^5B
\]B
^OB
`BB
`\B
`\B
_VB
aHB
aHB
aHB
aHB
abB
`BB
`BB
abB
abB
bNB
abB
a|B
cnB
cnB
bNB
cnB
b�B
dZB
e`B
dtB
dtB
dtB
ezB
ezB
dtB
ezB
ezB
ffB
f�B
g�B
hXB
hsB
hsB
hsB
hXB
h�B
h�B
i_B
i�B
hXB
hsB
h�B
h�B
h�B
iyB
i�B
h�B
i�B
i�B
k�B
k�B
l�B
l�B
k�B
k�B
k�B
j�B
j�B
k�B
l�B
l�B
lqB
l�B
l�B
m�B
m�B
m�B
m�B
m�B
mwB
m�B
l�B
m�B
m�B
m�B
m�B
n�B
n}B
n�B
n�B
n}B
n�B
n�B
m�B
n�B
o�B
o�B
o�B
n�B
o�B
o�B
o�B
n�B
n�B
p�B
q�B
p�B
q�B
q�B
q�B
q�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809210034272018092100342720180921003427201809210200162018092102001620180921020016201809220026122018092200261220180922002612  JA  ARFMdecpA19c                                                                20180917093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180917003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180917003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180917003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180917003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180917003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180917003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180917003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180917003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180917003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180917005615                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180917153432  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180920153427  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180920153427  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180920170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180921152612  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                