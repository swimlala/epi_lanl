CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-18T00:35:21Z creation;2017-12-18T00:35:24Z conversion to V3.1;2019-12-19T07:49:55Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20171218003521  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_191                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�=��� 1   @�=�www�@4CS����d�4�K1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @:=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B�B�B�B�B�B�B�B�8RB�8RB�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�x�A�x�A�x�A�v�A�v�A�z�A�|�A�z�A�|�A�z�A�z�A�x�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�~�ÃÃẢ7A̗�A���A���ȂhA�5?A��A˛�A�S�A�Aʣ�A�M�A�=qA��A�%A��`A��Aɴ9Aɗ�AȶFAǰ!Aƺ^Aď\A�M�A�/A��A��yA��A�A�33A���A�O�A��A��`A��FA�bA���A�E�A�r�A�1'A��A�dZA��HA�`BA��hA���A�%A�hsA�  A���A�dZA���A���A��-A�"�A��A���A���A�t�A���A��A�=qA��\A�bA��/A�"�A��#A�1'A��!A�(�A��A�1A���A���A���A�dZA�^5A��;A���A�bNA���A�ƨA���A�VA��yA�
=A���A�A�A���A���A�\)A���A��A�A}�Az(�Ax��AvZAt1ApjAk`BAk�AjM�Ag��Af$�Abz�A_t�A]C�AZ~�AV��AT��AN�AMK�AM%AL�RALn�AKAKp�AK"�AJbAH�AFjAC��AB�A@ĜA>�jA=��A<�jA;/A:$�A9�A9�A8�9A7ƨA7%A6�/A6�/A6~�A5�A4��A3�7A2A�A0��A/;dA.bNA.�A-?}A,��A,(�A+
=A)\)A'ƨA'O�A&�A&I�A%33A$�jA$�+A"��A"bA 1'A��A"�AbAI�A��Av�A�AM�AdZA�!AffA�FA�Az�A�+A�A
9XA	�;A	"�A�9AVAXA��A�A�-A�A��Ar�A5?A{A��A �@�|�@��u@�b@��R@��`@��@��@��H@���@��@�1@@��T@���@��@�~�@�ff@��
@�^@���@��/@���@��@�Z@�@�5?@�O�@�r�@�33@��T@�Q�@ڇ+@�O�@�A�@��;@���@Չ7@�V@�9X@�+@�&�@д9@�"�@Ο�@��@ʇ+@�x�@���@�S�@�ff@��@�\)@�~�@�/@��@���@�33@���@��j@��@��
@��w@�ȴ@���@�~�@�V@�`B@�r�@���@�~�@�J@�O�@���@�1@�"�@���@���@��R@�E�@��@��j@�r�@�A�@���@���@�|�@�|�@�\)@�
=@���@��H@��!@�v�@�ff@�{@���@�hs@�V@��D@�z�@�bN@�Z@�Q�@�b@��@�l�@�S�@�C�@�C�@�;d@�o@��@�ȴ@���@��\@�{@���@���@��@��@��@���@�$�@��T@���@��-@��h@�p�@�G�@�V@��@���@�I�@��m@���@�+@���@���@��\@�5?@��@��-@�x�@���@���@��7@��@��w@��@���@��\@��@�p�@�O�@�O�@��h@���@���@��^@��^@��-@��@��7@���@�x�@�&�@���@���@��@�Q�@�Q�@��@�ƨ@���@���@���@��@��w@��w@���@�l�@�;d@��@���@��!@�~�@�M�@�5?@�{@��@��^@���@��7@�hs@�/@��/@��D@�bN@�1'@�  @��
@���@�l�@�S�@�S�@�+@���@��+@�v�@�ff@�{@���@�`B@���@�Ĝ@��@���@�z�@�bN@�Z@�A�@�ƨ@�dZ@�;d@��H@���@���@���@���@���@�~�@��@���@���@���@�x�@���@�z�@�Z@�Z@��@��w@���@��P@��@��@�|�@�|�@�|�@�+@���@��R@���@��+@�v�@�V@�E�@�J@�@��@��-@�G�@��/@��u@�r�@�A�@�b@���@��
@���@�t�@���@��y@��!@���@�V@�@���@��@���@�G�@�/@�V@��j@�Z@�9X@��@��m@���@��P@�t�@�;d@��@���@��\@�ff@�M�@�=q@�=q@�J@��@��@��j@��9@��`@�Ĝ@��u@�I�@��
@��F@�t�@�;d@��@��R@�^5@�{@���@���@��h@�hs@�O�@�?}@��@�%@��`@��j@��D@� �@��@~��@~��@~�+@~$�@~@}�@~@~@}�T@}�@}�@|�j@|I�@|1@{�F@{��@{�@{C�@{@z��@y��@y%@x�9@xQ�@w�;@v��@vV@v$�@u�-@u�@t��@tz�@t�@s�m@s��@so@r��@r=q@rJ@q�@q�^@qx�@pĜ@p�u@pbN@p1'@o�@o�P@o
=@n�R@n��@nV@m�h@m?}@l�@k��@k33@j�\@jn�@j-@i�#@i�7@ihs@iG�@i%@hbN@g�@g+@f�@f�R@f�+@fff@f5?@e��@e�@d��@dI�@cƨ@c33@b��@b��@b~�@b~�@b-@a�^@a7L@`Ĝ@`r�@_��@_K�@^�y@^�+@]�@]�@]?}@]�@\�@\�/@\�/@\�j@\��@\z�@\Z@\9X@\1@[dZ@Z�@Z�!@Zn�@Z-@Y��@Y�^@Yhs@X��@X�@XA�@W�;@W�@W\)@W�@V�y@V��@VV@V{@U�@U�-@Up�@UO�@T��@T�@Tz�@TZ@TI�@S�
@S�@SC�@R�@R�\@R-@Q�@Q��@Q��@Q�7@Qx�@Q7L@Q�@P�`@PĜ@P�@P1'@O\)@N��@N��@N�+@N��@N�+@N{@M�-@MV@L(�@Kt�@KS�@Ko@J�\@J=q@J�@I��@I�#@I�7@I7L@I&�@I�@H�`@H�9@H��@H�@HQ�@H �@H  @G|�@F�y@F�+@E�T@E�-@E`B@E�@D�/@Dz�@D1@C�m@C��@CS�@C@B��@B^5@B�@A�@A�#@A��@Ahs@A7L@@��@@�@@r�@@ �@?�@>�@>V@>$�@=��@=��@=O�@=V@<�/@<�@<(�@<1@;��@;ƨ@;S�@;@:�!@:=q@:J@9��@9�@9�#@9x�@97L@9�@8Ĝ@8r�@8A�@8b@8  @8  @7�@7�w@7K�@6��@6ȴ@6�R@6��@6��@6v�@6ff@6E�@5�@5?}@4�@4Z@4�@3�
@3S�@3C�@3o@3@3@2�@2�H@2n�@2M�@1��@1��@1hs@1�@0�@0 �@0b@/�@/�@/l�@/;d@.��@.ȴ@.�+@-�T@-��@-p�@-?}@,��@,�j@,Z@,�@+�
@+�@+"�@*�@*�H@*�!@*n�@*J@)��@)��@)�7@)�7@)x�@)%@(�`@(�`@(�`@(��@(��@( �@'��@'��@'�P@'|�@'\)@';d@&�@&v�@&{@%�-@%p�@$��@$�j@$�j@$��@$�@#ƨ@#S�@"�H@"��@"~�@"^5@"-@"J@!��@!x�@!7L@!&�@!&�@ �`@ �@ A�@ b@   @�w@�P@�P@|�@l�@\)@�@��@5?@$�@$�@@�-@�@`B@�@�j@�D@Z@(�@1@�m@�m@��@S�@C�@"�@o@�@�H@��@��@~�@^5@J@��@hs@7L@�@��@�u@bN@ �@  @�;@�@�@l�@�@�y@�R@��@�+@v�@E�@@�T@�h@p�@`B@/@��@�@��@��@�D@z�@j@9X@1@��@�@t�@S�@C�@33@�@��@~�@=q@�@J@�@��@X@%@�`@Ĝ@�9@r�@1'@b@  @�@�;@�;@�w@�P@;d@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�~�A�x�A�x�A�x�A�v�A�v�A�z�A�|�A�z�A�|�A�z�A�z�A�x�A�z�A�z�A�z�A�|�A�|�A�|�A�|�A�~�ÃÃẢ7A̗�A���A���ȂhA�5?A��A˛�A�S�A�Aʣ�A�M�A�=qA��A�%A��`A��Aɴ9Aɗ�AȶFAǰ!Aƺ^Aď\A�M�A�/A��A��yA��A�A�33A���A�O�A��A��`A��FA�bA���A�E�A�r�A�1'A��A�dZA��HA�`BA��hA���A�%A�hsA�  A���A�dZA���A���A��-A�"�A��A���A���A�t�A���A��A�=qA��\A�bA��/A�"�A��#A�1'A��!A�(�A��A�1A���A���A���A�dZA�^5A��;A���A�bNA���A�ƨA���A�VA��yA�
=A���A�A�A���A���A�\)A���A��A�A}�Az(�Ax��AvZAt1ApjAk`BAk�AjM�Ag��Af$�Abz�A_t�A]C�AZ~�AV��AT��AN�AMK�AM%AL�RALn�AKAKp�AK"�AJbAH�AFjAC��AB�A@ĜA>�jA=��A<�jA;/A:$�A9�A9�A8�9A7ƨA7%A6�/A6�/A6~�A5�A4��A3�7A2A�A0��A/;dA.bNA.�A-?}A,��A,(�A+
=A)\)A'ƨA'O�A&�A&I�A%33A$�jA$�+A"��A"bA 1'A��A"�AbAI�A��Av�A�AM�AdZA�!AffA�FA�Az�A�+A�A
9XA	�;A	"�A�9AVAXA��A�A�-A�A��Ar�A5?A{A��A �@�|�@��u@�b@��R@��`@��@��@��H@���@��@�1@@��T@���@��@�~�@�ff@��
@�^@���@��/@���@��@�Z@�@�5?@�O�@�r�@�33@��T@�Q�@ڇ+@�O�@�A�@��;@���@Չ7@�V@�9X@�+@�&�@д9@�"�@Ο�@��@ʇ+@�x�@���@�S�@�ff@��@�\)@�~�@�/@��@���@�33@���@��j@��@��
@��w@�ȴ@���@�~�@�V@�`B@�r�@���@�~�@�J@�O�@���@�1@�"�@���@���@��R@�E�@��@��j@�r�@�A�@���@���@�|�@�|�@�\)@�
=@���@��H@��!@�v�@�ff@�{@���@�hs@�V@��D@�z�@�bN@�Z@�Q�@�b@��@�l�@�S�@�C�@�C�@�;d@�o@��@�ȴ@���@��\@�{@���@���@��@��@��@���@�$�@��T@���@��-@��h@�p�@�G�@�V@��@���@�I�@��m@���@�+@���@���@��\@�5?@��@��-@�x�@���@���@��7@��@��w@��@���@��\@��@�p�@�O�@�O�@��h@���@���@��^@��^@��-@��@��7@���@�x�@�&�@���@���@��@�Q�@�Q�@��@�ƨ@���@���@���@��@��w@��w@���@�l�@�;d@��@���@��!@�~�@�M�@�5?@�{@��@��^@���@��7@�hs@�/@��/@��D@�bN@�1'@�  @��
@���@�l�@�S�@�S�@�+@���@��+@�v�@�ff@�{@���@�`B@���@�Ĝ@��@���@�z�@�bN@�Z@�A�@�ƨ@�dZ@�;d@��H@���@���@���@���@���@�~�@��@���@���@���@�x�@���@�z�@�Z@�Z@��@��w@���@��P@��@��@�|�@�|�@�|�@�+@���@��R@���@��+@�v�@�V@�E�@�J@�@��@��-@�G�@��/@��u@�r�@�A�@�b@���@��
@���@�t�@���@��y@��!@���@�V@�@���@��@���@�G�@�/@�V@��j@�Z@�9X@��@��m@���@��P@�t�@�;d@��@���@��\@�ff@�M�@�=q@�=q@�J@��@��@��j@��9@��`@�Ĝ@��u@�I�@��
@��F@�t�@�;d@��@��R@�^5@�{@���@���@��h@�hs@�O�@�?}@��@�%@��`@��j@��D@� �@��@~��@~��@~�+@~$�@~@}�@~@~@}�T@}�@}�@|�j@|I�@|1@{�F@{��@{�@{C�@{@z��@y��@y%@x�9@xQ�@w�;@v��@vV@v$�@u�-@u�@t��@tz�@t�@s�m@s��@so@r��@r=q@rJ@q�@q�^@qx�@pĜ@p�u@pbN@p1'@o�@o�P@o
=@n�R@n��@nV@m�h@m?}@l�@k��@k33@j�\@jn�@j-@i�#@i�7@ihs@iG�@i%@hbN@g�@g+@f�@f�R@f�+@fff@f5?@e��@e�@d��@dI�@cƨ@c33@b��@b��@b~�@b~�@b-@a�^@a7L@`Ĝ@`r�@_��@_K�@^�y@^�+@]�@]�@]?}@]�@\�@\�/@\�/@\�j@\��@\z�@\Z@\9X@\1@[dZ@Z�@Z�!@Zn�@Z-@Y��@Y�^@Yhs@X��@X�@XA�@W�;@W�@W\)@W�@V�y@V��@VV@V{@U�@U�-@Up�@UO�@T��@T�@Tz�@TZ@TI�@S�
@S�@SC�@R�@R�\@R-@Q�@Q��@Q��@Q�7@Qx�@Q7L@Q�@P�`@PĜ@P�@P1'@O\)@N��@N��@N�+@N��@N�+@N{@M�-@MV@L(�@Kt�@KS�@Ko@J�\@J=q@J�@I��@I�#@I�7@I7L@I&�@I�@H�`@H�9@H��@H�@HQ�@H �@H  @G|�@F�y@F�+@E�T@E�-@E`B@E�@D�/@Dz�@D1@C�m@C��@CS�@C@B��@B^5@B�@A�@A�#@A��@Ahs@A7L@@��@@�@@r�@@ �@?�@>�@>V@>$�@=��@=��@=O�@=V@<�/@<�@<(�@<1@;��@;ƨ@;S�@;@:�!@:=q@:J@9��@9�@9�#@9x�@97L@9�@8Ĝ@8r�@8A�@8b@8  @8  @7�@7�w@7K�@6��@6ȴ@6�R@6��@6��@6v�@6ff@6E�@5�@5?}@4�@4Z@4�@3�
@3S�@3C�@3o@3@3@2�@2�H@2n�@2M�@1��@1��@1hs@1�@0�@0 �@0b@/�@/�@/l�@/;d@.��@.ȴ@.�+@-�T@-��@-p�@-?}@,��@,�j@,Z@,�@+�
@+�@+"�@*�@*�H@*�!@*n�@*J@)��@)��@)�7@)�7@)x�@)%@(�`@(�`@(�`@(��@(��@( �@'��@'��@'�P@'|�@'\)@';d@&�@&v�@&{@%�-@%p�@$��@$�j@$�j@$��@$�@#ƨ@#S�@"�H@"��@"~�@"^5@"-@"J@!��@!x�@!7L@!&�@!&�@ �`@ �@ A�@ b@   @�w@�P@�P@|�@l�@\)@�@��@5?@$�@$�@@�-@�@`B@�@�j@�D@Z@(�@1@�m@�m@��@S�@C�@"�@o@�@�H@��@��@~�@^5@J@��@hs@7L@�@��@�u@bN@ �@  @�;@�@�@l�@�@�y@�R@��@�+@v�@E�@@�T@�h@p�@`B@/@��@�@��@��@�D@z�@j@9X@1@��@�@t�@S�@C�@33@�@��@~�@=q@�@J@�@��@X@%@�`@Ĝ@�9@r�@1'@b@  @�@�;@�;@�w@�P@;d@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BE�BG�BG�BG�BH�BH�BH�BH�BH�BH�BH�BG�BH�BH�BG�BG�BG�BH�BH�BH�BI�BK�BM�BS�B]/B��B�3B��B��B+BoB�B!�B,B9XB;dB@�B@�BA�BA�B@�B9XB7LB;dB)�B7LBQ�B49B,B5?B[#BgmBo�BaHBQ�BiyBgmBs�B~�B~�B{�B�+B�7B�Bp�B�Bz�Bz�B}�B|�B|�B{�Bt�BiyBZBL�B@�BB�B9XB#�B�B�B�B�BJB
=B��B  BB��B��B�B�sB�B�wB�{B�bB�1Bm�BP�B�B
�B
�^B
��B
��B
� B
�B
u�B
y�B
n�B
O�B
J�B
K�B
49B
(�B
�B

=B	�B	��B	�B	�B	�RB	�B	��B	ĜB	��B	��B	�+B	z�B	q�B	^5B	K�B	6FB	uB	�B	.B	,B	(�B	"�B	�B	�B	
=B��B�TB��BƨB�}B�9B�RB�XB�3B�3B�qB�XB�3B�'B�9B�LB�FB�B��B��B��B��B�uB�VB�uB��B�hB�\B�PB�By�By�B�B�B}�Bz�By�By�Bl�BgmB_;BW
BP�BA�BJ�BR�BO�BJ�BN�BN�BO�BR�BH�BB�B0!B33B=qBH�BN�BJ�BL�BJ�BA�B=qBC�BF�BO�BZB[#BYBXBS�BG�B=qB@�B2-BK�BN�BQ�BXBS�BQ�BS�BQ�BN�BS�BN�BW
B[#BYBM�BQ�BaHBffBgmBe`BbNB_;BYB^5B_;B\)BZBXBT�BYB]/BcTBbNB`BBiyBiyBgmBdZBn�Bn�Bw�Bq�BiyBu�B�1B�B�7B�7B�DB�{B��B��B��B��B��B��B�B�'B�3B�!B�FB�RB�FB�3B�?B�jB�^BǮBȴB��B��B��B�;B�BB�BB�5B�5B�B�B�B�B��B��B��B��B��B	  B	B	B	B	+B	%B		7B	\B	hB	uB	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	'�B	'�B	(�B	+B	,B	.B	/B	-B	0!B	33B	2-B	/B	33B	6FB	;dB	@�B	D�B	D�B	E�B	G�B	G�B	H�B	H�B	J�B	L�B	L�B	P�B	R�B	YB	ZB	[#B	\)B	_;B	bNB	ffB	m�B	o�B	o�B	n�B	s�B	w�B	}�B	�7B	�+B	�1B	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�FB	�RB	�XB	�^B	�dB	�dB	�dB	�dB	�qB	�wB	��B	��B	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�/B	�;B	�BB	�;B	�;B	�BB	�;B	�/B	�;B	�NB	�NB	�ZB	�sB	�yB	�yB	�sB	�sB	�`B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
B
%B

=B
	7B

=B
	7B
DB
DB

=B

=B
JB
PB
VB
VB
bB
bB
\B
JB
PB
\B
oB
{B
{B
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
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
!�B
�B
�B
#�B
"�B
"�B
!�B
"�B
%�B
$�B
%�B
$�B
$�B
%�B
&�B
&�B
%�B
&�B
&�B
'�B
'�B
'�B
&�B
&�B
(�B
(�B
(�B
(�B
'�B
'�B
(�B
)�B
(�B
&�B
(�B
(�B
'�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
,B
+B
-B
.B
/B
.B
/B
.B
-B
-B
.B
.B
.B
.B
0!B
1'B
1'B
1'B
0!B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
49B
49B
49B
49B
49B
33B
33B
33B
2-B
1'B
2-B
49B
49B
49B
5?B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
8RB
8RB
9XB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
:^B
;dB
;dB
;dB
:^B
9XB
9XB
:^B
;dB
=qB
=qB
<jB
:^B
:^B
9XB
9XB
:^B
=qB
=qB
<jB
>wB
?}B
?}B
?}B
>wB
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
@�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
F�B
G�B
F�B
F�B
G�B
H�B
G�B
F�B
F�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
L�B
L�B
L�B
K�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
Q�B
P�B
O�B
Q�B
R�B
S�B
S�B
S�B
VB
VB
W
B
W
B
VB
VB
T�B
VB
T�B
VB
VB
VB
VB
W
B
YB
YB
XB
XB
YB
XB
YB
XB
W
B
ZB
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
\)B
]/B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
_;B
_;B
`BB
`BB
aHB
`BB
cTB
cTB
bNB
aHB
bNB
bNB
bNB
dZB
e`B
e`B
dZB
e`B
e`B
dZB
e`B
ffB
ffB
e`B
e`B
ffB
ffB
gmB
ffB
gmB
hsB
gmB
gmB
gmB
ffB
gmB
gmB
iyB
iyB
hsB
hsB
hsB
iyB
hsB
hsB
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
k�B
k�B
k�B
k�B
k�B
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
o�B
n�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
r�B
q�B
r�B
r�B
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
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BE�BG�BG�BG�BH�BH�BH�BH�BH�BH�BH�BG�BH�BH�BG�BG�BG�BH�BH�BH�BI�BK�BM�BS�B]dB�QB��BյB�PB�BBdB"�B,�B9rB;�B@�B@�BA�BBBAUB;B9�B=�B.IB9�BS@B9�B2GB9$B]dBh�BqABd@BUBk6Bi�BuB�B� B}�B��B�#B��Bs�B�-B|�B|�BHB~(B}�B|�Bu�BkQB\]BOvBC�BC�B;0B&�B"NB B vBqBB�B iB�B�B�jB��B��B��B�)B��B��B�oB�lBp�BUgBVB
�[B
��B
��B
��B
��B
�aB
w�B
z�B
poB
SuB
L�B
M�B
7�B
+�B
_B
�B	�B	��B	��B	�dB	�B	��B	�0B	�B	�B	��B	��B	~�B	t�B	a�B	PB	9�B	�B	!|B	.cB	,�B	)yB	#�B	 \B	xB	�B	 B�LB�$BȴB�oB��B��B��B�B��B��B�*B�nB�aB�B��B��B��B��B��B�VB�eB�gB�HB�aB�+B��B�HB�<B��B|B{�B��B��B~�B|6Bz�Bz�Bn�BiBa�BYKBS�BEBL�BS�BQ�BL~BO�BPBP�BS�BI�BD3B3�B5�B?.BI�BOvBK�BM�BKxBC-B?cBEBH1BP�BZ�B[qBYBXyBT�BIRB?cBB�B4�BL�BO�BR�BXyBT�BR�BT{BR�BO�BT�BPHBW�B[�BY�BO�BS@Ba�Bf�Bg�Be�Bb�B_�BZ7B^�B_�B]B[#BY1BVSBZB]�Bc�Bc BaHBi�BjBhXBe�BoBo�BxRBr�Bk6Bv�B��B�YB�	B�XB�dB�2B�sB�!B�VB�vB��B��B�}B�[B�MB��B�zB�lB��B�B��B��B�dB��B�RB�VBуB՛B�VB��B�vB޸B�B��B�B��B��B��B��B��B�B�*B	 B	-B	aB	3B	EB	tB		�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#B	%�B	'B	(
B	(
B	)*B	+B	,=B	.IB	/5B	-wB	0oB	3�B	2�B	0B	3�B	6�B	;�B	@�B	D�B	D�B	E�B	G�B	G�B	H�B	IB	J�B	MB	MB	Q4B	S@B	YKB	ZQB	[WB	\xB	_pB	b�B	f�B	mwB	o�B	o�B	oiB	tTB	xRB	~(B	�lB	��B	��B	�jB	�hB	�aB	�B	��B	��B	��B	��B	��B	��B	��B	�B	�2B	�LB	�B	�5B	�IB	�UB	�[B	�[B	�`B	�lB	�XB	�^B	�dB	�B	�dB	��B	��B	��B	��B	��B	��B	ðB	ĶB	żB	żB	��B	��B	ȴB	ȴB	��B	�	B	��B	��B	�B	�B	� B	�B	�B	�B	�B	�,B	�FB	�2B	�+B	�+B	�YB	�SB	�QB	�QB	�IB	�VB	�\B	�VB	�pB	�\B	�pB	ݘB	ߊB	�hB	�B	�B	�sB	�yB	�yB	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�0B	�B	�"B	�B	�.B
 4B
B
'B
[B
AB
9B
?B
mB
tB

XB
	lB

XB
	lB
^B
^B

rB

�B
dB
�B
pB
pB
}B
}B
�B
�B
�B
vB
oB
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
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
!�B
 B
 B
#�B
#B
#B
"B
"�B
%�B
%B
%�B
%B
%B
%�B
'B
'B
&B
'B
'B
(
B
(
B
(
B
&�B
'B
)B
)B
)B
)B
($B
($B
)B
*B
)*B
'8B
)*B
)DB
($B
*B
*0B
+B
+6B
+B
,B
-)B
-)B
-CB
,=B
+QB
-)B
./B
/B
./B
/5B
./B
-CB
-CB
./B
.IB
.IB
./B
0;B
1'B
1AB
1AB
0UB
/OB
/OB
0UB
0UB
0UB
0UB
1[B
1[B
1[B
1[B
2GB
33B
4TB
49B
49B
4TB
49B
3MB
3MB
3MB
2GB
1[B
2aB
4nB
4TB
4TB
5?B
4TB
4nB
4�B
4nB
5?B
5tB
6zB
6FB
6`B
7LB
7LB
7fB
7fB
8RB
8lB
7fB
8lB
8lB
8lB
9XB
9XB
9rB
8lB
8lB
9rB
8�B
9XB
9�B
:xB
:xB
;dB
;dB
;B
:xB
;B
;B
;B
:xB
9�B
9�B
:�B
;B
=qB
=qB
<�B
:�B
:�B
9�B
9�B
:�B
=�B
=�B
<�B
>�B
?�B
?}B
?�B
>�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
A�B
@�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
F�B
G�B
F�B
F�B
G�B
H�B
G�B
F�B
F�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
L�B
L�B
L�B
K�B
L�B
M�B
NB
N�B
O�B
O�B
O�B
OB
N�B
PB
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q B
PB
QB
RB
R�B
R�B
R�B
SB
R�B
RB
QB
PB
R B
SB
TB
T,B
T,B
U�B
VB
W
B
W
B
VB
VB
U2B
V9B
U2B
VB
VB
V9B
V9B
W
B
YB
Y1B
X+B
X+B
Y1B
X+B
YB
X+B
W?B
ZB
YKB
Z7B
Z7B
Z7B
ZQB
[=B
[=B
[WB
\CB
\CB
]IB
]IB
]IB
\]B
]dB
^OB
^OB
^B
^OB
]dB
^OB
_;B
_;B
_VB
^OB
^�B
^OB
_;B
`BB
`BB
`vB
`\B
_pB
_VB
`\B
`vB
abB
`vB
c:B
cTB
bhB
a|B
bhB
b�B
b�B
dtB
ezB
ezB
dtB
e`B
ezB
dtB
ezB
ffB
ffB
ezB
ezB
f�B
f�B
gmB
ffB
g�B
hsB
g�B
gmB
g�B
f�B
gmB
g�B
iyB
iyB
h�B
h�B
h�B
i�B
h�B
h�B
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
k�B
k�B
k�B
k�B
k�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
o�B
n�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
p�B
q�B
r�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
r�B
q�B
r�B
r�B
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
w�11111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712220041302017122200413020171222004130201806221323372018062213233720180622132337201804050726432018040507264320180405072643  JA  ARFMdecpA19c                                                                20171218093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171218003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171218003523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171218003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171218003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171218003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171218003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171218003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171218003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171218003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20171218005601                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171218153435  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20171218153435  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20171218153435  CV  LATITUDE        G�O�G�O�A� �                JM  ARGQJMQC2.0                                                                 20171218153435  CV  LONGITUDE       G�O�G�O��$@B                JM  ARSQJMQC2.0                                                                 20171219000000  CF  PSAL_ADJUSTED_QCB�  CF  G�O�                JM  ARCAJMQC2.0                                                                 20171221154130  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171221154130  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222643  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042337  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                