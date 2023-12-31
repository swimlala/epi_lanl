CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-10T00:35:09Z creation;2017-09-10T00:35:13Z conversion to V3.1;2019-12-19T08:02:03Z update;     
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
resolution        =���   axis      Z        D  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     D  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  �p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     D  ʴ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ڈ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170910003509  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_157                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�$��˩�1   @�$��/h�@:iXbM��d�4m��91   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BPp�BXp�B`
=Bg��Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CA��CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD�ÅD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RD�}D��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�C�D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD��D�C�D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A��A��yA��HA��;A���A���A�ĜA���A޾wA޼jA޺^A޲-Aޥ�Aޙ�A�p�A��A�jA��AڮA֏\A�7LA�33A�ȴA��
A��A��A�G�A���A�C�A�x�A���A��wA���A�ĜA���A��hA�A��A��DA�+A�9XA�XA���A�hsA�x�A�
=A��TA��A��A�{A�\)A���A�7LA�n�A���A�\)A���A��
A���A��A���A�M�A��hA�/A���A�I�A��#A�+A��A�v�A�XA���A�ƨA���A��jA��
A�VA�ĜA�VA�1A�^5A�E�A�t�A�A�A�AVA{+AydZAy
=Ax�Ax�/Ax�Aw��AuG�AtjAr��ArE�ArAq�AqƨAq��Aqt�Ap{An^5AmS�Ak��Aj�HAi�;Ah�\Af �Ad��Ac�hAb�Aa�A_�#A^(�A^1A]�A]�A\�A\�A\�A\�HA\ĜA\��A\M�A[�AZ�AYAX�AXffAW�hAV��AU�PAT�/AT  AQ��AQ�7AQ+AP-AO\)AN-AM�wAM�AMp�AMS�AM"�AL�HALAJ��AI�PAI�AG�^AF��AFE�AE��AES�AD�RAD�+ADI�AC��AB��AAt�A@~�A@1'A>��A=��A=�PA<��A;l�A;�A:�A:�+A:=qA:1'A9�mA9��A8��A8��A81A6�/A69XA5�A4��A3�A2ȴA2~�A1�#A0bNA.��A.A, �A*�A)��A)?}A(r�A'&�A%��A%��A%?}A$�jA#S�A"z�A!��A!K�A �/A�TA��A�^A=qA�^A�AoA��Az�An�AE�AbA�
A
=AA�AĜA�PA��AffA�AAQ�A�A��AoAƨAO�A��A��A/A
��A
1'A	�A	
=Ar�A$�A�A��AS�A�9A1AA�A�AS�A��Av�A�7@���@�G�@���@���@��@��y@�5?@�/@�A�@�K�@�V@�bN@���@�ƨ@��@��@�1@�ƨ@�@�h@柾@�(�@��y@��T@�p�@�z�@ߍP@�@�^5@ݡ�@���@�p�@���@�l�@Չ7@�&�@�V@�r�@җ�@��@Гu@�"�@�^5@���@��#@�@�O�@�ƨ@�"�@���@���@��@Ɨ�@�`B@ļj@��@��H@��-@�j@���@��@�J@�`B@�A�@�t�@���@��R@�E�@�@�`B@�V@�A�@�/@�1@�C�@���@�M�@�$�@��T@�I�@�t�@��@�p�@��D@�@�p�@���@��@���@���@�ƨ@��w@��@�;d@��y@���@���@��@��;@��P@�o@��+@�E�@�@�G�@�  @��F@��P@�|�@�l�@�\)@�C�@�;d@�+@�"�@��@��@�o@��@�E�@��7@��D@��@��7@�bN@�
=@�~�@���@�&�@��`@�Z@���@�S�@��y@��R@��+@�E�@�-@�J@���@�x�@�/@��u@�Q�@���@�S�@��R@�=q@�J@���@��@���@��@���@�j@��@�o@��!@�v�@�J@��h@�G�@��u@� �@�  @��m@��w@��P@�C�@��@�@���@���@�7L@�V@��@��@��m@���@�t�@�K�@�+@�@�ff@���@�`B@�&�@�%@���@���@���@�I�@�b@|�@~��@~�+@~V@~$�@}@}p�@|�j@{�
@{"�@z�\@z=q@y��@y�#@y�7@y&�@x��@x�`@xĜ@x�9@xbN@x1'@v�@v@u��@u��@uO�@u?}@t��@tZ@s�
@s"�@r=q@q%@p�9@pr�@pQ�@p  @o�@o\)@oK�@n�@n{@mV@l�@lZ@l�@l�@k��@ko@jJ@h��@h��@g��@g\)@g+@g+@g
=@f��@fV@e@e`B@e/@d��@d(�@c��@c�F@c��@c�@c�@c�@ct�@cS�@cC�@co@b�!@bn�@bn�@bM�@b=q@bJ@a�^@aX@`��@`r�@_�@^��@]��@\�@[�F@[dZ@[33@Z�\@Y�^@Yhs@Y7L@X��@X��@X�u@Xb@W|�@W
=@Vȴ@V��@Vv�@VE�@V5?@V{@U�@U�-@Up�@U?}@UV@T��@T�D@St�@R��@R~�@R^5@R=q@RJ@Q�#@Q��@Qhs@P�9@PQ�@O�;@O�w@O��@O��@O\)@O�@N�y@N��@NV@N5?@M�T@M?}@MV@L�@L�j@Lj@L1@K��@K��@KdZ@K"�@J��@J-@Ix�@H��@H�@HbN@HQ�@G��@G�P@GK�@G+@G�@F��@F��@Fȴ@Fff@F{@E��@E?}@D��@D�D@Dz�@DI�@D9X@D(�@D(�@D(�@D�@C33@C@B�@B��@B~�@A�^@@Ĝ@@r�@@A�@@A�@?�@?+@>�+@>V@>@=�@=V@<�@<j@<j@<I�@<(�@<(�@<(�@<1@;ƨ@;dZ@:=q@9%@8  @7�@7l�@7�@7
=@6��@6v�@65?@6$�@6{@6@5p�@4�@4�@4�D@4(�@3�
@3C�@3C�@333@3o@2�@2��@2��@2~�@2^5@2^5@2-@2J@1��@1��@1x�@1X@1�@0Ĝ@0�u@0A�@0b@/�;@/��@/l�@/l�@/K�@/;d@/;d@/+@/
=@.�y@.�R@.E�@.{@-�@-�h@-�@,��@,j@,I�@,(�@+�m@+�
@+��@+C�@+"�@*��@*��@*��@*n�@*=q@)��@)�@)�#@)��@)�7@)&�@)�@)%@(�u@( �@'��@'l�@&ȴ@&��@&v�@&E�@&$�@&@%@%`B@%V@$�/@$�D@$(�@#�
@#��@#t�@#dZ@#dZ@#"�@"�!@"�\@"�@!�#@!�#@!�^@!�7@!hs@!G�@!�@ �`@ �9@ ��@ 1'@�;@K�@;d@�@�y@��@v�@E�@E�@5?@$�@�@��@O�@�@�@�j@�@��@z�@I�@(�@1@��@�
@�F@dZ@@�H@n�@�@��@�^@��@�7@�7@�7@X@�@�`@�9@Q�@�;@��@l�@+@
=@��@�@��@@�-@�h@O�@V@�j@z�@I�@�@�F@��@��@t�@�@�@��@�^@�^@��@��@��@��@X@�`@ �@�@��@l�@\)@+@
=@��@�y@�@ȴ@�R@��@v�@�@p�@/@V@�j@9X@��@t�@33@"�@o@
�@
�@
��@
��@
�\@
^5@
=q@
�@	��@	�^@	��@	hs@	X@	G�@	7L@	&�@	�@	�@	%@�`@�`@��@Ĝ@��@r�@bN@1'@b@b@  @�;@�P@K�@+@�@�@v�@5?@�T@�-@��@�@�@�h@�@O�@?}@/@�@V@��@�@��@��@�/@�j@��@z�@j@j@9X@1@�
@��@t�@S�@C�@"�@�@�H@�H@�H@��@��@��@~�@=q@-@�@J@J@��@�@�^@��@�7@hs@�@ ��@ �`@ ��@ �@ bN@ 1'@  �@   ?��w?�|�?�|�?���?���?�v�?�5??�5??�{?�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A��A��yA��HA��;A���A���A�ĜA���A޾wA޼jA޺^A޲-Aޥ�Aޙ�A�p�A��A�jA��AڮA֏\A�7LA�33A�ȴA��
A��A��A�G�A���A�C�A�x�A���A��wA���A�ĜA���A��hA�A��A��DA�+A�9XA�XA���A�hsA�x�A�
=A��TA��A��A�{A�\)A���A�7LA�n�A���A�\)A���A��
A���A��A���A�M�A��hA�/A���A�I�A��#A�+A��A�v�A�XA���A�ƨA���A��jA��
A�VA�ĜA�VA�1A�^5A�E�A�t�A�A�A�AVA{+AydZAy
=Ax�Ax�/Ax�Aw��AuG�AtjAr��ArE�ArAq�AqƨAq��Aqt�Ap{An^5AmS�Ak��Aj�HAi�;Ah�\Af �Ad��Ac�hAb�Aa�A_�#A^(�A^1A]�A]�A\�A\�A\�A\�HA\ĜA\��A\M�A[�AZ�AYAX�AXffAW�hAV��AU�PAT�/AT  AQ��AQ�7AQ+AP-AO\)AN-AM�wAM�AMp�AMS�AM"�AL�HALAJ��AI�PAI�AG�^AF��AFE�AE��AES�AD�RAD�+ADI�AC��AB��AAt�A@~�A@1'A>��A=��A=�PA<��A;l�A;�A:�A:�+A:=qA:1'A9�mA9��A8��A8��A81A6�/A69XA5�A4��A3�A2ȴA2~�A1�#A0bNA.��A.A, �A*�A)��A)?}A(r�A'&�A%��A%��A%?}A$�jA#S�A"z�A!��A!K�A �/A�TA��A�^A=qA�^A�AoA��Az�An�AE�AbA�
A
=AA�AĜA�PA��AffA�AAQ�A�A��AoAƨAO�A��A��A/A
��A
1'A	�A	
=Ar�A$�A�A��AS�A�9A1AA�A�AS�A��Av�A�7@���@�G�@���@���@��@��y@�5?@�/@�A�@�K�@�V@�bN@���@�ƨ@��@��@�1@�ƨ@�@�h@柾@�(�@��y@��T@�p�@�z�@ߍP@�@�^5@ݡ�@���@�p�@���@�l�@Չ7@�&�@�V@�r�@җ�@��@Гu@�"�@�^5@���@��#@�@�O�@�ƨ@�"�@���@���@��@Ɨ�@�`B@ļj@��@��H@��-@�j@���@��@�J@�`B@�A�@�t�@���@��R@�E�@�@�`B@�V@�A�@�/@�1@�C�@���@�M�@�$�@��T@�I�@�t�@��@�p�@��D@�@�p�@���@��@���@���@�ƨ@��w@��@�;d@��y@���@���@��@��;@��P@�o@��+@�E�@�@�G�@�  @��F@��P@�|�@�l�@�\)@�C�@�;d@�+@�"�@��@��@�o@��@�E�@��7@��D@��@��7@�bN@�
=@�~�@���@�&�@��`@�Z@���@�S�@��y@��R@��+@�E�@�-@�J@���@�x�@�/@��u@�Q�@���@�S�@��R@�=q@�J@���@��@���@��@���@�j@��@�o@��!@�v�@�J@��h@�G�@��u@� �@�  @��m@��w@��P@�C�@��@�@���@���@�7L@�V@��@��@��m@���@�t�@�K�@�+@�@�ff@���@�`B@�&�@�%@���@���@���@�I�@�b@|�@~��@~�+@~V@~$�@}@}p�@|�j@{�
@{"�@z�\@z=q@y��@y�#@y�7@y&�@x��@x�`@xĜ@x�9@xbN@x1'@v�@v@u��@u��@uO�@u?}@t��@tZ@s�
@s"�@r=q@q%@p�9@pr�@pQ�@p  @o�@o\)@oK�@n�@n{@mV@l�@lZ@l�@l�@k��@ko@jJ@h��@h��@g��@g\)@g+@g+@g
=@f��@fV@e@e`B@e/@d��@d(�@c��@c�F@c��@c�@c�@c�@ct�@cS�@cC�@co@b�!@bn�@bn�@bM�@b=q@bJ@a�^@aX@`��@`r�@_�@^��@]��@\�@[�F@[dZ@[33@Z�\@Y�^@Yhs@Y7L@X��@X��@X�u@Xb@W|�@W
=@Vȴ@V��@Vv�@VE�@V5?@V{@U�@U�-@Up�@U?}@UV@T��@T�D@St�@R��@R~�@R^5@R=q@RJ@Q�#@Q��@Qhs@P�9@PQ�@O�;@O�w@O��@O��@O\)@O�@N�y@N��@NV@N5?@M�T@M?}@MV@L�@L�j@Lj@L1@K��@K��@KdZ@K"�@J��@J-@Ix�@H��@H�@HbN@HQ�@G��@G�P@GK�@G+@G�@F��@F��@Fȴ@Fff@F{@E��@E?}@D��@D�D@Dz�@DI�@D9X@D(�@D(�@D(�@D�@C33@C@B�@B��@B~�@A�^@@Ĝ@@r�@@A�@@A�@?�@?+@>�+@>V@>@=�@=V@<�@<j@<j@<I�@<(�@<(�@<(�@<1@;ƨ@;dZ@:=q@9%@8  @7�@7l�@7�@7
=@6��@6v�@65?@6$�@6{@6@5p�@4�@4�@4�D@4(�@3�
@3C�@3C�@333@3o@2�@2��@2��@2~�@2^5@2^5@2-@2J@1��@1��@1x�@1X@1�@0Ĝ@0�u@0A�@0b@/�;@/��@/l�@/l�@/K�@/;d@/;d@/+@/
=@.�y@.�R@.E�@.{@-�@-�h@-�@,��@,j@,I�@,(�@+�m@+�
@+��@+C�@+"�@*��@*��@*��@*n�@*=q@)��@)�@)�#@)��@)�7@)&�@)�@)%@(�u@( �@'��@'l�@&ȴ@&��@&v�@&E�@&$�@&@%@%`B@%V@$�/@$�D@$(�@#�
@#��@#t�@#dZ@#dZ@#"�@"�!@"�\@"�@!�#@!�#@!�^@!�7@!hs@!G�@!�@ �`@ �9@ ��@ 1'@�;@K�@;d@�@�y@��@v�@E�@E�@5?@$�@�@��@O�@�@�@�j@�@��@z�@I�@(�@1@��@�
@�F@dZ@@�H@n�@�@��@�^@��@�7@�7@�7@X@�@�`@�9@Q�@�;@��@l�@+@
=@��@�@��@@�-@�h@O�@V@�j@z�@I�@�@�F@��@��@t�@�@�@��@�^@�^@��@��@��@��@X@�`@ �@�@��@l�@\)@+@
=@��@�y@�@ȴ@�R@��@v�@�@p�@/@V@�j@9X@��@t�@33@"�@o@
�@
�@
��@
��@
�\@
^5@
=q@
�@	��@	�^@	��@	hs@	X@	G�@	7L@	&�@	�@	�@	%@�`@�`@��@Ĝ@��@r�@bN@1'@b@b@  @�;@�P@K�@+@�@�@v�@5?@�T@�-@��@�@�@�h@�@O�@?}@/@�@V@��@�@��@��@�/@�j@��@z�@j@j@9X@1@�
@��@t�@S�@C�@"�@�@�H@�H@�H@��@��@��@~�@=q@-@�@J@J@��@�@�^@��@�7@hs@�@ ��@ �`@ ��@ �@ bN@ 1'@  �@   ?��w?�|�?�|�?���?���?�v�?�5??�5??�{?�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�7B�7B�7B�=B�=B�=B�DB�=B�7B�1B�=B�bB��B�LB�^B�B
=BhB��B��B�B��BĜB�qB�!B��Bz�BT�B,B�BDB+BBB��B�mB�mB�B�B�mB�HB�
B�jB�9B�'B�B��B��B��B��B�hB�JB�Bw�BgmB[#BT�BJ�B=qB9XB8RB2-B%�B�BhBVB+B
��B
�B
�`B
�/B
��B
��B
ĜB
�qB
�LB
�B
��B
��B
��B
�oB
}�B
p�B
k�B
jB
hsB
dZB
]/B
P�B
H�B
C�B
>wB
<jB
<jB
;dB
9XB
6FB
/B
$�B
�B
�B
bB
	7B
  B	�B	�B	�ZB	�HB	�B	��B	ƨB	ƨB	ŢB	ÖB	��B	��B	��B	��B	�}B	�wB	�jB	�XB	�!B	�B	��B	��B	��B	��B	�{B	�\B	�=B	|�B	|�B	z�B	t�B	p�B	iyB	hsB	gmB	gmB	e`B	cTB	`BB	YB	Q�B	K�B	I�B	B�B	@�B	?}B	=qB	:^B	7LB	6FB	49B	0!B	)�B	"�B	�B	�B	�B	oB	uB	VB	1B		7B		7B	+B	%B	B	B	B��B��B��B�B�B�B�fB�;B�5B�5B�B��BƨBĜB�dB�FB�XB�?B�!B�B��B��B��B��B��B��B��B��B�{B�\B�=B�B�B�B�B�B�B�B�B� B~�B|�Bw�Bs�Bm�BjBiyBl�BjBffBdZB_;B^5BZBT�BXBVBR�BP�BQ�BO�BN�BL�BK�BK�BK�BI�BG�BD�B@�B;dB:^B=qB<jB<jB8RB33B/B2-B9XB:^B:^B<jB:^B7LB7LB6FB49B0!B-B.B.B2-B0!B2-B/B'�B"�B#�B(�B(�B,B+B+B-B+B(�B$�B"�B)�B(�B&�B/B0!B-B)�B-B,B,B33B5?B6FB5?B2-B.B1'B0!B49B5?B49B6FB7LB8RB6FB8RB:^B>wB@�BA�BB�BB�BF�BI�BJ�BJ�BJ�BK�BJ�BH�BA�BM�BS�BW
B[#B\)BZBW
B[#B[#BcTBcTBdZBgmBq�Bs�Bx�By�By�Bx�Bx�Bx�By�By�Bx�By�B�B�B�%B�1B�DB�DB�=B�DB�{B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B��B��B�B�!B�?B�LB�RB�^B�}BBĜBƨBȴB��B��B��B��B��B��B�B�
B�B�)B�BB�ZB�fB�fB�fB�`B�fB�B�B�B�B��B��B��B��B��B	B	%B	%B	+B	1B	1B	
=B	
=B		7B	
=B	hB	{B	�B	�B	�B	�B	 �B	 �B	!�B	!�B	!�B	#�B	(�B	)�B	,B	-B	-B	.B	.B	1'B	2-B	49B	6FB	8RB	9XB	9XB	:^B	:^B	<jB	@�B	B�B	E�B	G�B	H�B	H�B	I�B	K�B	K�B	L�B	L�B	L�B	M�B	L�B	Q�B	W
B	W
B	XB	YB	YB	YB	[#B	]/B	^5B	`BB	ffB	hsB	iyB	jB	k�B	l�B	n�B	n�B	n�B	q�B	v�B	w�B	y�B	z�B	y�B	x�B	{�B	}�B	�B	�B	�B	�+B	�1B	�1B	�1B	�1B	�=B	�JB	�VB	�VB	�\B	�oB	�oB	�uB	�{B	�{B	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�qB	�qB	�wB	�qB	��B	ĜB	ƨB	ƨB	ƨB	ǮB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�#B	�)B	�)B	�)B	�/B	�;B	�HB	�HB	�HB	�HB	�TB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
DB
JB
PB
VB
VB
VB
\B
bB
hB
bB
bB
hB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
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
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
-B
-B
-B
-B
.B
-B
-B
.B
/B
/B
0!B
1'B
2-B
2-B
33B
33B
2-B
33B
49B
49B
5?B
6FB
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
<jB
;dB
;dB
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
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
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
F�B
G�B
G�B
G�B
G�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
O�B
N�B
M�B
N�B
P�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
P�B
Q�B
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
S�B
T�B
W
B
W
B
VB
VB
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
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
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
_;B
_;B
`BB
`BB
`BB
`BB
aHB
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
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
iyB
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
m�B
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
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�MB�#B�=B�=B�=B�=B�=B�=B�=B�=B�XB�=B�XB�7B�RB�7B�=B�=B�XB�^B�XB�lB��B�^B��B�bB�B�'B�tBBsB�BuB�tB�OB�xB� B��B��B��B\]B4B/B�B�B9B+B�B��B�yB�B��B�yB��B��B� B��B�|B�5B�B��B�WB��B��B�"B�Bz�Bi�B\�BVmBL�B@4B:�B9XB4B)�BeBBB�B
�jB
�;B
�B
�!B
�2B
�"B
żB
��B
�$B
��B
��B
��B
�;B
�9B
�B
raB
k�B
j�B
h�B
d�B
^�B
S[B
I�B
EB
?.B
<�B
<�B
;�B
9�B
6�B
0�B
&�B
!B
EB
�B

�B
�B	�tB	�CB	�B	�hB	ۦB	�4B	�fB	��B	��B	�B	� B	��B	��B	��B	��B	��B	��B	�DB	�B	��B	�B	��B	��B	��B	��B	�}B	��B	B	}�B	{�B	u�B	q�B	j�B	h�B	g�B	g�B	e�B	c�B	`�B	ZkB	S�B	MB	J�B	D3B	A�B	@4B	>B	;B	8B	6�B	4�B	1B	+�B	$ZB	 �B	jB	B	�B	B	�B		�B		�B		�B	�B	�B	SB	�B	�B��B�dB��B�B�}B�]B�
B��B�!B��B�KB��BȴB�B��B�lB��B�FB�vB��B�>B��B��B��B�WB��B�_B�sB�MB��B��B��B��B��B��B��B��B�[B�AB�OBcB}�By	Bu%Bo�BlBj�BmBkkBg�BezB`�B_VB[�BV�BX�BV�BT,BRBR�BP�BO�BM�BL�BLJBL0BJ=BHKBE�BA�B<�B;B>B=B="B9XB4�B1AB3�B9�B:�B:�B<�B:�B8B8B7B5?B1�B.�B/�B/5B2�B0�B2|B/�B)�B$�B%`B)�B)�B,�B+�B+�B-�B+�B)�B&LB$ZB*�B*B(
B/iB0oB-�B+B-�B-B-B3�B5�B6zB5�B2�B/B1�B1'B4�B6B5?B7B7�B8�B72B9>B;JB?BA BBBC-BCaBGEBJ#BKBK)BK)BL0BKDBI�BC{BN�BT�BW�B[qB\xBZ�BX+B[�B\CBc�Bd&BezBh�Br-Bt9Bx�By�By�By	By$By	Bz*BzDBy�Bz�B�uB�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�2B�mB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�"B�(B�4B�TB�SB�sB�BܒB��B�tB�B�B�B��B��B��B��B�CB�B��B�2B�>B�6B�qB	[B	%B	?B	EB	fB	fB	
rB	
rB		�B	
�B	�B	�B	�B	
B	B	�B	 �B	 �B	!�B	"B	"NB	$ZB	)*B	*0B	,"B	-)B	-CB	.IB	.IB	1[B	2|B	4�B	6zB	8�B	9rB	9�B	:�B	:�B	<�B	@�B	B�B	E�B	G�B	H�B	H�B	I�B	K�B	K�B	L�B	L�B	MB	NB	MPB	R B	W$B	W$B	X+B	Y1B	Y1B	YeB	[qB	]~B	^�B	`�B	f�B	h�B	i�B	j�B	k�B	l�B	n�B	n�B	n�B	q�B	v�B	xB	y�B	z�B	zB	y>B	|PB	~BB	�3B	�aB	�SB	�EB	�KB	�KB	�KB	��B	�XB	�~B	�pB	�pB	��B	�oB	��B	�uB	�{B	�aB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�BB	��B	�B	�B	�DB	�6B	�/B	�5B	�!B	�AB	�[B	�aB	�MB	�tB	�LB	�lB	�rB	�rB	�xB	�xB	�xB	�B	�dB	��B	��B	��B	��B	��B	��B	ĶB	ƨB	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�9B	�?B	�=B	�CB	�CB	�]B	�dB	�VB	�HB	�bB	�bB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�$B	�B	�B	�B	�<B	�<B	�.B
B
UB
;B
AB
B
3B
B
9B
9B
B
B
9B
gB
MB
{B
�B
�B
^B
dB
PB
VB
pB
�B
vB
}B
hB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
(
B
(
B
(
B
(
B
)B
)B
)B
)*B
*0B
+B
+6B
-)B
-)B
-)B
-)B
./B
-)B
-CB
.B
/5B
/5B
0UB
1AB
2GB
2GB
33B
33B
2GB
3hB
4TB
4nB
5?B
6FB
5ZB
5ZB
6`B
6`B
6`B
6`B
6`B
6`B
6zB
7fB
7�B
9XB
9XB
9rB
9XB
:xB
;B
<�B
<jB
;B
;B
<�B
<�B
=qB
=�B
>�B
>wB
>wB
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
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
F�B
G�B
G�B
G�B
G�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
NB
N�B
O�B
N�B
NB
O(B
Q B
R�B
R�B
R�B
R�B
R�B
SB
SB
RB
QB
RB
R�B
SB
S�B
TB
TB
S�B
S�B
S�B
S�B
S�B
TB
TB
T,B
U2B
W$B
W?B
V9B
V9B
X+B
XEB
Y1B
YB
X�B
Y1B
YB
Y1B
ZB
Z7B
Z7B
[WB
[=B
[=B
[=B
\CB
]IB
]/B
]/B
]/B
]/B
]/B
]B
]/B
]IB
^5B
]IB
]/B
]IB
^OB
^OB
^OB
^OB
_;B
_VB
_VB
_;B
_VB
`\B
`\B
`\B
`vB
abB
bhB
bhB
c:B
cTB
cTB
cTB
cnB
cnB
dZB
dtB
dtB
ezB
e`B
e`B
e`B
e`B
ezB
f�B
f�B
f�B
ffB
ffB
f�B
g�B
g�B
g�B
hsB
h�B
h�B
i�B
iyB
i_B
jB
jB
jB
iyB
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
m�B
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
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709140032542017091400325420170914003254201806221230332018062212303320180622123033201804050425402018040504254020180405042540  JA  ARFMdecpA19c                                                                20170910093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170910003509  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170910003511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170910003511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170910003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170910003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170910003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170910003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170910003512  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170910003513                      G�O�G�O�G�O�                JA  ARUP                                                                        20170910005555                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170910153512  CV  JULD            G�O�G�O�F�'�                JM  ARCAJMQC2.0                                                                 20170913153254  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170913153254  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192540  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033033  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                