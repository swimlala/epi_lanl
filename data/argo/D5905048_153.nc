CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-08-26T00:35:15Z creation;2017-08-26T00:35:18Z conversion to V3.1;2019-12-19T07:59:10Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170826003515  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_153                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�!5�T2 1   @�!6�[�@4
=p���d��*0U21   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @:=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=Bxp�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA
DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^z=D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt�=Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD烅D��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�0R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߬A߬A߮A߰!A߰!A߰!A߰!A߰!A߮Aߩ�Aߥ�Aߥ�Aߧ�Aߥ�Aߥ�Aߣ�Aߣ�Aߥ�Aߥ�Aߥ�Aߥ�Aߧ�Aߧ�Aߧ�Aߧ�Aߟ�Aߏ\A�1AڋDA��Aף�A՟�A��A��TA���A�~�A�p�A�VA�x�A�-A�bAƙ�A�`BAĮAþwA¼jA�`BA���A�r�A�C�A�`BA�33A�{A�=qA�p�A���A��A��mA�K�A��DA��A�
=A��DA��A�C�A��uA��A�;dA�ZA�dZA�ȴA�v�A��/A��TA���A��\A�oA��A�(�A��TA�bA�jA���A�9XA���A��DA�7LA��A�dZA�A�A��hA�A���A��RA���A��^A��^A�=qA��`A���A���A��A�S�A�%A�-A�x�A�A�bA��FA��A�(�A�I�A�ƨA�;dA��PA��jA�E�A��A�v�A�-A~ �A{�;Az��Ay�Aw��Av�Aq�Am�Al�Akt�AiG�Af��Ad1Aa�A^��A]A\�RAZ1'AXZAV�jAT�\AQ�hAPQ�ANbAL�+AJZAG�AG?}AFn�AEC�ADz�AB-A@�A?7LA>1'A=��A=33A;?}A9x�A7�A6=qA5�7A5VA4�A3�A0�/A/�A.bA,��A,�A+�PA*�DA(ȴA'�A&VA%��A%VA$(�A#hsA"��A!ƨA�mA��A7LA�HA^5AAhsAĜA1A��A^5A?}A{A�-A��A9XA�;A|�A9XA�AZA�TA�A;dA�RA��A
-A	33A	A�A�DAp�AM�AVAz�A{A��AhsAbAl�A/A ��A M�@�n�@��-@�V@�dZ@�5?@�@��/@�M�@��H@�v�@�G�@�j@��^@�+@�9X@�
=@��@�%@��`@�9X@�t�@�+@�ȴ@���@�V@�1'@�K�@�+@���@��T@ى7@�%@�r�@׶F@�C�@�n�@թ�@�/@�+@�-@ёh@��@ЋD@��m@Χ�@͙�@̣�@���@ɺ^@ț�@�Z@��@�b@��;@�C�@ƸR@�M�@���@Õ�@��j@�V@��h@�&�@�Ĝ@�C�@�J@��T@���@�X@���@� �@�l�@���@�~�@���@��@��
@��@�"�@��H@���@��@���@�/@��u@�A�@��F@�;d@���@���@���@��+@�n�@���@�X@�?}@�7L@��@�%@��@�l�@�@���@�^5@��@�X@���@�A�@�S�@��@���@��+@�~�@�{@��@��^@���@�x�@�/@���@���@���@��9@���@�j@�9X@�1'@���@�l�@�33@��@�ȴ@�E�@��@��^@���@�hs@���@���@��D@��@��w@��@��P@��@�dZ@�33@���@���@�ff@�=q@�@��h@�x�@�`B@�7L@�V@���@��D@�Z@�I�@�9X@��@��F@���@��@�33@��@�^5@�E�@�J@��^@��7@�G�@�V@��`@��9@�Q�@� �@��w@�\)@�C�@�@��!@��+@�^5@�-@��@���@��h@�p�@�&�@�%@���@���@��@��/@��u@�Z@�A�@��@��;@���@��@�dZ@�C�@�;d@���@���@�^5@�5?@��@�$�@��@���@���@�`B@�G�@�/@��@��`@��D@�j@�  @��;@��F@�S�@�
=@��@�ȴ@���@�M�@�{@���@���@���@��#@��^@�`B@�7L@���@��@�9X@��@��@���@��@�S�@�"�@��y@���@��R@��+@�ff@�ff@�V@�5?@���@�x�@�%@��j@� �@��F@��@�l�@�
=@���@��@��T@��@�x�@�7L@��@�bN@� �@��;@�\)@��R@��\@�V@�V@�E�@��@�hs@�/@���@��/@�Z@�@\)@~�@~��@~E�@}�@|�@|(�@{��@{dZ@{o@{o@z�@z�\@zn�@zJ@yx�@yG�@x��@x  @w\)@v��@v�+@v$�@u�h@t��@t�@s��@sS�@r��@r��@r~�@q�^@q7L@q%@p��@pĜ@p�u@pQ�@p1'@p  @o�P@o+@n�y@n�R@nff@n5?@n$�@m�@m��@m�@m/@l�/@l(�@k�m@k�F@k�@kt�@kC�@k33@j�@j��@j^5@jJ@i��@i�@i��@ihs@i�@h��@h�9@h�u@h��@h�9@h�`@h�9@hbN@g�@g�w@g��@gK�@f�@f��@fff@f$�@e@eO�@eV@d��@d��@dI�@d�@c��@c��@ct�@c33@b��@b�\@b=q@a��@ax�@a&�@`��@`Q�@_�@_�P@^�R@^{@]�-@]O�@\��@\�@\�D@\9X@[��@[�
@[��@[S�@["�@Z�@Z��@Z��@Z^5@Z-@Y��@Y7L@X�u@XQ�@X �@W�w@W�@W;d@W�@V�@Vff@U�T@U�-@U�h@T��@T(�@T1@S��@Sƨ@S�@SC�@S"�@R�!@R^5@Q�@Q7L@PĜ@P�u@PA�@P  @O��@O��@O;d@N�@N�R@N5?@M@M�@M`B@L��@L�@K��@J�@J�\@J-@I�@I�^@I��@I��@I��@I��@Ihs@I7L@H�`@HĜ@H�9@H�9@H�9@H��@H�@HbN@Hb@G�;@G;d@G
=@Fȴ@Fv�@F{@E�T@E�-@E�@E?}@D��@D�@D(�@C�
@C�F@C�@Ct�@C"�@B��@A��@Ahs@AX@AX@A7L@@Ĝ@@1'@@b@?�w@?l�@>�@>��@>�+@>�+@>ff@>$�@=��@=�h@=p�@=O�@=/@=�@=�@<��@<z�@<I�@<9X@<(�@;��@;ƨ@;��@;�@;dZ@;C�@;33@;"�@:�@:~�@9hs@9%@8Ĝ@8bN@8 �@7�@7��@7l�@7;d@7�@6��@6�@6��@6��@6$�@5�@5�T@5��@5�T@5��@5��@5/@4�@4��@49X@41@3�F@333@2��@2��@2��@2��@2��@2-@1�#@1G�@0��@0��@0Q�@/|�@/�@.�@.�R@.��@.v�@.ff@.$�@-�@-��@-�@,��@,Z@,1@+��@+33@*�H@*�\@*-@)x�@)X@)&�@(�9@(�@(A�@( �@(b@'�@'�@'l�@'\)@'l�@';d@&��@&�+@&v�@&5?@&$�@&@%�@%�-@%��@%�h@%p�@%?}@%�@$�/@$�@$z�@$Z@$�@#ƨ@#��@#��@#S�@#"�@"��@"�!@"~�@"=q@!��@!�7@!X@!X@!G�@!G�@!G�@!G�@!7L@ �`@ Ĝ@ �9@ ��@ �@ r�@ Q�@  �@�;@|�@l�@\)@\)@;d@�y@�R@��@�+@5?@@�T@��@@�h@`B@�@�/@�j@�j@�@��@�D@I�@(�@�@1@��@�m@�m@�
@�F@��@��@��@��@��@�@dZ@33@33@"�@�@��@�!@~�@-@��@�7@x�@hs@7L@�@��@��@��@�u@�@r�@r�@r�@bN@b@�;@��@�P@�P@�P@l�@+@�R@�+@ff@V@5?@{@�T@@��@`B@O�@?}@�/@�@��@z�@Z@(�@1@��@�
@�F@��@��@�@dZ@C�@"�@@��@~�@=q@�@x�@hs@7L@��@�u@bN@1'@  @�@�@�;@��@|�@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A߬A߬A߮A߰!A߰!A߰!A߰!A߰!A߮Aߩ�Aߥ�Aߥ�Aߧ�Aߥ�Aߥ�Aߣ�Aߣ�Aߥ�Aߥ�Aߥ�Aߥ�Aߧ�Aߧ�Aߧ�Aߧ�Aߟ�Aߏ\A�1AڋDA��Aף�A՟�A��A��TA���A�~�A�p�A�VA�x�A�-A�bAƙ�A�`BAĮAþwA¼jA�`BA���A�r�A�C�A�`BA�33A�{A�=qA�p�A���A��A��mA�K�A��DA��A�
=A��DA��A�C�A��uA��A�;dA�ZA�dZA�ȴA�v�A��/A��TA���A��\A�oA��A�(�A��TA�bA�jA���A�9XA���A��DA�7LA��A�dZA�A�A��hA�A���A��RA���A��^A��^A�=qA��`A���A���A��A�S�A�%A�-A�x�A�A�bA��FA��A�(�A�I�A�ƨA�;dA��PA��jA�E�A��A�v�A�-A~ �A{�;Az��Ay�Aw��Av�Aq�Am�Al�Akt�AiG�Af��Ad1Aa�A^��A]A\�RAZ1'AXZAV�jAT�\AQ�hAPQ�ANbAL�+AJZAG�AG?}AFn�AEC�ADz�AB-A@�A?7LA>1'A=��A=33A;?}A9x�A7�A6=qA5�7A5VA4�A3�A0�/A/�A.bA,��A,�A+�PA*�DA(ȴA'�A&VA%��A%VA$(�A#hsA"��A!ƨA�mA��A7LA�HA^5AAhsAĜA1A��A^5A?}A{A�-A��A9XA�;A|�A9XA�AZA�TA�A;dA�RA��A
-A	33A	A�A�DAp�AM�AVAz�A{A��AhsAbAl�A/A ��A M�@�n�@��-@�V@�dZ@�5?@�@��/@�M�@��H@�v�@�G�@�j@��^@�+@�9X@�
=@��@�%@��`@�9X@�t�@�+@�ȴ@���@�V@�1'@�K�@�+@���@��T@ى7@�%@�r�@׶F@�C�@�n�@թ�@�/@�+@�-@ёh@��@ЋD@��m@Χ�@͙�@̣�@���@ɺ^@ț�@�Z@��@�b@��;@�C�@ƸR@�M�@���@Õ�@��j@�V@��h@�&�@�Ĝ@�C�@�J@��T@���@�X@���@� �@�l�@���@�~�@���@��@��
@��@�"�@��H@���@��@���@�/@��u@�A�@��F@�;d@���@���@���@��+@�n�@���@�X@�?}@�7L@��@�%@��@�l�@�@���@�^5@��@�X@���@�A�@�S�@��@���@��+@�~�@�{@��@��^@���@�x�@�/@���@���@���@��9@���@�j@�9X@�1'@���@�l�@�33@��@�ȴ@�E�@��@��^@���@�hs@���@���@��D@��@��w@��@��P@��@�dZ@�33@���@���@�ff@�=q@�@��h@�x�@�`B@�7L@�V@���@��D@�Z@�I�@�9X@��@��F@���@��@�33@��@�^5@�E�@�J@��^@��7@�G�@�V@��`@��9@�Q�@� �@��w@�\)@�C�@�@��!@��+@�^5@�-@��@���@��h@�p�@�&�@�%@���@���@��@��/@��u@�Z@�A�@��@��;@���@��@�dZ@�C�@�;d@���@���@�^5@�5?@��@�$�@��@���@���@�`B@�G�@�/@��@��`@��D@�j@�  @��;@��F@�S�@�
=@��@�ȴ@���@�M�@�{@���@���@���@��#@��^@�`B@�7L@���@��@�9X@��@��@���@��@�S�@�"�@��y@���@��R@��+@�ff@�ff@�V@�5?@���@�x�@�%@��j@� �@��F@��@�l�@�
=@���@��@��T@��@�x�@�7L@��@�bN@� �@��;@�\)@��R@��\@�V@�V@�E�@��@�hs@�/@���@��/@�Z@�@\)@~�@~��@~E�@}�@|�@|(�@{��@{dZ@{o@{o@z�@z�\@zn�@zJ@yx�@yG�@x��@x  @w\)@v��@v�+@v$�@u�h@t��@t�@s��@sS�@r��@r��@r~�@q�^@q7L@q%@p��@pĜ@p�u@pQ�@p1'@p  @o�P@o+@n�y@n�R@nff@n5?@n$�@m�@m��@m�@m/@l�/@l(�@k�m@k�F@k�@kt�@kC�@k33@j�@j��@j^5@jJ@i��@i�@i��@ihs@i�@h��@h�9@h�u@h��@h�9@h�`@h�9@hbN@g�@g�w@g��@gK�@f�@f��@fff@f$�@e@eO�@eV@d��@d��@dI�@d�@c��@c��@ct�@c33@b��@b�\@b=q@a��@ax�@a&�@`��@`Q�@_�@_�P@^�R@^{@]�-@]O�@\��@\�@\�D@\9X@[��@[�
@[��@[S�@["�@Z�@Z��@Z��@Z^5@Z-@Y��@Y7L@X�u@XQ�@X �@W�w@W�@W;d@W�@V�@Vff@U�T@U�-@U�h@T��@T(�@T1@S��@Sƨ@S�@SC�@S"�@R�!@R^5@Q�@Q7L@PĜ@P�u@PA�@P  @O��@O��@O;d@N�@N�R@N5?@M@M�@M`B@L��@L�@K��@J�@J�\@J-@I�@I�^@I��@I��@I��@I��@Ihs@I7L@H�`@HĜ@H�9@H�9@H�9@H��@H�@HbN@Hb@G�;@G;d@G
=@Fȴ@Fv�@F{@E�T@E�-@E�@E?}@D��@D�@D(�@C�
@C�F@C�@Ct�@C"�@B��@A��@Ahs@AX@AX@A7L@@Ĝ@@1'@@b@?�w@?l�@>�@>��@>�+@>�+@>ff@>$�@=��@=�h@=p�@=O�@=/@=�@=�@<��@<z�@<I�@<9X@<(�@;��@;ƨ@;��@;�@;dZ@;C�@;33@;"�@:�@:~�@9hs@9%@8Ĝ@8bN@8 �@7�@7��@7l�@7;d@7�@6��@6�@6��@6��@6$�@5�@5�T@5��@5�T@5��@5��@5/@4�@4��@49X@41@3�F@333@2��@2��@2��@2��@2��@2-@1�#@1G�@0��@0��@0Q�@/|�@/�@.�@.�R@.��@.v�@.ff@.$�@-�@-��@-�@,��@,Z@,1@+��@+33@*�H@*�\@*-@)x�@)X@)&�@(�9@(�@(A�@( �@(b@'�@'�@'l�@'\)@'l�@';d@&��@&�+@&v�@&5?@&$�@&@%�@%�-@%��@%�h@%p�@%?}@%�@$�/@$�@$z�@$Z@$�@#ƨ@#��@#��@#S�@#"�@"��@"�!@"~�@"=q@!��@!�7@!X@!X@!G�@!G�@!G�@!G�@!7L@ �`@ Ĝ@ �9@ ��@ �@ r�@ Q�@  �@�;@|�@l�@\)@\)@;d@�y@�R@��@�+@5?@@�T@��@@�h@`B@�@�/@�j@�j@�@��@�D@I�@(�@�@1@��@�m@�m@�
@�F@��@��@��@��@��@�@dZ@33@33@"�@�@��@�!@~�@-@��@�7@x�@hs@7L@�@��@��@��@�u@�@r�@r�@r�@bN@b@�;@��@�P@�P@�P@l�@+@�R@�+@ff@V@5?@{@�T@@��@`B@O�@?}@�/@�@��@z�@Z@(�@1@��@�
@�F@��@��@�@dZ@C�@"�@@��@~�@=q@�@x�@hs@7L@��@�u@bN@1'@  @�@�@�;@��@|�@�P11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BȴBǮBȴBȴBȴBȴBȴBȴBȴBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBÖB�3B�hB�`B	7B��B��B�B�BB"�B'�B49BD�BJ�BE�B:^BA�BD�BM�B^5BiyB}�B|�Bp�Bm�Bp�Bw�B~�B�+B�=B�1B�%B�+B�B��B��B��B��B��B��B��B��B�uB��B� Bz�Bl�B~�Bp�Br�BffBaHBT�BT�BT�BO�BN�BC�B1'B �BoB\B��B��B��B�sB�)B�B��B�wB�B��B�PB{�B_;B\)BK�B<jB49B�B�B�BVB
��B
�B
�NB
��B
�!B
��B
��B
�B
s�B
n�B
e`B
T�B
J�B
G�B
9XB
$�B
	7B	�/B	�/B	��B	B	�B	��B	�\B	x�B	z�B	p�B	\)B	N�B	B�B	49B	�B	�B	JB	1B��B�B��B��B�B�sB�B��B�B��B��B��B��B�XB�qB��B��B�qB�dB�-B��B��B��B�oB��B�oB�7B~�B� B�B�%B�%B� B~�B|�Bu�Bm�Bs�Bt�B�B�B�B�Bz�Bk�Bq�Bw�Br�Bk�Br�Bp�Bm�Bq�Bn�BgmBffBn�Bp�Bq�Bp�Bl�BffBcTBk�Bu�Bt�Bo�BgmBhsBjBp�Bs�Bq�Bq�BjBq�Bw�Bt�Br�Bm�Br�Bq�Bq�Bw�B|�Bv�Bo�Bq�B�B� B�B}�B� B� B�7B�DB�7B�\B�VB�VB�oB�hB�DB�B�B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�LB�RB�RB�LB�9B�-B�'B�-B�B�B�3B�}BB��B�}BÖB��B��B��B��B��B��B�B�)B�;B�`B�yB��B��B��B��B��B��B��B��B	B	B	
=B	\B	�B	�B	�B	�B	�B	�B	)�B	+B	)�B	+B	)�B	(�B	/B	2-B	6FB	;dB	?}B	B�B	G�B	H�B	N�B	VB	[#B	]/B	^5B	bNB	e`B	hsB	iyB	k�B	o�B	r�B	t�B	u�B	v�B	w�B	z�B	{�B	{�B	{�B	�B	�B	�B	�+B	�JB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�9B	�XB	�^B	�^B	�dB	�jB	�qB	�}B	B	B	��B	ÖB	ŢB	ŢB	ĜB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�/B	�;B	�;B	�;B	�5B	�5B	�;B	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B
  B
  B
B
%B
B
B
B
%B
1B
1B

=B

=B

=B
DB
JB
JB
DB
DB
\B
bB
hB
hB
bB
bB
oB
uB
oB
hB
oB
uB
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
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
'�B
'�B
(�B
'�B
'�B
'�B
(�B
)�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
,B
-B
,B
,B
-B
-B
,B
,B
,B
+B
,B
,B
+B
+B
,B
.B
.B
.B
0!B
0!B
0!B
0!B
1'B
0!B
0!B
1'B
1'B
2-B
1'B
1'B
2-B
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
49B
49B
5?B
6FB
5?B
7LB
8RB
8RB
8RB
8RB
8RB
9XB
8RB
9XB
9XB
8RB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
<jB
<jB
=qB
>wB
=qB
=qB
=qB
=qB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
A�B
A�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
H�B
H�B
H�B
G�B
G�B
I�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
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
M�B
M�B
L�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
W
B
VB
VB
VB
W
B
W
B
XB
W
B
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
ZB
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
aHB
aHB
aHB
aHB
aHB
aHB
aHB
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
cTB
cTB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
e`B
e`B
ffB
ffB
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
hsB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
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
jB
jB
jB
jB
k�B
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
jB
k�B
k�B
k�B
jB
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
m�B
m�B
m�B
n�B
n�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
n�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
o�B
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
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BȴBǮBȴBȴBȴBȴBȴBȴBȴBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	BāB�+B�B�B�B��B�B�AB��BmB$�B*B5�BEBKDBGB<�BCGBF�BP.B`�Bj�B~BB}�Br�Bp;Br�By�B��B�KB�^B��B��B�RB�9B�?B��B�4B�HB�fB�zB��B��B��B�?B�GB}�Bn}B�BsBtBh�BcTBW�BV�BV�BQ�BPBESB3�B#�B�BB iB��B��B�6B��B�yB�:B�;B�vB��B�B~�Bb�B^BN�B>�B6`B#:BBsB�B
��B
�B
�@B
�vB
��B
��B
�OB
��B
v`B
pUB
gmB
WsB
LdB
IB
;�B
'�B
�B	��B	��B	��B	�mB	�}B	�B	��B	{�B	|B	r|B	_;B	QNB	D�B	72B	# B	 �B	(B	
rB	 �B�ZB��B��B�5B��B��B�hB�$B�,B��B�(B�B��B��B�B�oB�(B�6B�9B�VB��B�'B�,B�YB�uB��B�;B��B�3B�+B�B�;B�B~Bw�Bo�ButBvzB�mB��B��B��B|PBn�Bs3Bx�Bt9BmBshBq�Bn�BrGBo�Bi*Bg�Bo�Bq[BrGBqABmwBg�Be`Bl�BvBu%BpoBiBjBlBqvBtTBraBraBl=Br|BxRButBs�Bo BsMBraBr�Bx�B}VBw�Bq�Bs�B�oB�B�B�B��B��B�	B��B�=B��B��B��B��B��B�JB�9B�tB��B��B�B�CB�B�-B�BB�OB�'B�OB�CB�CB��B�IB�-B�-B�;B�VB��B��B��B�*B��B��B��B��B�lB��B��B��B��B��B��B��B��B� B��B�B�iB�MB��B�B�VB�\B�aB�gB�yBܒB߾B��B�KB��B�B��B�B�+B�2B�>B�JB	aB	�B	
�B	�B	�B	�B	�B	�B	
B	B	)�B	+6B	*B	+6B	*B	)�B	/iB	2aB	6�B	;�B	@ B	C-B	HB	IRB	O(B	V9B	[=B	]IB	^�B	b�B	e�B	h�B	i�B	k�B	o�B	r�B	t�B	u�B	v�B	xB	z�B	|B	|B	|PB	�;B	�3B	�mB	��B	�~B	�}B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�
B	�$B	�*B	�KB	�IB	�UB	�aB	�hB	��B	�rB	�xB	��B	��B	��B	��B	��B	ªB	ªB	��B	��B	żB	żB	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�.B	�B	�4B	�:B	�B	�2B	�9B	�+B	�KB	�QB	�QB	�QB	�CB	�CB	�]B	�IB	�;B	�VB	�VB	�OB	ބB	�pB	�bB	�|B	�|B	�B	�nB	�nB	�tB	�tB	�B	�B	�B	�zB	�B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	��B	�B	��B	�B	�B	�B	��B	��B	�(B	�B
  B
  B
 B	�HB	�cB	�HB
 OB
 iB
[B
?B
gB
�B
mB
YB
fB
�B

XB

�B

�B
xB
~B
~B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
'B
'B
'B
'B
'�B
'�B
(�B
($B
'�B
($B
)B
*B
)*B
)*B
*B
)�B
)�B
*0B
+B
,"B
,"B
,"B
,"B
,"B
-)B
,"B
-)B
,B
,=B
-)B
-)B
,=B
,B
,=B
+6B
,"B
,"B
+6B
+QB
,=B
.B
.IB
.IB
0;B
0;B
0;B
0;B
1AB
0;B
0;B
1AB
1AB
2-B
1AB
1AB
2-B
1[B
2GB
2aB
3MB
4nB
4nB
4TB
4nB
49B
4TB
4TB
4nB
5ZB
6`B
5tB
7fB
8lB
8�B
8lB
8lB
8lB
9rB
8�B
9rB
9�B
8�B
:�B
;B
;B
;B
<�B
<�B
<�B
<�B
=qB
<�B
<�B
=�B
>�B
=�B
=�B
=�B
=�B
?�B
@�B
@�B
A�B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
BuB
B�B
B�B
B�B
B�B
A�B
A�B
A�B
B�B
C�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
H�B
H�B
H�B
G�B
G�B
I�B
H�B
I�B
J	B
J�B
K�B
K�B
K�B
J�B
K�B
K�B
MB
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
OB
N�B
N�B
M�B
NB
M6B
O�B
O�B
Q B
Q B
RB
RB
RB
SB
SB
SB
R�B
SB
SB
S&B
TB
T�B
T�B
T�B
TB
S�B
TB
TB
T,B
UB
UB
UB
UB
VB
W$B
W
B
W
B
W$B
V9B
V9B
V9B
W
B
W?B
XEB
WYB
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
YKB
ZQB
[=B
\CB
[WB
\]B
\CB
\CB
\]B
\]B
^OB
^OB
^jB
_VB
_VB
`BB
`\B
`\B
`BB
`\B
`BB
`'B
`\B
_pB
aHB
abB
abB
abB
abB
abB
abB
a-B
aHB
abB
abB
abB
abB
bhB
bhB
bhB
bhB
bhB
cnB
cnB
b�B
cnB
cnB
dtB
dtB
dtB
dtB
d�B
ezB
e`B
ffB
ffB
ffB
f�B
e`B
ezB
ffB
ffB
ffB
f�B
ffB
f�B
f�B
f�B
f�B
gmB
gmB
gmB
g�B
g�B
gmB
hsB
g�B
gmB
h�B
h�B
hsB
h�B
h�B
h�B
h�B
i�B
jeB
jB
jB
jB
j�B
jB
j�B
jB
jB
jB
jB
k�B
j�B
j�B
kkB
k�B
k�B
k�B
kkB
k�B
j�B
k�B
k�B
k�B
j�B
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
m�B
m�B
m�B
n�B
n�B
m�B
m�B
l�B
m�B
n�B
n�B
o�B
n�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
o�B
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
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708300038572017083000385720170830003857201806221318142018062213181420180622131814201804050720332018040507203320180405072033  JA  ARFMdecpA19c                                                                20170826093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170826003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170826003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170826003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170826003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170826003517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170826003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170826003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170826003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170826003518                      G�O�G�O�G�O�                JA  ARUP                                                                        20170826005531                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170826153636  CV  JULD            G�O�G�O�F�	�                JM  ARCAJMQC2.0                                                                 20170829153857  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170829153857  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222033  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041814  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                