CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-12T21:35:12Z creation;2017-10-12T21:35:15Z conversion to V3.1;2019-12-19T07:55:12Z update;     
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
_FillValue                 �  IT   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  MD   PRES_ADJUSTED_QC         
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
_FillValue                 �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
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
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20171012213512  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_169                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�-2�r 1   @�-3}'Ҁ@4Ձ$�/�d��|���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D��3D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:=q@�Q�@�Q�A (�A (�AAA`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B���B���B�B�B�8RB�8RB�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D�=D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Dt�=Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD�ÅD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�I�D�c�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�7LA�7LA�9XA�9XA�;dA�9XA�;dA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�A�A�A�A�A�A�C�A�C�A�E�A�E�A�G�A�A�A�9XA�1A�jA�`BA��A�  Aͺ^A�oA���Ả7A�  Aʩ�A��A�jA�bNAƇ+AŸRA�1A�n�A�1'A��/A��A�~�A��`A���A��#A��A���A��A�z�A�oA�E�A���A��yA� �A�XA��A���A�oA�r�A�1A�+A�ȴA�v�A�
=A�|�A���A���A��!A�bNA�JA��A�1'A��jA�A�A��A�1'A��A���A�l�A���A�jA��A��A���A���A�(�A���A��^A�VA�E�A�1A��9A�K�A�K�A�VA�oA��A��A���A��A���A�hsA��A���A�%A�\)A���A�/A��A���A�;dA�ĜA��hA�r�A�1'A���A��A~9XA{�hAx��AvVAsC�An�jAl��Al��Al��Ak��Aj��Aj^5Ai�#Ah~�Afn�Ac�AbJA`JA]ƨA\�AZ�!AY��AX�9AV�RASƨAR��AQ/AP��AN5?AM&�AKAI��AIC�AH=qAEhsAB�uAB$�AA�A@�jA?�PA>�DA<z�A;&�A:n�A9��A9A9��A7A6�A6r�A5dZA3�A2�RA1��A0$�A.�A.1'A-ƨA-S�A-oA,bNA+G�A)S�A&bNA%l�A%\)A%VA$�A#�mA"�HA!oAJA^5A  A��A\)A�A;dA�/A�A9XA�;A%A�A��Az�A�^A9XA��A�A=qAO�A�\A"�An�AJA�A
ĜA	�AVA�A��A��A��AA��Az�A �A 5?@�o@��H@�V@��@�7L@� �@�O�@�l�@��h@�&�@�%@��u@��
@�~�@�hs@�@�\)@��#@�\)@��@���@�v�@�V@���@�l�@�"�@�h@�9X@ߍP@ް!@ݡ�@���@�b@ڗ�@�j@�9X@�Q�@ش9@�`B@ٲ-@���@���@�{@Ѓ@�I�@�;d@���@��@�t�@��@���@���@�9X@�
=@őh@��@�Ĝ@�r�@�  @Ý�@�o@�@���@��7@���@��/@��F@�J@�G�@��9@�Q�@� �@���@��w@�\)@�;d@���@��\@�@�O�@�r�@���@���@���@�?}@��u@�A�@���@�33@�n�@���@��@���@���@�hs@�?}@�%@���@� �@�t�@���@�{@��#@���@�G�@��@��@�%@��@��/@���@�Ĝ@��@�b@��H@�n�@�^5@�ff@�n�@��#@�7L@��9@�j@�Z@�A�@�9X@�1@��w@���@��@�t�@�dZ@�S�@��@���@�n�@�M�@�5?@�J@��@���@�X@��@���@���@�I�@�1'@��@��m@�ƨ@���@�|�@�l�@�l�@�S�@��H@��\@�5?@�x�@�/@�%@��@�Ĝ@��u@�Z@�9X@�(�@� �@� �@���@���@�|�@�33@���@���@���@��@�p�@�O�@�&�@�%@���@�j@�(�@�  @��@���@�|�@�;d@�@�ȴ@���@�$�@��@���@�X@�O�@�G�@�?}@��/@��j@��9@��u@�bN@�  @�|�@�K�@�;d@�;d@���@���@���@��+@�5?@�{@�@��T@���@���@��7@��7@�G�@�%@��u@�z�@�Z@�Q�@�A�@�9X@�1'@�(�@��@�ƨ@��w@��F@���@�l�@�S�@�S�@�C�@�+@�+@�o@���@�^5@�-@���@��@���@��-@�`B@��9@��@�r�@�j@�j@�j@�A�@�  @���@�;d@���@���@��+@�-@���@��@�@��h@��@�hs@�G�@�/@��/@�Q�@�  @��F@�l�@��@��@�ȴ@��\@�n�@�{@��@�X@�G�@�G�@�/@��@���@�r�@�Z@�(�@�b@��m@���@�;d@�@���@���@�v�@�ff@�^5@�ff@�V@���@��#@���@���@��-@��@��D@�A�@��@���@�ƨ@��P@�dZ@�S�@�+@��y@���@�V@��@��@�@�?}@���@�Ĝ@�r�@�A�@�@�P@�@~��@~E�@}�T@}�-@}�@|�j@|�@{�m@{C�@z^5@y��@y��@yx�@x�`@x  @w\)@v�y@v�+@vff@v@u`B@st�@r��@r��@r��@r�\@r�\@r�\@r�\@rn�@r^5@r�@q7L@o
=@nE�@m��@mp�@l�D@l�@k�m@kt�@kC�@j~�@j-@i7L@i&�@hĜ@hQ�@g�@g��@g\)@gK�@fȴ@fff@f{@e��@e�@e/@e�@e�@eV@d��@d�j@dZ@d9X@d1@ct�@b�@bM�@a�@a�7@`��@`�@`Q�@`b@_�@_��@_�P@_|�@_�P@_�P@_|�@_�@^ȴ@^��@^V@^$�@^{@]��@]�-@]p�@\�@\�D@\�@[�
@Z��@ZM�@Y��@Y��@Y�7@YX@Y&�@Y�@Y�@X��@X��@XĜ@X�@XQ�@X �@W�P@W
=@U�@U�@UO�@T��@T�j@Tj@T�@S��@S33@R��@Q��@Qx�@QX@QG�@P�`@P��@PbN@O�@O|�@O+@Nv�@M�T@M��@L��@L��@L��@L�@Lz�@LZ@L(�@K�
@KC�@J��@J~�@J^5@J^5@J�@IG�@HĜ@HQ�@Hb@G��@G�P@G
=@F�y@F�R@F@EO�@D�@D��@Dz�@DI�@DI�@D�@C�
@C33@B��@Bn�@A�#@A��@A�7@A7L@@Ĝ@@bN@@bN@@  @?��@?|�@?�@>�y@>��@>�+@>�+@>5?@=p�@<�j@<�@<��@<�D@<I�@<(�@<�@;��@;�m@;dZ@;33@;@:�H@:�\@:^5@:=q@9�#@9�7@9G�@8��@8�u@81'@7�@7l�@7;d@7�@6�y@6V@5�T@5@5��@5�@5`B@4��@4Z@49X@3�m@3ƨ@3��@3t�@3dZ@3C�@3o@2�@2��@2~�@2M�@2-@1�#@1�^@1��@1hs@0��@0��@0��@0�u@0Q�@0  @/��@/�@/�P@/|�@/|�@/|�@/\)@/K�@/
=@.ȴ@.v�@.ff@.ff@.V@.V@.E�@.5?@.5?@.{@-�T@-p�@-O�@,�@,9X@,�@,1@+�m@+��@+dZ@+"�@+@*�H@*��@*��@*�\@*~�@*M�@*�@)��@)��@)��@)x�@)hs@)G�@)&�@(��@(��@(�`@(�9@(r�@(A�@( �@(  @'�;@'�@'|�@'K�@'+@'+@'+@&��@&ȴ@&�+@&v�@&{@%�T@%@%��@%�@%?}@$�@$��@$�j@$�@$I�@#�
@#ƨ@#ƨ@#�F@#��@#�@#t�@#dZ@#S�@#C�@#o@"��@"n�@!��@!��@!hs@!7L@!&�@!%@ �`@ Q�@�@\)@;d@
=@�y@��@�+@�+@�+@v�@v�@ff@ff@V@�@��@@�-@�-@p�@/@��@�@��@��@��@��@��@�D@Z@�m@dZ@o@@�@�H@��@~�@^5@�@�#@��@bN@1'@�w@K�@�@�@�@�y@��@�+@v�@5?@�T@p�@/@V@�/@��@(�@��@��@�@dZ@S�@C�@C�@33@o@@@@��@��@^5@-@��@�#@�#@�#@��@�^@��@��@x�@&�@%@��@��@�@b@��@�@|�@\)@K�@;d@+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�7LA�7LA�7LA�9XA�9XA�;dA�9XA�;dA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�A�A�A�A�A�A�C�A�C�A�E�A�E�A�G�A�A�A�9XA�1A�jA�`BA��A�  Aͺ^A�oA���Ả7A�  Aʩ�A��A�jA�bNAƇ+AŸRA�1A�n�A�1'A��/A��A�~�A��`A���A��#A��A���A��A�z�A�oA�E�A���A��yA� �A�XA��A���A�oA�r�A�1A�+A�ȴA�v�A�
=A�|�A���A���A��!A�bNA�JA��A�1'A��jA�A�A��A�1'A��A���A�l�A���A�jA��A��A���A���A�(�A���A��^A�VA�E�A�1A��9A�K�A�K�A�VA�oA��A��A���A��A���A�hsA��A���A�%A�\)A���A�/A��A���A�;dA�ĜA��hA�r�A�1'A���A��A~9XA{�hAx��AvVAsC�An�jAl��Al��Al��Ak��Aj��Aj^5Ai�#Ah~�Afn�Ac�AbJA`JA]ƨA\�AZ�!AY��AX�9AV�RASƨAR��AQ/AP��AN5?AM&�AKAI��AIC�AH=qAEhsAB�uAB$�AA�A@�jA?�PA>�DA<z�A;&�A:n�A9��A9A9��A7A6�A6r�A5dZA3�A2�RA1��A0$�A.�A.1'A-ƨA-S�A-oA,bNA+G�A)S�A&bNA%l�A%\)A%VA$�A#�mA"�HA!oAJA^5A  A��A\)A�A;dA�/A�A9XA�;A%A�A��Az�A�^A9XA��A�A=qAO�A�\A"�An�AJA�A
ĜA	�AVA�A��A��A��AA��Az�A �A 5?@�o@��H@�V@��@�7L@� �@�O�@�l�@��h@�&�@�%@��u@��
@�~�@�hs@�@�\)@��#@�\)@��@���@�v�@�V@���@�l�@�"�@�h@�9X@ߍP@ް!@ݡ�@���@�b@ڗ�@�j@�9X@�Q�@ش9@�`B@ٲ-@���@���@�{@Ѓ@�I�@�;d@���@��@�t�@��@���@���@�9X@�
=@őh@��@�Ĝ@�r�@�  @Ý�@�o@�@���@��7@���@��/@��F@�J@�G�@��9@�Q�@� �@���@��w@�\)@�;d@���@��\@�@�O�@�r�@���@���@���@�?}@��u@�A�@���@�33@�n�@���@��@���@���@�hs@�?}@�%@���@� �@�t�@���@�{@��#@���@�G�@��@��@�%@��@��/@���@�Ĝ@��@�b@��H@�n�@�^5@�ff@�n�@��#@�7L@��9@�j@�Z@�A�@�9X@�1@��w@���@��@�t�@�dZ@�S�@��@���@�n�@�M�@�5?@�J@��@���@�X@��@���@���@�I�@�1'@��@��m@�ƨ@���@�|�@�l�@�l�@�S�@��H@��\@�5?@�x�@�/@�%@��@�Ĝ@��u@�Z@�9X@�(�@� �@� �@���@���@�|�@�33@���@���@���@��@�p�@�O�@�&�@�%@���@�j@�(�@�  @��@���@�|�@�;d@�@�ȴ@���@�$�@��@���@�X@�O�@�G�@�?}@��/@��j@��9@��u@�bN@�  @�|�@�K�@�;d@�;d@���@���@���@��+@�5?@�{@�@��T@���@���@��7@��7@�G�@�%@��u@�z�@�Z@�Q�@�A�@�9X@�1'@�(�@��@�ƨ@��w@��F@���@�l�@�S�@�S�@�C�@�+@�+@�o@���@�^5@�-@���@��@���@��-@�`B@��9@��@�r�@�j@�j@�j@�A�@�  @���@�;d@���@���@��+@�-@���@��@�@��h@��@�hs@�G�@�/@��/@�Q�@�  @��F@�l�@��@��@�ȴ@��\@�n�@�{@��@�X@�G�@�G�@�/@��@���@�r�@�Z@�(�@�b@��m@���@�;d@�@���@���@�v�@�ff@�^5@�ff@�V@���@��#@���@���@��-@��@��D@�A�@��@���@�ƨ@��P@�dZ@�S�@�+@��y@���@�V@��@��@�@�?}@���@�Ĝ@�r�@�A�@�@�P@�@~��@~E�@}�T@}�-@}�@|�j@|�@{�m@{C�@z^5@y��@y��@yx�@x�`@x  @w\)@v�y@v�+@vff@v@u`B@st�@r��@r��@r��@r�\@r�\@r�\@r�\@rn�@r^5@r�@q7L@o
=@nE�@m��@mp�@l�D@l�@k�m@kt�@kC�@j~�@j-@i7L@i&�@hĜ@hQ�@g�@g��@g\)@gK�@fȴ@fff@f{@e��@e�@e/@e�@e�@eV@d��@d�j@dZ@d9X@d1@ct�@b�@bM�@a�@a�7@`��@`�@`Q�@`b@_�@_��@_�P@_|�@_�P@_�P@_|�@_�@^ȴ@^��@^V@^$�@^{@]��@]�-@]p�@\�@\�D@\�@[�
@Z��@ZM�@Y��@Y��@Y�7@YX@Y&�@Y�@Y�@X��@X��@XĜ@X�@XQ�@X �@W�P@W
=@U�@U�@UO�@T��@T�j@Tj@T�@S��@S33@R��@Q��@Qx�@QX@QG�@P�`@P��@PbN@O�@O|�@O+@Nv�@M�T@M��@L��@L��@L��@L�@Lz�@LZ@L(�@K�
@KC�@J��@J~�@J^5@J^5@J�@IG�@HĜ@HQ�@Hb@G��@G�P@G
=@F�y@F�R@F@EO�@D�@D��@Dz�@DI�@DI�@D�@C�
@C33@B��@Bn�@A�#@A��@A�7@A7L@@Ĝ@@bN@@bN@@  @?��@?|�@?�@>�y@>��@>�+@>�+@>5?@=p�@<�j@<�@<��@<�D@<I�@<(�@<�@;��@;�m@;dZ@;33@;@:�H@:�\@:^5@:=q@9�#@9�7@9G�@8��@8�u@81'@7�@7l�@7;d@7�@6�y@6V@5�T@5@5��@5�@5`B@4��@4Z@49X@3�m@3ƨ@3��@3t�@3dZ@3C�@3o@2�@2��@2~�@2M�@2-@1�#@1�^@1��@1hs@0��@0��@0��@0�u@0Q�@0  @/��@/�@/�P@/|�@/|�@/|�@/\)@/K�@/
=@.ȴ@.v�@.ff@.ff@.V@.V@.E�@.5?@.5?@.{@-�T@-p�@-O�@,�@,9X@,�@,1@+�m@+��@+dZ@+"�@+@*�H@*��@*��@*�\@*~�@*M�@*�@)��@)��@)��@)x�@)hs@)G�@)&�@(��@(��@(�`@(�9@(r�@(A�@( �@(  @'�;@'�@'|�@'K�@'+@'+@'+@&��@&ȴ@&�+@&v�@&{@%�T@%@%��@%�@%?}@$�@$��@$�j@$�@$I�@#�
@#ƨ@#ƨ@#�F@#��@#�@#t�@#dZ@#S�@#C�@#o@"��@"n�@!��@!��@!hs@!7L@!&�@!%@ �`@ Q�@�@\)@;d@
=@�y@��@�+@�+@�+@v�@v�@ff@ff@V@�@��@@�-@�-@p�@/@��@�@��@��@��@��@��@�D@Z@�m@dZ@o@@�@�H@��@~�@^5@�@�#@��@bN@1'@�w@K�@�@�@�@�y@��@�+@v�@5?@�T@p�@/@V@�/@��@(�@��@��@�@dZ@S�@C�@C�@33@o@@@@��@��@^5@-@��@�#@�#@�#@��@�^@��@��@x�@&�@%@��@��@�@b@��@�@|�@\)@K�@;d@+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�sB�mB�TB�)BŢB��B��B�B��B��BBBB  B\BDB��BJB�B1'B,B)�B<jB<jB]/Bn�BiyBk�Bq�BjBv�B�+B�B~�B}�B� B}�B�B�Bw�Bu�Bl�Bx�Bp�BaHBiyBffBbNBZBR�BI�BH�BC�B6FB+B)�B#�B �B�B!�B�BDB  B�B�)B�5B��BŢBBB�3B�B��B�JBx�BiyB^5BVBS�BL�B=qB/B"�B#�B �BoB
��B
�B
��B
�dB
��B
��B
�PB
�DB
�%B
�B
~�B
x�B
m�B
\)B
E�B
0!B
�B
+B	��B	�/B	��B	�B	�B	��B	ȴB	ĜB	�wB	�-B	��B	�uB	�1B	}�B	p�B	gmB	dZB	\)B	W
B	J�B	<jB	9XB	33B	0!B	!�B	�B	�B	JB	VB	B�B�`B�B�B�fB�;B�5B��B�B�B�B�
B��BɺBǮBȴB��B�RB�^B�?B�'B�B�'B�'B�!B�B��B��B�oB�7B�hB��B�{B�hB�+B~�Br�BffBgmBs�Br�Bq�Bl�BiyBp�Bp�Bo�Bm�BffBcTBaHBdZB`BBXBaHB`BB[#B_;BffB_;Bl�Bq�Bn�BjBffBdZBk�Bk�Be`B`BBffBcTBffBcTBVB^5BcTBbNBaHB]/BXBO�BVBYBgmBjBjBjBhsBjBjBgmBhsBhsBiyBe`Bn�BjBjBl�Bl�BgmBgmBjBjBiyBn�Bn�Bs�Bx�B�\B�uB��B��B��B��B��B��B��B�B�B��B�-B�?B�FB�9B�3B�3B�'B�3B�RB�qB�qB�}B��B��BÖBĜBɺB��B��B��B��B�5B�TB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B	B	B		7B	PB	hB	�B	 �B	'�B	'�B	(�B	+B	,B	-B	.B	/B	0!B	5?B	<jB	C�B	D�B	F�B	H�B	I�B	I�B	I�B	J�B	J�B	I�B	H�B	F�B	G�B	P�B	VB	W
B	XB	XB	\)B	`BB	bNB	e`B	ffB	gmB	ffB	gmB	iyB	jB	k�B	l�B	l�B	l�B	q�B	s�B	t�B	u�B	u�B	u�B	u�B	u�B	w�B	|�B	|�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�1B	�=B	�=B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�LB	�RB	�dB	�jB	�dB	�wB	�}B	��B	��B	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�#B	�B	�B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�;B	�BB	�HB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
%B
+B
1B
1B
	7B
DB

=B
	7B
DB
JB
DB

=B
	7B

=B
JB
PB
PB
PB
JB
PB
PB
PB
JB
JB
JB
PB
PB
PB
JB
VB
\B
\B
bB
bB
hB
hB
oB
uB
uB
{B
uB
uB
uB
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
!�B
#�B
#�B
#�B
$�B
%�B
%�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
&�B
'�B
'�B
(�B
(�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
+B
+B
,B
-B
-B
-B
.B
-B
-B
-B
,B
-B
-B
-B
,B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
1'B
0!B
49B
5?B
5?B
5?B
6FB
5?B
5?B
6FB
6FB
6FB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
=qB
=qB
?}B
?}B
>wB
>wB
?}B
>wB
>wB
>wB
?}B
@�B
A�B
A�B
@�B
>wB
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
D�B
F�B
G�B
F�B
H�B
H�B
H�B
H�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
J�B
K�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
S�B
R�B
S�B
T�B
T�B
VB
VB
VB
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
XB
YB
YB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
ZB
ZB
[#B
ZB
ZB
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
^5B
]/B
]/B
]/B
]/B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
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
aHB
aHB
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
cTB
cTB
cTB
cTB
e`B
e`B
e`B
e`B
e`B
dZB
dZB
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
gmB
hsB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
jB
iyB
iyB
hsB
iyB
jB
l�B
k�B
k�B
k�B
l�B
k�B
k�B
jB
iyB
k�B
l�B
l�B
l�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
m�B
n�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
p�B
p�B
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
r�B
r�B
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
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�sB�B��B��B�	B� B�B�B��B��B�B�B�B�B�BPB �B<B�B3hB1AB-�B=VB>]B^�Bp;BlBmCBsBm�Bx�B�B�9B��B}B��B�B��B�{B{�ByrBo�By�Br�Bc�BjKBg�Bc�B\BT�BK�BI�BD�B8lB,�B+B$�B!�B#B"NB!BB'B�IB��BߊB�FBɆBāB��B�?B��B��B�BB|Bl�B`vBW�BT�BN"B?�B0�B$�B$�B!�BaB
�qB
�B
��B
�cB
�eB
��B
��B
�0B
�+B
��B
}B
y�B
oiB
_!B
IB
3�B
 'B

�B	��B	��B	յB	�eB	ևB	�(B	ɺB	�9B	�}B	�9B	��B	��B	�XB	��B	shB	i�B	fB	]�B	X�B	M�B	?�B	:�B	4�B	1[B	$�B	 BB	yB	pB	�B	B�2B�sB�-B�UB�
B��B߾BՁB׍B��BخB�sBөB��B��BɠB�-B��B��B��B��B��B�B��B��B��B�*B��B�MB�dB�oB��B��B�B��B��ButBi�Bi_Bt9BsMBraBm�Bj�Bq'BqABp;BnIBg�BeBb�Bd�Ba|BY�Ba�Ba-B\�B`�Bg�BaBmwBrGBo�Bk�Bh
Be�Bl=BlqBgBbNBg�Bd�BgBdZBX_B_Bc�Bb�Ba�B]�BY1BQ�BWYBZQBg�Bj�BkBk6Bi_BkQBk6Bh�Bi�Bj0Bj�BgBo Bk�Bk6BmBl�Bh�BhXBkBkBjKBo5BoiBt�Bz*B�BB�[B�SB�kB��B��B�B� B��B�iB�B�eB��B��B��B�B��B��B�B�B��B��B��B��B��B�B��B�9B��B�B̈́B��B�B޸B�B��B�B�B�B��B��B��B��B�B�B�[B�ZB��B��B	mB	�B		�B	�B	B	B	!B	(
B	(>B	)*B	+6B	,=B	-]B	.}B	/�B	0�B	5�B	<�B	C�B	D�B	F�B	H�B	I�B	I�B	I�B	J�B	J�B	I�B	H�B	GEB	HfB	QB	VB	W$B	XEB	X�B	\�B	`�B	b�B	ezB	f�B	g�B	f�B	g�B	i�B	j�B	k�B	l�B	l�B	l�B	q�B	s�B	t�B	u�B	u�B	u�B	u�B	v+B	xB	}B	}"B	~(B	�B	�'B	�AB	�3B	�9B	�9B	�?B	�?B	�SB	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�B	�$B	�*B	�DB	�_B	�_B	�)B	�CB	�IB	�iB	�|B	�tB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�4B	�B	�$B	�$B	�?B	�YB	�	B	�B	�QB	�IB	�OB	�OB	�OB	�OB	�VB	�\B	�pB	�vB	�B	�zB	�B	�B	�B	�mB	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�6B	�PB	�(B	�.B
 4B
 B
 B
'B
;B
AB
;B
 iB
3B
B
B
3B
3B
-B
GB
9B
?B
?B
YB
9B
gB
SB
YB
EB
KB
KB
	7B
DB

XB
	lB
^B
JB
^B

rB
	�B

�B
~B
jB
jB
�B
~B
jB
jB
�B
~B
~B
~B
jB
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
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
B
KB
�B
�B
�B
�B
�B
�B
�B
�B
B
!�B
!B
!�B
$B
$B
$B
$�B
%�B
%�B
%B
%,B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
(
B
'B
(
B
(
B
(
B
'B
($B
($B
)*B
)*B
($B
($B
(�B
(�B
*B
*B
*B
+B
+B
,B
+B
+6B
,"B
-)B
-)B
-)B
.B
-)B
-B
-)B
,=B
-CB
-CB
-]B
,qB
.IB
/5B
/5B
0;B
0;B
0;B
1'B
1'B
1AB
1AB
1[B
1AB
1AB
1AB
0oB
1[B
0�B
4TB
5ZB
5ZB
5ZB
6`B
5tB
5ZB
6zB
6`B
6�B
8lB
9rB
9rB
9XB
9rB
9rB
:�B
:xB
;�B
;�B
;�B
=�B
=�B
?}B
?}B
>�B
>�B
?�B
>�B
>�B
>�B
?�B
@�B
A�B
A�B
@�B
>�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
B�B
B�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
D�B
F�B
G�B
F�B
H�B
H�B
H�B
H�B
I�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
KB
K�B
N�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
NB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
O�B
PB
QB
QB
RB
Q�B
SB
RB
R B
RB
TB
TB
T,B
TB
S&B
TB
UB
UB
VB
VB
V9B
VB
VB
VB
VB
VB
VB
W$B
W$B
W$B
W$B
W$B
W
B
W$B
X+B
YB
Y1B
X+B
X+B
Y1B
Y1B
Z7B
ZB
ZB
ZB
Z7B
Y1B
Y1B
YB
Z7B
[#B
[#B
[#B
[#B
[#B
[	B
[#B
Z7B
Z7B
ZkB
[=B
ZQB
ZQB
\CB
\CB
\)B
\CB
\)B
\CB
]IB
]IB
]/B
]IB
^5B
]IB
]IB
]IB
]IB
^OB
_;B
^OB
_!B
_VB
_pB
_VB
_;B
_VB
_VB
^5B
_VB
_VB
_pB
_VB
_VB
_VB
`\B
`\B
`BB
`BB
`\B
`\B
`\B
`\B
`BB
abB
abB
abB
abB
abB
a|B
bhB
bhB
bhB
abB
a|B
cTB
cTB
cTB
c�B
cTB
cTB
cTB
c:B
cnB
cnB
cnB
cnB
c�B
c�B
ezB
ezB
ezB
e`B
ezB
dtB
d�B
f�B
g�B
g�B
gmB
g�B
hsB
hsB
hsB
hXB
hsB
hsB
hsB
hsB
g�B
hsB
iyB
h�B
hsB
h�B
h�B
i�B
iyB
jB
j�B
jB
jB
jB
i�B
i�B
h�B
i�B
jB
l�B
k�B
k�B
k�B
l�B
k�B
k�B
j�B
i�B
k�B
l�B
l�B
l�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
m�B
n�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
q�B
p�B
p�B
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
r�B
r�B
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
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710170034242017101700342420171017003424201806221320292018062213202920180622132029201804050723092018040507230920180405072309  JA  ARFMdecpA19c                                                                20171013063511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171012213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171012213513  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171012213514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171012213514  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171012213514  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171012213515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171012213515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171012213515  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171012213515                      G�O�G�O�G�O�                JA  ARUP                                                                        20171012215616                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171013153323  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20171016153424  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171016153424  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222309  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042029  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                