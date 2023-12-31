CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-24T00:42:31Z creation;2018-07-24T00:42:35Z conversion to V3.1;2019-12-19T07:33:03Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180724004231  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_263                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�s���.�1   @�s���O�@3�F
�L0�d\_o� 1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A���A���A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @'
=@�Q�@�Q�A (�A!A@(�A`(�A�{A�{A�{A�{A�{A��HA��HA�G�B 
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
=B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D1�=D2z=D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA�
DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRDҽD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��HA��TA��HA��;A��/A��#A��/A���A�XA��mAס�AָRA�S�A���A�+A�bNA��A�1'A�/A�O�A���A��A���A�C�A�VA�n�A��yA��A��A�bA�ȴA�/A�~�A�n�A�;dA�{A��A�n�A��A��DA��9A��yA���A���A��A���A�  A�~�A�dZA�/A��HA�dZA�dZA�ffA��9A�=qA�z�A���A�{A�VA�ffA��FA�?}A��7A��mA�1'A�7LA�S�A�jA�"�A��A�7LA���A�33A�`BA�v�A���A�n�A�%A��A��9A�n�A��A�A�O�A���A�5?A���A�Q�A�C�A��HA�  A�(�A���A�C�A���A���A���A��A;dA~bNA}��A|$�A{��Ay�mAwl�At�As�As
=Ar�RArJAq�Ap1An�An{Akp�Ah�/Ah�AgAghsAf��Ae��Ad�DAa�A_|�A^ĜA]�A\=qAZ^5AY�AVn�AT��AS�mAR�\AQ��AOAM��AL��AJ{AF�RAC��AA��A@�jA@5?A@�A?�FA=/A;�-A;dZA:��A9XA7ƨA6�uA6 �A5��A4�!A2�/A1C�A0��A/�A/+A.9XA,ĜA*�`A*bA)�A)XA(�9A'�A&~�A%/A$�A$(�A#��A#7LA"ffA �AS�A�PA�A��A��A1A��A�A/AȴAbA{A��A��A�DA��A��A
n�A	�A��A��A{A&�A�A�DAn�A�#A+A��A9XA��A�AC�A �@���@�~�@���@�E�@�ƨ@�^5@�/@�@��H@�@�/@���@�z�@�  @�
=@���@�&�@�  @���@��@�Z@�x�@�bN@�(�@�+@��@ߍP@���@��m@��y@ٲ-@��@�  @և+@�r�@�ff@��@�I�@��@ϕ�@�K�@θR@��T@̬@̓u@�z�@��`@ǥ�@�@�5?@�p�@�bN@î@�ȴ@+@���@��`@�I�@�K�@�M�@��@��@�z�@�ƨ@�v�@��@��-@���@�Ĝ@��u@�  @�t�@���@�M�@��^@��`@�bN@�(�@�b@���@�@�v�@�{@���@�X@��@�I�@��@��F@�K�@�dZ@�o@���@��@��7@��`@���@��;@���@�E�@�@���@�G�@�V@���@��u@�r�@�I�@�b@�ƨ@���@�+@���@�^5@�E�@��@��-@�x�@�`B@��`@���@���@�A�@� �@���@��@�33@�
=@�@�ȴ@���@�^5@�5?@��@��^@�x�@��@��/@��@�A�@�1@���@��w@�dZ@�
=@��R@�V@��\@�o@���@�$�@���@�V@��@�Q�@�(�@�1@��;@��w@�ƨ@���@�\)@�C�@�33@��H@���@�ff@�E�@��@�{@��@��@�-@��\@�E�@�hs@���@�Z@�A�@��@��@�K�@���@�@��@��y@��@���@���@��+@�M�@�=q@�$�@�{@�J@��@�@���@�hs@�hs@�&�@���@��@��@��D@�I�@�1@��
@���@�
=@��!@���@��\@��+@��+@�n�@�-@��#@��-@�x�@�X@�?}@�Ĝ@�I�@��@��
@��@�S�@�;d@��@���@��@��+@�n�@�^5@�V@�5?@��@�{@��@���@�x�@��@��j@���@�z�@�9X@�ƨ@�dZ@�@�ȴ@�^5@�E�@�$�@��h@�G�@���@��@���@���@�9X@��F@���@���@��+@��@�@���@�x�@�X@�7L@�V@�%@�%@�%@���@���@���@�Z@�A�@��m@��
@��
@��@�l�@�;d@�;d@�+@��@�+@�\)@�S�@�o@�33@�C�@�;d@�33@�@��R@�V@�@��-@��@�O�@��@�Ĝ@��@��@�Z@�(�@|�@
=@~�R@~E�@~$�@}�-@}?}@}�@|�@|1@{�
@{C�@z��@zM�@zJ@z�@y�#@y�^@y7L@w�@w|�@w\)@v��@v5?@u�T@u�-@t��@tz�@tI�@t1@s�
@sdZ@sC�@sC�@s33@so@r��@r^5@q�^@q7L@p�`@p��@pr�@p1'@o�;@o�@o;d@o+@o
=@n��@n�y@n�y@n�y@n��@nv�@n�+@n5?@n@m�-@mO�@m�@l�@l��@l��@k��@kS�@j�@jM�@i��@i��@i&�@h��@h  @g��@gl�@f�@f5?@e�T@e�@d(�@c�m@cƨ@ct�@b��@a�@a��@a�^@a�7@a7L@`��@`�`@`�`@`�@`b@_��@_l�@_
=@^��@^v�@^V@^$�@]�T@]��@]�@]/@\�/@\�D@\9X@[�
@[o@Z��@Zn�@Z�@Y�#@Y��@Y��@YX@X�`@X�@X  @W�;@W�@Wl�@WK�@W
=@V��@V$�@U@UO�@T�j@T�@Sƨ@St�@R�H@R�\@R~�@R^5@R-@Q��@Q�@Q%@P�`@Pr�@Pb@O�;@Ol�@O�@N�@N�R@Nv�@NV@N{@M@MO�@L�j@LZ@K�
@K�@KC�@Ko@J�@J��@Jn�@J-@I�@Ix�@HĜ@Hr�@Hr�@HA�@H  @G�w@GK�@F�@F�R@F��@F$�@E��@E�-@E�h@Ep�@E?}@E�@D��@D�/@D��@D(�@C�m@CdZ@C@B��@B^5@A��@Ax�@@��@@Ĝ@@�9@@�@@A�@@1'@@b@?�;@?�@?\)@?�@>��@>V@>$�@=�@=��@=@=��@=p�@=O�@=V@<�@<�@<z�@<j@<j@<Z@<9X@<(�@<�@<1@;�m@:��@9��@9x�@9G�@9%@8�`@8�`@8�`@8��@8Q�@7��@7\)@6�y@6v�@6@5�T@5��@5p�@4��@4I�@3��@3�m@3�m@3�m@3�m@3�F@3C�@2�!@2=q@1�@1�#@1��@1�^@1X@1X@1X@1G�@1%@0�u@0Q�@0b@/�;@/��@/
=@.ȴ@.��@.�+@.�+@.ff@.{@-��@-`B@-V@,�D@,Z@,(�@,1@+�m@+�@+C�@+33@+33@+"�@+"�@+@*��@*��@*��@*�!@*n�@*=q@)��@)X@)&�@(�9@(r�@( �@'�@'�;@'��@'�w@'�@'K�@';d@'
=@&��@%�-@%p�@%/@$�@$I�@$(�@#�
@#��@#t�@#C�@"�@"�\@"M�@!�#@!��@!��@!�7@!hs@!G�@!&�@ �`@ Ĝ@ ��@ bN@ A�@ b@�@l�@�@�y@�R@V@{@��@�-@�h@�h@�@p�@`B@`B@?}@��@j@Z@Z@I�@�@�F@��@C�@"�@�H@��@^5@��@��@��@��@x�@G�@%@Ĝ@�@bN@A�@1'@�@�w@�@�P@\)@K�@;d@+@�y@��@�+@V@5?@$�@@��@@��@p�@O�@�/@�@�D@Z@I�@1@�
@�F@��@S�@33@@��@�!@��@�\@�\@n�@M�@�@�@�^@�7@hs@7L@&�@�@%@�`@Ĝ@�@r�@r�@bN@Q�@A�@1'@1'@ �@�@�@|�@l�@l�@l�@\)@;d@
=@�y@�R@ff@E�@E�@5?@��@�-@�@`B@?}@V@�@�/@�j@I�@(�@(�@��@�F@�@dZ@33@@
�@
��@
�\@
~�@
n�@
-@
J@	�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��HA��TA��HA��;A��/A��#A��/A���A�XA��mAס�AָRA�S�A���A�+A�bNA��A�1'A�/A�O�A���A��A���A�C�A�VA�n�A��yA��A��A�bA�ȴA�/A�~�A�n�A�;dA�{A��A�n�A��A��DA��9A��yA���A���A��A���A�  A�~�A�dZA�/A��HA�dZA�dZA�ffA��9A�=qA�z�A���A�{A�VA�ffA��FA�?}A��7A��mA�1'A�7LA�S�A�jA�"�A��A�7LA���A�33A�`BA�v�A���A�n�A�%A��A��9A�n�A��A�A�O�A���A�5?A���A�Q�A�C�A��HA�  A�(�A���A�C�A���A���A���A��A;dA~bNA}��A|$�A{��Ay�mAwl�At�As�As
=Ar�RArJAq�Ap1An�An{Akp�Ah�/Ah�AgAghsAf��Ae��Ad�DAa�A_|�A^ĜA]�A\=qAZ^5AY�AVn�AT��AS�mAR�\AQ��AOAM��AL��AJ{AF�RAC��AA��A@�jA@5?A@�A?�FA=/A;�-A;dZA:��A9XA7ƨA6�uA6 �A5��A4�!A2�/A1C�A0��A/�A/+A.9XA,ĜA*�`A*bA)�A)XA(�9A'�A&~�A%/A$�A$(�A#��A#7LA"ffA �AS�A�PA�A��A��A1A��A�A/AȴAbA{A��A��A�DA��A��A
n�A	�A��A��A{A&�A�A�DAn�A�#A+A��A9XA��A�AC�A �@���@�~�@���@�E�@�ƨ@�^5@�/@�@��H@�@�/@���@�z�@�  @�
=@���@�&�@�  @���@��@�Z@�x�@�bN@�(�@�+@��@ߍP@���@��m@��y@ٲ-@��@�  @և+@�r�@�ff@��@�I�@��@ϕ�@�K�@θR@��T@̬@̓u@�z�@��`@ǥ�@�@�5?@�p�@�bN@î@�ȴ@+@���@��`@�I�@�K�@�M�@��@��@�z�@�ƨ@�v�@��@��-@���@�Ĝ@��u@�  @�t�@���@�M�@��^@��`@�bN@�(�@�b@���@�@�v�@�{@���@�X@��@�I�@��@��F@�K�@�dZ@�o@���@��@��7@��`@���@��;@���@�E�@�@���@�G�@�V@���@��u@�r�@�I�@�b@�ƨ@���@�+@���@�^5@�E�@��@��-@�x�@�`B@��`@���@���@�A�@� �@���@��@�33@�
=@�@�ȴ@���@�^5@�5?@��@��^@�x�@��@��/@��@�A�@�1@���@��w@�dZ@�
=@��R@�V@��\@�o@���@�$�@���@�V@��@�Q�@�(�@�1@��;@��w@�ƨ@���@�\)@�C�@�33@��H@���@�ff@�E�@��@�{@��@��@�-@��\@�E�@�hs@���@�Z@�A�@��@��@�K�@���@�@��@��y@��@���@���@��+@�M�@�=q@�$�@�{@�J@��@�@���@�hs@�hs@�&�@���@��@��@��D@�I�@�1@��
@���@�
=@��!@���@��\@��+@��+@�n�@�-@��#@��-@�x�@�X@�?}@�Ĝ@�I�@��@��
@��@�S�@�;d@��@���@��@��+@�n�@�^5@�V@�5?@��@�{@��@���@�x�@��@��j@���@�z�@�9X@�ƨ@�dZ@�@�ȴ@�^5@�E�@�$�@��h@�G�@���@��@���@���@�9X@��F@���@���@��+@��@�@���@�x�@�X@�7L@�V@�%@�%@�%@���@���@���@�Z@�A�@��m@��
@��
@��@�l�@�;d@�;d@�+@��@�+@�\)@�S�@�o@�33@�C�@�;d@�33@�@��R@�V@�@��-@��@�O�@��@�Ĝ@��@��@�Z@�(�@|�@
=@~�R@~E�@~$�@}�-@}?}@}�@|�@|1@{�
@{C�@z��@zM�@zJ@z�@y�#@y�^@y7L@w�@w|�@w\)@v��@v5?@u�T@u�-@t��@tz�@tI�@t1@s�
@sdZ@sC�@sC�@s33@so@r��@r^5@q�^@q7L@p�`@p��@pr�@p1'@o�;@o�@o;d@o+@o
=@n��@n�y@n�y@n�y@n��@nv�@n�+@n5?@n@m�-@mO�@m�@l�@l��@l��@k��@kS�@j�@jM�@i��@i��@i&�@h��@h  @g��@gl�@f�@f5?@e�T@e�@d(�@c�m@cƨ@ct�@b��@a�@a��@a�^@a�7@a7L@`��@`�`@`�`@`�@`b@_��@_l�@_
=@^��@^v�@^V@^$�@]�T@]��@]�@]/@\�/@\�D@\9X@[�
@[o@Z��@Zn�@Z�@Y�#@Y��@Y��@YX@X�`@X�@X  @W�;@W�@Wl�@WK�@W
=@V��@V$�@U@UO�@T�j@T�@Sƨ@St�@R�H@R�\@R~�@R^5@R-@Q��@Q�@Q%@P�`@Pr�@Pb@O�;@Ol�@O�@N�@N�R@Nv�@NV@N{@M@MO�@L�j@LZ@K�
@K�@KC�@Ko@J�@J��@Jn�@J-@I�@Ix�@HĜ@Hr�@Hr�@HA�@H  @G�w@GK�@F�@F�R@F��@F$�@E��@E�-@E�h@Ep�@E?}@E�@D��@D�/@D��@D(�@C�m@CdZ@C@B��@B^5@A��@Ax�@@��@@Ĝ@@�9@@�@@A�@@1'@@b@?�;@?�@?\)@?�@>��@>V@>$�@=�@=��@=@=��@=p�@=O�@=V@<�@<�@<z�@<j@<j@<Z@<9X@<(�@<�@<1@;�m@:��@9��@9x�@9G�@9%@8�`@8�`@8�`@8��@8Q�@7��@7\)@6�y@6v�@6@5�T@5��@5p�@4��@4I�@3��@3�m@3�m@3�m@3�m@3�F@3C�@2�!@2=q@1�@1�#@1��@1�^@1X@1X@1X@1G�@1%@0�u@0Q�@0b@/�;@/��@/
=@.ȴ@.��@.�+@.�+@.ff@.{@-��@-`B@-V@,�D@,Z@,(�@,1@+�m@+�@+C�@+33@+33@+"�@+"�@+@*��@*��@*��@*�!@*n�@*=q@)��@)X@)&�@(�9@(r�@( �@'�@'�;@'��@'�w@'�@'K�@';d@'
=@&��@%�-@%p�@%/@$�@$I�@$(�@#�
@#��@#t�@#C�@"�@"�\@"M�@!�#@!��@!��@!�7@!hs@!G�@!&�@ �`@ Ĝ@ ��@ bN@ A�@ b@�@l�@�@�y@�R@V@{@��@�-@�h@�h@�@p�@`B@`B@?}@��@j@Z@Z@I�@�@�F@��@C�@"�@�H@��@^5@��@��@��@��@x�@G�@%@Ĝ@�@bN@A�@1'@�@�w@�@�P@\)@K�@;d@+@�y@��@�+@V@5?@$�@@��@@��@p�@O�@�/@�@�D@Z@I�@1@�
@�F@��@S�@33@@��@�!@��@�\@�\@n�@M�@�@�@�^@�7@hs@7L@&�@�@%@�`@Ĝ@�@r�@r�@bN@Q�@A�@1'@1'@ �@�@�@|�@l�@l�@l�@\)@;d@
=@�y@�R@ff@E�@E�@5?@��@�-@�@`B@?}@V@�@�/@�j@I�@(�@(�@��@�F@�@dZ@33@@
�@
��@
�\@
~�@
n�@
-@
J@	�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B33B49B49B49B49B49B49B1'B,BA�BN�B��B��B�7B7LBx�B�NB%BB�B  B	7B7LBL�BVBn�Bq�Bx�By�Bs�Bs�BjBiyBdZB{�B|�By�Bp�Bo�Bl�B^5BbNB^5BdZBn�BjBp�B�B�JB�7B�By�BgmBdZBhsBR�B8RB-BbB�B��BJB��B�B�sB�/B��B�^B�B��B��B��By�B[#B_;BT�BJ�BT�BG�B5?B�B �B%B
�B
�`B
�5B
��B
�?B
�qB
��B
�bB
�DB
�1B
�7B
�%B
�B
�B
~�B
r�B
jB
hsB
cTB
R�B
N�B
:^B
(�B
�B
 �B
$�B
"�B
�B
VB
B	��B	�B	�B	ȴB	�B	�B	��B	ǮB	�?B	��B	�7B	�+B	�VB	�B	w�B	dZB	]/B	J�B	=qB	H�B	7LB	6FB	�B	�B	�B��B�)B��B�B�`B�sB�yB�;BŢB�RB��BB�9B��B��B��B��B��B�1B�%B�hB�hB�\B�Bx�Bp�B� B�B�B{�Bu�Bn�Bm�B}�B{�B|�Bz�Bu�BgmBjBe`B`BBffBl�BcTBL�BD�BP�BW
BL�B9XB7LB=qB8RB#�B6FB.B-B/B<jB<jB<jBF�BO�BP�BJ�BM�BW
BXB\)B^5BW
BP�BW
BR�BN�BF�BD�BP�BR�BYBT�BdZBo�Br�Bs�Br�Bq�Bn�By�Bs�Bq�B{�Bz�Bq�B� B�7B�B|�Bt�Bu�Bz�B|�Bv�B{�Bx�Bt�Bt�By�B�B�PB�oB�oB�hB�VB�DB�JB��B�hB� B�bB��B��B��B��B��B��B��B��B��B�B�B�-B�XB�dB�XB�}B�}BɺB��B��B��B��B��B��B��B�B�B�)B�NB�mB�sB�fB�mB�B�B�B�B�B��B	B	B	B	1B		7B	
=B	+B	PB	PB	{B	oB	hB	�B	�B	�B	"�B	&�B	(�B	+B	.B	0!B	1'B	5?B	6FB	6FB	:^B	C�B	F�B	F�B	F�B	J�B	N�B	N�B	T�B	XB	YB	\)B	[#B	\)B	\)B	`BB	dZB	ffB	hsB	hsB	jB	iyB	k�B	k�B	k�B	m�B	n�B	o�B	r�B	u�B	u�B	s�B	t�B	v�B	x�B	�B	�B	�B	~�B	�B	� B	�%B	�B	�+B	�7B	�7B	�=B	�JB	�JB	�DB	�\B	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�?B	�?B	�FB	�XB	�XB	�XB	�XB	�^B	�qB	�}B	��B	B	ĜB	ɺB	ƨB	ƨB	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�)B	�5B	�;B	�;B	�5B	�5B	�5B	�/B	�)B	�/B	�)B	�5B	�BB	�BB	�5B	�/B	�/B	�;B	�HB	�HB	�ZB	�TB	�HB	�ZB	�fB	�fB	�sB	�sB	�`B	�ZB	�NB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
%B

=B
	7B
+B
DB
JB
DB
DB

=B

=B

=B

=B
DB
PB
VB
PB
\B
\B
\B
VB
VB
\B
hB
uB
uB
{B
uB
{B
�B
{B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
!�B
"�B
#�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
$�B
!�B
#�B
$�B
$�B
$�B
&�B
%�B
$�B
%�B
&�B
'�B
&�B
%�B
'�B
'�B
$�B
)�B
+B
)�B
&�B
&�B
+B
,B
,B
,B
-B
.B
.B
-B
,B
-B
-B
,B
.B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
1'B
0!B
0!B
2-B
2-B
33B
49B
49B
49B
33B
2-B
2-B
33B
5?B
49B
49B
5?B
49B
49B
49B
5?B
49B
33B
5?B
6FB
7LB
6FB
8RB
9XB
8RB
8RB
7LB
7LB
:^B
9XB
8RB
8RB
:^B
9XB
:^B
;dB
;dB
;dB
<jB
;dB
:^B
:^B
:^B
;dB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
@�B
A�B
A�B
@�B
@�B
@�B
@�B
B�B
B�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
C�B
D�B
C�B
D�B
E�B
E�B
D�B
F�B
F�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
J�B
K�B
K�B
L�B
L�B
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
M�B
M�B
M�B
L�B
J�B
G�B
H�B
N�B
N�B
N�B
O�B
O�B
O�B
M�B
M�B
L�B
N�B
N�B
M�B
N�B
P�B
O�B
O�B
M�B
M�B
Q�B
R�B
S�B
S�B
R�B
Q�B
O�B
P�B
Q�B
R�B
T�B
T�B
T�B
S�B
VB
VB
T�B
S�B
R�B
T�B
T�B
VB
VB
T�B
W
B
XB
YB
YB
XB
W
B
VB
XB
XB
XB
ZB
ZB
[#B
[#B
ZB
[#B
\)B
]/B
\)B
]/B
\)B
\)B
]/B
]/B
\)B
[#B
[#B
ZB
\)B
]/B
\)B
]/B
]/B
^5B
_;B
_;B
_;B
^5B
]/B
^5B
^5B
\)B
[#B
_;B
`BB
_;B
`BB
aHB
aHB
aHB
bNB
bNB
aHB
aHB
bNB
aHB
dZB
dZB
e`B
dZB
dZB
dZB
dZB
dZB
e`B
dZB
e`B
dZB
dZB
e`B
dZB
e`B
e`B
e`B
ffB
ffB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
ffB
gmB
iyB
iyB
iyB
hsB
gmB
iyB
hsB
iyB
iyB
iyB
hsB
iyB
jB
l�B
l�B
k�B
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
n�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
t�B
t�B
s�B
s�B
s�B
t�B
s�B
s�B
r�B
r�B
s�B
t�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
w�B
w�B
v�B
v�B
w�B
x�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B3MB49B49B49B49B49B4TB1�B-wBB�BQ�B��B�yB�bBE9B�;B��B�B�B�BBpB:DBO�BX_Bo�Br�ByXBz^Bt�Bt�BlBkBfLB|PB}VBz^Bq�Bp�Bm�B`\Bd@B`�Bf�BpUBl�Br�B��B��B��B��B{JBi�Bf�BjeBVmB<B/�B,B�TB�DBVB�B��B��B�B�B��B�[B�bB�B�B}B^�BaHBW
BL�BU�BIB7�B�B!�B	�B
�)B
�B
��B
�mB
��B
��B
��B
��B
�jB
�	B
�XB
�B
��B
��B
�B
tB
k�B
iyB
dZB
T�B
O�B
<�B
,B
kB
!�B
%zB
#TB
�B
�B
�B	�rB	�?B	�1B	˒B	ּB	ؓB	ӏB	ȴB	��B	��B	�B	�B	�\B	��B	y�B	f�B	_!B	M�B	?�B	I�B	9	B	7�B	"�B	�B	�B��B�vBѝB�QB�fB�*B��B�\BȀB�DB�"BðB�FB��B�kB��B��B�$B��B�B�TB�oB��B��Bz�Br�B�B��B��B|�BwBpoBo5B~wB|�B}�B{�Bw2Bi�BlqBg�BbNBg�BmCBd�BO�BGBRBW�BN<B;�B9XB>�B9�B&fB7fB0!B/ B0�B=qB=�B=�BGEBP.BQ4BK�BN�BW�BX�B\�B^�BXBR BW�BTBP.BH�BFYBQ�BS�BY�BV9BeBpBr�BtBs3BraBo�BzxBt�BsB|�B{�BshB��B��B��B~BvzBwfB{�B}�Bw�B|jBy�Bu�BvFB{JB��B��B��B��B��B��B��B�B��B�B�AB�4B�5B�]B�OB��B�nB�zB�KB��B��B��B��B��B��B��B�*B�B�iB��B�)B�DB� B�4B�\B�hBуB�eBٚB��B�B�B�B��B��B��B��B��B�B�MB�"B	;B	aB	gB	KB		�B	
�B	�B	�B	�B	�B	B	 B	�B	�B	B	# B	'B	)*B	+6B	.IB	0UB	1vB	5�B	6zB	6�B	:�B	C�B	F�B	F�B	F�B	J�B	N�B	O(B	UB	XEB	YeB	\CB	[WB	\xB	\�B	`\B	dtB	f�B	h�B	h�B	j�B	i�B	k�B	k�B	k�B	m�B	n�B	o�B	r�B	u�B	u�B	tB	uB	v�B	y	B	��B	��B	�[B	cB	�aB	��B	�%B	��B	�EB	�RB	�RB	�XB	�JB	�~B	�xB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�DB	��B	�DB	�`B	�B	�=B	�kB	�]B	�]B	�%B	�ZB	�`B	�>B	�rB	��B	��B	��B	��B	��B	��B	ªB	ĶB	ɺB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�0B	�B	�B	�$B	�B	�$B	�B	�2B	�2B	�$B	�?B	�B	�EB	�mB	�SB	�KB	�KB	�KB	�=B	�CB	�CB	�CB	�/B	�]B	�OB	�VB	�VB	�OB	�5B	�5B	�dB	�]B	�dB	�xB	�jB	�\B	�\B	ބB	�~B	�~B	ߊB	�|B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B
  B
 B
B
B

#B
	7B
_B
)B
JB
^B
^B

rB

rB

�B

rB
xB
�B
�B
�B
vB
vB
vB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
!�B
"�B
#�B
"�B
"�B
#�B
$B
$�B
%�B
%�B
$�B
"B
#�B
%B
%B
%B
'B
&B
%B
&B
'B
(
B
'B
&B
'�B
($B
%FB
*B
+B
*0B
'B
'B
*�B
,B
,"B
,"B
-)B
.B
./B
-CB
,"B
-)B
-)B
,=B
./B
0;B
1AB
1AB
1AB
1AB
1AB
1AB
0;B
0;B
1AB
0UB
0oB
2GB
2aB
3MB
4TB
4TB
4TB
3MB
2aB
2aB
3hB
5ZB
4TB
4TB
5ZB
4TB
4nB
4nB
5ZB
4TB
3hB
5tB
6`B
7fB
6`B
8lB
9XB
8lB
8lB
7�B
7�B
:^B
9rB
8�B
8�B
:xB
9�B
:xB
;�B
;B
;�B
<�B
;�B
:xB
:�B
:�B
;�B
<�B
=�B
>wB
>�B
>�B
>wB
>�B
>�B
>�B
=�B
=�B
@�B
A�B
A�B
@�B
@�B
@�B
@�B
B�B
B�B
A�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
C�B
D�B
C�B
D�B
E�B
E�B
D�B
F�B
F�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
J�B
K�B
K�B
L�B
L�B
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
M�B
M�B
M�B
L�B
J�B
H1B
IB
N�B
N�B
N�B
O�B
O�B
O�B
M�B
NB
MB
N�B
OB
NB
OB
QB
O�B
O�B
NB
N"B
RB
R�B
S�B
S�B
R�B
Q�B
PB
QB
R B
SB
T�B
T�B
UB
TB
VB
VB
UB
TB
S&B
UB
UB
VB
VB
U2B
W$B
X+B
YB
Y1B
XEB
W$B
V9B
X+B
X+B
X+B
Z7B
Z7B
[#B
[=B
ZQB
[=B
\)B
]/B
\CB
]/B
\CB
\CB
]IB
]/B
\CB
[=B
[#B
ZQB
\)B
]IB
\]B
]IB
]IB
^OB
_;B
_VB
_;B
^OB
]/B
^5B
^OB
\CB
[qB
_VB
`BB
_VB
`\B
abB
abB
abB
bhB
bhB
abB
a|B
bhB
a|B
dtB
dZB
e`B
dtB
dtB
dtB
dtB
d�B
e`B
dtB
e`B
dtB
d�B
ezB
dtB
ezB
ezB
e�B
f�B
f�B
hXB
hsB
hsB
hsB
hXB
hsB
hsB
g�B
f�B
g�B
iyB
iyB
i�B
h�B
g�B
iyB
hsB
i�B
i�B
i�B
h�B
i�B
j�B
lqB
l�B
k�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
n�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
r�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
t�B
t�B
s�B
s�B
s�B
t�B
s�B
s�B
r�B
r�B
s�B
t�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
w�B
w�B
v�B
v�B
w�B
x�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
{�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<^҉<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807260036002018072600360020180726003600201807260200242018072602002420180726020024201807270027062018072700270620180727002706  JA  ARFMdecpA19c                                                                20180724093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180724004231  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180724004234  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180724004234  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180724004235  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180724004235  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180724004235  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180724004235  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180724004235                      G�O�G�O�G�O�                JA  ARUP                                                                        20180724005607                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180722153600  CV  JULD            G�O�G�O�FÝ�                JM  ARCAJMQC2.0                                                                 20180725153600  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180725153600  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180725170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180726152706  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                