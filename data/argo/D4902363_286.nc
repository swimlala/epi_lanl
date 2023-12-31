CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-01T21:37:17Z creation;2018-10-01T21:38:33Z conversion to V3.1;2019-12-19T07:31:24Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181001213717  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_286                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؅�-�� 1   @؅�`� @9���%���d=;dZ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@�Q�A (�A (�A@(�A^�\A�{A�{A�{A�{A��HA�{A�{A�{B 
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
=B`p�Bh
=Bp
=Bx
=B�B�B�B�B�8RB�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C)C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI�
DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di�
Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՃ�D��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�C�DڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��yA��mA��mA��yA��yA��yA��yA��A��A��A��A��A��A��A��A���A���A�1A�1A�1A�JA�VA�VA�1A�A�A��Aҝ�A�=qAΉ7A͟�A��A�bA�dZA�oA�
=A�9XA�ƨA�bA�~�A��uA��!A��A��FA�VA��RA�S�A���A�ȴA���A��A��A��
A�p�A���A�~�A���A�\)A��!A���A���A��PA�  A��PA��A�\)A���A�l�A��A�^5A��yA�~�A��+A�33A��\A�p�A�-A��A�l�A�{A��FA��A�dZA��A�?}A�
=A�7LA���A��jA�M�A��`A��!A�(�A�t�A�"�A�/A�XA�p�A���A�~�A��A}�-A|�jA{�PAz�yAy��Ayl�Ax=qAw�Av9XAt��Ast�Ar��Arz�Ar�Ap��AohsAm��AlbNAl$�Ak��Ak�
Aj��Ai�TAh5?AgVAd��Ab��Aa�A`A�A_O�A^��A]�A\�DAZ�DAY�AYAY�^AY�FAY�FAY�-AY��AY��AX�+AV~�AU\)ATM�AR��AQ�wAP��AP9XAO?}AM�AM��AMl�AL��ALZAK;dAG;dAF$�AD��AB��A@=qA=t�A<�`A<ĜA<�+A;��A;ƨA;�PA;�A:JA9��A9C�A8�A8{A7p�A7
=A6��A6^5A6A�A6(�A5�mA3�TA1��A0�A//A.VA-��A+�;A+�A+7LA)��A)A'��A%�
A%+A$��A$�!A$ZA#ƨA#�A#&�A"-A!hsA JAx�A5?A�/At�A33A�AffA�A�7A�/A�AZA�
A��A1Ar�A��A�7A�+Ap�A�!A�A?}A�A�A\)A
=A
�RA
^5A	K�AbA`BAZAG�A1A�RA$�A+A ^5@�33@�@�V@�Z@�(�@�1@��@�=q@�G�@��@��!@�@�9X@�|�@�|�@�|�@�dZ@��y@�O�@�/@��@@�(�@�o@ꗍ@�^5@�{@陚@�J@�t�@�M�@�{@���@ᙚ@�X@��@��u@�A�@�9X@�(�@�^5@ۥ�@���@ڧ�@ڇ+@ڇ+@�^5@�=q@���@�X@��`@ؓu@�r�@��m@ָR@ԛ�@�dZ@�O�@�;d@�^5@�$�@�/@���@˕�@�J@�
=@�V@��@�O�@ċD@���@öF@�-@�%@���@��@�ȴ@�n�@��-@��@�`B@�%@�A�@�|�@�5?@��@���@��+@��@��^@��7@��@�@�V@��^@�7L@��
@��R@�/@�r�@�o@��-@��@��@�1@��
@��F@���@�t�@�;d@��R@���@�G�@�Q�@��P@��@���@�9X@�t�@�M�@���@���@�/@�A�@��w@��@���@���@��+@�{@�@�G�@���@���@��u@�9X@��@��F@���@��R@�n�@�5?@���@���@�p�@�G�@�V@��`@��u@���@��H@�M�@�@��^@�X@��@���@�9X@��F@�@�ff@��7@�%@�(�@���@�;d@�@��!@�-@��h@�/@�bN@� �@��;@�|�@�;d@��@���@��@���@�^5@��@�V@���@��9@��D@�9X@���@�;d@�"�@�@��H@��H@��\@�ff@�o@�dZ@�\)@�+@�"�@�+@�o@�o@�S�@��@���@�Z@���@�x�@�@�hs@��@���@��j@��j@��D@�9X@�b@l�@~5?@}O�@|z�@{��@{��@{o@z��@z~�@z^5@z�@y�^@y�@x��@x�@x1'@w�@wK�@v�y@v@u�@u/@tz�@s��@s��@s�F@s��@t1@tZ@tZ@tj@t1@s��@sdZ@sC�@sC�@sC�@sC�@s"�@s@r��@rn�@r-@r=q@r-@q�@qx�@p��@o�;@o�@o��@o;d@nff@n$�@m/@l�@k��@k��@kdZ@kC�@j��@j-@i�#@j-@j�@j�@j�@j�@k@j�H@jM�@j-@i��@h�u@hb@g�@g+@f��@f�+@f5?@f{@e�@e�T@e@e�@eO�@e�@eV@eV@d�@dI�@cdZ@c"�@c@bn�@aX@a�@`��@`��@`r�@_�;@_K�@_�@^��@^�@^��@]�@\�@\��@\j@\I�@[�m@[S�@Z��@Zn�@Z�@Y��@Y&�@X�9@Xr�@XQ�@XA�@X1'@X  @W�w@W|�@W\)@V�@V$�@U�@T�j@Tz�@TZ@TZ@TZ@S��@S�m@S�
@SS�@R-@Q�#@QX@QG�@QG�@Q&�@Q%@P��@PĜ@P�@P1'@O�@O�w@O��@PQ�@PbN@P1'@O�;@O�@O��@O�P@Nȴ@N$�@M@M�h@M�h@MV@Lj@K�
@KC�@K@J��@J~�@J=q@I��@I�^@IG�@I�@H�9@HA�@H  @G�;@G��@G�@G�P@G�P@G��@G�;@G|�@GK�@F��@Fff@F$�@F{@F@E�T@E�@E�@E�@E@E�@EO�@E?}@E/@E/@E/@E/@E�@D�@D��@D�@C�m@C33@B��@B~�@B-@A�^@A7L@@��@@��@@bN@@A�@@  @?�;@?��@?�w@?l�@>�@>$�@=@=�@=?}@=�@=V@<�@<��@<j@;t�@:�!@:�!@:�\@:M�@9��@9x�@97L@8��@8��@8��@8�u@8�@8r�@8Q�@8Q�@8A�@8 �@8b@7�;@7��@7\)@7K�@7+@7+@7
=@7
=@6��@6��@6�y@6�+@5��@5@5�-@5�h@5`B@4��@3�
@3��@3t�@3C�@2�H@2��@2=q@2=q@2=q@2�@1��@1�@1�7@1hs@1X@1�@0r�@01'@0  @/�@/�@.�+@.5?@-�@-�T@-@-�-@-��@-�h@-�@-`B@-O�@-/@,��@,��@,��@,��@,I�@,�@,1@+�
@+��@+S�@+@*�\@*~�@*n�@*n�@*n�@*^5@*=q@)�@)��@)x�@)G�@(�9@(Q�@( �@'�;@'�w@'�P@'+@'
=@&�y@&�@&�@&�R@&��@&�+@&5?@%/@$9X@$�@#�m@#��@#t�@#C�@#o@#@"�H@"��@"��@"��@"�!@"��@"�!@"��@"~�@!�#@!��@!X@ ��@ �9@ ��@ �u@ Q�@  �@|�@
=@��@�y@�y@�@�R@�+@V@$�@�@�T@��@�-@��@p�@�@��@�D@�D@�D@z�@Z@�@�@t�@C�@�@�\@-@�^@��@��@x�@&�@%@��@��@�`@�@bN@A�@�w@\)@�R@E�@�@@p�@?}@�@V@V@��@�/@�@z�@Z@�@(�@ƨ@��@��@��@t�@C�@��@M�@��@hs@7L@�@��@Q�@  @�w@�w@�w@�w@�w@�w@�w@�P@\)@;d@+@
=@��@ȴ@��@�+@V@E�@$�@{@@@�@��@�-@��@�@�@p�@`B@O�@O�@/@V@��@�@�D@I�@��@C�@@
��@
��@
��@
�\@
�\@
n�@	�@	hs@	X@	%@��@bN@bN@bN@r�@�u@r�@Q�@bN@ �@�w@�@��@+@ȴ@�R@�R@��@v�@5?@{@�T@�T@��@�-@�-@�-@��@/@��@�@j@I�@�@1@�m@�
@ƨ@ƨ@ƨ@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��mA��yA��mA��mA��yA��yA��yA��yA��A��A��A��A��A��A��A��A���A���A�1A�1A�1A�JA�VA�VA�1A�A�A��Aҝ�A�=qAΉ7A͟�A��A�bA�dZA�oA�
=A�9XA�ƨA�bA�~�A��uA��!A��A��FA�VA��RA�S�A���A�ȴA���A��A��A��
A�p�A���A�~�A���A�\)A��!A���A���A��PA�  A��PA��A�\)A���A�l�A��A�^5A��yA�~�A��+A�33A��\A�p�A�-A��A�l�A�{A��FA��A�dZA��A�?}A�
=A�7LA���A��jA�M�A��`A��!A�(�A�t�A�"�A�/A�XA�p�A���A�~�A��A}�-A|�jA{�PAz�yAy��Ayl�Ax=qAw�Av9XAt��Ast�Ar��Arz�Ar�Ap��AohsAm��AlbNAl$�Ak��Ak�
Aj��Ai�TAh5?AgVAd��Ab��Aa�A`A�A_O�A^��A]�A\�DAZ�DAY�AYAY�^AY�FAY�FAY�-AY��AY��AX�+AV~�AU\)ATM�AR��AQ�wAP��AP9XAO?}AM�AM��AMl�AL��ALZAK;dAG;dAF$�AD��AB��A@=qA=t�A<�`A<ĜA<�+A;��A;ƨA;�PA;�A:JA9��A9C�A8�A8{A7p�A7
=A6��A6^5A6A�A6(�A5�mA3�TA1��A0�A//A.VA-��A+�;A+�A+7LA)��A)A'��A%�
A%+A$��A$�!A$ZA#ƨA#�A#&�A"-A!hsA JAx�A5?A�/At�A33A�AffA�A�7A�/A�AZA�
A��A1Ar�A��A�7A�+Ap�A�!A�A?}A�A�A\)A
=A
�RA
^5A	K�AbA`BAZAG�A1A�RA$�A+A ^5@�33@�@�V@�Z@�(�@�1@��@�=q@�G�@��@��!@�@�9X@�|�@�|�@�|�@�dZ@��y@�O�@�/@��@@�(�@�o@ꗍ@�^5@�{@陚@�J@�t�@�M�@�{@���@ᙚ@�X@��@��u@�A�@�9X@�(�@�^5@ۥ�@���@ڧ�@ڇ+@ڇ+@�^5@�=q@���@�X@��`@ؓu@�r�@��m@ָR@ԛ�@�dZ@�O�@�;d@�^5@�$�@�/@���@˕�@�J@�
=@�V@��@�O�@ċD@���@öF@�-@�%@���@��@�ȴ@�n�@��-@��@�`B@�%@�A�@�|�@�5?@��@���@��+@��@��^@��7@��@�@�V@��^@�7L@��
@��R@�/@�r�@�o@��-@��@��@�1@��
@��F@���@�t�@�;d@��R@���@�G�@�Q�@��P@��@���@�9X@�t�@�M�@���@���@�/@�A�@��w@��@���@���@��+@�{@�@�G�@���@���@��u@�9X@��@��F@���@��R@�n�@�5?@���@���@�p�@�G�@�V@��`@��u@���@��H@�M�@�@��^@�X@��@���@�9X@��F@�@�ff@��7@�%@�(�@���@�;d@�@��!@�-@��h@�/@�bN@� �@��;@�|�@�;d@��@���@��@���@�^5@��@�V@���@��9@��D@�9X@���@�;d@�"�@�@��H@��H@��\@�ff@�o@�dZ@�\)@�+@�"�@�+@�o@�o@�S�@��@���@�Z@���@�x�@�@�hs@��@���@��j@��j@��D@�9X@�b@l�@~5?@}O�@|z�@{��@{��@{o@z��@z~�@z^5@z�@y�^@y�@x��@x�@x1'@w�@wK�@v�y@v@u�@u/@tz�@s��@s��@s�F@s��@t1@tZ@tZ@tj@t1@s��@sdZ@sC�@sC�@sC�@sC�@s"�@s@r��@rn�@r-@r=q@r-@q�@qx�@p��@o�;@o�@o��@o;d@nff@n$�@m/@l�@k��@k��@kdZ@kC�@j��@j-@i�#@j-@j�@j�@j�@j�@k@j�H@jM�@j-@i��@h�u@hb@g�@g+@f��@f�+@f5?@f{@e�@e�T@e@e�@eO�@e�@eV@eV@d�@dI�@cdZ@c"�@c@bn�@aX@a�@`��@`��@`r�@_�;@_K�@_�@^��@^�@^��@]�@\�@\��@\j@\I�@[�m@[S�@Z��@Zn�@Z�@Y��@Y&�@X�9@Xr�@XQ�@XA�@X1'@X  @W�w@W|�@W\)@V�@V$�@U�@T�j@Tz�@TZ@TZ@TZ@S��@S�m@S�
@SS�@R-@Q�#@QX@QG�@QG�@Q&�@Q%@P��@PĜ@P�@P1'@O�@O�w@O��@PQ�@PbN@P1'@O�;@O�@O��@O�P@Nȴ@N$�@M@M�h@M�h@MV@Lj@K�
@KC�@K@J��@J~�@J=q@I��@I�^@IG�@I�@H�9@HA�@H  @G�;@G��@G�@G�P@G�P@G��@G�;@G|�@GK�@F��@Fff@F$�@F{@F@E�T@E�@E�@E�@E@E�@EO�@E?}@E/@E/@E/@E/@E�@D�@D��@D�@C�m@C33@B��@B~�@B-@A�^@A7L@@��@@��@@bN@@A�@@  @?�;@?��@?�w@?l�@>�@>$�@=@=�@=?}@=�@=V@<�@<��@<j@;t�@:�!@:�!@:�\@:M�@9��@9x�@97L@8��@8��@8��@8�u@8�@8r�@8Q�@8Q�@8A�@8 �@8b@7�;@7��@7\)@7K�@7+@7+@7
=@7
=@6��@6��@6�y@6�+@5��@5@5�-@5�h@5`B@4��@3�
@3��@3t�@3C�@2�H@2��@2=q@2=q@2=q@2�@1��@1�@1�7@1hs@1X@1�@0r�@01'@0  @/�@/�@.�+@.5?@-�@-�T@-@-�-@-��@-�h@-�@-`B@-O�@-/@,��@,��@,��@,��@,I�@,�@,1@+�
@+��@+S�@+@*�\@*~�@*n�@*n�@*n�@*^5@*=q@)�@)��@)x�@)G�@(�9@(Q�@( �@'�;@'�w@'�P@'+@'
=@&�y@&�@&�@&�R@&��@&�+@&5?@%/@$9X@$�@#�m@#��@#t�@#C�@#o@#@"�H@"��@"��@"��@"�!@"��@"�!@"��@"~�@!�#@!��@!X@ ��@ �9@ ��@ �u@ Q�@  �@|�@
=@��@�y@�y@�@�R@�+@V@$�@�@�T@��@�-@��@p�@�@��@�D@�D@�D@z�@Z@�@�@t�@C�@�@�\@-@�^@��@��@x�@&�@%@��@��@�`@�@bN@A�@�w@\)@�R@E�@�@@p�@?}@�@V@V@��@�/@�@z�@Z@�@(�@ƨ@��@��@��@t�@C�@��@M�@��@hs@7L@�@��@Q�@  @�w@�w@�w@�w@�w@�w@�w@�P@\)@;d@+@
=@��@ȴ@��@�+@V@E�@$�@{@@@�@��@�-@��@�@�@p�@`B@O�@O�@/@V@��@�@�D@I�@��@C�@@
��@
��@
��@
�\@
�\@
n�@	�@	hs@	X@	%@��@bN@bN@bN@r�@�u@r�@Q�@bN@ �@�w@�@��@+@ȴ@�R@�R@��@v�@5?@{@�T@�T@��@�-@�-@�-@��@/@��@�@j@I�@�@1@�m@�
@ƨ@ƨ@ƨ@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�RB�FB�FB�FB�FB�LB�LB�LB�LB�LB�LB�LB�LB�RB�RB�^B�jB�}BÖBŢB��B�
B�B�B�B�B�B�;B�5B��BO�BjBx�B�7B��B��B�%BM�B��B�JBq�BffBz�Bp�B�Bq�BffBv�Bt�Bk�BW
B+B�B&�B)�B#�B�B33B$�B%�B'�B'�B�B��B�`B�B�B�fB�;B��BƨB�3B��B��B�oBe`Br�B�Bz�Bu�Bt�Bk�BYBQ�BQ�BL�B<jB!�B�B+BDB
=B
��B
�B
�B
�;B
��B
ÖB
�9B
��B
x�B
n�B
p�B
iyB
e`B
[#B
T�B
K�B
=qB
=qB
/B
&�B
+B
-B
$�B
�B
%B
B	��B
JB
1B
B	�B	�NB	�B	ǮB	�wB	�B	�-B	�XB	�!B	�B	��B	��B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�%B	l�B	t�B	jB	bNB	aHB	\)B	`BB	XB	I�B	R�B	M�B	E�B	6FB	�B�B��B�B�NB��BĜB�B��B�B�B�B�B�mB�/B�fB�fB�`B�HB�HB�HB�NB�;B�;B�B��B�'B��B��B��B��B��B��B��B��B�=B��B�DB�B�{B��B��B��B�hB��B�PB� B~�Br�Bs�BbNBD�BYBo�Bn�BiyBhsBdZB_;BdZBdZB[#BP�BN�BB�BN�BP�BC�BA�BA�BD�B>wBF�B?}B<jBC�BA�B;dB.B%�B.B&�B �B�B�B)�B#�B"�B&�B&�B1'B0!B5?B33B-B%�B#�B$�B�B'�B!�B+B33B2-B/B)�B�B,B%�B�BoB!�B'�B'�B"�B�BBDB�B)�B)�B)�B)�B(�B&�B'�B(�B"�B�B	7B#�B+B,B,B)�B)�B&�B$�B%�B$�B$�B�BoBJBhBVBPB�B$�B�B�B �B{BJB%�B)�B%�B'�B#�B)�B �B �B.B(�B%�B1'B/B49B49B1'B+B+B%�B&�B)�B/B;dB:^B:^B0!B;dB>wB?}B?}B9XB:^B=qBG�BG�BJ�BXB[#B[#BbNBbNBcTBaHB_;B\)BYB]/B[#B[#BW
BhsBo�Bo�Bo�B{�B}�B|�Bz�B�B�%B�\B�bB�bB�VB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�RB�^B�dB�wB�wB��B��BBȴB��B��B��B�)B�HB�TB�ZB�`B�fB�B�B��B��B��B	  B	B	B	B	B	B	B	DB	uB	�B	�B	�B	�B	�B	$�B	%�B	'�B	)�B	+B	0!B	@�B	C�B	?}B	>wB	@�B	A�B	A�B	B�B	D�B	B�B	9XB	9XB	O�B	Q�B	P�B	M�B	M�B	P�B	VB	VB	S�B	Q�B	R�B	P�B	O�B	Q�B	R�B	VB	[#B	^5B	bNB	dZB	e`B	e`B	dZB	e`B	hsB	m�B	l�B	n�B	n�B	r�B	s�B	w�B	{�B	~�B	�B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�'B	�-B	�'B	�B	�!B	�B	�'B	�?B	�LB	�^B	�dB	�jB	�wB	B	ƨB	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	ɺB	��B	��B	��B	��B	�B	�B	�#B	�/B	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�BB	�`B	�fB	�ZB	�TB	�mB	�sB	�sB	�sB	�sB	�mB	�B	�B	�yB	�yB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B	��B	��B
B
B

=B
DB
DB
DB
DB
PB
JB
JB
VB
\B
bB
{B
uB
oB
oB
{B
{B
uB
bB
oB
{B
�B
�B
{B
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#�B
$�B
$�B
%�B
%�B
$�B
$�B
#�B
$�B
%�B
%�B
&�B
&�B
%�B
%�B
$�B
$�B
"�B
 �B
 �B
"�B
$�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
&�B
$�B
%�B
'�B
)�B
+B
,B
,B
+B
)�B
(�B
&�B
(�B
/B
.B
.B
-B
/B
0!B
0!B
2-B
33B
49B
33B
49B
49B
49B
49B
49B
49B
49B
49B
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
6FB
49B
9XB
8RB
8RB
7LB
5?B
49B
8RB
9XB
9XB
8RB
:^B
:^B
<jB
<jB
<jB
;dB
;dB
:^B
;dB
<jB
:^B
9XB
;dB
<jB
;dB
:^B
;dB
<jB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
?}B
?}B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
B�B
B�B
B�B
B�B
E�B
E�B
E�B
E�B
E�B
D�B
C�B
C�B
D�B
D�B
C�B
D�B
F�B
F�B
G�B
F�B
F�B
G�B
H�B
G�B
G�B
G�B
F�B
E�B
C�B
@�B
?}B
E�B
D�B
D�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
B�B
E�B
E�B
D�B
F�B
G�B
G�B
G�B
G�B
F�B
H�B
K�B
L�B
L�B
L�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
Q�B
Q�B
Q�B
P�B
O�B
M�B
Q�B
P�B
O�B
O�B
O�B
P�B
R�B
S�B
R�B
Q�B
S�B
S�B
S�B
S�B
Q�B
S�B
R�B
P�B
Q�B
Q�B
R�B
T�B
VB
T�B
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
YB
YB
[#B
ZB
YB
YB
W
B
YB
YB
YB
[#B
\)B
[#B
\)B
^5B
^5B
aHB
aHB
aHB
aHB
aHB
aHB
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
cTB
dZB
dZB
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
e`B
e`B
ffB
e`B
dZB
e`B
e`B
dZB
hsB
hsB
iyB
jB
jB
iyB
hsB
gmB
gmB
jB
hsB
iyB
jB
l�B
l�B
l�B
m�B
m�B
l�B
m�B
l�B
l�B
n�B
n�B
l�B
m�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
q�B
o�B
p�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�8B�FB�FB�FB�FB�LB�LB�LB�LB�LB�LB�LB�LB�RB�RB�^B�jB�cBÖBżB��B�
B�B�B�+B�KBںB�B��B��BQ�Bl=B{JB��B��B�vB��BX�B��B�TBxRBl�B�Bs�B��Bt�Bi*BwfBuZBlWBYB/�B7B)�B+�B&�B!�B4B&�B'�B)�B)*BeB�>B��B��B��B�mB�vB�aB�KB�+B�B��B��Bi�BuB��B|Bv�Bu�Bl�B[�BS�BR�BM�B>�B%,B;B
	B0B
�B OB
�TB
��B
�|B
�B
��B
��B
�B
~B
qAB
q�B
j�B
f2B
\CB
U�B
M6B
>�B
>�B
0�B
(sB
+�B
-wB
%�B
B
1B
�B	�jB
~B
�B
�B	�B	��B	�B	ɆB	�B	��B	�9B	�xB	�[B	�B	�8B	�IB	�pB	�CB	��B	��B	��B	��B	��B	��B	�B	��B	o5B	vB	lB	dB	b�B	]~B	aB	YeB	K)B	S@B	N<B	FtB	7�B	 �B�nB�dB��B�B�B��B��B�B�B�QB��B�B�>BޞB��B�B�B�NB�B��B�B߾B�pB�B��B��B��B�_B��B�$B�B��B�DB��B�0B��B�6B�9B�2B��B�#B��B�:B��B�<B��B�4BtnBt�Bd�BH�BZ�Bp!BoBjKBiBeB`'Bd�Bd�B\BR:BPHBD�BO�BQ�BEBB�BB�BE�B?�BGEB@�B=qBDBB'B<6B/�B'�B/OB(sB"NBVBOB*�B%FB$B'�B'�B1�B0�B5tB3�B-�B&�B$�B%�B �B(�B"�B+kB3MB2GB/iB*B �B,WB&�B
B�B"�B(XB(>B#TBkBEB�B5B*B*KB*KB*KB)DB'RB(>B)*B#:B�B
�B$@B+B,"B,"B*0B*0B'RB%FB&LB%,B%BIBuB�BTB�B�BVB%FB�B�B!HB�BB&2B*KB&�B(�B$�B*KB!�B!|B.IB)�B&�B1vB/�B4nB4nB1vB+�B+�B&�B'�B+B/�B;�B:�B:�B1AB;�B>�B@ B@B:DB;JB>�BHfBH�BK�BXyB[�B[�BbhBb�BcnBa�B_�B\�BY�B]�B[�B[�BX�Bh�BpBpUBpoB|B~BB}qB{�B��B��B�vB��B��B��B��B��B��B��B��B��B��B�B�7B��B�&B�B�2B�2B�*B�*B�>B�*B�RB��B��B��B��B��B��B��B��B��B�B�-B�7B�~B�oB՛BܒB�B�B�B�B��B��B�5B�B�B�6B	 4B	B	3B	3B	GB	�B	�B	�B	�B	�B	�B	�B	B	B	$�B	%�B	(
B	*B	+6B	0;B	@ B	CaB	?�B	>�B	@�B	A�B	A�B	B�B	D�B	B�B	:^B	:*B	O\B	Q�B	P�B	N"B	N<B	QB	VB	VB	T,B	R:B	S&B	QB	PbB	RTB	S&B	V9B	[=B	^OB	bhB	dtB	ezB	ezB	d�B	e�B	h�B	m�B	l�B	n�B	n�B	r�B	tB	xB	|B	HB	�aB	�JB	�bB	�oB	�gB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�*B	�B	�B	�0B	�=B	�IB	�]B	�'B	�GB	�[B	�cB	�;B	�}B	�[B	�ZB	��B	�xB	�dB	��B	��B	ªB	ƎB	̘B	��B	��B	��B	��B	��B	��B	��B	�B	�=B	�B	�B	��B	�B	�9B	�+B	�=B	�IB	�BB	�VB	�VB	�VB	�\B	�HB	�bB	�HB	��B	�vB	�zB	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�B	�B
 B
'B
'B
 B
'B
'B	�.B	�VB
3B
9B

#B
)B
^B
^B
^B
PB
dB
dB
pB
vB
bB
aB
uB
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#�B
$�B
$�B
%�B
%�B
$�B
$�B
#�B
$�B
%�B
%�B
&�B
&�B
%�B
%�B
$�B
$�B
"�B
!B
!B
#B
$�B
#�B
$B
%B
$�B
'B
'B
'�B
(
B
)B
)B
)B
'B
%,B
&B
'�B
*B
+B
,"B
,B
+B
*B
)*B
'8B
)*B
/B
./B
./B
-CB
/OB
0;B
0UB
2GB
33B
49B
3MB
49B
4TB
4B
49B
49B
4TB
4TB
4TB
6`B
7LB
8RB
8RB
8lB
8RB
8RB
8RB
8lB
6zB
4nB
9XB
8lB
8lB
7fB
5�B
4�B
8lB
9rB
9rB
8�B
:^B
:xB
<jB
<jB
<�B
;B
;B
:�B
;B
<�B
:�B
9�B
;B
<�B
;�B
:�B
;�B
<�B
>�B
?cB
?�B
?}B
?}B
?}B
@�B
@�B
@�B
?�B
?�B
A�B
B�B
B�B
A�B
B�B
C�B
C�B
B�B
B�B
B�B
B�B
E�B
E�B
E�B
E�B
E�B
D�B
C�B
C�B
D�B
D�B
C�B
D�B
F�B
F�B
G�B
F�B
F�B
G�B
H�B
G�B
G�B
G�B
F�B
E�B
C�B
@�B
?�B
E�B
D�B
D�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
B�B
E�B
E�B
D�B
F�B
G�B
G�B
G�B
G�B
F�B
H�B
K�B
L�B
L�B
L�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
NB
NB
N�B
O�B
Q�B
Q�B
Q�B
Q B
O�B
NB
RB
P�B
PB
PB
PB
Q B
R�B
S�B
SB
RB
S�B
S�B
S�B
TB
R B
S�B
SB
QB
R B
RB
S&B
UB
VB
UB
W$B
W$B
XB
XB
XB
X+B
X+B
X+B
X+B
X+B
X�B
Y1B
Y1B
[=B
ZB
YB
Y1B
W?B
Y1B
YKB
YKB
[=B
\)B
[WB
\CB
^OB
^OB
aHB
aHB
aHB
aHB
aHB
aHB
`\B
`\B
abB
aHB
aHB
abB
aHB
aHB
bhB
bhB
cTB
dtB
d@B
dZB
dZB
dZB
dtB
dtB
e`B
ezB
e`B
e`B
ffB
ffB
ffB
ezB
ezB
ffB
ezB
d�B
e�B
e�B
d�B
h�B
hsB
i�B
jB
jB
iyB
h�B
g�B
g�B
j�B
h�B
iyB
jB
l�B
l�B
l�B
m�B
m�B
l�B
m�B
l�B
l�B
n�B
n�B
l�B
m�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
q�B
o�B
p�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810060037182018100600371820181006003718201810060200172018100602001720181006020017201810070023592018100700235920181007002359  JA  ARFMdecpA19c                                                                20181002063632  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181001213717  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181001213723  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181001213734  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181001213740  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181001213740  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181001213742  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181001213742  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181001213831  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181001213833                      G�O�G�O�G�O�                JA  ARUP                                                                        20181001215856                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181002153617  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20181005153718  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181005153718  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181005170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181006152359  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                