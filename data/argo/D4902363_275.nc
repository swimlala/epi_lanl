CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-30T00:35:12Z creation;2018-08-30T00:35:17Z conversion to V3.1;2019-12-19T07:33:57Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180830003512  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_275                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�}s�c� 1   @�}t�W @9��-��d^��E�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @3�
@�Q�@�Q�A (�A (�A@(�A`(�A��HA�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=Bp�B 
=B(
=B/��B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bw��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՃ�D��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD��D�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AӺ^AӼjAӼjAӾwAӾwAӾwAӼjAӼjA���AӾwA���AӼjAӸRAӼjAӼjA�A�ƨAӶFAӇ+A�ffA�%A���A�ȴA�n�Aʙ�A��A�O�A�l�A�
=A�ȴA��A���A� �A��A���A�\)A��7A��A��TA�9XA���A���A�~�A��mA��A�;dA���A�;dA��A���A��A��A�ĜA�(�A�C�A�1'A�ȴA��A���A�1'A��A���A�9XA��/A��DA�E�A��A��wA�A�A�/A���A���A�VA�-A�ZA�ȴA�K�A�%A�-A�v�A�dZA���A�oA���A���A�E�A��TA��wA���A��A�XA��hA�-A�ƨA��;A��A��A�x�A���A��HA�bNA
=A|�!A{��Az �Ax�AxjAw��AwK�Avr�As�FArM�Aq��Aq��Aq��An�uAl=qAjffAit�Ah��AghsAe�Adn�AbȴAaG�A^r�A\�/A[��AZ$�AX��AW��AW"�AVjAU�AU|�AT�AS\)AQ�wAQK�AP��AP��APbAOC�AN��ALv�AK%AJ5?AI�AI/AH��AH��AH=qAG�hAE��AC�AB�/ABffAB$�AA|�A@�DA?��A>�A>��A=A;�;A:ȴA:bA8��A8�!A8��A8r�A8^5A7�A6��A5�A57LA4�9A49XA3�FA3|�A3G�A2��A2 �A1��A0�A0A�A09XA/&�A-�A,�A,��A+&�A)��A(�HA'7LA&  A#�7A"n�A"9XA!��A ��A�A�A+A�/A��A�A�#A�#A��A��A�^AȴA{A�A��A1'A�hA+A��A�An�A9XA�FA�AA�+A=qA-AVA�AO�Av�A`BA��A�A�9AZA1'A�A�AȴA{A�A �9@�\)@�S�@��-@��7@�`B@��@�K�@��@�I�@�@�;d@�V@���@��@�bN@��@��T@�u@�33@���@�z�@�o@��@�@�`B@�@߶F@�M�@�?}@�Z@���@���@�J@�&�@���@�v�@�p�@��/@ӥ�@�ȴ@�M�@љ�@��@�
=@�?}@�r�@�1@�C�@�@��@ʰ!@ʏ\@�n�@�E�@ǝ�@�^5@ŉ7@�z�@�\)@�5?@��7@���@��@�7L@��y@�{@�O�@��u@�I�@��@�C�@�ȴ@�V@��@�I�@��y@�~�@�{@�x�@��;@�
=@�n�@�5?@�J@��T@��7@�hs@�C�@���@�V@�r�@�
=@��@�hs@���@���@�ff@�M�@��@�`B@��9@�Z@�|�@�33@��@��!@�$�@�p�@�?}@���@�A�@���@��R@�v�@�V@�=q@�@��h@��/@���@�  @�C�@�
=@��@���@�M�@�{@�@���@��/@�I�@�  @���@�o@�ȴ@��+@�V@�$�@�x�@�V@���@�l�@��@�V@��D@�A�@��@�ƨ@��@��P@��@�t�@�\)@�;d@�+@�
=@�ȴ@�-@��@���@�`B@��@��9@��@�Q�@��@��;@��F@���@�C�@�@���@��R@��!@���@�n�@�J@�x�@�V@��/@��@��@��@���@�S�@���@�^5@�-@���@���@�/@���@�9X@��@�@~ff@~@}@}��@}�h@}�h@}`B@|�@|��@|z�@|�@{�
@{o@z�\@z=q@y�@y��@y��@y��@y�^@y��@y��@yx�@yhs@y7L@y%@x�9@xr�@xb@w��@w+@vv�@vE�@v5?@u�@u/@t��@t�D@s�m@s��@s��@s�F@s��@s33@r�@rn�@rM�@q�#@qG�@p�`@p��@pbN@pA�@pb@o��@o|�@o+@o�@n��@n�y@nȴ@n��@nff@nE�@m@m�@m?}@l�@l(�@k�@j�@j��@j^5@jM�@jM�@jM�@jM�@j-@i��@i��@i�#@i7L@h��@h�u@h�@hbN@hA�@h �@g�@g�P@g|�@gl�@g\)@f��@e��@e�@d�@c��@cƨ@c��@b��@a��@a%@`��@` �@`  @`  @_�@_l�@^�y@^��@^V@^@]@]�h@]O�@]?}@]V@\�@\��@\��@\9X@\�@[��@[�m@[�
@[��@[o@Z��@Z��@Z��@Z�\@Z~�@Zn�@Z^5@Z=q@ZJ@Y�@Y�#@Y��@Y�7@X��@X�9@Xr�@XA�@W�w@W|�@V�+@U�-@U`B@UO�@U?}@U/@T��@T��@T�@T��@T�D@Tj@TI�@Sƨ@R��@RM�@RJ@Q��@QX@P�9@Pr�@PQ�@PQ�@P1'@Pb@O�;@O�w@Ol�@Nȴ@NE�@M�T@M��@MO�@L��@L�@L�D@Lz�@Lj@LI�@L(�@Kƨ@Kt�@KC�@Ko@J�@J�H@J�H@J��@J^5@J-@I�#@IG�@HĜ@H �@G�;@G�P@G+@F�y@F�y@F�R@Fff@E�@E��@E�@D��@D�@D�@D�D@D(�@C�m@Cƨ@C��@CdZ@C@B��@BM�@B-@A��@A�#@A�^@A�7@AG�@@Ĝ@@��@@A�@@  @?�;@?�w@?�@?\)@>��@>v�@>5?@>@=�T@=��@=�-@=p�@=/@=�@<��@<(�@;�F@;��@;S�@:�H@:��@:��@:^5@9�@9��@9�7@9hs@97L@9%@8�9@8�u@8bN@8Q�@8 �@8  @7�w@7l�@7�@6�@6ȴ@6��@65?@6@5@5�@5?}@4��@4�@4j@4I�@49X@49X@3��@3�F@3�@333@2��@2�\@2=q@1��@1�^@1x�@0�9@0Q�@/�w@/�P@/\)@/+@/
=@.ff@-�@-��@-�@-p�@-/@,��@,�@,�@,z�@+��@+��@+dZ@+"�@*��@*-@*-@*�@)��@)��@)��@)�7@)X@)7L@(�`@(bN@(A�@(  @'l�@&�@&��@&�+@&E�@%@%�@%`B@%/@$��@$�@$(�@#��@#t�@#33@"��@"~�@"=q@"J@!�@!�7@!X@!G�@!7L@!&�@!�@!%@ �`@ ��@ Ĝ@ ��@ �u@ Q�@ b@�@��@\)@\)@\)@K�@+@�@�R@ff@5?@$�@�@�-@/@��@z�@9X@�m@��@S�@�H@��@��@n�@�@�@�7@G�@�@Ĝ@r�@bN@bN@1'@��@��@l�@;d@ȴ@�R@��@v�@E�@{@�@��@�-@�@O�@��@��@�@�m@ƨ@t�@dZ@S�@o@@@@�@�H@��@�!@�!@��@n�@-@J@��@�@��@x�@�@�9@bN@ �@�;@�w@�w@�P@l�@\)@K�@K�@+@�@��@��@E�@@�-@�h@�h@�h@�h@O�@V@�/@�D@Z@1@��@�m@ƨ@�F@�F@�F@��@S�@"�@
��@
��@
��@
�\@
�\@
n�@
�@	�@	��@	�^@	��@	hs@	&�@��@�9@bN@�@�w@��@|�@l�@\)@\)@+@
=@�@��@�+@�+@v�@v�@V@�T@��@�h@p�@?}@��@Z@9X@�@�@�@1@�@�@�@�
@��@t�@C�@"�@@��@��@��@�\@~�@~�@~�@~�@^5@=q@-@J@J@��@�@�#@��@�^@��@7L@7L@%@ �9@ �@ r�@ r�@ r�@ A�@  �@ b@   ?���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AӺ^AӼjAӼjAӾwAӾwAӾwAӼjAӼjA���AӾwA���AӼjAӸRAӼjAӼjA�A�ƨAӶFAӇ+A�ffA�%A���A�ȴA�n�Aʙ�A��A�O�A�l�A�
=A�ȴA��A���A� �A��A���A�\)A��7A��A��TA�9XA���A���A�~�A��mA��A�;dA���A�;dA��A���A��A��A�ĜA�(�A�C�A�1'A�ȴA��A���A�1'A��A���A�9XA��/A��DA�E�A��A��wA�A�A�/A���A���A�VA�-A�ZA�ȴA�K�A�%A�-A�v�A�dZA���A�oA���A���A�E�A��TA��wA���A��A�XA��hA�-A�ƨA��;A��A��A�x�A���A��HA�bNA
=A|�!A{��Az �Ax�AxjAw��AwK�Avr�As�FArM�Aq��Aq��Aq��An�uAl=qAjffAit�Ah��AghsAe�Adn�AbȴAaG�A^r�A\�/A[��AZ$�AX��AW��AW"�AVjAU�AU|�AT�AS\)AQ�wAQK�AP��AP��APbAOC�AN��ALv�AK%AJ5?AI�AI/AH��AH��AH=qAG�hAE��AC�AB�/ABffAB$�AA|�A@�DA?��A>�A>��A=A;�;A:ȴA:bA8��A8�!A8��A8r�A8^5A7�A6��A5�A57LA4�9A49XA3�FA3|�A3G�A2��A2 �A1��A0�A0A�A09XA/&�A-�A,�A,��A+&�A)��A(�HA'7LA&  A#�7A"n�A"9XA!��A ��A�A�A+A�/A��A�A�#A�#A��A��A�^AȴA{A�A��A1'A�hA+A��A�An�A9XA�FA�AA�+A=qA-AVA�AO�Av�A`BA��A�A�9AZA1'A�A�AȴA{A�A �9@�\)@�S�@��-@��7@�`B@��@�K�@��@�I�@�@�;d@�V@���@��@�bN@��@��T@�u@�33@���@�z�@�o@��@�@�`B@�@߶F@�M�@�?}@�Z@���@���@�J@�&�@���@�v�@�p�@��/@ӥ�@�ȴ@�M�@љ�@��@�
=@�?}@�r�@�1@�C�@�@��@ʰ!@ʏ\@�n�@�E�@ǝ�@�^5@ŉ7@�z�@�\)@�5?@��7@���@��@�7L@��y@�{@�O�@��u@�I�@��@�C�@�ȴ@�V@��@�I�@��y@�~�@�{@�x�@��;@�
=@�n�@�5?@�J@��T@��7@�hs@�C�@���@�V@�r�@�
=@��@�hs@���@���@�ff@�M�@��@�`B@��9@�Z@�|�@�33@��@��!@�$�@�p�@�?}@���@�A�@���@��R@�v�@�V@�=q@�@��h@��/@���@�  @�C�@�
=@��@���@�M�@�{@�@���@��/@�I�@�  @���@�o@�ȴ@��+@�V@�$�@�x�@�V@���@�l�@��@�V@��D@�A�@��@�ƨ@��@��P@��@�t�@�\)@�;d@�+@�
=@�ȴ@�-@��@���@�`B@��@��9@��@�Q�@��@��;@��F@���@�C�@�@���@��R@��!@���@�n�@�J@�x�@�V@��/@��@��@��@���@�S�@���@�^5@�-@���@���@�/@���@�9X@��@�@~ff@~@}@}��@}�h@}�h@}`B@|�@|��@|z�@|�@{�
@{o@z�\@z=q@y�@y��@y��@y��@y�^@y��@y��@yx�@yhs@y7L@y%@x�9@xr�@xb@w��@w+@vv�@vE�@v5?@u�@u/@t��@t�D@s�m@s��@s��@s�F@s��@s33@r�@rn�@rM�@q�#@qG�@p�`@p��@pbN@pA�@pb@o��@o|�@o+@o�@n��@n�y@nȴ@n��@nff@nE�@m@m�@m?}@l�@l(�@k�@j�@j��@j^5@jM�@jM�@jM�@jM�@j-@i��@i��@i�#@i7L@h��@h�u@h�@hbN@hA�@h �@g�@g�P@g|�@gl�@g\)@f��@e��@e�@d�@c��@cƨ@c��@b��@a��@a%@`��@` �@`  @`  @_�@_l�@^�y@^��@^V@^@]@]�h@]O�@]?}@]V@\�@\��@\��@\9X@\�@[��@[�m@[�
@[��@[o@Z��@Z��@Z��@Z�\@Z~�@Zn�@Z^5@Z=q@ZJ@Y�@Y�#@Y��@Y�7@X��@X�9@Xr�@XA�@W�w@W|�@V�+@U�-@U`B@UO�@U?}@U/@T��@T��@T�@T��@T�D@Tj@TI�@Sƨ@R��@RM�@RJ@Q��@QX@P�9@Pr�@PQ�@PQ�@P1'@Pb@O�;@O�w@Ol�@Nȴ@NE�@M�T@M��@MO�@L��@L�@L�D@Lz�@Lj@LI�@L(�@Kƨ@Kt�@KC�@Ko@J�@J�H@J�H@J��@J^5@J-@I�#@IG�@HĜ@H �@G�;@G�P@G+@F�y@F�y@F�R@Fff@E�@E��@E�@D��@D�@D�@D�D@D(�@C�m@Cƨ@C��@CdZ@C@B��@BM�@B-@A��@A�#@A�^@A�7@AG�@@Ĝ@@��@@A�@@  @?�;@?�w@?�@?\)@>��@>v�@>5?@>@=�T@=��@=�-@=p�@=/@=�@<��@<(�@;�F@;��@;S�@:�H@:��@:��@:^5@9�@9��@9�7@9hs@97L@9%@8�9@8�u@8bN@8Q�@8 �@8  @7�w@7l�@7�@6�@6ȴ@6��@65?@6@5@5�@5?}@4��@4�@4j@4I�@49X@49X@3��@3�F@3�@333@2��@2�\@2=q@1��@1�^@1x�@0�9@0Q�@/�w@/�P@/\)@/+@/
=@.ff@-�@-��@-�@-p�@-/@,��@,�@,�@,z�@+��@+��@+dZ@+"�@*��@*-@*-@*�@)��@)��@)��@)�7@)X@)7L@(�`@(bN@(A�@(  @'l�@&�@&��@&�+@&E�@%@%�@%`B@%/@$��@$�@$(�@#��@#t�@#33@"��@"~�@"=q@"J@!�@!�7@!X@!G�@!7L@!&�@!�@!%@ �`@ ��@ Ĝ@ ��@ �u@ Q�@ b@�@��@\)@\)@\)@K�@+@�@�R@ff@5?@$�@�@�-@/@��@z�@9X@�m@��@S�@�H@��@��@n�@�@�@�7@G�@�@Ĝ@r�@bN@bN@1'@��@��@l�@;d@ȴ@�R@��@v�@E�@{@�@��@�-@�@O�@��@��@�@�m@ƨ@t�@dZ@S�@o@@@@�@�H@��@�!@�!@��@n�@-@J@��@�@��@x�@�@�9@bN@ �@�;@�w@�w@�P@l�@\)@K�@K�@+@�@��@��@E�@@�-@�h@�h@�h@�h@O�@V@�/@�D@Z@1@��@�m@ƨ@�F@�F@�F@��@S�@"�@
��@
��@
��@
�\@
�\@
n�@
�@	�@	��@	�^@	��@	hs@	&�@��@�9@bN@�@�w@��@|�@l�@\)@\)@+@
=@�@��@�+@�+@v�@v�@V@�T@��@�h@p�@?}@��@Z@9X@�@�@�@1@�@�@�@�
@��@t�@C�@"�@@��@��@��@�\@~�@~�@~�@~�@^5@=q@-@J@J@��@�@�#@��@�^@��@7L@7L@%@ �9@ �@ r�@ r�@ r�@ A�@  �@ b@   ?���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B  BB  B  B  B  BBBB��B��B��B��B��B�B�sB(�Bn�Bt�B�B�7B��B��B�+BO�Bx�B�BcTB@�Bz�B��B� By�B�Bx�Bv�BbNBgmBy�Bw�BjBjBR�BD�B9XB,B0!BJB�B.B"�B"�B�B$�B(�B�B�B�BuBB�TB�mB��BȴB�3B��B�oB�{B�{B�VB�1BiyB]/B?}B?}BJBhB
=B
�B
�)B
��B
��B
�B
B
�?B
�'B
�B
�oB
�=B
�1B
y�B
s�B
`BB
]/B
H�B
7LB
@�B
9XB
:^B
I�B
E�B
;dB
.B
uB
�B
"�B
�B

=B	�5B	��B	��B	��B	��B	�9B	��B	��B	�7B	}�B	\)B	cTB	ffB	[#B	XB	ZB	XB	VB	YB	Q�B	J�B	7LB	1'B	F�B	G�B	B�B	33B	(�B	 �B	B	1B	\B	oB	�B	�B	hB	JB	  B�B�;B�B�B�B�B�ZB�BB�;B�)B��B�qB�dBÖB�wBɺB��BɺBŢB�^B�-B��B�!B�9B�-B�'B�?B�3B�B��B�B��B��B�B��B�JB��B��B�DBv�B�Bq�Bm�BbNBq�B�+B}�Bq�Bo�Bw�Bw�Bu�Br�Bk�BXB6FB2-BJ�BD�BC�BF�B?}BJ�BH�BA�BC�B0!BD�BI�BK�BA�B49B-B+B<jB8RB�BBuB'�B'�B9XB>wB=qB8RB8RB49B-B#�B#�B�B"�B�B+B�B1'B.B&�B�BbB�BJBVB�B"�B�B!�B�B�B�B{B�B{B�B�B#�B"�B�B�B�B�B�B�B�B�B�BuB{B�B�B�B�B �B�B�BVBbB!�B$�B$�B)�B,B)�B(�B#�B�B
=B�B �B�B�B�B#�B�B�BhBoB)�B+B.B33B2-B0!B2-B33B.B.B0!B=qB=qB9XB5?B?}BF�BK�BL�BK�BG�BD�B6FBC�BH�B=qBD�BXBN�BP�B\)BiyBk�BiyBdZBhsBl�Bl�Bw�Bx�Bw�Bu�Bv�B~�B|�B|�B� B� B�VB�hB�oB�hB�\B�\B��B�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�3B�!B�?B�?B�'B�FB��B��B�B�)B�;B�NB�TB�`B�`B�`B�fB�mB�fB�fB�ZB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	+B	1B	%B	B	B	%B	VB	uB	�B	�B	�B	�B	�B	�B	&�B	,B	)�B	-B	+B	.B	,B	2-B	7LB	9XB	?}B	A�B	D�B	E�B	E�B	E�B	E�B	I�B	L�B	L�B	M�B	M�B	Q�B	W
B	ZB	]/B	^5B	^5B	^5B	^5B	_;B	_;B	_;B	_;B	`BB	`BB	bNB	cTB	e`B	e`B	gmB	m�B	o�B	n�B	m�B	r�B	v�B	v�B	y�B	|�B	|�B	{�B	z�B	|�B	|�B	�B	�B	�B	�7B	�PB	�VB	�\B	�\B	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�!B	�B	�-B	�RB	�RB	�RB	�XB	�XB	�RB	�dB	�jB	�dB	�^B	�RB	�^B	��B	��B	ĜB	ɺB	ɺB	ƨB	ɺB	��B	��B	��B	�B	�B	�B	�B	�
B	�#B	�B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�BB	�NB	�TB	�TB	�TB	�NB	�HB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�fB	�`B	�sB	�yB	�yB	�sB	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
+B
%B
%B
+B
%B
B
+B
+B

=B
DB
DB
PB
\B
VB
JB
PB
VB
VB
hB
oB
hB
\B
bB
oB
uB
oB
uB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
%�B
%�B
&�B
'�B
(�B
(�B
&�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
-B
-B
,B
+B
,B
,B
,B
-B
-B
.B
.B
.B
,B
.B
/B
2-B
2-B
2-B
2-B
0!B
2-B
49B
6FB
6FB
5?B
6FB
6FB
5?B
5?B
49B
6FB
7LB
7LB
6FB
8RB
;dB
;dB
:^B
:^B
:^B
;dB
;dB
:^B
:^B
9XB
<jB
;dB
:^B
;dB
>wB
>wB
>wB
=qB
?}B
@�B
@�B
?}B
?}B
>wB
?}B
B�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
H�B
G�B
H�B
J�B
J�B
J�B
I�B
H�B
I�B
H�B
J�B
J�B
I�B
I�B
H�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
P�B
P�B
O�B
O�B
P�B
O�B
Q�B
Q�B
Q�B
Q�B
T�B
S�B
R�B
Q�B
S�B
S�B
S�B
R�B
W
B
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
VB
T�B
W
B
W
B
XB
YB
YB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
[#B
\)B
^5B
^5B
_;B
`BB
_;B
`BB
`BB
`BB
`BB
`BB
_;B
`BB
`BB
_;B
`BB
aHB
bNB
cTB
cTB
bNB
aHB
aHB
bNB
bNB
bNB
bNB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
ffB
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
m�B
m�B
m�B
m�B
k�B
jB
l�B
m�B
m�B
l�B
k�B
l�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
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
r�B
p�B
s�B
r�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B  BB  B  B  B  BBBB��B�"B�jB��B�qB�oB��B-�Bq�BxRB�B��B��B��B�rBXEB|�B��Bi�BI7B~wB��B��B}�B�B{�By�Be�Bi�Bz�Bx�Bk�BkkBU2BF�B;�B.�B1�BHB�B.�B$ZB#�B �B%zB)_BB�BeB,B3B��B��B�[B�=B��B��B��B�9B��B�vB�7Bk�B^�BBABAUB�B�B�B
�B
�;B
҉B
յB
�eB
�B
��B
�GB
�/B
��B
�~B
�	B
{�B
uZB
bhB
^�B
J�B
:B
A�B
;dB
;�B
J#B
FYB
<�B
/�B
�B
B
# B
/B
DB	��B	͹B	��B	�B	͹B	�FB	�B	�hB	�xB	� B	_�B	eFB	h
B	]B	Y�B	[=B	YB	V�B	Y�B	R�B	K�B	9XB	3B	GB	G�B	CB	4TB	*0B	!�B	�B		�B	HB	@B	�B	�B	B	B	UB��B�HB�B�MB�9B�B�B�bB�'B��B�HB��B��B�gB��B�	B�B�	B�B�dB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B�BB�pB��B��B�6By	B�?Bs�Bo�BeFBr�B��B~�BsBp�BxRBxRBvFBsBlBYB9$B5BK�BF%BD�BG�B@�BK^BIlBB�BDgB2GBEmBJrBL0BBuB5�B.�B,�B<�B8�B �B�BgB(�B)_B9�B>�B=�B8�B8�B4�B-�B%B$�B�B#�BB	�B~B1[B.cB'mB�B�B�B"B�B5B#nB \B"NB�BsBmB�B_BgBYBVB$&B#:BOBQBsBYBOB;BWBeBeBaBgBKB5B�BCB!-BIB1B�B�B"4B%FB%zB*KB,=B*0B)*B$&BCBBmB!bB�B�B �B$tB�B�B&BB*B+�B.�B3�B2�B0�B2�B3�B.�B/ B1B=�B=�B:B6`B@BGBK�BMBLBHBEB7�BDBI7B>�BE�BX_BO�BRB\�Bi�Bk�Bi�BeBh�Bl�Bm)BxBy$BxBvFBwLB.B}<B}qB��B��B�pB��B��B��B��B��B��B�B�
B��B��B��B��B�B�B�2B�NB�LB�QB�WB�WB�UB�aB�MB��B��B��B��B�-B�2B�;B�.B�EB�]B�pB�hB�nB�zB�zB�zB�B�B�B�B��B��B��B��B� B��B��B�	B�B�B�(B�.B�HB	AB	SB	EB	fB	YB	SB	mB	�B	�B	�B	�B	�B	�B	�B	B	;B	'8B	,WB	*KB	-CB	+kB	.cB	,qB	2|B	7fB	9�B	?�B	A�B	D�B	E�B	E�B	E�B	E�B	I�B	L�B	MB	NB	N"B	R B	W
B	ZB	]IB	^5B	^5B	^OB	^OB	_;B	_VB	_VB	_pB	`\B	`vB	bhB	c�B	e�B	e�B	g�B	m�B	o�B	n�B	m�B	r�B	v�B	v�B	y�B	|�B	|�B	|B	{B	}B	}B	�'B	�AB	�SB	�lB	�jB	�VB	�vB	�vB	�}B	��B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	�B	�B	�B	�B	�5B	�'B	�;B	�cB	�aB	�RB	�lB	�RB	�rB	�rB	��B	�B	�jB	�B	�xB	��B	��B	��B	��B	��B	��B	ɺB	�B	�	B	�(B	�B	�@B	�EB	�B	�+B	�9B	�$B	�=B	�QB	�CB	�IB	�OB	�;B	�\B	�\B	�bB	�bB	�bB	�vB	�hB	�nB	�nB	�nB	�hB	�bB	�tB	�`B	�B	�fB	�B	�B	�B	�B	�B	�B	�mB	�B	�fB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	�B	�0B	�B	�.B
  B
 B
;B
3B
9B
9B
9B
9B
3B
GB
B
?B
?B
EB
1B
EB
?B
?B
EB
?B
mB
_B
_B

XB
^B
xB
jB
\B
pB
~B
�B
pB
pB
hB
oB
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
'B
'B
&�B
&�B
%�B
%�B
'B
(
B
)B
)B
'B
)B
)B
(�B
)B
*B
*B
+B
,"B
-B
-B
,"B
+B
,B
,B
,=B
-)B
-B
./B
./B
.IB
,WB
.cB
/OB
2GB
2GB
2GB
2GB
0UB
2aB
49B
6+B
6`B
5ZB
6`B
6`B
5tB
5ZB
4nB
6zB
7fB
7fB
6zB
8lB
;JB
;dB
:xB
:xB
:xB
;B
;B
:�B
:xB
9�B
<jB
;B
:xB
;�B
>�B
>�B
>wB
=�B
?}B
@�B
@�B
?�B
?�B
>�B
?�B
B�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
H�B
G�B
H�B
J�B
J�B
J�B
I�B
H�B
I�B
H�B
J�B
J�B
I�B
I�B
H�B
J�B
K�B
L�B
L�B
L�B
M�B
NB
P�B
Q B
O�B
O�B
Q B
PB
RB
RB
RB
RB
T�B
S�B
SB
R B
TB
TB
TB
S&B
W
B
VB
VB
V9B
VB
W$B
W?B
W$B
W$B
VB
U2B
W$B
W?B
X+B
YB
Y1B
[#B
[=B
[=B
\)B
\)B
\)B
\B
\)B
[WB
\)B
\CB
\)B
[=B
[=B
\)B
\)B
\CB
\CB
[=B
[WB
[WB
\CB
^OB
^5B
_VB
`BB
_VB
`\B
`BB
`BB
`BB
`\B
_VB
`\B
`BB
_pB
`\B
abB
bhB
cTB
cTB
bNB
abB
abB
bhB
bhB
bhB
bhB
dZB
dZB
dZB
e`B
e`B
e`B
dtB
d�B
dtB
dtB
ffB
f�B
ffB
ffB
f�B
ezB
f�B
gmB
g�B
g�B
f�B
ffB
f�B
f�B
g�B
g�B
i�B
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
m�B
mwB
m�B
m�B
k�B
j�B
l�B
m�B
m�B
l�B
k�B
l�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
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
r�B
p�B
s�B
r�B
q�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809030035482018090300354820180903003548201809030200172018090302001720180903020017201809040022522018090400225220180904002252  JA  ARFMdecpA19c                                                                20180830093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180830003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180830003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180830003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180830003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180830003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180830003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180830003517  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180830003517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180830003517  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180830003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180830003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180830005726                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180830153859  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180831000000  CF  PSAL_ADJUSTED_QCC`  Cf  G�O�                JM  ARSQJMQC2.0                                                                 20180831000000  CF  TEMP_ADJUSTED_QCCd  Cf  G�O�                JM  ARCAJMQC2.0                                                                 20180902153548  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180902153548  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180902170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180903152252  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                