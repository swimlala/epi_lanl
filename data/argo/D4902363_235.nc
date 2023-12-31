CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-02T00:35:26Z creation;2018-05-02T00:35:32Z conversion to V3.1;2019-12-19T07:43:23Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20180502003526  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_235                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�_u��j 1   @�_v����@:+��a@�da��!�.1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�G�A�{A�{B 
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl�
Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�F�D�f�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��HA���A���A�ƨA�ĜA�ĜA�ĜA�ĜA���A���A�A���A��wA��wA��jA��wA��^A��RA��FA��FA��FA��FA��FA��^A��^A��^A��RA��9A��-A��A���A�5?A��7A��A�XA�hsA�&�A��A���A�$�A�dZA�p�A�;dA�G�A�\)A���A�=qA��;A�ZA��HA�bNA�VA��A��wA�VA�K�A�ƨA�=qA�JA��TA���A��A���A��A��uA��hA��7A��;A�A�A���A���A�`BA��/A���A�n�A�O�A���A�/A��A�JA�\)A�p�A���A�t�A�A}�7A{t�Ayx�Ax{Aw+Avz�Au�;At�Asl�ArffAqAp�An�Al�jAk��Aj��Ai��Ai+Ah9XAg��AgoAfQ�Ae�#Ae�hAeAdJAb��Ab�Aa��Aa?}A`��A^Q�A\�A[l�AY��AXffAVZAUAT�AT�uASl�AQt�AP��AO�
ANĜAMALAKG�AJ�9AI�AI&�AH�yAHZAG�FAG|�AF�HAF �AE��AEdZADv�AC��AA��A@=qA>�`A=7LA;�A:��A:�+A9�FA9oA8��A8v�A8I�A7ƨA7�A6��A5�FA5�A5�A5|�A5x�A5x�A5x�A5O�A5�A4��A4=qA45?A3�TA3��A3��A3�A3p�A3K�A333A2�A1�A0M�A/t�A.A�A+�A*��A)�wA(��A'�-A'\)A'7LA&��A& �A%C�A$�HA$�DA$VA$ �A#�FA"�uA!�#A!+A��A��AZAA�A1A�yA��A7LA��AJA?}A��A��AXA5?A�A��A�uA\)A��A�\A�wAz�AE�A��A9XA7LA
-A	\)A	%AjA��A?}A�uA�Ax�A�+AdZA��A�wA"�A 9X@��m@���@��^@�7L@�Ĝ@�@�5?@��@�@�X@�S�@�V@�G�@��@��-@���@웦@�Q�@��@�dZ@���@�V@�  @�J@��/@� �@�t�@�+@�j@�ȴ@�~�@�M�@�$�@ݡ�@ܴ9@� �@�"�@�E�@ٲ-@��@؛�@�Q�@׾w@�V@�G�@�Ĝ@�  @�o@Ѓ@��@ͩ�@͙�@�x�@�&�@̬@��
@�~�@��T@�(�@�t�@��@���@��@ģ�@ă@�bN@�S�@�@��-@�G�@�Ĝ@�1@�t�@�E�@�Z@���@�S�@��H@��@�?}@�ƨ@�p�@�C�@���@��@�"�@�$�@���@�/@��@�Ĝ@�(�@��F@�"�@���@�n�@�5?@���@���@���@�v�@�E�@�J@�x�@�%@���@��@��w@���@���@�%@���@�1'@��F@�t�@�S�@�~�@�hs@�/@�|�@�v�@��@���@��^@�O�@��@�Ĝ@��j@�Q�@���@�l�@�;d@��R@��@�@��@�?}@��/@���@�A�@��@�b@��m@��@�ff@�@�7L@���@��u@�bN@�Z@�Q�@��@���@�S�@�C�@�"�@��\@�J@���@�(�@��@��@�v�@��@���@�O�@��@���@��j@���@�Q�@� �@��
@�t�@�+@�
=@���@��H@��!@���@�~�@�M�@�=q@�-@�X@���@���@��@�r�@�(�@���@��@���@��@���@�X@�&�@��@���@���@�9X@��@�@�P@K�@~ȴ@~@}p�@}�@|��@|��@|��@|I�@|1@{S�@z�H@zn�@y��@yx�@y&�@x�`@xĜ@x��@x1'@w�w@w��@w|�@wl�@w�@v�@vȴ@v��@vv�@v5?@u��@u/@t�/@tz�@sƨ@s�@st�@sdZ@sdZ@st�@s�@s�@s@r-@qx�@q�@p��@p1'@o�@n�y@n��@n�@n�y@m�@l��@l9X@l�@l1@l1@k��@k��@k"�@j��@j��@j�\@jM�@j-@j�@i��@ix�@h�`@h1'@g�w@g+@f�R@f�R@f�R@f�R@f�R@f�+@f$�@e�@e�T@e@e`B@eO�@dZ@d1@c��@c��@ct�@cdZ@cC�@c@c@b��@b^5@b=q@b�@b�@a��@a��@aG�@a�@`�`@`��@`Q�@_�@_�P@^��@^5?@]p�@]�@\�/@\��@\Z@\(�@\�@\1@[��@[o@Zn�@Y�@Y��@Y��@Y�7@YG�@X��@XQ�@W��@WK�@V��@Vȴ@V@Up�@T��@T9X@S�m@S�
@Sƨ@S��@S"�@R�!@Q��@Qhs@Q&�@PĜ@Pr�@PbN@PQ�@PA�@P �@O�@O|�@N��@M��@Mp�@M/@L��@L�@L�@L��@Lz�@K��@Kt�@J�@J��@J��@J��@J��@J��@J��@J�!@I��@I�7@I�7@Ix�@IG�@H��@HA�@H �@G�@G
=@Fȴ@F��@F��@F�+@Fv�@FV@F$�@F@E�@E�T@E��@E�-@E�@D�/@DZ@C�m@C��@CC�@B�@B�\@B-@A�#@AG�@@��@@bN@@1'@?��@?l�@?+@>�y@>��@>V@>$�@>@=��@=p�@=p�@=`B@=O�@=?}@=?}@<��@<I�@<1@;t�@;C�@;33@:�@:�!@:M�@:�@9�^@9G�@9�@8�9@8�@8A�@7�@7�@7;d@7
=@6��@6ȴ@6�+@6{@5@5�@5V@4��@4�D@4j@4Z@4�@3dZ@333@2�@2�H@2��@2=q@2J@1��@1��@1�@1�@1��@1X@17L@0�`@0r�@01'@0 �@0  @/��@/�P@/\)@/;d@/�@.�R@.�+@.v�@.v�@.ff@.@-�h@-`B@-/@,��@,�/@,��@,�@,j@,j@,I�@,1@+�
@+�@+@*��@*^5@*-@*J@)��@(��@(r�@(A�@'�@'�w@'�@'�w@'�w@'|�@&��@&��@&ȴ@%��@%O�@%/@$�j@$�@$z�@$Z@$9X@$�@#��@#�
@#�@"��@"�\@"=q@"J@!��@!�^@!x�@!%@ r�@ 1'@ 1'@ b@�;@�@��@\)@+@�y@��@5?@$�@{@��@@�h@O�@�@��@�j@9X@�F@��@��@�@��@��@�!@��@^5@-@��@��@G�@Ĝ@�@1'@1'@b@�@��@�w@��@�P@|�@\)@\)@;d@�@��@�R@�+@ff@V@V@E�@5?@{@@�@�-@p�@O�@/@�@��@�/@�@�D@I�@�@�
@ƨ@�@"�@o@o@o@@��@n�@�@��@�#@��@�^@G�@Ĝ@A�@  @�;@�@�P@\)@
=@�@ȴ@�R@�+@5?@�@��@@@�-@��@��@�h@`B@�@�@��@I�@1@1@1@1@�m@�@"�@@
�@
�!@
~�@
M�@
=q@
-@	��@	�#@	��@	��@	��@	x�@	x�@	x�@	hs@	G�@	�@��@��@Ĝ@�9@�9@��@�u@r�@bN@1'@�@��@��@�w@�P@|�@K�@+@�@�@;d@K�@K�@;d@�@�y@�@ȴ@��@v�@v�@ff@ff@E�@{@�@��@�-@�@O�@?}@/@/@/@�@�@��@�/@�D@9X@9X@9X@(�@(�@1@�m@�
@ƨ@��@�@C�@@��@�!@��@��@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��HA���A���A�ƨA�ĜA�ĜA�ĜA�ĜA���A���A�A���A��wA��wA��jA��wA��^A��RA��FA��FA��FA��FA��FA��^A��^A��^A��RA��9A��-A��A���A�5?A��7A��A�XA�hsA�&�A��A���A�$�A�dZA�p�A�;dA�G�A�\)A���A�=qA��;A�ZA��HA�bNA�VA��A��wA�VA�K�A�ƨA�=qA�JA��TA���A��A���A��A��uA��hA��7A��;A�A�A���A���A�`BA��/A���A�n�A�O�A���A�/A��A�JA�\)A�p�A���A�t�A�A}�7A{t�Ayx�Ax{Aw+Avz�Au�;At�Asl�ArffAqAp�An�Al�jAk��Aj��Ai��Ai+Ah9XAg��AgoAfQ�Ae�#Ae�hAeAdJAb��Ab�Aa��Aa?}A`��A^Q�A\�A[l�AY��AXffAVZAUAT�AT�uASl�AQt�AP��AO�
ANĜAMALAKG�AJ�9AI�AI&�AH�yAHZAG�FAG|�AF�HAF �AE��AEdZADv�AC��AA��A@=qA>�`A=7LA;�A:��A:�+A9�FA9oA8��A8v�A8I�A7ƨA7�A6��A5�FA5�A5�A5|�A5x�A5x�A5x�A5O�A5�A4��A4=qA45?A3�TA3��A3��A3�A3p�A3K�A333A2�A1�A0M�A/t�A.A�A+�A*��A)�wA(��A'�-A'\)A'7LA&��A& �A%C�A$�HA$�DA$VA$ �A#�FA"�uA!�#A!+A��A��AZAA�A1A�yA��A7LA��AJA?}A��A��AXA5?A�A��A�uA\)A��A�\A�wAz�AE�A��A9XA7LA
-A	\)A	%AjA��A?}A�uA�Ax�A�+AdZA��A�wA"�A 9X@��m@���@��^@�7L@�Ĝ@�@�5?@��@�@�X@�S�@�V@�G�@��@��-@���@웦@�Q�@��@�dZ@���@�V@�  @�J@��/@� �@�t�@�+@�j@�ȴ@�~�@�M�@�$�@ݡ�@ܴ9@� �@�"�@�E�@ٲ-@��@؛�@�Q�@׾w@�V@�G�@�Ĝ@�  @�o@Ѓ@��@ͩ�@͙�@�x�@�&�@̬@��
@�~�@��T@�(�@�t�@��@���@��@ģ�@ă@�bN@�S�@�@��-@�G�@�Ĝ@�1@�t�@�E�@�Z@���@�S�@��H@��@�?}@�ƨ@�p�@�C�@���@��@�"�@�$�@���@�/@��@�Ĝ@�(�@��F@�"�@���@�n�@�5?@���@���@���@�v�@�E�@�J@�x�@�%@���@��@��w@���@���@�%@���@�1'@��F@�t�@�S�@�~�@�hs@�/@�|�@�v�@��@���@��^@�O�@��@�Ĝ@��j@�Q�@���@�l�@�;d@��R@��@�@��@�?}@��/@���@�A�@��@�b@��m@��@�ff@�@�7L@���@��u@�bN@�Z@�Q�@��@���@�S�@�C�@�"�@��\@�J@���@�(�@��@��@�v�@��@���@�O�@��@���@��j@���@�Q�@� �@��
@�t�@�+@�
=@���@��H@��!@���@�~�@�M�@�=q@�-@�X@���@���@��@�r�@�(�@���@��@���@��@���@�X@�&�@��@���@���@�9X@��@�@�P@K�@~ȴ@~@}p�@}�@|��@|��@|��@|I�@|1@{S�@z�H@zn�@y��@yx�@y&�@x�`@xĜ@x��@x1'@w�w@w��@w|�@wl�@w�@v�@vȴ@v��@vv�@v5?@u��@u/@t�/@tz�@sƨ@s�@st�@sdZ@sdZ@st�@s�@s�@s@r-@qx�@q�@p��@p1'@o�@n�y@n��@n�@n�y@m�@l��@l9X@l�@l1@l1@k��@k��@k"�@j��@j��@j�\@jM�@j-@j�@i��@ix�@h�`@h1'@g�w@g+@f�R@f�R@f�R@f�R@f�R@f�+@f$�@e�@e�T@e@e`B@eO�@dZ@d1@c��@c��@ct�@cdZ@cC�@c@c@b��@b^5@b=q@b�@b�@a��@a��@aG�@a�@`�`@`��@`Q�@_�@_�P@^��@^5?@]p�@]�@\�/@\��@\Z@\(�@\�@\1@[��@[o@Zn�@Y�@Y��@Y��@Y�7@YG�@X��@XQ�@W��@WK�@V��@Vȴ@V@Up�@T��@T9X@S�m@S�
@Sƨ@S��@S"�@R�!@Q��@Qhs@Q&�@PĜ@Pr�@PbN@PQ�@PA�@P �@O�@O|�@N��@M��@Mp�@M/@L��@L�@L�@L��@Lz�@K��@Kt�@J�@J��@J��@J��@J��@J��@J��@J�!@I��@I�7@I�7@Ix�@IG�@H��@HA�@H �@G�@G
=@Fȴ@F��@F��@F�+@Fv�@FV@F$�@F@E�@E�T@E��@E�-@E�@D�/@DZ@C�m@C��@CC�@B�@B�\@B-@A�#@AG�@@��@@bN@@1'@?��@?l�@?+@>�y@>��@>V@>$�@>@=��@=p�@=p�@=`B@=O�@=?}@=?}@<��@<I�@<1@;t�@;C�@;33@:�@:�!@:M�@:�@9�^@9G�@9�@8�9@8�@8A�@7�@7�@7;d@7
=@6��@6ȴ@6�+@6{@5@5�@5V@4��@4�D@4j@4Z@4�@3dZ@333@2�@2�H@2��@2=q@2J@1��@1��@1�@1�@1��@1X@17L@0�`@0r�@01'@0 �@0  @/��@/�P@/\)@/;d@/�@.�R@.�+@.v�@.v�@.ff@.@-�h@-`B@-/@,��@,�/@,��@,�@,j@,j@,I�@,1@+�
@+�@+@*��@*^5@*-@*J@)��@(��@(r�@(A�@'�@'�w@'�@'�w@'�w@'|�@&��@&��@&ȴ@%��@%O�@%/@$�j@$�@$z�@$Z@$9X@$�@#��@#�
@#�@"��@"�\@"=q@"J@!��@!�^@!x�@!%@ r�@ 1'@ 1'@ b@�;@�@��@\)@+@�y@��@5?@$�@{@��@@�h@O�@�@��@�j@9X@�F@��@��@�@��@��@�!@��@^5@-@��@��@G�@Ĝ@�@1'@1'@b@�@��@�w@��@�P@|�@\)@\)@;d@�@��@�R@�+@ff@V@V@E�@5?@{@@�@�-@p�@O�@/@�@��@�/@�@�D@I�@�@�
@ƨ@�@"�@o@o@o@@��@n�@�@��@�#@��@�^@G�@Ĝ@A�@  @�;@�@�P@\)@
=@�@ȴ@�R@�+@5?@�@��@@@�-@��@��@�h@`B@�@�@��@I�@1@1@1@1@�m@�@"�@@
�@
�!@
~�@
M�@
=q@
-@	��@	�#@	��@	��@	��@	x�@	x�@	x�@	hs@	G�@	�@��@��@Ĝ@�9@�9@��@�u@r�@bN@1'@�@��@��@�w@�P@|�@K�@+@�@�@;d@K�@K�@;d@�@�y@�@ȴ@��@v�@v�@ff@ff@E�@{@�@��@�-@�@O�@?}@/@/@/@�@�@��@�/@�D@9X@9X@9X@(�@(�@1@�m@�
@ƨ@��@�@C�@@��@�!@��@��@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�}B��BBÖBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBÖBÖBÖBÖBÖBÖBBB��B��B�qB�XB�!B��B�%BbNBcTBn�B�B|�Bs�BaHBS�B2-B7LB49B)�B1'BPB�BuBB�BĜBz�BffB~�Bz�Bt�Bm�Bu�Bq�BhsB]/BP�B>wB�B{BB
��B
�B
�
B
��B
�;B
��B
�B
�B
��B
B
��B
�oB
~�B
}�B
r�B
o�B
p�B
cTB
D�B
6FB
1'B
33B
5?B
2-B
-B
�B
bB
oB
  B
B	�B	�5B	�NB	�)B	��B	�B	��B	��B	��B	ɺB	��B	ɺB	��B	�9B	��B	�B	��B	��B	�{B	v�B	q�B	bNB	ZB	L�B	?}B	E�B	S�B	H�B	5?B	!�B	-B	-B	 �B	�B	\B	hB	uB	
=B	+B	PB	B��B	B��B�B�B�B�;B�B��B�^B�9B�3B�-B�LBB�wB�}BĜBȴBŢB�}B�XB�wB�RBÖBɺBɺBȴBȴBǮBÖB��B�wB�}BǮBÖBŢBĜB��B��B�jB�LB�B�\B��B�JB�Bl�B{�B�B�B�B�VB�\B�+B�B� B�%B�%B�B�Bw�Bl�BgmBhsB]/B_;BjBn�BgmBYBR�Be`B_;BaHBXBYBS�BS�BI�BB�BM�BH�B5?B%�B0!B7LB33BH�B?}B49B2-B9XB8RBA�B=qB7LB>wB6FB7LB1'B+B%�B.B(�B/B,B5?B2-B2-B5?B33B&�B'�B�B%�B(�B�B�B"�B�B �B%�B/B,B(�B$�B �B{BuBhB{B�B�B�BPBPB"�B#�B!�B�B�B�B�B�B�B�B�B�B�BDBhB�B{BPBBB'�B+B'�B#�B�B�B�B�B�B �B%�B#�B#�B,B.B,B"�B �B/B-B+B'�B&�B"�B�B1'B.B/B)�B(�B �B�B#�B6FB2-B1'B?}BK�BJ�BP�BN�BK�BN�BO�BP�BVBVBT�BP�BI�BK�BbNBaHB^5BaHBcTBbNB^5B[#BdZBjBq�Bl�Bp�Bt�Bs�Bm�Bl�Bw�Bo�By�B�=B�PB�PB�DB�\B�PB�hB�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�B�LB�qB��BÖBB��B��B��BǮBŢB��B��B��B��B�
B��B�/B�HB�fB�yB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	%B	B	B	DB	oB	uB	{B	�B	�B	�B	"�B	&�B	49B	6FB	;dB	=qB	=qB	@�B	A�B	F�B	L�B	M�B	M�B	N�B	P�B	VB	\)B	_;B	_;B	^5B	`BB	bNB	bNB	hsB	jB	m�B	p�B	s�B	t�B	u�B	v�B	v�B	x�B	~�B	� B	�B	�B	�B	�+B	�+B	�+B	�1B	�1B	�1B	�DB	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�-B	�'B	�'B	�3B	�3B	�3B	�?B	�?B	�FB	�^B	�qB	��B	ŢB	ŢB	ŢB	ĜB	ÖB	ĜB	ȴB	��B	ɺB	ȴB	ɺB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�NB	�NB	�ZB	�fB	�yB	�B	�B	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
	7B
1B
+B
B
+B

=B

=B
1B
+B
1B
DB

=B

=B
PB
\B
bB
bB
bB
\B
\B
bB
hB
hB
bB
bB
\B
PB
VB
bB
oB
hB
oB
oB
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
!�B
!�B
"�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
(�B
'�B
'�B
'�B
'�B
)�B
(�B
+B
,B
,B
-B
+B
)�B
-B
.B
/B
/B
-B
0!B
1'B
1'B
1'B
1'B
0!B
.B
1'B
/B
/B
2-B
33B
33B
2-B
33B
49B
49B
49B
33B
49B
6FB
6FB
5?B
33B
33B
6FB
7LB
7LB
7LB
8RB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
6FB
8RB
8RB
9XB
9XB
8RB
5?B
8RB
:^B
:^B
;dB
<jB
=qB
<jB
:^B
:^B
<jB
<jB
8RB
<jB
?}B
>wB
A�B
A�B
A�B
A�B
B�B
A�B
A�B
?}B
>wB
@�B
A�B
A�B
A�B
C�B
B�B
A�B
A�B
C�B
E�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
D�B
E�B
G�B
H�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
L�B
K�B
I�B
L�B
M�B
M�B
L�B
K�B
L�B
L�B
K�B
K�B
L�B
N�B
N�B
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
T�B
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
YB
ZB
YB
YB
XB
W
B
YB
ZB
[#B
ZB
YB
XB
W
B
YB
[#B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
]/B
\)B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
_;B
`BB
bNB
bNB
aHB
`BB
_;B
_;B
aHB
bNB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
dZB
dZB
dZB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
iyB
hsB
gmB
hsB
hsB
hsB
iyB
jB
k�B
k�B
jB
jB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
jB
jB
jB
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
k�B
l�B
n�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
m�B
m�B
l�B
m�B
n�B
o�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��BªBÖBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBĜBÖBÖBÖBÖBÖBÖBÖBÖBBB��B��B��B�rB��B�B�KBf�BfBpUB�{B}�Bt�Bb�BVSB6�B9�B6`B,B2|B�BsB�BuB�OB�B�ABj�B�iB|�Bu�Bn�Bv+Br-BiDB^jBR B@�BKB�BzB �B
��B
�QB
��B
��B
�2B
ؓB
ևB
�bB
ðB
�B
��B
��B
�B
t�B
q'B
q�B
d�B
G�B
8�B
3hB
4�B
6`B
3B
-�B
!HB
B
�B
�B
AB	�B	�\B	�B	�~B	�MB	ּB	� B	ԕB	��B	ʦB	�^B	�=B	�oB	��B	�eB	��B	��B	��B	��B	yrB	s�B	dZB	\)B	N�B	A�B	GB	TB	I�B	6�B	$&B	-�B	.IB	"NB	5B	hB	oB	FB	DB	B	�B	�B��B	�B��B�B�nB�aB��B�?B�B��B�+B�?B��B��B�aB��B�OB�B�B�B�OB�^B�.B�>B��BɺB��BȴB��B��B��B��B�B� B��B�B��B��B��B��B��B��B�B��B��B��B�Bo�B}qB�GB�uB�AB��B��B�B��B� B��B��B��B��Bx�BnBh�Bi�B_B`�BkBo Bh$BZ�BT�Be�B`'BbBYKBZBUBT�BKDBC�BNVBI�B7B($B1�B8�B4�BH�B@�B6+B3�B:�B9rBB'B>]B8lB>�B7LB8B2GB,qB'�B/B*0B0B-CB5�B3B2�B5�B3�B(>B(�B�B&�B)�B!B�B#�B
B!�B&�B/OB,WB)DB%`B!bB�BFB�B2B/B7B?B�BpB"�B$B"B/B$B�B?B+B#B)BB	BBdBB
BB<B�B�B'�B+6B($B$&B 'B]B�BCB�B!bB&fB$�B$�B,WB./B,=B#�B!�B/OB-wB+�B(�B'�B#�B�B1[B.�B/�B*�B)�B"BdB%FB6�B3hB2|B@BL0BKDBQ BOBBLJBOBBPbBQNBV9BVSBUMBQhBJ�BL�BbhBa|B^�Ba�Bc�Bb�B^�B\BeBj�Bq�BmBp�BuBtBnIBmwBx8Bp�Bz�B�XB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�,B�*B�0B�8B��B�eB��B��B��B��B��BðBªB��B��B��B��B��B�B�B�oB�NB�?B��B�~B�B�B��B�B��B�B��B��B��B�B�B�	B��B�B	  B	;B	'B	-B	MB	YB	SB	�B	�B	�B	�B	�B	�B	
B	B	#:B	'mB	4nB	6�B	;B	=qB	=�B	@�B	A�B	F�B	L�B	M�B	N"B	O(B	QB	V9B	\CB	_VB	_VB	^jB	`vB	b�B	b�B	h�B	j�B	m�B	p�B	s�B	t�B	u�B	v�B	v�B	y	B	B	� B	�B	�;B	�3B	�EB	�EB	�_B	�KB	�KB	��B	�^B	�~B	��B	�uB	��B	��B	��B	��B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�$B	�B	�B	�B	�B	��B	�_B	�5B	�-B	�AB	�'B	�MB	�3B	�hB	�tB	��B	��B	��B	��B	��B	ŢB	ňB	ŢB	ĶB	ðB	ĶB	��B	��B	��B	��B	��B	�B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�2B	�$B	�+B	�B	�+B	�$B	�?B	�1B	�1B	�1B	�KB	�KB	�KB	�eB	�eB	�qB	�\B	�bB	�hB	�TB	�tB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�0B	�JB	�JB
 B
-B
-B
B
B
-B
AB
AB
GB
SB
1B
	7B
	RB
	7B
	B
1B
EB
mB
EB

=B

=B
KB
zB
fB
^B

rB

rB
PB
\B
bB
bB
bB
vB
vB
�B
hB
hB
}B
}B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
 �B
!�B
!�B
#B
$�B
$�B
%�B
%�B
%�B
%�B
&B
(
B
(�B
(
B
(
B
($B
($B
*B
)*B
+B
,"B
,B
-B
+6B
*0B
-)B
./B
/OB
/5B
-)B
0UB
1'B
1B
1'B
1B
0!B
.IB
1AB
/OB
/iB
2-B
33B
3MB
2GB
3MB
4TB
4TB
4TB
3�B
4TB
6FB
6FB
5ZB
3hB
3hB
6`B
7fB
7fB
7fB
8RB
7LB
7fB
8RB
8lB
7fB
7fB
7fB
6`B
8lB
8lB
9rB
9rB
8�B
5tB
8lB
:xB
:xB
;B
<jB
=qB
<jB
:xB
:�B
<�B
<�B
8�B
<�B
?�B
>�B
A�B
A�B
A�B
A�B
BuB
A�B
A�B
?�B
>�B
@�B
A�B
A�B
A�B
C�B
B�B
A�B
A�B
C�B
E�B
D�B
D�B
D�B
E�B
D�B
D�B
D�B
D�B
E�B
G�B
H�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
H�B
I�B
L�B
K�B
I�B
L�B
M�B
M�B
L�B
K�B
L�B
L�B
K�B
K�B
MB
N�B
N�B
P�B
Q B
O�B
P�B
P�B
Q B
Q�B
Q�B
RB
Q�B
RB
Q B
RB
RB
RB
R�B
S�B
S�B
S�B
S�B
SB
S�B
TB
TB
TB
UB
VB
VB
VB
V9B
UB
VB
VB
VB
VB
W$B
W
B
W$B
YB
ZB
YB
Y1B
X+B
W$B
Y1B
Z7B
[#B
ZB
Y1B
XEB
W?B
YKB
[=B
\CB
\CB
\CB
\CB
\CB
]IB
^OB
^OB
]IB
\]B
^OB
`BB
`'B
`BB
`BB
`'B
`BB
`\B
_VB
_VB
_VB
_VB
_VB
`\B
bNB
bNB
aHB
`\B
_pB
_VB
aHB
bNB
a|B
abB
bhB
cTB
cnB
cnB
cnB
dZB
dtB
dZB
dZB
e`B
e`B
dtB
dtB
dZB
dZB
ffB
ffB
gmB
gmB
gmB
gmB
g�B
g�B
g�B
g�B
hsB
iyB
h�B
g�B
hsB
h�B
h�B
iyB
jeB
k�B
k�B
jB
jB
i�B
i�B
jB
j�B
j�B
j�B
kkB
kkB
k�B
j�B
jB
jB
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
k�B
l�B
n�B
n�B
n�B
n�B
m�B
m�B
n}B
n�B
m�B
m�B
l�B
m�B
n�B
o�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805060035072018050600350720180506003507201806221241172018062212411720180622124117201806042119422018060421194220180604211942  JA  ARFMdecpA19c                                                                20180502093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180502003526  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180502003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180502003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180502003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180502003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180502003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180502003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180502003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180502003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180502005659                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180502153516  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180505153507  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180505153507  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604121942  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034117  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                