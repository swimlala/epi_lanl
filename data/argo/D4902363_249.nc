CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-13T00:35:05Z creation;2018-06-13T00:35:09Z conversion to V3.1;2019-12-19T07:40:04Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20180613003505  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_249                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�i��.E 1   @�i�8�@:e+���d@҈�p;1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ D�|�D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D�=D��D �D��D �D��D
D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D  �D ��D! �D!z=D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Ds�=Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RD�}D��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD�D�&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A��A�A��;A��7A�hsA�ZA�E�A�A�bA�oA��A�
=A��/A��hA��A���A��A��FA�VA��
A��A���A���A���A���A�l�A�"�A�v�A��yA�`BA�A�A�VA�bA��+A���A��A�33A��A�+A��+A��yA�`BA�1A��FA��A��
A�ĜA���A�S�A��`A�C�A�JA�{A�`BA���A���A�I�A�z�A�&�A�x�A�+A��A���A�|�A�5?A���A�C�A�A�=qA���A�+A���A�+A��A���A�C�A��hA��A|�A{�Az��Ay�
Ax��Av�9At��As�Ar�Ar��Aq�FAo�PAmt�AjĜAi�Ag��Ad�Ad��Ad�9AdE�Ac��AchsAc33Ab�+A`�jA\bAY�wAX1'AV�HAT�ARffAQ�AQ�TAQ��AP��AP�!APAO&�ANAM+AL�AK+AJ�yAJ��AJ~�AJ(�AIS�AHAFQ�AD�yAC%AA+A@��A@�\A@r�A@E�A@=qA@JA?x�A>�9A<z�A;\)A:��A:�!A:�A:(�A8�+A7�;A7�A77LA65?A2�!A1�A1?}A1�A0��A0~�A/�TA/��A//A.��A-|�A,n�A,5?A+��A*{A(�/A(ZA'��A'l�A'
=A&$�A${A#�wA#VA!�A!�7A!"�A ~�A�TAv�A��A33Ar�A�^AM�A�FAK�A�A�RAz�A{A�
A��Ap�A�A/AXA�AȴAAt�AE�Ap�AoA�`A��A�9AffA�A`BAoA�A�A��A-A��A
Q�A-A"�A�A`BA?}A��AffA��AhsA�9A�A�hA ��@��@�z�@�C�@���@�j@�^5@��@�@�9X@��@�ƨ@���@�@�j@� �@�P@��@�ff@��@�J@��`@�|�@�+@�u@�33@��@�9X@ߍP@�C�@�
=@ݙ�@�;d@ٺ^@��@��/@ؓu@�z�@�1'@ו�@�C�@�@ְ!@�%@��
@�
=@�b@��@�~�@���@��@���@�E�@Ɂ@�z�@ǶF@�|�@�;d@���@��@Ų-@�/@ļj@�(�@�\)@��@\@�5?@�@���@���@�t�@�5?@���@���@�A�@���@���@���@�Z@�n�@��@���@�/@�V@���@��/@��j@���@�r�@�(�@�
=@�v�@���@�O�@��@��m@�S�@��@�@�/@�9X@��!@���@�`B@���@�Q�@���@�ƨ@�33@���@��@���@��
@�;d@�V@�@��@�r�@��w@�S�@��@��\@�ff@���@�x�@��@�%@���@���@���@�Z@��
@�t�@��H@�V@�{@��h@���@��9@�o@�z�@��@���@�\)@�C�@�"�@���@�ff@�G�@�Ĝ@�z�@�9X@�1@��
@���@��w@�;d@���@�E�@�-@���@���@��`@��u@� �@�dZ@�+@��@���@�V@���@�@�`B@��@�I�@�1@���@�;d@���@�-@���@�`B@��@��9@��u@��D@��D@��@�r�@�Q�@�1@��w@���@��@�|�@�l�@�;d@�
=@�ȴ@�ff@�$�@��#@��^@��7@��@��`@��/@���@��u@��@�bN@�I�@�A�@�1'@��@��@�1@�1@�w@l�@l�@\)@K�@+@�@
=@~��@~�y@~�@~��@}�T@}��@}�h@}�@}p�@}`B@}O�@|�@|j@{C�@z�@zJ@y�@y��@y�7@y�7@yhs@yX@y&�@x�`@x�u@xA�@w��@w\)@v�@vV@vv�@vv�@v$�@u?}@s��@s�@sC�@r��@rM�@r�@qhs@q%@pĜ@pQ�@p  @o��@o�@o�P@ol�@o+@nȴ@n$�@m`B@l��@l�@ix�@iX@i�@i%@i%@i%@i%@h��@h��@hr�@hA�@h  @g�@g\)@f��@f�+@e��@d�D@ct�@b��@a�^@a7L@`��@`��@`Ĝ@`�9@`�@_�@_��@_�@_K�@^��@]@]�@]?}@]/@]/@\��@\j@[�F@[dZ@[o@Z�@Y��@Y�@Xr�@Wl�@V��@V�R@VE�@V{@U�T@Up�@UV@T�@Tj@T�@S��@S@Qhs@Q&�@PĜ@P�9@P�9@P��@P��@PbN@PQ�@PQ�@PQ�@PQ�@PQ�@PQ�@PA�@P1'@P �@Pb@O�;@O�;@O�w@O�@O�@O�P@O
=@N@KS�@Jn�@I�@I�7@IX@I&�@H�9@H�@HA�@Hb@G�;@G�P@G
=@F�+@FE�@F$�@F$�@F{@F$�@F{@F@F@E�@F{@E�@E�T@E�-@E��@E��@E�-@E�-@E�-@E�-@E�-@E�-@E��@E��@E��@E`B@EO�@E?}@E�@D��@EV@D�@Dj@D(�@C�
@B~�@A�#@AG�@@Ĝ@@��@?�@>�y@>E�@=��@<��@<1@;�
@;��@;S�@;33@;"�@;o@:�!@:-@:J@9�@9��@9��@9%@8�u@8b@8  @7�@7�w@7�P@7�P@7|�@7\)@7
=@6��@6�+@6v�@6ff@65?@5��@5/@4��@4�@4(�@333@2�H@2^5@2�@1�@1�7@1%@0�@/�;@.�y@.E�@.{@.{@.@.@-�@-��@-?}@-/@,��@,j@,Z@,(�@+�m@+��@+dZ@+"�@*��@*=q@)G�@)%@(�9@(�@(bN@(b@'��@'�@'�P@'l�@'K�@';d@'
=@&�R@&v�@%��@%�h@%O�@%�@$��@$��@$�@$��@$��@$��@$�D@$Z@$�@#�F@#dZ@#C�@#33@#"�@#o@"�@"��@"~�@"n�@"M�@!�@!�#@!�^@!�7@!G�@ ��@ �9@ �u@ A�@�@�w@|�@l�@\)@\)@\)@K�@;d@�@
=@�y@�@ȴ@�R@�R@��@��@�+@v�@ff@�@��@��@��@��@�h@p�@`B@V@��@�D@j@I�@I�@9X@�@��@�m@�
@��@�@dZ@C�@33@33@33@33@�@n�@J@�^@��@��@�7@x�@X@7L@�@�u@Q�@A�@ �@  @�@l�@\)@K�@�@��@ȴ@�+@v�@V@$�@�@�T@@?}@V@V@�/@j@j@j@j@Z@9X@(�@�@1@��@�m@�
@�
@�
@�
@�F@��@�@�@t�@t�@@��@�!@��@��@��@�\@^5@-@J@�@�@��@�^@��@��@��@��@��@�7@hs@%@��@�@�;@��@K�@
=@��@�@��@��@E�@��@p�@V@��@�j@j@9X@�m@�F@�F@��@��@S�@@
��@
��@
�\@
n�@
M�@	��@	%@�9@��@�@1'@�;@��@�;@�;@�;@�@�@�w@�P@l�@�@�+@v�@E�@@�h@`B@/@��@�@�/@��@�j@�D@j@1@�F@�@�@�@C�@�@��@�!@��@�\@^5@^5@M�@�#@�^@�^@��@��@hs@X@G�@X@G�@G�@&�@�@�@�@%@ Ĝ@ Q�?��;?��w?�;d?�5??�{?��?���?���?��h?�O�?�O�?�V?�V?��?���?��?��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A��A�A��;A��7A�hsA�ZA�E�A�A�bA�oA��A�
=A��/A��hA��A���A��A��FA�VA��
A��A���A���A���A���A�l�A�"�A�v�A��yA�`BA�A�A�VA�bA��+A���A��A�33A��A�+A��+A��yA�`BA�1A��FA��A��
A�ĜA���A�S�A��`A�C�A�JA�{A�`BA���A���A�I�A�z�A�&�A�x�A�+A��A���A�|�A�5?A���A�C�A�A�=qA���A�+A���A�+A��A���A�C�A��hA��A|�A{�Az��Ay�
Ax��Av�9At��As�Ar�Ar��Aq�FAo�PAmt�AjĜAi�Ag��Ad�Ad��Ad�9AdE�Ac��AchsAc33Ab�+A`�jA\bAY�wAX1'AV�HAT�ARffAQ�AQ�TAQ��AP��AP�!APAO&�ANAM+AL�AK+AJ�yAJ��AJ~�AJ(�AIS�AHAFQ�AD�yAC%AA+A@��A@�\A@r�A@E�A@=qA@JA?x�A>�9A<z�A;\)A:��A:�!A:�A:(�A8�+A7�;A7�A77LA65?A2�!A1�A1?}A1�A0��A0~�A/�TA/��A//A.��A-|�A,n�A,5?A+��A*{A(�/A(ZA'��A'l�A'
=A&$�A${A#�wA#VA!�A!�7A!"�A ~�A�TAv�A��A33Ar�A�^AM�A�FAK�A�A�RAz�A{A�
A��Ap�A�A/AXA�AȴAAt�AE�Ap�AoA�`A��A�9AffA�A`BAoA�A�A��A-A��A
Q�A-A"�A�A`BA?}A��AffA��AhsA�9A�A�hA ��@��@�z�@�C�@���@�j@�^5@��@�@�9X@��@�ƨ@���@�@�j@� �@�P@��@�ff@��@�J@��`@�|�@�+@�u@�33@��@�9X@ߍP@�C�@�
=@ݙ�@�;d@ٺ^@��@��/@ؓu@�z�@�1'@ו�@�C�@�@ְ!@�%@��
@�
=@�b@��@�~�@���@��@���@�E�@Ɂ@�z�@ǶF@�|�@�;d@���@��@Ų-@�/@ļj@�(�@�\)@��@\@�5?@�@���@���@�t�@�5?@���@���@�A�@���@���@���@�Z@�n�@��@���@�/@�V@���@��/@��j@���@�r�@�(�@�
=@�v�@���@�O�@��@��m@�S�@��@�@�/@�9X@��!@���@�`B@���@�Q�@���@�ƨ@�33@���@��@���@��
@�;d@�V@�@��@�r�@��w@�S�@��@��\@�ff@���@�x�@��@�%@���@���@���@�Z@��
@�t�@��H@�V@�{@��h@���@��9@�o@�z�@��@���@�\)@�C�@�"�@���@�ff@�G�@�Ĝ@�z�@�9X@�1@��
@���@��w@�;d@���@�E�@�-@���@���@��`@��u@� �@�dZ@�+@��@���@�V@���@�@�`B@��@�I�@�1@���@�;d@���@�-@���@�`B@��@��9@��u@��D@��D@��@�r�@�Q�@�1@��w@���@��@�|�@�l�@�;d@�
=@�ȴ@�ff@�$�@��#@��^@��7@��@��`@��/@���@��u@��@�bN@�I�@�A�@�1'@��@��@�1@�1@�w@l�@l�@\)@K�@+@�@
=@~��@~�y@~�@~��@}�T@}��@}�h@}�@}p�@}`B@}O�@|�@|j@{C�@z�@zJ@y�@y��@y�7@y�7@yhs@yX@y&�@x�`@x�u@xA�@w��@w\)@v�@vV@vv�@vv�@v$�@u?}@s��@s�@sC�@r��@rM�@r�@qhs@q%@pĜ@pQ�@p  @o��@o�@o�P@ol�@o+@nȴ@n$�@m`B@l��@l�@ix�@iX@i�@i%@i%@i%@i%@h��@h��@hr�@hA�@h  @g�@g\)@f��@f�+@e��@d�D@ct�@b��@a�^@a7L@`��@`��@`Ĝ@`�9@`�@_�@_��@_�@_K�@^��@]@]�@]?}@]/@]/@\��@\j@[�F@[dZ@[o@Z�@Y��@Y�@Xr�@Wl�@V��@V�R@VE�@V{@U�T@Up�@UV@T�@Tj@T�@S��@S@Qhs@Q&�@PĜ@P�9@P�9@P��@P��@PbN@PQ�@PQ�@PQ�@PQ�@PQ�@PQ�@PA�@P1'@P �@Pb@O�;@O�;@O�w@O�@O�@O�P@O
=@N@KS�@Jn�@I�@I�7@IX@I&�@H�9@H�@HA�@Hb@G�;@G�P@G
=@F�+@FE�@F$�@F$�@F{@F$�@F{@F@F@E�@F{@E�@E�T@E�-@E��@E��@E�-@E�-@E�-@E�-@E�-@E�-@E��@E��@E��@E`B@EO�@E?}@E�@D��@EV@D�@Dj@D(�@C�
@B~�@A�#@AG�@@Ĝ@@��@?�@>�y@>E�@=��@<��@<1@;�
@;��@;S�@;33@;"�@;o@:�!@:-@:J@9�@9��@9��@9%@8�u@8b@8  @7�@7�w@7�P@7�P@7|�@7\)@7
=@6��@6�+@6v�@6ff@65?@5��@5/@4��@4�@4(�@333@2�H@2^5@2�@1�@1�7@1%@0�@/�;@.�y@.E�@.{@.{@.@.@-�@-��@-?}@-/@,��@,j@,Z@,(�@+�m@+��@+dZ@+"�@*��@*=q@)G�@)%@(�9@(�@(bN@(b@'��@'�@'�P@'l�@'K�@';d@'
=@&�R@&v�@%��@%�h@%O�@%�@$��@$��@$�@$��@$��@$��@$�D@$Z@$�@#�F@#dZ@#C�@#33@#"�@#o@"�@"��@"~�@"n�@"M�@!�@!�#@!�^@!�7@!G�@ ��@ �9@ �u@ A�@�@�w@|�@l�@\)@\)@\)@K�@;d@�@
=@�y@�@ȴ@�R@�R@��@��@�+@v�@ff@�@��@��@��@��@�h@p�@`B@V@��@�D@j@I�@I�@9X@�@��@�m@�
@��@�@dZ@C�@33@33@33@33@�@n�@J@�^@��@��@�7@x�@X@7L@�@�u@Q�@A�@ �@  @�@l�@\)@K�@�@��@ȴ@�+@v�@V@$�@�@�T@@?}@V@V@�/@j@j@j@j@Z@9X@(�@�@1@��@�m@�
@�
@�
@�
@�F@��@�@�@t�@t�@@��@�!@��@��@��@�\@^5@-@J@�@�@��@�^@��@��@��@��@��@�7@hs@%@��@�@�;@��@K�@
=@��@�@��@��@E�@��@p�@V@��@�j@j@9X@�m@�F@�F@��@��@S�@@
��@
��@
�\@
n�@
M�@	��@	%@�9@��@�@1'@�;@��@�;@�;@�;@�@�@�w@�P@l�@�@�+@v�@E�@@�h@`B@/@��@�@�/@��@�j@�D@j@1@�F@�@�@�@C�@�@��@�!@��@�\@^5@^5@M�@�#@�^@�^@��@��@hs@X@G�@X@G�@G�@&�@�@�@�@%@ Ĝ@ Q�?��;?��w?�;d?�5??�{?��?���?���?��h?�O�?�O�?�V?�V?��?���?��?��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�yB�mB�TB�5B�;B�BB�B��B�3B��B�XB�jB��BŢB�B��B�B_;B�VBw�B�uB�uB}�Bo�B}�Bv�BjBC�BXB;dB�B#�B6FB'�B�BDB��B�HB��B�B�B�B�B�ZB�B�)B�#B��BŢB�LB��B��Bu�B,B?}B9XB �B,B/B�B�B1B
�mB
�B
��B
�mB
�;B
�)B
ĜB
�RB
��B
��B
��B
�{B
�%B
�+B
r�B
gmB
B�B
H�B
O�B
D�B
2-B
 �B
\B
{B
DB
PB	��B	�B	��B	�9B	��B	�'B	��B	B	��B	�^B	�'B	�!B	��B	�oB	q�B	6FB	H�B	L�B	G�B	2-B	33B	L�B	R�B	L�B	B�B	D�B	=qB	2-B	&�B	+B	�B	!�B	+B	(�B	%�B	�B	JB	B�sB�B�BB�;B��B��B��B��B��B�B�TB�
B�qB��B�#B�BB�;B��B�dBȴBǮB�}B�B�B��B�XB�XB�LB�B��B�B��B��B�{B�VB��B�\B� B~�B�DB�=B�1B�Bu�B]/B|�Bv�Bl�Bs�Bq�BhsBbNBW
B\)BbNBZBW
BJ�BZB_;B`BBaHB`BB[#B\)BXBS�BG�B1'B'�BL�BH�B=qB<jB7LB6FBE�BH�BI�BF�BA�B7LBA�B?}BC�B@�B9XB0!B&�B{BB�B'�B�B2-B-B)�B'�B"�B�B�B�B{B	7BBVB��B1B�B�B'�B)�B)�B)�B�B�B-B,B'�B%�B%�B%�B#�B�BoB{B\BbBoB�B!�B&�B"�B�B\B�B'�B.B-B.B,B(�B(�B(�B"�B�B�B�B
=B�B%�B"�B�B!�B,B(�B+B,B33B33B0!B-B33B1'B0!B0!B/B7LB49B7LB7LB5?B,B+B.B8RB49B7LB9XB2-B.B/B/BB�BF�BF�BJ�BK�BI�BJ�BH�BG�BC�B=qBC�BF�BH�BL�BF�BL�BN�BG�BG�BJ�BI�BS�B]/B_;BbNBdZBgmBdZBdZBffBffBbNBjBhsBp�Bk�Bt�Bw�B{�B|�B� B�B� B� B�B�1B�1B�+B�%B�B�B�%B�%B�7B�DB�DB�=B�JB�B� B��B�B�B�!B�!B�B�B��B�?B�jB�wB��B��BB��B�wB��BŢBɺBǮBȴB��B��B��B��B�#B�/B�;B�;B�HB�ZB�TB�ZB�B�B�B��B��B��B	B	B	1B	PB	bB	oB	uB	oB	oB	hB	hB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	+B	/B	/B	0!B	49B	7LB	7LB	7LB	<jB	=qB	>wB	@�B	B�B	C�B	D�B	D�B	D�B	D�B	E�B	H�B	I�B	I�B	J�B	K�B	K�B	L�B	L�B	L�B	L�B	K�B	Q�B	S�B	T�B	S�B	S�B	S�B	Q�B	R�B	S�B	W
B	cTB	bNB	cTB	e`B	ffB	e`B	ffB	e`B	e`B	ffB	hsB	iyB	k�B	m�B	p�B	v�B	v�B	t�B	r�B	v�B	~�B	�B	�B	�B	�1B	�1B	�PB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	�FB	�RB	�XB	�XB	�XB	�RB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	�qB	�qB	�qB	B	ɺB	��B	��B	�
B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�;B	�BB	�NB	�HB	�;B	�;B	�;B	�NB	�TB	�NB	�;B	�NB	�`B	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B
B
%B
1B
1B
+B
	7B

=B

=B

=B

=B
	7B
DB
VB
\B
bB
bB
bB
bB
bB
bB
bB
hB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
bB
\B
hB
hB
bB
hB
bB
\B
\B
\B
PB

=B
bB
oB
uB
�B
oB
{B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
 �B
�B
�B
"�B
"�B
"�B
!�B
 �B
!�B
#�B
&�B
&�B
&�B
&�B
'�B
&�B
&�B
%�B
%�B
'�B
(�B
'�B
&�B
&�B
%�B
'�B
(�B
&�B
%�B
)�B
+B
,B
-B
,B
,B
,B
,B
,B
/B
49B
5?B
5?B
5?B
49B
33B
33B
49B
33B
5?B
6FB
6FB
5?B
5?B
6FB
6FB
5?B
49B
33B
9XB
:^B
:^B
;dB
:^B
;dB
<jB
<jB
<jB
=qB
=qB
<jB
;dB
<jB
;dB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
@�B
?}B
@�B
A�B
C�B
C�B
C�B
C�B
B�B
B�B
C�B
D�B
C�B
B�B
D�B
D�B
C�B
C�B
C�B
E�B
E�B
D�B
E�B
F�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
I�B
H�B
H�B
I�B
L�B
K�B
K�B
K�B
J�B
J�B
I�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
N�B
M�B
L�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
O�B
Q�B
R�B
R�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
R�B
VB
W
B
VB
VB
XB
YB
YB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
YB
YB
YB
YB
ZB
ZB
YB
W
B
YB
[#B
[#B
[#B
[#B
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
[#B
ZB
[#B
[#B
YB
]/B
]/B
^5B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
_;B
bNB
aHB
aHB
bNB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
gmB
ffB
gmB
ffB
cTB
e`B
ffB
hsB
hsB
hsB
hsB
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
jB
jB
l�B
l�B
k�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
o�B
o�B
m�B
n�B
n�B
p�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
o�B
q�B
r�B
r�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
q�B
q�B
s�B
r�B
q�B
t�B
t�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B��BߊB�vB�yB͹B�ZB��B�DB��B�0B��B��B�sB�YBe�B��B|�B�9B�B�ABr�B� Bx�Bm]BG+BY�B?B#B%�B72B)DBYBB�BB�tB��B�?B�B�B�]B�,B�eBܬB�qB�{B�tB�lB�FB��Bx�B2|BB�B;�B$@B-�B/�B;B�B	�B
�B
�B
��B
��B
�BB
��B
�tB
��B
�)B
�B
�B
�9B
�1B
�1B
t�B
i�B
E�B
JXB
P}B
E�B
3�B
#:B
�B
�B
dB
�B	�cB	�B	�}B	�LB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	t�B	;�B	KxB	N�B	I�B	5tB	5B	M6B	SB	M6B	C{B	E9B	>wB	3hB	(sB	,"B	!B	"�B	+QB	)_B	&LB	jB	�B	�B��B�}B�B�bB�B�0B�B�$B��B�'B�ZB�_B�OB�B��B�vB�pB��B�VBɆB�KB�OB��B��B�B��B��B��B��B��B��B��B��B��B��B�B�}B��B��B��B��B��B��Bw2B_�B}qBw�Bm�BtTBraBiyBcnBX�B]IBc B[=BX+BL�BZ�B_�B`�Ba�B`�B[�B\�BX�BT{BH�B3�B*0BL�BIRB>�B=qB8�B7�BFBIBI�BF�BB'B8RBA�B@ BC�B@�B9�B0�B'�BmB�B�B(�B BB2aB-�B*�B(�B#�B �B�B�B�B
�B�BvB��B	�B�B]B(>B*KB*KB*�B�B�B-CB,WB(sB&fB&2B&2B$@BkBuBMB�BNBuByB"NB'RB#TB�B BxB(XB.IB-]B.IB,WB)_B)DB)DB#nB�B�BkBB �B&LB#TB	B"�B,�B)�B+�B,�B3�B3�B0�B-�B3�B1�B0�B0�B/�B7�B4�B7�B7�B5�B,�B+�B/ B8�B4�B7�B9�B2�B/5B0!B0UBB�BF�BF�BJ�BK�BI�BJ�BH�BG�BC�B>]BDBGBIBMBGzBM6BO(BH�BHfBKxBJ�BT�B]~B_�Bb�Bd�Bg�Bd�Bd�BgBgBc:BkBi*Bp�BlWBu?BxRB|6B}VB�OB�AB�iB�iB��B�KB�KB�_B�YB�mB�{B��B��B��B��B��B��B��B�{B��B�2B�CB�cB�;B�oB�cB��B��B��B��B��B��B��B��B��B��B�B��B��B�B�7B�B�4B�.B҉B�WB�~B�pBߊB�B�B�B��B�B�B�B�B�B�JB	UB	{B	�B	�B	}B	�B	�B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	"B	'B	+6B	/5B	/OB	0oB	4nB	7fB	7�B	7�B	<�B	=�B	>�B	@�B	B�B	C�B	D�B	D�B	D�B	D�B	E�B	H�B	I�B	I�B	J�B	K�B	K�B	L�B	L�B	L�B	L�B	LB	RB	TB	T�B	TB	S�B	TB	R B	S@B	TaB	WsB	c:B	bhB	cnB	ezB	ffB	e`B	f�B	ezB	ezB	f�B	h�B	i�B	k�B	m�B	p�B	v�B	v�B	t�B	sB	wLB	B	�;B	�AB	�MB	�1B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	��B	�B	�`B	�RB	�>B	�XB	�XB	�lB	�RB	��B	�XB	�xB	��B	��B	��B	��B	��B	��B	��B	�	B	�"B	�&B	�$B	�+B	�B	�1B	�+B	�?B	�7B	�7B	�KB	�_B	�eB	�VB	�BB	�B	�bB	�pB	�pB	ߊB	�hB	�nB	�B	ߤB	�B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�B	��B	��B	��B	��B	�/B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�.B
MB
?B
fB
KB
EB
	RB

XB

XB

XB

XB
	RB
xB
pB
vB
HB
bB
bB
bB
HB
bB
bB
�B
}B
}B
}B
bB
hB
hB
NB
hB
hB
hB
hB
hB
hB
HB
�B
NB
�B
bB
hB
bB
�B
vB
vB
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
 �B
�B
�B
"�B
"�B
#B
!�B
 �B
!�B
#�B
&�B
'B
'B
'B
'�B
&�B
'B
%�B
&B
(
B
(�B
(
B
&�B
'B
&2B
'�B
)B
'B
&LB
*B
+6B
,"B
-)B
,=B
,=B
,=B
,WB
,qB
/5B
4TB
5?B
5?B
5?B
4TB
3MB
3MB
4TB
3MB
5ZB
6`B
6zB
5?B
5ZB
6`B
6`B
5tB
4�B
3�B
9rB
:xB
:�B
;B
:^B
;B
<�B
<�B
<�B
=�B
=�B
<�B
;B
<�B
;�B
>wB
?�B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
A�B
@�B
?�B
@�B
A�B
C�B
C�B
C�B
C�B
B�B
B�B
C�B
D�B
C�B
B�B
D�B
D�B
C�B
C�B
C�B
E�B
E�B
D�B
E�B
F�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
J�B
J�B
I�B
H�B
H�B
I�B
L�B
K�B
K�B
K�B
J�B
J�B
I�B
J�B
K�B
MB
NB
M�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
N�B
M�B
MB
N�B
O�B
Q�B
Q�B
Q�B
Q�B
Q B
Q B
Q B
O�B
RB
R�B
SB
RB
RB
R�B
S�B
S�B
TB
TB
TB
TB
T�B
UB
U2B
UB
UB
UB
SB
VB
W
B
VB
VB
XB
YB
YB
X+B
X+B
YB
YB
YB
YB
YB
ZB
ZB
ZB
X�B
Y1B
X�B
Y1B
ZB
ZB
YB
W?B
Y1B
[#B
[#B
[#B
[#B
Z7B
Z7B
Z7B
[#B
[=B
\)B
\CB
\B
\)B
]IB
]/B
]/B
\)B
\)B
[WB
ZQB
[=B
[=B
YKB
]IB
]/B
^OB
_;B
_VB
_VB
_VB
^jB
^jB
_VB
_pB
bNB
aHB
abB
b�B
b�B
dZB
e`B
e`B
ezB
ezB
ezB
f�B
gmB
f�B
g�B
f�B
c�B
e�B
f�B
hXB
h�B
h�B
h�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
j�B
j�B
l�B
l�B
k�B
m�B
m�B
m�B
m�B
o�B
o�B
p�B
p�B
o�B
o�B
m�B
n�B
n�B
p�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
q�B
p�B
o�B
q�B
r�B
r�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
q�B
q�B
s�B
r�B
q�B
t�B
t�B
u�B
u�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806170035352018061700353520180617003535201806221243222018062212432220180622124322201806180022172018061800221720180618002217  JA  ARFMdecpA19c                                                                20180613093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180613003505  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180613003508  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180613003508  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180613003509  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180613003509  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180613003509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180613003509  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180613003509  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180613003509                      G�O�G�O�G�O�                JA  ARUP                                                                        20180613005509                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180613153240  CV  JULD            G�O�G�O�F�O�                JM  ARCAJMQC2.0                                                                 20180616153535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180616153535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180617152217  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034322  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                