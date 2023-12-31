CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-23T00:35:28Z creation;2016-07-23T00:35:30Z conversion to V3.1;2019-12-19T08:31:08Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20160723003528  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_020                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׽t��~ 1   @׽ufff�@3,�����d���}Vm1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B��B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ D˃3D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=Bp�B
=B��B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bhp�Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$)C&�C(�C*�C,)C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D&�=D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-�
D. �D.��D/ �D/�
D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DT�=DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RD˃�D��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD��D�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A݁A݁A�~�A�~�A�~�A�~�A�|�A�x�A�x�A�x�A�x�A�z�A�z�A�|�A�|�A�~�A�z�A�\)A٩�A�AօA�I�A�&�AլA�C�A�1A���A��/A���A�z�A�  AЗ�A�dZA���A��A�O�Aȕ�A�-A���A�bA�Q�A�O�A��A��yA��A�ZA��FA�XA�ffA�"�A��A���A�E�A��TA��9A��7A��RA�z�A�jA��A�;dA��TA�ȴA�(�A�n�A��/A�\)A��-A�l�A�ƨA�7LA��A���A�(�A��A���A�7LA�t�A��!A���A��RA�S�A�oA��A�~�A�5?A���A�(�A�  A�n�A��FA�O�A��;A��wA���A��A��jA���A�ƨA�`BA���A}��A{33Az��AyG�Aw�AwhsAvv�AtjAs�Ar�uAq�FAqS�Ap5?An�jAn$�Am��Am��Am&�Ak%Ah�Ae�mAc��Ac%AbȴA`~�A\1'A[VAZ~�AY�AXbAU��AR^5AP��AO�ANr�AMt�ALQ�AK�AK%AI�mAG�;AFM�AE�AD$�AA�;A@�9A?�A>�yA;�
A9VA7�wA6�RA3�-A1;dA0bNA/33A.Q�A-��A-��A-�7A-x�A-7LA*M�A)��A)�A'�A%��A#�
A"�!A"ffA"A!�A �A�A�yAZA�-A��Ap�A�`A=qA��A��A�FA�AVA;dA�^A��AVA��AK�A5?Ap�Ax�A��A��A\)AG�A^5A9XA��A7LA
��A
�A	33A{A�yA��AXA�TA"�A �DA 5?@���@�J@���@��@��^@���@�(�@���@�9X@�ƨ@�`B@�1'@���@�E�@�A�@�"�@�@�R@�hs@�r�@ߕ�@�~�@��@�&�@�Z@ܼj@�j@��@���@��@��@��
@ץ�@��@֧�@թ�@���@�I�@��;@Ӆ@�C�@��H@Ұ!@�V@�j@ύP@�"�@���@�v�@�Z@�E�@�G�@��@ȓu@�(�@ǝ�@�o@�=q@�x�@�?}@���@�Ĝ@�j@öF@Ý�@�t�@�;d@§�@�ff@�V@���@�O�@��@���@�X@��`@��@��D@��P@�\)@�;d@���@�M�@��h@���@� �@�;d@���@�{@�`B@��j@� �@��@�+@��R@��#@�G�@�?}@�/@��@�%@��/@��@���@�@���@�`B@���@�5?@��h@���@�z�@��F@��@��+@��+@��y@�dZ@�33@�+@�=q@�-@�v�@�V@�5?@��T@��@�G�@���@���@�9X@���@���@�-@�p�@��j@��
@��H@��H@���@��+@���@���@���@�
=@��`@��u@�9X@�
=@���@�v�@���@��@��u@�S�@��@���@��-@��D@�1'@�1'@�j@��@�x�@�ȴ@�t�@�1@��@�I�@�Z@� �@��;@�l�@�o@���@��@��@��@�j@��F@�33@��y@���@�J@��-@��-@��@�Z@�ƨ@�S�@�
=@�ȴ@�o@�C�@��@�E�@�@��h@�p�@�Ĝ@��/@�%@��@�&�@�G�@�O�@�O�@�%@��/@��@�V@��@��u@���@���@�t�@�"�@��R@�v�@��@���@��@�Q�@�%@���@��7@��@�?}@���@���@��D@� �@��@�C�@��H@�v�@�M�@���@���@��@�7L@���@�I�@�b@��w@�l�@�C�@�@�ȴ@���@�E�@���@��@��^@�p�@�G�@��@�%@���@���@��@���@�j@�A�@��
@�"�@���@��!@�ff@�=q@�{@���@���@���@���@��^@���@��7@�x�@�X@��@��`@���@�bN@��@���@��@�v�@�^5@�=q@��@�J@���@�hs@�/@���@�z�@��u@���@��D@�z�@�j@�I�@�  @��@�\)@�+@�@�ȴ@�~�@���@��h@�/@�r�@�  @+@~��@~5?@}��@}�-@}��@}�@}/@|Z@{�F@{33@z�!@zn�@y��@y7L@x�u@w�@w+@v�+@vV@v{@u@t�@st�@r��@q��@q�#@q��@p��@pr�@p  @o��@o�@ol�@n��@m�@m?}@l�j@l9X@k�F@k@j~�@jM�@i�#@i��@iX@hĜ@h1'@h  @g�P@f�@e�@e��@e�h@d�/@dZ@d1@c�m@c��@ct�@c"�@b��@b-@a��@ax�@aG�@a�@`�9@`bN@`A�@` �@_|�@^�y@^�@^�@^�@^��@^$�@]��@]p�@\�j@\Z@\�@[�m@[S�@Z�!@Y�#@YX@X�`@X��@Xr�@X1'@W�w@WK�@W;d@W;d@V�@V�+@V$�@U�T@U�-@U�@UO�@U?}@UV@UV@T�@T�D@TI�@T(�@S�
@SdZ@R�@R�\@RM�@Q�#@Q��@Qhs@Q%@P��@P�`@P1'@O�w@O�P@N��@N��@NE�@M�h@Mp�@M?}@L��@L9X@L1@Kƨ@K��@K"�@J��@Ko@J�!@I��@Ihs@IG�@IG�@I7L@H�`@HĜ@HĜ@H��@Hr�@HQ�@HA�@G�@G�P@Gl�@G;d@F�y@F��@F$�@E�h@D��@D�D@D�@C�@CdZ@C"�@C@B�!@B^5@B-@A�#@A�#@A�@A�^@A�7@Ahs@A�@@�u@@Q�@@Q�@@  @?��@?l�@?K�@>�R@>v�@>ff@>E�@=�@=��@=O�@=/@<�/@<Z@;��@;�
@;�@:�@:~�@:�@9x�@9&�@8�@8b@8  @7�w@7�P@7+@6v�@5��@5`B@5O�@5?}@5/@4�@4�/@4��@4z�@3��@2��@2=q@2�@1x�@17L@1�@0��@0��@0�@/�;@/�P@.�y@.E�@.{@-�T@-�-@-�h@-`B@-V@+�m@+�@+dZ@+C�@+"�@+@*�!@*n�@*^5@*=q@*�@)��@)��@)x�@)&�@(�`@(r�@( �@'�;@'\)@'K�@';d@';d@'�@&�+@&E�@%�@%�T@%��@%O�@%V@$�j@$�@$(�@#�
@#�@#C�@#o@"��@"�!@"�\@"n�@"J@!��@!��@!��@!�7@!x�@!x�@!7L@ ��@ �u@ bN@  �@   @��@��@l�@K�@+@��@ȴ@v�@$�@��@�@O�@��@�@�/@�j@z�@Z@1@�m@��@t�@33@��@n�@��@�7@x�@x�@X@G�@%@�9@�u@r�@A�@A�@1'@ �@�;@|�@
=@ȴ@��@E�@5?@5?@@�T@@��@`B@/@V@V@��@�/@�/@��@�j@�@�@�@�D@j@j@Z@�m@�@�@t�@S�@�H@~�@M�@=q@-@-@-@��@�#@��@�^@��@��@��@hs@7L@7L@�@��@�9@��@�u@�u@�@r�@Q�@1'@b@��@��@;d@
=@�y@�@�@ȴ@ȴ@�R@��@ff@E�@5?@{@�T@@@�-@�-@�-@�-@��@��@��@�h@�h@�@O�@?}@/@�@��@�j@�@�D@j@(�@��@�
@�F@��@��@�@�@t�@dZ@C�@33@"�@
�@
��@
��@
��@
��@
~�@	��@	��@	��@	�7@	X@	G�@	X@	G�@	&�@�`@��@�@�@r�@bN@Q�@Q�@Q�@Q�@A�@ �@1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A݁A݁A�~�A�~�A�~�A�~�A�|�A�x�A�x�A�x�A�x�A�z�A�z�A�|�A�|�A�~�A�z�A�\)A٩�A�AօA�I�A�&�AլA�C�A�1A���A��/A���A�z�A�  AЗ�A�dZA���A��A�O�Aȕ�A�-A���A�bA�Q�A�O�A��A��yA��A�ZA��FA�XA�ffA�"�A��A���A�E�A��TA��9A��7A��RA�z�A�jA��A�;dA��TA�ȴA�(�A�n�A��/A�\)A��-A�l�A�ƨA�7LA��A���A�(�A��A���A�7LA�t�A��!A���A��RA�S�A�oA��A�~�A�5?A���A�(�A�  A�n�A��FA�O�A��;A��wA���A��A��jA���A�ƨA�`BA���A}��A{33Az��AyG�Aw�AwhsAvv�AtjAs�Ar�uAq�FAqS�Ap5?An�jAn$�Am��Am��Am&�Ak%Ah�Ae�mAc��Ac%AbȴA`~�A\1'A[VAZ~�AY�AXbAU��AR^5AP��AO�ANr�AMt�ALQ�AK�AK%AI�mAG�;AFM�AE�AD$�AA�;A@�9A?�A>�yA;�
A9VA7�wA6�RA3�-A1;dA0bNA/33A.Q�A-��A-��A-�7A-x�A-7LA*M�A)��A)�A'�A%��A#�
A"�!A"ffA"A!�A �A�A�yAZA�-A��Ap�A�`A=qA��A��A�FA�AVA;dA�^A��AVA��AK�A5?Ap�Ax�A��A��A\)AG�A^5A9XA��A7LA
��A
�A	33A{A�yA��AXA�TA"�A �DA 5?@���@�J@���@��@��^@���@�(�@���@�9X@�ƨ@�`B@�1'@���@�E�@�A�@�"�@�@�R@�hs@�r�@ߕ�@�~�@��@�&�@�Z@ܼj@�j@��@���@��@��@��
@ץ�@��@֧�@թ�@���@�I�@��;@Ӆ@�C�@��H@Ұ!@�V@�j@ύP@�"�@���@�v�@�Z@�E�@�G�@��@ȓu@�(�@ǝ�@�o@�=q@�x�@�?}@���@�Ĝ@�j@öF@Ý�@�t�@�;d@§�@�ff@�V@���@�O�@��@���@�X@��`@��@��D@��P@�\)@�;d@���@�M�@��h@���@� �@�;d@���@�{@�`B@��j@� �@��@�+@��R@��#@�G�@�?}@�/@��@�%@��/@��@���@�@���@�`B@���@�5?@��h@���@�z�@��F@��@��+@��+@��y@�dZ@�33@�+@�=q@�-@�v�@�V@�5?@��T@��@�G�@���@���@�9X@���@���@�-@�p�@��j@��
@��H@��H@���@��+@���@���@���@�
=@��`@��u@�9X@�
=@���@�v�@���@��@��u@�S�@��@���@��-@��D@�1'@�1'@�j@��@�x�@�ȴ@�t�@�1@��@�I�@�Z@� �@��;@�l�@�o@���@��@��@��@�j@��F@�33@��y@���@�J@��-@��-@��@�Z@�ƨ@�S�@�
=@�ȴ@�o@�C�@��@�E�@�@��h@�p�@�Ĝ@��/@�%@��@�&�@�G�@�O�@�O�@�%@��/@��@�V@��@��u@���@���@�t�@�"�@��R@�v�@��@���@��@�Q�@�%@���@��7@��@�?}@���@���@��D@� �@��@�C�@��H@�v�@�M�@���@���@��@�7L@���@�I�@�b@��w@�l�@�C�@�@�ȴ@���@�E�@���@��@��^@�p�@�G�@��@�%@���@���@��@���@�j@�A�@��
@�"�@���@��!@�ff@�=q@�{@���@���@���@���@��^@���@��7@�x�@�X@��@��`@���@�bN@��@���@��@�v�@�^5@�=q@��@�J@���@�hs@�/@���@�z�@��u@���@��D@�z�@�j@�I�@�  @��@�\)@�+@�@�ȴ@�~�@���@��h@�/@�r�@�  @+@~��@~5?@}��@}�-@}��@}�@}/@|Z@{�F@{33@z�!@zn�@y��@y7L@x�u@w�@w+@v�+@vV@v{@u@t�@st�@r��@q��@q�#@q��@p��@pr�@p  @o��@o�@ol�@n��@m�@m?}@l�j@l9X@k�F@k@j~�@jM�@i�#@i��@iX@hĜ@h1'@h  @g�P@f�@e�@e��@e�h@d�/@dZ@d1@c�m@c��@ct�@c"�@b��@b-@a��@ax�@aG�@a�@`�9@`bN@`A�@` �@_|�@^�y@^�@^�@^�@^��@^$�@]��@]p�@\�j@\Z@\�@[�m@[S�@Z�!@Y�#@YX@X�`@X��@Xr�@X1'@W�w@WK�@W;d@W;d@V�@V�+@V$�@U�T@U�-@U�@UO�@U?}@UV@UV@T�@T�D@TI�@T(�@S�
@SdZ@R�@R�\@RM�@Q�#@Q��@Qhs@Q%@P��@P�`@P1'@O�w@O�P@N��@N��@NE�@M�h@Mp�@M?}@L��@L9X@L1@Kƨ@K��@K"�@J��@Ko@J�!@I��@Ihs@IG�@IG�@I7L@H�`@HĜ@HĜ@H��@Hr�@HQ�@HA�@G�@G�P@Gl�@G;d@F�y@F��@F$�@E�h@D��@D�D@D�@C�@CdZ@C"�@C@B�!@B^5@B-@A�#@A�#@A�@A�^@A�7@Ahs@A�@@�u@@Q�@@Q�@@  @?��@?l�@?K�@>�R@>v�@>ff@>E�@=�@=��@=O�@=/@<�/@<Z@;��@;�
@;�@:�@:~�@:�@9x�@9&�@8�@8b@8  @7�w@7�P@7+@6v�@5��@5`B@5O�@5?}@5/@4�@4�/@4��@4z�@3��@2��@2=q@2�@1x�@17L@1�@0��@0��@0�@/�;@/�P@.�y@.E�@.{@-�T@-�-@-�h@-`B@-V@+�m@+�@+dZ@+C�@+"�@+@*�!@*n�@*^5@*=q@*�@)��@)��@)x�@)&�@(�`@(r�@( �@'�;@'\)@'K�@';d@';d@'�@&�+@&E�@%�@%�T@%��@%O�@%V@$�j@$�@$(�@#�
@#�@#C�@#o@"��@"�!@"�\@"n�@"J@!��@!��@!��@!�7@!x�@!x�@!7L@ ��@ �u@ bN@  �@   @��@��@l�@K�@+@��@ȴ@v�@$�@��@�@O�@��@�@�/@�j@z�@Z@1@�m@��@t�@33@��@n�@��@�7@x�@x�@X@G�@%@�9@�u@r�@A�@A�@1'@ �@�;@|�@
=@ȴ@��@E�@5?@5?@@�T@@��@`B@/@V@V@��@�/@�/@��@�j@�@�@�@�D@j@j@Z@�m@�@�@t�@S�@�H@~�@M�@=q@-@-@-@��@�#@��@�^@��@��@��@hs@7L@7L@�@��@�9@��@�u@�u@�@r�@Q�@1'@b@��@��@;d@
=@�y@�@�@ȴ@ȴ@�R@��@ff@E�@5?@{@�T@@@�-@�-@�-@�-@��@��@��@�h@�h@�@O�@?}@/@�@��@�j@�@�D@j@(�@��@�
@�F@��@��@�@�@t�@dZ@C�@33@"�@
�@
��@
��@
��@
��@
~�@	��@	��@	��@	�7@	X@	G�@	X@	G�@	&�@�`@��@�@�@r�@bN@Q�@Q�@Q�@Q�@A�@ �@1'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B:^B9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^BA�B�;B��B��B��B��B��BB	7B�B!�B,BE�BiyB� B�DB��BĜB��B�TB�B�B��B�B�B�HB�;B�BB+BB��B��B�B�B��B�BoB�B�ZB�fB�B�/B�?B�B�'B�B��B��B�B��B��B��B�hB�Bu�Bm�Bv�Bq�BiyB_;BR�BA�B+BoB��B��BǮBB�RB�B��B�VB�PBgmBD�B?}B0!B	7B
��B
ƨB
��B
��B
�1B
p�B
l�B
p�B
l�B
_;B
\)B
R�B
H�B
=qB
:^B
6FB
2-B
,B
!�B
�B
�B
�B
bB
B	�B	�)B	��B	ÖB	�}B	�-B	�uB	�+B	� B	w�B	m�B	cTB	M�B	E�B	?}B	9XB	33B	-B	(�B	%�B	 �B	�B	PB	
=B	PB	B��B��B��B��B�`B�/B�)B��B��BŢBĜB�}B�wB�qB�qB�dB�^B�9B�!B�B�B��B��B��B��B��B��B��B�oB�PB�DB�DB�7B�1B�%B�B�B�B�B� B|�By�Bw�Bs�Bs�Bv�Bw�B}�B�DB�hB��B��B�B�dBBƨBǮBȴBƨBŢBĜB�^B�FB��B�B��B��B��B��B��B�'B�3B�?B�3B�'B�B��B��B��B��B��B��B�-B�B�!B�B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�B�B�3B�9B�RB�wB��BĜBǮBǮB��B��B��B��B��B��B��B�
B�B�
B�B�#B�#B�/B�BB�TB�`B�B�B�B��B��B��B��B	  B	B	B	B	B	+B	\B	\B	\B	bB	�B	�B	�B	%�B	+B	+B	,B	,B	/B	/B	/B	0!B	49B	6FB	9XB	<jB	?}B	D�B	F�B	I�B	K�B	O�B	R�B	T�B	VB	W
B	XB	XB	YB	[#B	XB	W
B	ZB	e`B	ffB	e`B	dZB	cTB	e`B	e`B	ffB	ffB	l�B	q�B	u�B	y�B	|�B	~�B	�B	�B	�%B	�1B	�\B	�JB	�PB	�PB	�bB	�hB	�hB	�\B	�\B	�\B	�bB	�VB	�\B	��B	��B	��B	��B	��B	��B	�LB	�LB	�LB	�9B	�?B	�FB	�^B	�dB	�RB	�9B	�dB	�qB	�RB	�9B	�9B	�?B	�RB	�dB	B	��B	��B	�#B	�/B	�;B	�NB	�TB	�TB	�NB	�HB	�BB	�;B	�5B	�/B	�)B	�/B	�#B	�#B	�)B	�#B	�)B	�;B	�/B	�#B	�#B	�B	�#B	�#B	�5B	�HB	�NB	�HB	�HB	�HB	�NB	�NB	�ZB	�mB	�sB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
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
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
=qB
>wB
>wB
?}B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
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
E�B
E�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
H�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
K�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
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
_;B
`BB
`BB
`BB
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
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
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
{�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B:^B9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:xB:�B;B<�BIRB�B��B�RB�RB��B��B%B�BqB#�B-)BF�BkB�{B�4B�DBƨB�B�
B�B�BAB�BޞB�ZB�TB�B�B
#B�B��B��B�|B��B�B�B9B�[B��B�_B�!B��B�+B��B��B��B�KB�mB��B�kB�=B�$B�uB�BwLBoBxRBs3BkQBa-BU2BDMB.cB�B��B�,BȴB��B��B��B��B�4B��BjKBFBB�B4�B�B
�B
ʌB
��B
��B
�B
s3B
m�B
r-B
m�B
`BB
]�B
U2B
J#B
>BB
;dB
7B
3�B
-�B
"�B
B
QB
�B
[B
YB	�OB	ބB	͹B	��B	��B	�FB	��B	�KB	��B	y�B	p�B	gB	O�B	GEB	@�B	:�B	4�B	./B	)�B	'�B	#:B	kB	�B	~B	�B	�B�VB��B��B��B�RB�VBߤBյB�B�BŢB� B��B��B��B��B�VB�?B�[B�UB�]B�$B��B�IB�]B��B��B��B��B�VB�dB��B��B�B�+B�?B�mB�SB�3B� B~�B{�By	BtTBt�Bw�By>B~�B�DB�hB��B�XB��B�jB��B�+BȚB�7B�zB��B�?B��B��B�KB��B�B��B�nB��B��B��B��B��B��B�hB��B��B��B�_B��B��B��B�hB�!B�[B��B��B��B��B��B�`B�sB�_B��B�kB��B�qB��B��B��B�iB��B��B�	B��B��B��B��B��B�B�<B��B�bB�}B�[B�gBרB�B�_BںB�qBیBݘB��B��B��B�B��B�B��B�B�8B�B	 4B	UB	oB	GB	mB	�B	�B	�B	\B	HB	�B	�B	/B	&fB	+6B	+6B	,qB	,�B	/�B	/�B	/�B	0�B	4�B	6�B	9�B	<�B	@ B	EB	F�B	J#B	LdB	P.B	SB	T�B	VB	W$B	XEB	XyB	Y�B	\B	X�B	V�B	Y�B	e�B	f�B	e�B	d�B	c�B	e�B	ezB	ffB	fB	lWB	q�B	u�B	z^B	|�B	~�B	�3B	�SB	�YB	�KB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�pB	�vB	��B	��B	��B	��B	�VB	�@B	�fB	��B	�B	�nB	�ZB	�`B	��B	�B	��B	�B	�B	��B	��B	�nB	�TB	�B	�B	��B	��B	�DB	бB	�	B	�B	�VB	�B	�B	�B	�B	�B	��B	ߤB	ޞB	ݲB	ܬB	�~B	�qB	�qB	ܒB	�qB	�]B	ߤB	ݲB	یB	�qB	�QB	�=B	�	B	�5B	�B	��B	�B	�|B	�|B	�B	�4B	�@B	�mB	�XB	�yB	�B	�B	��B	�B	�B	��B	��B	�8B	�>B	�B	�B	�B	�B	�$B	�xB	�2B	�B	��B	�fB	��B
 B
'B
[B
GB
3B
MB
gB
SB
gB
mB
tB
?B
zB
KB
fB
�B
	�B
	�B

rB
xB
~B
~B
~B
�B
�B
�B
�B
pB
�B
�B
\B
}B
}B
bB
}B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
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
B
!B
�B
B
5B
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
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
;B
!�B
"B
!�B
"�B
#B
#B
#�B
%B
$�B
$�B
%B
%B
&2B
'B
'B
'B
(
B
(>B
)*B
)B
)*B
*B
*0B
*0B
*B
+B
+6B
+QB
,=B
-)B
-)B
-CB
.IB
.B
./B
./B
./B
.IB
/OB
/OB
0;B
0;B
1AB
1AB
1AB
1AB
1AB
1AB
1[B
2aB
2B
2-B
2GB
2GB
2aB
3MB
3hB
3hB
4TB
4TB
4TB
4nB
5�B
5�B
6zB
6zB
6`B
6zB
7fB
7�B
7fB
7LB
7fB
7fB
8RB
8lB
8RB
8lB
9rB
9rB
:DB
:xB
:^B
:^B
:xB
;B
;dB
;B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
=�B
>�B
>�B
?�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
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
E�B
E�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
H�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
K�B
L�B
L�B
M�B
M�B
L�B
NB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
OB
O�B
O�B
PB
Q B
QB
QB
Q B
QB
PB
Q B
Q B
Q B
RB
R B
S@B
TB
T�B
T�B
T�B
TB
TB
UB
UB
UB
UMB
VSB
VB
W$B
W$B
W$B
W
B
W$B
X+B
XEB
XEB
XEB
XEB
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
ZkB
\)B
\CB
\CB
\CB
\CB
\CB
]IB
]B
]IB
]IB
]/B
]IB
]IB
^OB
^OB
^jB
^OB
_VB
_pB
_;B
_;B
_;B
_pB
`�B
`\B
`\B
abB
abB
abB
aHB
abB
abB
a|B
abB
abB
bNB
bhB
cnB
cnB
cnB
cnB
cTB
cnB
dZB
dtB
d@B
dZB
dZB
dtB
ezB
ezB
ezB
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
gmB
h�B
h�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
q�B
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
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
|B
|B
|B
{�B
{�B
{�B
{�B
{�B
{�B
|B
{�B
|B
{�B
|B
{�B
|�B
|�B
}B
}"B
|B
|B
|B
z�B
z�B
z�B
{�B
|B
|B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607270033502016072700335020160727003350201806221259452018062212594520180622125945201804050658302018040506583020180405065830  JA  ARFMdecpA19c                                                                20160723093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160723003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160723003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160723003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160723003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160723003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160723003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160723003530  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160723003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160723003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20160723012037                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160723153737  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160723153737  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20160723153737  CV  LATITUDE        G�O�G�O�A�hs                JM  ARGQJMQC2.0                                                                 20160723153737  CV  LONGITUDE       G�O�G�O��$�u                JM  ARCAJMQC2.0                                                                 20160726153350  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160726153350  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215830  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035945  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                