CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-18T00:35:18Z creation;2016-09-18T00:35:20Z conversion to V3.1;2019-12-19T08:26:25Z update;     
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
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160918003518  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA  I2_0577_039                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�˵�,_�1   @�˶����@3�1&�y�d�>�6z1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,)C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �Dz=D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;z=D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds
Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD��D�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD�ÅD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�bNA�VA�ZA�K�A�K�A�33A�"�A�&�A� �A� �A� �A� �A��A��A��A��A��A�{A�bA�oA�oA�oA�oA�{A��A��A�{A�{A�oA�%A��#Aޙ�A���A�G�A��
A֓uA�{A�ȴA��AӇ+Aѧ�A·+AˮA��A���A�ƨA�-AǛ�A�r�A�XAĮA�\)A�9XA��A�ȴA��9A�bNA�oA��A��`A��A���A��A���A���A�bA��;A���A��A�dZA�A���A�\)A�bA�1A���A��^A�~�A��A��wA�S�A��A���A���A�x�A��9A�hsA�A���A�C�A��!A�S�A�{A��^A��A�K�A��TA�hsA�\)A��TA���A�  A��;A��A�C�A�%A��yA��9A��RA�A�=qA�
=A���A�G�A��A�G�A��DA��A��A��mA�r�A��A��wA��DA�G�A�1A��!A�A��A~M�A|�HAw�;AuC�As��Aq�AnE�AjbNAfM�Ac�^A`1'A]��A\bAZ�AX�AU��AR�AP1AO;dAN�ANZAMXAL�AI��AHz�AGƨAGG�AGO�AG
=AES�ACK�AAA?�#A?�hA>JA<A9
=A7C�A5�FA3K�A0��A/�7A.jA-��A,��A+��A*�A'K�A$�/A$9XA#33A!�#A!+A ��AdZAbA��AI�A�wA��A�hA�uA;dA��A�`A�/A(�AG�A+A"�AI�AG�Av�A��A�^A�hAO�AA{A33A�`A��A��AQ�A�Al�A	�
A	�A��AbAl�Ax�A��A�AO�AA��Ar�A��A ��@�l�@���@�r�@��#@�%@���@��@���@��j@�^5@�-@��D@��@��y@��@���@��
@�\@��@�|�@�!@߮@�G�@�Z@�b@���@ڗ�@٩�@�hs@�&�@���@ؼj@��/@�I�@�S�@�
=@���@���@ԣ�@�"�@��T@У�@Ͼw@�
=@�@��@�A�@ˍP@ʏ\@�=q@ɡ�@���@�r�@�Q�@��m@��y@�$�@ŉ7@��@Ĵ9@�A�@� �@�1@��
@�@�p�@�G�@���@�I�@�1@���@�S�@��!@�G�@���@�9X@�K�@���@��!@�~�@�^5@�M�@�5?@���@��#@���@�j@��@��@�dZ@�+@�M�@���@��-@�p�@��9@�1'@��@�ƨ@��P@�C�@���@��!@�v�@�E�@�-@���@��@��@��D@�bN@�1'@��
@�dZ@���@���@��@��^@��7@�p�@�X@��@�%@�Ĝ@��@���@�;d@���@�M�@�@���@��h@�V@�Ĝ@��9@��u@��@��m@���@�\)@�"�@���@���@�~�@��^@�?}@��@���@��9@��@��u@��D@��;@�\)@�;d@�
=@��R@��+@���@�@���@���@�X@��/@��@�z�@�Q�@�b@��;@��m@��P@�C�@�ȴ@�ff@�$�@���@���@��h@��@��@�I�@��m@���@�t�@�t�@�dZ@�S�@�t�@��y@���@�M�@�{@�$�@�^5@���@��\@�E�@��@��h@�x�@�`B@�O�@�x�@��@��@��@��@��m@���@�l�@�
=@���@��+@��+@�v�@�M�@�{@��7@��@��9@�bN@�1'@�t�@�+@��@��+@�n�@�V@�E�@�=q@�5?@���@��^@���@��@�hs@��@��`@��9@��u@�j@�I�@�1'@� �@��@�  @��@��w@��@�S�@�"�@��@�@��H@���@�E�@�$�@��@�@��@��-@���@�p�@�X@��@�%@���@��9@�(�@�  @���@�\)@��R@�n�@�M�@�5?@�-@�{@��@���@��h@�`B@�V@��j@���@�9X@�|�@�+@�@���@�J@��^@��-@���@��-@�x�@�O�@�?}@��@��`@�bN@��
@��@�S�@�"�@�@��y@��@��\@�M�@��@�{@�J@���@���@�@�?}@��`@���@�Ĝ@��9@���@�Q�@�(�@��@�1@��@�w@\)@+@~��@~V@}O�@}�@|�/@|��@|Z@{�
@{@z�!@z��@zM�@y��@x��@x��@w�@wK�@vE�@u�T@uO�@tz�@sS�@r�@r��@r�\@r~�@r^5@r�@q�@q�^@q&�@pbN@o��@o�P@o|�@o\)@o;d@o;d@n�R@m�@m��@m�@m`B@l�j@lj@lI�@lI�@l9X@l1@k�
@kC�@j��@j��@j^5@i�#@i��@i��@i�7@i%@h�@hb@g�@g��@g��@g��@f�@f��@f�+@f$�@d��@dz�@d9X@c�
@ct�@b��@b�@ax�@`�u@`r�@`  @_�P@_+@_
=@^��@^V@^@]�T@]�h@\�@\��@\9X@[ƨ@[�F@[33@Z~�@Z=q@Y�#@Y��@Y��@YX@Y%@X��@X��@X�u@Xr�@XQ�@W�w@W
=@Vff@U�@UO�@T�/@T�@S�
@S��@S�@St�@SC�@SC�@S"�@So@R�H@RM�@Q�^@Qhs@QG�@P��@Pr�@P �@O�@O\)@N��@N��@Nv�@N$�@M�T@M��@M�@M?}@L9X@K�F@KdZ@KC�@J�H@J�!@J�\@J~�@Jn�@JM�@JJ@I�^@I��@I��@I��@I��@I��@Ix�@IX@H��@HA�@G��@G|�@G�@F�@F��@F$�@Ep�@D��@Dz�@C�m@C��@C33@B��@B~�@B=q@A��@AX@@�`@@�9@@��@@�@@Q�@@A�@@ �@?�@?l�@?
=@>�@>ff@=��@=�-@=�h@=�@=O�@=V@<�D@;�
@;��@;dZ@;"�@:��@:M�@:M�@9�@9�7@8��@8r�@8 �@7|�@7K�@7;d@6��@6�@6��@6@5�@4��@4�j@4z�@4I�@4�@3ƨ@3��@3"�@2��@2=q@1��@1�7@1&�@1%@0��@0��@0Q�@/�@/�P@/\)@/K�@/�@.�R@.��@.v�@-��@-?}@,�D@,�@+�m@+ƨ@+t�@+"�@*��@*��@*�\@*�\@*~�@*M�@*-@)�#@)X@(��@(r�@( �@'��@'
=@&�y@&ȴ@&��@&5?@&{@%�@%�T@%@%��@%�@%O�@%/@%�@%�@$�/@$Z@$9X@$(�@$�@$�@$1@#�m@#"�@"��@"n�@!�@!G�@!%@!%@ Ĝ@ �u@ bN@ A�@ A�@ 1'@  �@�@�w@��@�P@l�@�y@E�@$�@{@@�T@��@��@�-@��@�@O�@?}@�@�/@�/@�/@��@�D@I�@�@�F@�@C�@~�@=q@-@J@��@��@hs@7L@%@�`@Ĝ@�9@�@bN@A�@b@�@�;@�w@l�@��@��@��@ff@V@V@5?@�T@�-@��@�h@�h@�@�@p�@`B@�@��@�@�D@�D@�D@�D@�D@�D@z�@j@�@��@C�@"�@@�H@��@��@^5@�#@��@�7@hs@G�@&�@�@�@%@��@�`@��@�9@�u@r�@bN@A�@ �@ �@  @�;@�;@��@��@�w@�w@�w@�w@�w@�w@�w@�P@\)@�y@�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�bNA�VA�ZA�K�A�K�A�33A�"�A�&�A� �A� �A� �A� �A��A��A��A��A��A�{A�bA�oA�oA�oA�oA�{A��A��A�{A�{A�oA�%A��#Aޙ�A���A�G�A��
A֓uA�{A�ȴA��AӇ+Aѧ�A·+AˮA��A���A�ƨA�-AǛ�A�r�A�XAĮA�\)A�9XA��A�ȴA��9A�bNA�oA��A��`A��A���A��A���A���A�bA��;A���A��A�dZA�A���A�\)A�bA�1A���A��^A�~�A��A��wA�S�A��A���A���A�x�A��9A�hsA�A���A�C�A��!A�S�A�{A��^A��A�K�A��TA�hsA�\)A��TA���A�  A��;A��A�C�A�%A��yA��9A��RA�A�=qA�
=A���A�G�A��A�G�A��DA��A��A��mA�r�A��A��wA��DA�G�A�1A��!A�A��A~M�A|�HAw�;AuC�As��Aq�AnE�AjbNAfM�Ac�^A`1'A]��A\bAZ�AX�AU��AR�AP1AO;dAN�ANZAMXAL�AI��AHz�AGƨAGG�AGO�AG
=AES�ACK�AAA?�#A?�hA>JA<A9
=A7C�A5�FA3K�A0��A/�7A.jA-��A,��A+��A*�A'K�A$�/A$9XA#33A!�#A!+A ��AdZAbA��AI�A�wA��A�hA�uA;dA��A�`A�/A(�AG�A+A"�AI�AG�Av�A��A�^A�hAO�AA{A33A�`A��A��AQ�A�Al�A	�
A	�A��AbAl�Ax�A��A�AO�AA��Ar�A��A ��@�l�@���@�r�@��#@�%@���@��@���@��j@�^5@�-@��D@��@��y@��@���@��
@�\@��@�|�@�!@߮@�G�@�Z@�b@���@ڗ�@٩�@�hs@�&�@���@ؼj@��/@�I�@�S�@�
=@���@���@ԣ�@�"�@��T@У�@Ͼw@�
=@�@��@�A�@ˍP@ʏ\@�=q@ɡ�@���@�r�@�Q�@��m@��y@�$�@ŉ7@��@Ĵ9@�A�@� �@�1@��
@�@�p�@�G�@���@�I�@�1@���@�S�@��!@�G�@���@�9X@�K�@���@��!@�~�@�^5@�M�@�5?@���@��#@���@�j@��@��@�dZ@�+@�M�@���@��-@�p�@��9@�1'@��@�ƨ@��P@�C�@���@��!@�v�@�E�@�-@���@��@��@��D@�bN@�1'@��
@�dZ@���@���@��@��^@��7@�p�@�X@��@�%@�Ĝ@��@���@�;d@���@�M�@�@���@��h@�V@�Ĝ@��9@��u@��@��m@���@�\)@�"�@���@���@�~�@��^@�?}@��@���@��9@��@��u@��D@��;@�\)@�;d@�
=@��R@��+@���@�@���@���@�X@��/@��@�z�@�Q�@�b@��;@��m@��P@�C�@�ȴ@�ff@�$�@���@���@��h@��@��@�I�@��m@���@�t�@�t�@�dZ@�S�@�t�@��y@���@�M�@�{@�$�@�^5@���@��\@�E�@��@��h@�x�@�`B@�O�@�x�@��@��@��@��@��m@���@�l�@�
=@���@��+@��+@�v�@�M�@�{@��7@��@��9@�bN@�1'@�t�@�+@��@��+@�n�@�V@�E�@�=q@�5?@���@��^@���@��@�hs@��@��`@��9@��u@�j@�I�@�1'@� �@��@�  @��@��w@��@�S�@�"�@��@�@��H@���@�E�@�$�@��@�@��@��-@���@�p�@�X@��@�%@���@��9@�(�@�  @���@�\)@��R@�n�@�M�@�5?@�-@�{@��@���@��h@�`B@�V@��j@���@�9X@�|�@�+@�@���@�J@��^@��-@���@��-@�x�@�O�@�?}@��@��`@�bN@��
@��@�S�@�"�@�@��y@��@��\@�M�@��@�{@�J@���@���@�@�?}@��`@���@�Ĝ@��9@���@�Q�@�(�@��@�1@��@�w@\)@+@~��@~V@}O�@}�@|�/@|��@|Z@{�
@{@z�!@z��@zM�@y��@x��@x��@w�@wK�@vE�@u�T@uO�@tz�@sS�@r�@r��@r�\@r~�@r^5@r�@q�@q�^@q&�@pbN@o��@o�P@o|�@o\)@o;d@o;d@n�R@m�@m��@m�@m`B@l�j@lj@lI�@lI�@l9X@l1@k�
@kC�@j��@j��@j^5@i�#@i��@i��@i�7@i%@h�@hb@g�@g��@g��@g��@f�@f��@f�+@f$�@d��@dz�@d9X@c�
@ct�@b��@b�@ax�@`�u@`r�@`  @_�P@_+@_
=@^��@^V@^@]�T@]�h@\�@\��@\9X@[ƨ@[�F@[33@Z~�@Z=q@Y�#@Y��@Y��@YX@Y%@X��@X��@X�u@Xr�@XQ�@W�w@W
=@Vff@U�@UO�@T�/@T�@S�
@S��@S�@St�@SC�@SC�@S"�@So@R�H@RM�@Q�^@Qhs@QG�@P��@Pr�@P �@O�@O\)@N��@N��@Nv�@N$�@M�T@M��@M�@M?}@L9X@K�F@KdZ@KC�@J�H@J�!@J�\@J~�@Jn�@JM�@JJ@I�^@I��@I��@I��@I��@I��@Ix�@IX@H��@HA�@G��@G|�@G�@F�@F��@F$�@Ep�@D��@Dz�@C�m@C��@C33@B��@B~�@B=q@A��@AX@@�`@@�9@@��@@�@@Q�@@A�@@ �@?�@?l�@?
=@>�@>ff@=��@=�-@=�h@=�@=O�@=V@<�D@;�
@;��@;dZ@;"�@:��@:M�@:M�@9�@9�7@8��@8r�@8 �@7|�@7K�@7;d@6��@6�@6��@6@5�@4��@4�j@4z�@4I�@4�@3ƨ@3��@3"�@2��@2=q@1��@1�7@1&�@1%@0��@0��@0Q�@/�@/�P@/\)@/K�@/�@.�R@.��@.v�@-��@-?}@,�D@,�@+�m@+ƨ@+t�@+"�@*��@*��@*�\@*�\@*~�@*M�@*-@)�#@)X@(��@(r�@( �@'��@'
=@&�y@&ȴ@&��@&5?@&{@%�@%�T@%@%��@%�@%O�@%/@%�@%�@$�/@$Z@$9X@$(�@$�@$�@$1@#�m@#"�@"��@"n�@!�@!G�@!%@!%@ Ĝ@ �u@ bN@ A�@ A�@ 1'@  �@�@�w@��@�P@l�@�y@E�@$�@{@@�T@��@��@�-@��@�@O�@?}@�@�/@�/@�/@��@�D@I�@�@�F@�@C�@~�@=q@-@J@��@��@hs@7L@%@�`@Ĝ@�9@�@bN@A�@b@�@�;@�w@l�@��@��@��@ff@V@V@5?@�T@�-@��@�h@�h@�@�@p�@`B@�@��@�@�D@�D@�D@�D@�D@�D@z�@j@�@��@C�@"�@@�H@��@��@^5@�#@��@�7@hs@G�@&�@�@�@%@��@�`@��@�9@�u@r�@bN@A�@ �@ �@  @�;@�;@��@��@�w@�w@�w@�w@�w@�w@�w@�P@\)@�y@�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B+B+B+B)�B)�B(�B'�B'�B'�B'�B'�B'�B'�B&�B&�B&�B&�B&�B&�B&�B&�B&�B%�B&�B&�B&�B&�B&�B&�B&�B)�B`BB�B��B�-B�wBɺB��B�NB�sB�TB�B�B�#B�B�Bo�Bt�B��B��B�B�?B�9B�'B�B�B�?B�?B�LB�^BÖB��B�B�5B�`B�fB�mB�sB�B�B��BB%B\B�B�B�B�B�BhB\BJB
=BB��B�B�B�B�B�fB�HB�)B�B��B��BƨB�}B�XB�3B��B�oB�1Bz�Bk�Be`BW
BG�B<jB�B	7B�B�;B��B��B�jB�XB�-B�JBs�BcTBH�B&�BB
�B
��B
�wB
�B
�PB
x�B
n�B
bNB
I�B
0!B
%�B
bB	��B	�5B	��B	�B	��B	�B	y�B	p�B	dZB	P�B	@�B	33B	.B	.B	)�B	#�B	�B	�B	�B	�B	uB	uB	oB	DB	B��B��B��B��B�B�BB�
B��BǮB�}B�XB�9B�'B�B��B��B��B��B�uB�oB�\B�PB�DB�JB�%B�B�B�B�B|�B{�Bx�Bw�Bu�Bt�Bw�Bx�Bx�Bx�B|�By�B}�B�=B��B��B��B�-B�'B�B�?B�dB�jB�qB�jB�qB�RB�9B�?B�FB�-B�XB�jB�jB�jB�jB�dB�LB�B�B��B��B��B��B��B��B��B��B��B��B��B��B�3B�9B�B��B��B�B�B��B��B��B��B�B��B��B�B�B�!B�3B�dBBȴB��B��B��B��B�B�
B��B�
B�B�
B�B�/B�5B�ZB�mB�B�B�B��B��B��B��B	  B	B	%B		7B	DB	VB	VB	VB	\B	oB	�B	{B	�B	�B	�B	�B	�B	"�B	&�B	(�B	.B	5?B	7LB	7LB	8RB	8RB	9XB	:^B	>wB	A�B	C�B	M�B	S�B	T�B	VB	W
B	ZB	\)B	\)B	]/B	aHB	cTB	dZB	e`B	gmB	l�B	m�B	o�B	p�B	q�B	q�B	r�B	s�B	u�B	u�B	v�B	v�B	w�B	x�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�JB	�VB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�3B	�3B	�9B	�?B	�FB	�^B	�dB	�dB	�dB	�jB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	��B	�}B	��B	��B	��B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�TB	�ZB	�ZB	�ZB	�`B	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
oB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
oB
oB
hB
\B
\B
bB
bB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
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
/B
/B
/B
/B
/B
/B
/B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
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
8RB
9XB
9XB
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
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
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
M�B
M�B
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
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
T�B
T�B
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
XB
XB
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
ZB
ZB
[#B
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
]/B
]/B
^5B
^5B
^5B
^5B
_;B
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
bNB
bNB
bNB
cTB
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
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
iyB
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
l�B
l�B
l�B
m�B
m�B
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
o�B
p�B
o�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
t�B
t�B
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
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B+B+B+B*B*0B)B'�B'�B'�B'�B'�B'�B'�B&�B&�B&�B&�B&�B&�B&�B&�B&�B%�B&�B&�B&�B'B'B'�B)*B0oBe�B��B�|B�TB��B��B�:B��B�)B�B��B��B�IB�+B�Bp;BwfB��B��B�5B��B��B��B�!B�UB��B��B�	B��BƎBΊBڠB�'B�fB�B�>B��B��B�GB��B�B�BvB�BEB�B�BBTB.B�BdB�B�qB�|B��B�B�qB�B�B��B�BևBуB��B� B�B�`B�tB�FB��B}Bm�Bh>BY�BJ�B@iB!|B�B�|B��B�6B�GB��B�B�2B�(Bv�BgBM6B*�BEB
�B
�_B
B
��B
��B
z�B
qvB
gmB
L�B
2�B
)�B
aB	��B	��B	�3B	�;B	�kB	�%B	{�B	s�B	hXB	T{B	CGB	49B	.�B	/ B	+�B	%�B	"4B	CB	yB	B	�B	{B	�B	�B	3B	B��B�B��B�B�B�B��B�rB�UB��B�ZB��B��B�B�B�bB��B��B�B�bB�<B�6B�B��B�%B�B��B��B~�B}�Bz�Bx�Bv+Bu�Bx�By$ByXBz*B~(Bz�B~wB��B��B�\B��B�hB�-B��B��B��B��B�B��B�.B�XB�B�B��B�|B�xB�qB�VB�<B�B�<B��B�UB��B�B�B�_B��B��B��B��B�8B�QB�`B��B��B��B�+B��B�kB�B�IB�B�B��B�RB��B�QB�eB��B��B�OB�oB��B��BªB�7B�VB�.B�@BՁB�sB�B��B��BּBרB��B��B��B��B�
B��B�'B�9B�8B�B�jB��B	 �B	{B	tB		�B	�B	pB	�B	�B	B	[B	�B	�B	�B	�B	�B	�B	QB	#�B	'RB	)yB	.�B	5�B	7fB	7�B	8lB	8lB	9XB	:�B	>�B	A�B	DMB	N"B	TB	U2B	VSB	W�B	ZkB	\]B	\xB	]�B	a|B	cnB	d�B	e�B	g�B	l�B	m�B	o�B	p�B	q�B	q�B	sB	tB	vB	vB	v�B	wB	x8B	y	B	{B	}<B	HB	� B	� B	� B	�AB	�GB	�GB	��B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�2B	�mB	�KB	�=B	�)B	�/B	�B	�;B	�[B	��B	��B	�MB	�MB	�nB	�tB	��B	�xB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	��B	��B	��B	�NB	�,B	�2B	�$B	��B	�	B	�B	�nB	�B	�B	�B	�zB	�B	�B	�B	��B	��B	��B	��B	�2B	��B	��B	�*B	�B	�B	�B	�B	�(B	�(B	�BB	�HB	�HB	�HB	�HB
 �B
GB
MB
SB
?B
?B
?B
+B
EB
_B
_B
KB
KB
	RB
	�B

rB

rB

XB

XB
^B
^B
^B
DB
^B
^B
xB
~B
~B
dB
jB
jB
jB
�B
�B
pB
pB
pB
pB
�B
pB
vB
pB
�B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
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
!B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#B
"�B
# B
$B
$&B
$B
%,B
%B
&2B
&2B
'RB
'B
(
B
'�B
(
B
'�B
(
B
(
B
(
B
($B
)DB
*KB
)�B
*B
*B
*B
*B
*0B
*0B
+B
+B
+B
+6B
,"B
,"B
+�B
,"B
,B
,B
,=B
-)B
-B
-)B
-)B
./B
./B
./B
.IB
.IB
.cB
/5B
/B
/5B
/5B
/OB
/5B
/5B
/OB
/�B
1[B
1AB
1[B
1[B
2|B
2aB
3�B
3hB
4TB
4nB
4nB
5ZB
5ZB
5tB
5ZB
6`B
6`B
6zB
6zB
7fB
7fB
7�B
7fB
7�B
8�B
9XB
9rB
9rB
9rB
:xB
:xB
:xB
:�B
:�B
;dB
;B
;�B
;�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?cB
?�B
?�B
?}B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
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
NB
M�B
M�B
NB
M�B
M�B
M�B
M�B
M�B
M�B
NB
N�B
N�B
N�B
N�B
OB
O�B
PB
PB
O�B
QB
RB
R B
R B
SB
SB
SB
SB
SB
S&B
T,B
T,B
TB
UB
UB
UB
UB
T�B
UB
V9B
V9B
W$B
W?B
W$B
X+B
X+B
X+B
X+B
YB
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
ZQB
Z7B
[WB
\)B
\)B
\)B
\CB
]IB
]IB
]IB
]/B
]/B
]dB
]IB
]IB
]dB
^OB
^jB
^OB
^jB
_VB
`vB
`BB
`\B
`\B
`vB
abB
abB
aHB
abB
a|B
abB
bhB
bhB
bNB
bNB
bhB
b�B
cTB
cTB
c:B
cTB
cnB
cnB
c�B
dtB
d�B
d�B
e�B
e`B
e`B
ezB
f�B
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
g�B
gmB
g�B
h�B
hsB
hsB
hsB
h�B
hsB
iyB
h�B
iyB
i�B
i�B
i�B
i�B
jeB
jB
jB
i�B
iyB
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
l�B
l�B
l�B
m�B
m�B
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
n}B
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
o�B
p�B
o�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
t�B
t�B
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
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609220035532016092200355320160922003553201806221302202018062213022020180622130220201804050701432018040507014320180405070143  JA  ARFMdecpA19c                                                                20160918093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160918003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160918003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160918003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160918003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160918003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160918003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160918003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160918003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160918003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20160918012108                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160918153316  CV  JULD            G�O�G�O�F�]�                JM  ARGQJMQC2.0                                                                 20160918153316  CV  JULD_LOCATION   G�O�G�O�F�]�                JM  ARGQJMQC2.0                                                                 20160918153316  CV  LATITUDE        G�O�G�O�A��/                JM  ARGQJMQC2.0                                                                 20160918153316  CV  LONGITUDE       G�O�G�O��%��                JM  ARCAJMQC2.0                                                                 20160921153553  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160921153553  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220143  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040220  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                