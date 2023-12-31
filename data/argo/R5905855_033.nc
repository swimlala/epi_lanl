CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:16:27Z creation;2022-06-04T19:16:28Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604191627  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               !A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��^RL�A1   @��^���B@/Qhr� ��c�j~��#1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�ffB���B���B���B���B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C �C�C  C  C�C
�C�fC�fC�fC  C  C  C  C  C  C  C   C"L�C$  C%�fC'�fC*  C,  C.�C0�C2�C3�fC6  C7�fC:  C<  C>  C@  CB33CC�fCE�fCH  CI�fCL  CN  CP  CR  CT  CV  CX�CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�3D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=BXp�B`
=Bh
=Bp
=Bxp�B�B�B�B�B�B�B�B�B�8RB���B���B�B�k�B���B���B���B���B�B�B�8RB�k�B�B�B�B�B�B�B�B�B�B���B�C )C)C�C�C)C
)C��C��C��C�C�C�C�C�C�C�C �C"O\C$�C%��C'��C*�C,�C.)C0)C2)C3��C6�C7��C:�C<�C>�C@�CB5�CC��CE��CH�CI��CL�CN�CP�CR�CT�CV�CX)CZ�C\�C^)C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD��D�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD��D�C�D؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�0R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��?A��9A���A�ܒA��A��`A��A���A���A��+A��fA���A��A�oA�oA��A��A�1A��A�+A�!�A�%�A�%A�%�A�-CA�0�A�5A�8�A�B�A�MjA�PA�Y�A�\]A�[#A�ZQA�ZQA�YKA�XEA�U2A�=�AҦ�AжzA�d�A��#A�T�A�бA�g�A�uZA�qA��A��A�'�A�5tA�g8A�JA��UA�� A�GEA�=A�y	A���A�m)A��A���A��A�ҽA��tA�1�A��A��A��A�B�A�HKA�[WA���A��A�VmA��CA��XA�b�A�רA~#:Ax�Av��Atc Aoc Ag��Acp;Ab�A\��AT��AO�&AH@�AD$�A@�A>�A<i�A9?}A6oA3$A1�PA/�*A-ɆA-�hA-e�A,�&A+�A*bA)��A)XA) iA'f�A"�A iA[WAo A�+A��A��A�Ae�AI�A�+A��A�AY�A%FA��A�$AY�A�@A�XA�~AS�A�oA?�Aj�AxA�A��A�{A,�A��A�"AjA8�A`BA_A��A�A
�]A
��A
{�A	�A	Q�A:*A��At�AC�AQA��A��A��A�)A�A��AƨA�A��A��A;dA��A��A��Ah
A:�A��A�A4�A ��A �A �pA ;�@���@��X@�m]@��n@���@���@�$�@���@��'@���@�&�@�	@�X�@��@�9X@�"@���@�($@�iD@�'�@�g8@��@���@�Vm@��@�9@�`�@�6�@��@���@�U�@�V@�~(@�@���@�+�@�=�@曦@��Q@���@�4@�Ɇ@߼@�
=@ޏ\@ݲ�@��B@ܬ�@��@�/�@��@�T�@��)@�#:@�@ץ�@�L�@֢4@�u%@�!@�:*@��@��&@շ@�}�@�1�@�H�@�w2@�@��c@��"@���@�ѷ@�ں@Ҳ�@Ң4@Ҿ@ң�@Қ@�j@ю�@�:�@�ѷ@φ�@�Mj@��@��o@͓@�n/@�<6@̊r@���@�X@�Q@��@��W@ɇ�@��f@��	@�;@��@���@Ȑ.@�@ǌ~@�|�@�YK@��@�@ĕ@�'R@��a@�8�@�@�(�@��a@���@�c@�4@���@�O@���@��P@�u�@���@�v`@��|@���@��_@��h@���@���@�u%@�6�@�b@��6@���@�N<@�͟@��F@�kQ@�6@��Q@���@���@�D�@�0U@��r@�^�@�{�@�!@���@���@���@�Q�@��|@��j@�m�@��@�ƨ@�e�@��v@��$@�:*@��a@��	@�/�@��@�I�@��r@��q@�X@��@�Ɇ@�(�@���@�iD@�@@�֡@�ff@��)@���@�T�@�1�@��@���@��+@�:*@�  @��H@�k�@�)_@��@�ں@���@�GE@��]@��#@���@�f�@�)_@���@�]d@�4@��K@�{J@�-w@�Ov@���@��@@�B�@��L@�C-@��@��@�\�@�8@��@�l"@�V�@�-@��T@���@�n/@� \@��c@�}V@�M@��4@��@��c@��@�I�@���@���@���@�zx@�&@��@��R@���@�9X@���@�Mj@��H@��@�E�@�1�@�	@��>@�x@�1�@�!�@���@�J�@��@�{@���@�B�@�8@�'�@��/@�h�@��@��W@���@��@�s�@� \@���@�� @�9X@��z@��P@�A�@��@��/@���@���@�.�@�#:@� �@��@�e,@�V@��@���@�:*@�,=@���@�N<@��@���@��@��O@��+@�u�@�#:@��@��@���@�k�@�Q�@�&�@��v@��F@�YK@�O@��@���@�u�@�S@��p@���@�u%@�_@�W�@�K^@���@���@�y�@�U�@�<6@��@��M@��.@�:*@���@��M@�[W@�-w@�Y@�	l@��@���@���@�~�@�I�@��]@��*@�a�@�&@���@���@���@���@�Z@��@���@�Z�@�Y@��f@�Z�@���@�6z@�E9@��@���@�bN@�+k@��@��@��@$t@~��@~5?@}ϫ@}��@}S&@|��@|�@{ݘ@{6z@z�h@y�o@yL�@x�K@x]d@w� @w�	@v��@va|@v($@up�@t�@t1@s�@r��@q�@q�@p��@p��@p_@p%�@p�@o�@o|�@n��@n^5@n&�@n4@m��@m��@m�@l��@lU2@k��@k��@kF�@j��@j�@i�~@i�@h�I@h4n@g�m@g�@gJ#@g+@gS@f��@e��@e�@d��@d��@d��@dZ@cx@bߤ@b��@a�@a�@a`B@`�?@`|�@`r�@`K^@`6@_�@_s@^�c@^Z�@^B[@^6�@]�D@]��@]�@\|�@\%�@[�@[j�@Z��@Zff@Z\�@Z#:@Ys�@Y[W@YN<@X�f@XA�@W]�@V�1@V1�@U��@U�)@U�@U��@U�@Ux�@U�@T��@Tm�@TH@T6@T~@T �@T7@S�+@S�F@Sb�@S@O@S�@R�<@R\�@R0U@Q�.@Q�"@QL�@P�$@PQ�@O�	@O�@N��@N1�@M|@M�@L�@L�@L��@L��@LI�@K��@J��@J�\@J3�@I��@I�7@I^�@I*0@I�@H�D@HPH@G�;@GH�@F�8@F�A@FL0@F0U@E��@E��@E�=@Em]@E*0@D�K@Dh�@D  @Cƨ@C.I@B5?@A�@A�o@A�"@Ao @@�@?Mj@>�<@>�@?\)@?9�@>�@=p�@<��@<�@;��@;�a@;��@;+@:�@:��@:Q@:O@9�z@9�"@9IR@9�@8��@8c�@7��@7��@7�@6u%@6Q@65?@5��@5�~@5/@5�@4�U@4D�@3�@3�a@3�f@3Y@2�,@2��@2��@2~�@2;�@2�@1�@1�@1�^@1 \@0��@0��@0�u@0l"@0!@/��@/ƨ@/��@/��@/��@/j�@/]�@/@O@/�@.�6@.-@-��@-��@--w@,��@,��@,��@,S�@+��@+��@+�4@+_p@+4�@*�y@*�R@*��@*ff@*.�@)�Z@)�d@)m]@)@(�p@(�e@(c�@(1'@'�Q@'��@'��@'U�@'/�@'�@&��@&��@&�1@&!�@%�z@%��@%��@%L�@%4@%	l@$��@$�z@$I�@#��@#�K@#�q@#9�@#@"�]@"�@"xl@"0U@!�d@![W@!�@ ��@ ��@ ]d@ A�@ @��@�q@g�@ i@�@�y@�,@�@��@kQ@_�@^5@E�@1�@�@�)@�H@��@rG@Y�@x�@	l@�O@��@z�@Z@�@ƨ@�	@_p@'�@�]@��@M�@4@�)@��@��@k�@N<@?}@�	@Ĝ@��@m�@Xy@Q�@C-@�@�]@�;@�@��@��@��@6z@C@�@�@��@kQ@R�@5?@3�@�@�C@��@k�@F@%F@�/@��@~(@_@Xy@/�@%�@  @�@��@s@P�@'�@��@�,@��@a|@�@u@�@�@��@�X@�h@�@k�@J�@@�@�@��@��@M@x@��@~�@{J@x@n/@K�@'�@�@��@�@ߤ@�s@�'@�x@u%@Ta@=q@$�@�@ �@��@�#@��@��@�@k�@[W@Y�@X@O�@B�@+�@%@�K@�@��@��@��@�@��@�@6@$@M@��@�w@��@H�@
�y@
��@
��@
�R@
�L@
�@
�r@
l�@
?@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��?A��9A���A�ܒA��A��`A��A���A���A��+A��fA���A��A�oA�oA��A��A�1A��A�+A�!�A�%�A�%A�%�A�-CA�0�A�5A�8�A�B�A�MjA�PA�Y�A�\]A�[#A�ZQA�ZQA�YKA�XEA�U2A�=�AҦ�AжzA�d�A��#A�T�A�бA�g�A�uZA�qA��A��A�'�A�5tA�g8A�JA��UA�� A�GEA�=A�y	A���A�m)A��A���A��A�ҽA��tA�1�A��A��A��A�B�A�HKA�[WA���A��A�VmA��CA��XA�b�A�רA~#:Ax�Av��Atc Aoc Ag��Acp;Ab�A\��AT��AO�&AH@�AD$�A@�A>�A<i�A9?}A6oA3$A1�PA/�*A-ɆA-�hA-e�A,�&A+�A*bA)��A)XA) iA'f�A"�A iA[WAo A�+A��A��A�Ae�AI�A�+A��A�AY�A%FA��A�$AY�A�@A�XA�~AS�A�oA?�Aj�AxA�A��A�{A,�A��A�"AjA8�A`BA_A��A�A
�]A
��A
{�A	�A	Q�A:*A��At�AC�AQA��A��A��A�)A�A��AƨA�A��A��A;dA��A��A��Ah
A:�A��A�A4�A ��A �A �pA ;�@���@��X@�m]@��n@���@���@�$�@���@��'@���@�&�@�	@�X�@��@�9X@�"@���@�($@�iD@�'�@�g8@��@���@�Vm@��@�9@�`�@�6�@��@���@�U�@�V@�~(@�@���@�+�@�=�@曦@��Q@���@�4@�Ɇ@߼@�
=@ޏ\@ݲ�@��B@ܬ�@��@�/�@��@�T�@��)@�#:@�@ץ�@�L�@֢4@�u%@�!@�:*@��@��&@շ@�}�@�1�@�H�@�w2@�@��c@��"@���@�ѷ@�ں@Ҳ�@Ң4@Ҿ@ң�@Қ@�j@ю�@�:�@�ѷ@φ�@�Mj@��@��o@͓@�n/@�<6@̊r@���@�X@�Q@��@��W@ɇ�@��f@��	@�;@��@���@Ȑ.@�@ǌ~@�|�@�YK@��@�@ĕ@�'R@��a@�8�@�@�(�@��a@���@�c@�4@���@�O@���@��P@�u�@���@�v`@��|@���@��_@��h@���@���@�u%@�6�@�b@��6@���@�N<@�͟@��F@�kQ@�6@��Q@���@���@�D�@�0U@��r@�^�@�{�@�!@���@���@���@�Q�@��|@��j@�m�@��@�ƨ@�e�@��v@��$@�:*@��a@��	@�/�@��@�I�@��r@��q@�X@��@�Ɇ@�(�@���@�iD@�@@�֡@�ff@��)@���@�T�@�1�@��@���@��+@�:*@�  @��H@�k�@�)_@��@�ں@���@�GE@��]@��#@���@�f�@�)_@���@�]d@�4@��K@�{J@�-w@�Ov@���@��@@�B�@��L@�C-@��@��@�\�@�8@��@�l"@�V�@�-@��T@���@�n/@� \@��c@�}V@�M@��4@��@��c@��@�I�@���@���@���@�zx@�&@��@��R@���@�9X@���@�Mj@��H@��@�E�@�1�@�	@��>@�x@�1�@�!�@���@�J�@��@�{@���@�B�@�8@�'�@��/@�h�@��@��W@���@��@�s�@� \@���@�� @�9X@��z@��P@�A�@��@��/@���@���@�.�@�#:@� �@��@�e,@�V@��@���@�:*@�,=@���@�N<@��@���@��@��O@��+@�u�@�#:@��@��@���@�k�@�Q�@�&�@��v@��F@�YK@�O@��@���@�u�@�S@��p@���@�u%@�_@�W�@�K^@���@���@�y�@�U�@�<6@��@��M@��.@�:*@���@��M@�[W@�-w@�Y@�	l@��@���@���@�~�@�I�@��]@��*@�a�@�&@���@���@���@���@�Z@��@���@�Z�@�Y@��f@�Z�@���@�6z@�E9@��@���@�bN@�+k@��@��@��@$t@~��@~5?@}ϫ@}��@}S&@|��@|�@{ݘ@{6z@z�h@y�o@yL�@x�K@x]d@w� @w�	@v��@va|@v($@up�@t�@t1@s�@r��@q�@q�@p��@p��@p_@p%�@p�@o�@o|�@n��@n^5@n&�@n4@m��@m��@m�@l��@lU2@k��@k��@kF�@j��@j�@i�~@i�@h�I@h4n@g�m@g�@gJ#@g+@gS@f��@e��@e�@d��@d��@d��@dZ@cx@bߤ@b��@a�@a�@a`B@`�?@`|�@`r�@`K^@`6@_�@_s@^�c@^Z�@^B[@^6�@]�D@]��@]�@\|�@\%�@[�@[j�@Z��@Zff@Z\�@Z#:@Ys�@Y[W@YN<@X�f@XA�@W]�@V�1@V1�@U��@U�)@U�@U��@U�@Ux�@U�@T��@Tm�@TH@T6@T~@T �@T7@S�+@S�F@Sb�@S@O@S�@R�<@R\�@R0U@Q�.@Q�"@QL�@P�$@PQ�@O�	@O�@N��@N1�@M|@M�@L�@L�@L��@L��@LI�@K��@J��@J�\@J3�@I��@I�7@I^�@I*0@I�@H�D@HPH@G�;@GH�@F�8@F�A@FL0@F0U@E��@E��@E�=@Em]@E*0@D�K@Dh�@D  @Cƨ@C.I@B5?@A�@A�o@A�"@Ao @@�@?Mj@>�<@>�@?\)@?9�@>�@=p�@<��@<�@;��@;�a@;��@;+@:�@:��@:Q@:O@9�z@9�"@9IR@9�@8��@8c�@7��@7��@7�@6u%@6Q@65?@5��@5�~@5/@5�@4�U@4D�@3�@3�a@3�f@3Y@2�,@2��@2��@2~�@2;�@2�@1�@1�@1�^@1 \@0��@0��@0�u@0l"@0!@/��@/ƨ@/��@/��@/��@/j�@/]�@/@O@/�@.�6@.-@-��@-��@--w@,��@,��@,��@,S�@+��@+��@+�4@+_p@+4�@*�y@*�R@*��@*ff@*.�@)�Z@)�d@)m]@)@(�p@(�e@(c�@(1'@'�Q@'��@'��@'U�@'/�@'�@&��@&��@&�1@&!�@%�z@%��@%��@%L�@%4@%	l@$��@$�z@$I�@#��@#�K@#�q@#9�@#@"�]@"�@"xl@"0U@!�d@![W@!�@ ��@ ��@ ]d@ A�@ @��@�q@g�@ i@�@�y@�,@�@��@kQ@_�@^5@E�@1�@�@�)@�H@��@rG@Y�@x�@	l@�O@��@z�@Z@�@ƨ@�	@_p@'�@�]@��@M�@4@�)@��@��@k�@N<@?}@�	@Ĝ@��@m�@Xy@Q�@C-@�@�]@�;@�@��@��@��@6z@C@�@�@��@kQ@R�@5?@3�@�@�C@��@k�@F@%F@�/@��@~(@_@Xy@/�@%�@  @�@��@s@P�@'�@��@�,@��@a|@�@u@�@�@��@�X@�h@�@k�@J�@@�@�@��@��@M@x@��@~�@{J@x@n/@K�@'�@�@��@�@ߤ@�s@�'@�x@u%@Ta@=q@$�@�@ �@��@�#@��@��@�@k�@[W@Y�@X@O�@B�@+�@%@�K@�@��@��@��@�@��@�@6@$@M@��@�w@��@H�@
�y@
��@
��@
�R@
�L@
�@
�r@
l�@
?@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BpBn�Bm�Bo BpoBqABr�Bt�Bt�BtTBt�Bv�Bz�BxRBw�B|B{�By$BzxB~�B��B�3B��B��B��B��B�B��B�B�WB��B��B��B�cB��B��B��B�OB�OB��B��B	[B
1�B
9XB
AoB
H�B
o B
��B
�BB
�B
��B
�KB
� B
�yB
��B
�B
�B
�,B
�B
�B
ՁB
�B
�,B
��B
tTB
kkB
Z�B
LdB
BB
4�B
~B
�B	��B	�B	��B	��B	�OB	�B	�LB	�?B	�TB	��B	�6B	��B	u�B	]/B	=�B	)*B	%B	
	B�DB�_BȴB��B��B�B��BāB��B��B��B�NB�.B�BB�VB�B�PB�=BȀBȴBɺB�lBňB�B��B�BB�+B��B�GB� B��BŢB��B�xBϑB�B�sB	 �B	EB		�B	B	bB	mB	YB	B	IB	 �B	"�B	%�B	(sB	+kB	/�B	1�B	3�B	5�B	=�B	A B	G�B	I�B	J�B	I�B	IB	H1B	KxB	N�B	OBB	L�B	L�B	M�B	RB	P.B	J�B	H�B	LB	a-B	i�B	y�B	�B	��B	��B	��B	�sB	��B	�pB	��B	�B	�!B	�)B	��B	�B	��B	�'B	�bB	�nB	��B	��B	�DB	��B	�B	�UB	�CB	��B	�B	�]B	�CB	��B	��B	��B	��B	��B	�hB	��B	�lB	�8B	�^B	�jB	��B	��B	�B	�B	��B	�B	��B	�	B	��B	��B	�/B	�oB	�$B	��B	�dB	�XB	�2B	�$B	�B	��B	�}B	��B	�3B	�9B	�B	��B	��B	��B	��B	�8B	�B	�B	�$B	��B	��B	��B	��B	��B	�GB	ÖB	�B	��B	�B	āB	�B	�aB	��B	�EB	�B	�lB	��B	�DB	��B	� B	�uB	өB	ԕB	��B	�oB	ѷB	� B	��B	�B	�uB	��B	ՁB	�B	ԯB	�[B	� B	��B	�uB	�2B	�B	�B	�B	�kB	ںB	��B	��B	��B	��B	��B	��B	רB	յB	�9B	��B	ںB	�=B	��B	�WB	�QB	�B	��B	��B	��B	�1B	�B	�1B	�KB	��B	خB	�+B	��B	�B	�KB	ݘB	�B	�BB	�B	�LB	��B	�B	�B	�B	�DB	�DB	�B	�sB	�B	�B	�B	�B	��B	�yB	��B	�QB	�kB	�B	�B	�"B	��B	��B	�]B	��B	�/B	�B	�B	�B	�oB	�B	��B	�'B	�B	�GB	�aB	��B	�B	�B	�B	�FB	��B	��B	��B	��B	��B	�+B	�`B	��B	�zB	�zB	��B	��B	�2B	��B	�B	�lB	��B	��B	��B	�XB	��B	�B	�^B	�B	��B	��B	�(B	��B	��B
 B
  B
 �B
�B
�B
�B
uB
uB
�B
-B
{B
�B
�B
�B
gB
�B
�B
%B
�B
�B
tB
?B
zB
�B
_B
�B
�B
tB
�B
_B
�B
�B
KB
�B

�B

rB

�B

�B

#B

�B

rB

�B
)B
)B
^B
^B
�B
�B
�B
JB
~B
�B
PB
VB
B
pB
BB
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
,B
�B
2B
MB
�B
�B
�B
�B
?B

B
�B
$B
?B
YB
�B
+B
�B
�B
�B
yB
�B
B
1B
1B
�B
	B
xB
�B
�B
�B
IB
�B
dB
�B
OB
OB
B
�B
�B
VB
 'B
 B
 vB
 'B
 BB
 vB
 �B
!�B
!�B
!�B
"B
"NB
"�B
"�B
#�B
#�B
$�B
%B
%B
%`B
%�B
%�B
%�B
%�B
%�B
&LB
&�B
&2B
&2B
&fB
%�B
'8B
(
B
(sB
(�B
)B
)�B
*B
*�B
+�B
,WB
.�B
0oB
0oB
1�B
2�B
33B
3�B
3�B
4B
4B
4B
3�B
4�B
4�B
4�B
5%B
5?B
5ZB
5�B
5�B
6B
6`B
6�B
72B
7B
7�B
7�B
8B
8�B
9�B
9rB
9XB
9�B
9�B
9	B
9�B
:^B
;JB
;JB
;B
=B
=�B
=�B
=qB
=�B
>BB
>�B
>�B
?B
?cB
?cB
?�B
?�B
@B
@iB
@OB
@�B
@�B
A;B
A�B
B'B
B[B
B[B
B�B
C�B
C�B
C�B
C�B
C�B
DMB
D�B
E9B
ESB
EmB
E�B
FB
F?B
F?B
F�B
F�B
F�B
F�B
F�B
GzB
G_B
G�B
HKB
HfB
HfB
HfB
H�B
IlB
IRB
I7B
IlB
IRB
IlB
I�B
J�B
KDB
K�B
KxB
K�B
L~B
L~B
LdB
L�B
L�B
M�B
NB
N�B
N�B
OB
OBB
OBB
O(B
OBB
OBB
O�B
O�B
P.B
P.B
P.B
PB
PB
PHB
PbB
PHB
P.B
PHB
P}B
P�B
Q B
QhB
Q�B
Q�B
RB
R B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TB
T�B
TaB
T�B
T�B
T�B
UMB
UB
UgB
UMB
U�B
U�B
W$B
WsB
W�B
W�B
XB
X+B
X_B
XyB
X�B
X�B
X�B
YB
Y�B
YB
Y1B
W�B
W�B
X�B
YeB
Y1B
W�B
W�B
W�B
YKB
[#B
[=B
Z�B
Y�B
YKB
Y�B
Y�B
ZB
Z7B
[�B
\)B
[�B
[�B
[�B
\)B
\B
\�B
\�B
\�B
]IB
]~B
]~B
]�B
^5B
^jB
^�B
_;B
_!B
_B
_B
_VB
_�B
_�B
_�B
_�B
`B
`B
`'B
`'B
`\B
`�B
aB
aB
aB
a-B
a�B
a�B
bB
a�B
bB
bNB
b�B
b�B
b�B
c�B
d@B
dZB
dZB
dtB
d�B
d�B
eFB
e�B
e�B
e�B
ezB
e�B
e�B
fLB
f�B
f�B
f�B
gB
g8B
g�B
g�B
g�B
h
B
h$B
h>B
h>B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
i�B
j0B
jeB
jB
j�B
j�B
j�B
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
lWB
l�B
l�B
mB
m�B
m�B
m�B
nB
m�B
nIB
n}B
o B
oOB
oiB
o�B
o�B
o�B
pB
p;B
pUB
p�B
q'B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
sMB
tnB
t�B
t�B
t�B
t�B
u%B
u?B
utB
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
v�B
w2B
wB
w2B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
y	B
yXB
yXB
y>B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
z�B
{B
{JB
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|B
|jB
|�B
|�B
|�B
}B
}B
}B
}"B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
HB
cB
cB
cB
cB
}B
}B
�B
�B
�B
� B
� B
� B
� B
�B
� B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
�;B
�;B
�;B
� B
� B
�oB
��B
��B
��B
��B
��B
�B
�'B
�[B
�AB
�'B
�AB
�[B
�uB
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BpBn�Bm�Bo BpoBqABr�Bt�Bt�BtTBt�Bv�Bz�BxRBw�B|B{�By$BzxB~�B��B�3B��B��B��B��B�B��B�B�WB��B��B��B�cB��B��B��B�OB�OB��B��B	[B
1�B
9XB
AoB
H�B
o B
��B
�BB
�B
��B
�KB
� B
�yB
��B
�B
�B
�,B
�B
�B
ՁB
�B
�,B
��B
tTB
kkB
Z�B
LdB
BB
4�B
~B
�B	��B	�B	��B	��B	�OB	�B	�LB	�?B	�TB	��B	�6B	��B	u�B	]/B	=�B	)*B	%B	
	B�DB�_BȴB��B��B�B��BāB��B��B��B�NB�.B�BB�VB�B�PB�=BȀBȴBɺB�lBňB�B��B�BB�+B��B�GB� B��BŢB��B�xBϑB�B�sB	 �B	EB		�B	B	bB	mB	YB	B	IB	 �B	"�B	%�B	(sB	+kB	/�B	1�B	3�B	5�B	=�B	A B	G�B	I�B	J�B	I�B	IB	H1B	KxB	N�B	OBB	L�B	L�B	M�B	RB	P.B	J�B	H�B	LB	a-B	i�B	y�B	�B	��B	��B	��B	�sB	��B	�pB	��B	�B	�!B	�)B	��B	�B	��B	�'B	�bB	�nB	��B	��B	�DB	��B	�B	�UB	�CB	��B	�B	�]B	�CB	��B	��B	��B	��B	��B	�hB	��B	�lB	�8B	�^B	�jB	��B	��B	�B	�B	��B	�B	��B	�	B	��B	��B	�/B	�oB	�$B	��B	�dB	�XB	�2B	�$B	�B	��B	�}B	��B	�3B	�9B	�B	��B	��B	��B	��B	�8B	�B	�B	�$B	��B	��B	��B	��B	��B	�GB	ÖB	�B	��B	�B	āB	�B	�aB	��B	�EB	�B	�lB	��B	�DB	��B	� B	�uB	өB	ԕB	��B	�oB	ѷB	� B	��B	�B	�uB	��B	ՁB	�B	ԯB	�[B	� B	��B	�uB	�2B	�B	�B	�B	�kB	ںB	��B	��B	��B	��B	��B	��B	רB	յB	�9B	��B	ںB	�=B	��B	�WB	�QB	�B	��B	��B	��B	�1B	�B	�1B	�KB	��B	خB	�+B	��B	�B	�KB	ݘB	�B	�BB	�B	�LB	��B	�B	�B	�B	�DB	�DB	�B	�sB	�B	�B	�B	�B	��B	�yB	��B	�QB	�kB	�B	�B	�"B	��B	��B	�]B	��B	�/B	�B	�B	�B	�oB	�B	��B	�'B	�B	�GB	�aB	��B	�B	�B	�B	�FB	��B	��B	��B	��B	��B	�+B	�`B	��B	�zB	�zB	��B	��B	�2B	��B	�B	�lB	��B	��B	��B	�XB	��B	�B	�^B	�B	��B	��B	�(B	��B	��B
 B
  B
 �B
�B
�B
�B
uB
uB
�B
-B
{B
�B
�B
�B
gB
�B
�B
%B
�B
�B
tB
?B
zB
�B
_B
�B
�B
tB
�B
_B
�B
�B
KB
�B

�B

rB

�B

�B

#B

�B

rB

�B
)B
)B
^B
^B
�B
�B
�B
JB
~B
�B
PB
VB
B
pB
BB
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
,B
�B
2B
MB
�B
�B
�B
�B
?B

B
�B
$B
?B
YB
�B
+B
�B
�B
�B
yB
�B
B
1B
1B
�B
	B
xB
�B
�B
�B
IB
�B
dB
�B
OB
OB
B
�B
�B
VB
 'B
 B
 vB
 'B
 BB
 vB
 �B
!�B
!�B
!�B
"B
"NB
"�B
"�B
#�B
#�B
$�B
%B
%B
%`B
%�B
%�B
%�B
%�B
%�B
&LB
&�B
&2B
&2B
&fB
%�B
'8B
(
B
(sB
(�B
)B
)�B
*B
*�B
+�B
,WB
.�B
0oB
0oB
1�B
2�B
33B
3�B
3�B
4B
4B
4B
3�B
4�B
4�B
4�B
5%B
5?B
5ZB
5�B
5�B
6B
6`B
6�B
72B
7B
7�B
7�B
8B
8�B
9�B
9rB
9XB
9�B
9�B
9	B
9�B
:^B
;JB
;JB
;B
=B
=�B
=�B
=qB
=�B
>BB
>�B
>�B
?B
?cB
?cB
?�B
?�B
@B
@iB
@OB
@�B
@�B
A;B
A�B
B'B
B[B
B[B
B�B
C�B
C�B
C�B
C�B
C�B
DMB
D�B
E9B
ESB
EmB
E�B
FB
F?B
F?B
F�B
F�B
F�B
F�B
F�B
GzB
G_B
G�B
HKB
HfB
HfB
HfB
H�B
IlB
IRB
I7B
IlB
IRB
IlB
I�B
J�B
KDB
K�B
KxB
K�B
L~B
L~B
LdB
L�B
L�B
M�B
NB
N�B
N�B
OB
OBB
OBB
O(B
OBB
OBB
O�B
O�B
P.B
P.B
P.B
PB
PB
PHB
PbB
PHB
P.B
PHB
P}B
P�B
Q B
QhB
Q�B
Q�B
RB
R B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TB
T�B
TaB
T�B
T�B
T�B
UMB
UB
UgB
UMB
U�B
U�B
W$B
WsB
W�B
W�B
XB
X+B
X_B
XyB
X�B
X�B
X�B
YB
Y�B
YB
Y1B
W�B
W�B
X�B
YeB
Y1B
W�B
W�B
W�B
YKB
[#B
[=B
Z�B
Y�B
YKB
Y�B
Y�B
ZB
Z7B
[�B
\)B
[�B
[�B
[�B
\)B
\B
\�B
\�B
\�B
]IB
]~B
]~B
]�B
^5B
^jB
^�B
_;B
_!B
_B
_B
_VB
_�B
_�B
_�B
_�B
`B
`B
`'B
`'B
`\B
`�B
aB
aB
aB
a-B
a�B
a�B
bB
a�B
bB
bNB
b�B
b�B
b�B
c�B
d@B
dZB
dZB
dtB
d�B
d�B
eFB
e�B
e�B
e�B
ezB
e�B
e�B
fLB
f�B
f�B
f�B
gB
g8B
g�B
g�B
g�B
h
B
h$B
h>B
h>B
h�B
h�B
h�B
i*B
i_B
iyB
i�B
i�B
j0B
jeB
jB
j�B
j�B
j�B
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lB
lWB
l�B
l�B
mB
m�B
m�B
m�B
nB
m�B
nIB
n}B
o B
oOB
oiB
o�B
o�B
o�B
pB
p;B
pUB
p�B
q'B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
r�B
sMB
tnB
t�B
t�B
t�B
t�B
u%B
u?B
utB
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
v�B
w2B
wB
w2B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xlB
x�B
x�B
x�B
y	B
yXB
yXB
y>B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
z�B
{B
{JB
{dB
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|B
|jB
|�B
|�B
|�B
}B
}B
}B
}"B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
HB
cB
cB
cB
cB
}B
}B
�B
�B
�B
� B
� B
� B
� B
�B
� B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
�;B
�;B
�;B
� B
� B
�oB
��B
��B
��B
��B
��B
�B
�'B
�[B
�AB
�'B
�AB
�[B
�uB
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105233  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191627  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191628  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191628                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041635  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041635  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                