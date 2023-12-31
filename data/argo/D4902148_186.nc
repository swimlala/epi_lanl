CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-20T15:37:23Z creation;2019-09-20T15:37:28Z conversion to V3.1;2022-11-21T05:28:19Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20190920153723  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_186                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @����u 1   @�� ��-�@<(ۋ�q�dg�ᰉ�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B��B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @@��@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A��HB 
=B
=B
=B��B 
=B(
=B0p�B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&)C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C?��CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aܲ-Aܲ-Aܲ-Aܴ9Aܴ9Aܲ-AܶFAܸRAܸRAܺ^Aܺ^Aܺ^Aܺ^AܾwAܼjAܼjAܺ^A���A�A԰!A�jA��A��A�r�A�A�S�A�/A���A�?}A�ĜA�r�A���A��
A��A��A��TA�1A��uA�C�A��yA�VA���A���A��A�1'A��TA��RA���A�1'A���A��;A���A���A�1'A�z�A�JA�oA��A�Q�A���A���A��9A��/A�$�A�z�A���A�E�A��+A��A�A� �A��A��PA��jA�VA�9XA�O�A�`BA��7A�A���A��A�1'A�O�A��jA�bNA�bA�=qA��A�C�A�x�A���A��A��HA�p�A��yA��/A�A�ZA��yA�+A~�A}hsA|�RA{�TAz�+AyC�AwhsAt�uAs�TAs�hAs7LAr�`Aq��Am��Al��Al�Ak7LAi+AhA�AgVAf9XAeAd�Ac��Aa�A_�A^ĜA\ffAZȴAY�-AW&�AS��ARM�AQ��AQ�AP��APbAO`BANĜAMt�ALn�AIAHz�AH-AG��AG�PAG"�AF�HAF�!AF�AF1'AE��AE��AE�AE&�ADz�AD�AC/AA�
AA/A?\)A<��A;�A;�hA;dZA:�A:�A9��A8�`A7
=A6�+A5��A4�A3�TA3
=A2��A2M�A1��A0A�A/�A.��A.VA-��A-O�A-VA,��A,$�A+�A+"�A*�jA*A�A)��A)�7A)/A( �A'�A&ffA%�^A$��A#XA"��A!�#A ȴAS�AȴAz�AI�AAoAr�A�A
=A�A�RA(�Ax�A�yA��An�A�A$�A��A\)A�7Az�A�FAXA�AZA+A~�A�7AG�A��A�AoA
VA	|�A	VA�^A�Av�A�#A�A�RAffAx�A~�A �A 5?@���@�33@��\@�5?@���@��h@���@�t�@�{@�O�@��j@�1'@�z�@���@��@�+@�x�@�%@��/@�j@�"�@��@�7@ߍP@��@�&�@�\)@���@ؓu@ו�@և+@��@��;@���@�-@���@�O�@� �@�~�@�@͑h@�G�@�1'@�C�@���@ʟ�@�$�@�&�@�z�@Ǿw@�n�@őh@�x�@�`B@��/@Å@�V@��@�9X@�1@��@�l�@��@��H@��!@�{@��-@�`B@���@�C�@�M�@�G�@���@�1@�|�@��y@���@��P@��@�O�@���@���@�"�@��+@��`@��m@�E�@��@�ȴ@�{@���@��h@�O�@��u@���@��/@�K�@���@��@��^@��-@�`B@���@��@��`@���@��u@���@�{@��T@��-@��7@�G�@�%@��`@� �@��
@��F@���@���@���@�|�@�o@��@��R@�ff@�-@�J@�@��7@�/@��/@��@��@�Q�@�b@�ƨ@�l�@��@��R@�M�@�J@���@�X@�7L@�&�@���@��u@�r�@�b@���@���@��P@��@�l�@�@��@���@��7@�?}@�%@���@�Z@�(�@���@��@�v�@��@�{@���@��7@�?}@�bN@���@��P@�dZ@�;d@�o@�@��H@���@��\@�E�@��@���@��@��@���@��j@���@��D@�z�@�bN@�Z@�Q�@�A�@�(�@�1@��;@��F@�|�@��@�E�@���@�x�@�&�@�%@���@��`@��9@���@��u@�z�@�bN@�9X@�@;d@~�y@~ff@~E�@~E�@}�T@}?}@|��@|�@|�@|��@{�
@{o@z�@z��@z��@z��@z�!@z-@y��@x��@x �@w�@wK�@w
=@v��@v5?@u`B@u?}@uO�@u�h@u�@t��@tz�@t9X@s�m@st�@so@so@sS�@r�@r��@rJ@q��@p�`@o��@n��@nv�@n{@m�h@mV@l��@lI�@j�@j��@jn�@j-@i��@i��@ihs@ihs@iX@h��@h�@h  @g�w@gl�@f�y@f��@f5?@e�@e��@eV@d��@eV@d��@d��@d�@d�/@d��@d��@d9X@b�@bM�@a��@`��@`r�@`bN@`bN@`1'@`  @_�@^��@]�h@\�@\��@\��@\z�@\I�@\�@[��@[�@Z�\@ZJ@Y�7@YG�@Y7L@Y%@XĜ@X�u@XQ�@X  @W�;@W�@Vv�@VV@V5?@V@U�T@U��@V@V5?@VV@U�-@UV@T9X@SdZ@So@R�\@Q��@QG�@Q&�@Q�@Q%@P��@P��@P �@O�;@Nȴ@NE�@M�T@M��@M�-@MV@L��@L��@L��@L�/@L�j@LI�@Kƨ@K33@J��@J~�@J~�@J~�@J~�@J�\@J=q@I�#@I��@I��@IX@I�@H��@H�u@HbN@G\)@G�@F��@Fȴ@F��@Fv�@F5?@EV@D��@Dj@DI�@D(�@C��@C�m@C�m@C�m@Cƨ@C��@CS�@B�@B�H@B~�@B-@BJ@A�^@A7L@A�@A%@@r�@@ �@?�@?|�@?\)@?;d@?�@>��@>ȴ@>v�@>ff@>V@>5?@>{@=��@=p�@=?}@<��@<��@;t�@:�!@:M�@:�@9��@9�^@9��@9��@9x�@9�@8��@8�u@8�@8r�@8r�@8A�@8b@7��@7l�@7\)@6��@6��@6{@5@5`B@5�@4�@4�D@4Z@4I�@49X@3�
@3t�@3S�@333@3@2�!@2��@2�\@2^5@2�@1�@1�7@0�`@0bN@0A�@0b@/�@/�;@/�@/\)@/+@/�@.�y@.v�@.$�@-��@-�@-O�@-�@,�@,��@,I�@,�@+�
@+dZ@+S�@+33@+@*��@*^5@*=q@*-@)�@)��@)x�@)G�@)�@(�u@(r�@(r�@(bN@(A�@(1'@(1'@(b@'�;@'��@'�w@'�@'��@'l�@&��@&�y@&�y@&ȴ@&��@&v�@&5?@&{@%��@%��@%O�@%�@$��@$z�@$j@$Z@$9X@$(�@$�@#�m@#�@"�@"�H@"�H@"��@"��@"��@"��@"��@"~�@"M�@!��@!�^@!��@!x�@!X@!X@!X@!7L@ �`@ ��@ A�@ b@�@�;@��@|�@l�@
=@�y@ȴ@��@�+@ff@$�@��@�h@`B@`B@�/@9X@(�@�@1@�@C�@33@��@-@��@X@�@%@�`@Ĝ@�9@��@��@bN@��@�w@�P@K�@;d@+@�y@�R@V@{@��@/@�@�@�@j@�@t�@C�@"�@�@��@�!@��@n�@=q@�@��@X@��@��@r�@Q�@A�@b@�@�w@�P@\)@�@��@�+@�+@�+@v�@ff@V@$�@@��@��@�@�@�@�j@��@j@9X@(�@1@�F@t�@"�@o@@
�@
�@
�H@
��@
��@
�\@
M�@	��@	��@	��@	X@	�@	%@��@�9@��@r�@bN@bN@Q�@A�@A�@1'@1'@ �@ �@b@�@��@�w@|�@|�@l�@l�@\)@+@��@ȴ@�@��@��@�+@v�@v�@v�@ff@V@V@E�@5?@$�@$�@{@{@@@��@@��@`B@V@�@z�@Z@�@��@�
@��@��@�@dZ@"�@o@�@�@��@~�@-@�@�#@�^@�7@G�@G�@G�@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aܲ-Aܲ-Aܲ-Aܴ9Aܴ9Aܲ-AܶFAܸRAܸRAܺ^Aܺ^Aܺ^Aܺ^AܾwAܼjAܼjAܺ^A���A�A԰!A�jA��A��A�r�A�A�S�A�/A���A�?}A�ĜA�r�A���A��
A��A��A��TA�1A��uA�C�A��yA�VA���A���A��A�1'A��TA��RA���A�1'A���A��;A���A���A�1'A�z�A�JA�oA��A�Q�A���A���A��9A��/A�$�A�z�A���A�E�A��+A��A�A� �A��A��PA��jA�VA�9XA�O�A�`BA��7A�A���A��A�1'A�O�A��jA�bNA�bA�=qA��A�C�A�x�A���A��A��HA�p�A��yA��/A�A�ZA��yA�+A~�A}hsA|�RA{�TAz�+AyC�AwhsAt�uAs�TAs�hAs7LAr�`Aq��Am��Al��Al�Ak7LAi+AhA�AgVAf9XAeAd�Ac��Aa�A_�A^ĜA\ffAZȴAY�-AW&�AS��ARM�AQ��AQ�AP��APbAO`BANĜAMt�ALn�AIAHz�AH-AG��AG�PAG"�AF�HAF�!AF�AF1'AE��AE��AE�AE&�ADz�AD�AC/AA�
AA/A?\)A<��A;�A;�hA;dZA:�A:�A9��A8�`A7
=A6�+A5��A4�A3�TA3
=A2��A2M�A1��A0A�A/�A.��A.VA-��A-O�A-VA,��A,$�A+�A+"�A*�jA*A�A)��A)�7A)/A( �A'�A&ffA%�^A$��A#XA"��A!�#A ȴAS�AȴAz�AI�AAoAr�A�A
=A�A�RA(�Ax�A�yA��An�A�A$�A��A\)A�7Az�A�FAXA�AZA+A~�A�7AG�A��A�AoA
VA	|�A	VA�^A�Av�A�#A�A�RAffAx�A~�A �A 5?@���@�33@��\@�5?@���@��h@���@�t�@�{@�O�@��j@�1'@�z�@���@��@�+@�x�@�%@��/@�j@�"�@��@�7@ߍP@��@�&�@�\)@���@ؓu@ו�@և+@��@��;@���@�-@���@�O�@� �@�~�@�@͑h@�G�@�1'@�C�@���@ʟ�@�$�@�&�@�z�@Ǿw@�n�@őh@�x�@�`B@��/@Å@�V@��@�9X@�1@��@�l�@��@��H@��!@�{@��-@�`B@���@�C�@�M�@�G�@���@�1@�|�@��y@���@��P@��@�O�@���@���@�"�@��+@��`@��m@�E�@��@�ȴ@�{@���@��h@�O�@��u@���@��/@�K�@���@��@��^@��-@�`B@���@��@��`@���@��u@���@�{@��T@��-@��7@�G�@�%@��`@� �@��
@��F@���@���@���@�|�@�o@��@��R@�ff@�-@�J@�@��7@�/@��/@��@��@�Q�@�b@�ƨ@�l�@��@��R@�M�@�J@���@�X@�7L@�&�@���@��u@�r�@�b@���@���@��P@��@�l�@�@��@���@��7@�?}@�%@���@�Z@�(�@���@��@�v�@��@�{@���@��7@�?}@�bN@���@��P@�dZ@�;d@�o@�@��H@���@��\@�E�@��@���@��@��@���@��j@���@��D@�z�@�bN@�Z@�Q�@�A�@�(�@�1@��;@��F@�|�@��@�E�@���@�x�@�&�@�%@���@��`@��9@���@��u@�z�@�bN@�9X@�@;d@~�y@~ff@~E�@~E�@}�T@}?}@|��@|�@|�@|��@{�
@{o@z�@z��@z��@z��@z�!@z-@y��@x��@x �@w�@wK�@w
=@v��@v5?@u`B@u?}@uO�@u�h@u�@t��@tz�@t9X@s�m@st�@so@so@sS�@r�@r��@rJ@q��@p�`@o��@n��@nv�@n{@m�h@mV@l��@lI�@j�@j��@jn�@j-@i��@i��@ihs@ihs@iX@h��@h�@h  @g�w@gl�@f�y@f��@f5?@e�@e��@eV@d��@eV@d��@d��@d�@d�/@d��@d��@d9X@b�@bM�@a��@`��@`r�@`bN@`bN@`1'@`  @_�@^��@]�h@\�@\��@\��@\z�@\I�@\�@[��@[�@Z�\@ZJ@Y�7@YG�@Y7L@Y%@XĜ@X�u@XQ�@X  @W�;@W�@Vv�@VV@V5?@V@U�T@U��@V@V5?@VV@U�-@UV@T9X@SdZ@So@R�\@Q��@QG�@Q&�@Q�@Q%@P��@P��@P �@O�;@Nȴ@NE�@M�T@M��@M�-@MV@L��@L��@L��@L�/@L�j@LI�@Kƨ@K33@J��@J~�@J~�@J~�@J~�@J�\@J=q@I�#@I��@I��@IX@I�@H��@H�u@HbN@G\)@G�@F��@Fȴ@F��@Fv�@F5?@EV@D��@Dj@DI�@D(�@C��@C�m@C�m@C�m@Cƨ@C��@CS�@B�@B�H@B~�@B-@BJ@A�^@A7L@A�@A%@@r�@@ �@?�@?|�@?\)@?;d@?�@>��@>ȴ@>v�@>ff@>V@>5?@>{@=��@=p�@=?}@<��@<��@;t�@:�!@:M�@:�@9��@9�^@9��@9��@9x�@9�@8��@8�u@8�@8r�@8r�@8A�@8b@7��@7l�@7\)@6��@6��@6{@5@5`B@5�@4�@4�D@4Z@4I�@49X@3�
@3t�@3S�@333@3@2�!@2��@2�\@2^5@2�@1�@1�7@0�`@0bN@0A�@0b@/�@/�;@/�@/\)@/+@/�@.�y@.v�@.$�@-��@-�@-O�@-�@,�@,��@,I�@,�@+�
@+dZ@+S�@+33@+@*��@*^5@*=q@*-@)�@)��@)x�@)G�@)�@(�u@(r�@(r�@(bN@(A�@(1'@(1'@(b@'�;@'��@'�w@'�@'��@'l�@&��@&�y@&�y@&ȴ@&��@&v�@&5?@&{@%��@%��@%O�@%�@$��@$z�@$j@$Z@$9X@$(�@$�@#�m@#�@"�@"�H@"�H@"��@"��@"��@"��@"��@"~�@"M�@!��@!�^@!��@!x�@!X@!X@!X@!7L@ �`@ ��@ A�@ b@�@�;@��@|�@l�@
=@�y@ȴ@��@�+@ff@$�@��@�h@`B@`B@�/@9X@(�@�@1@�@C�@33@��@-@��@X@�@%@�`@Ĝ@�9@��@��@bN@��@�w@�P@K�@;d@+@�y@�R@V@{@��@/@�@�@�@j@�@t�@C�@"�@�@��@�!@��@n�@=q@�@��@X@��@��@r�@Q�@A�@b@�@�w@�P@\)@�@��@�+@�+@�+@v�@ff@V@$�@@��@��@�@�@�@�j@��@j@9X@(�@1@�F@t�@"�@o@@
�@
�@
�H@
��@
��@
�\@
M�@	��@	��@	��@	X@	�@	%@��@�9@��@r�@bN@bN@Q�@A�@A�@1'@1'@ �@ �@b@�@��@�w@|�@|�@l�@l�@\)@+@��@ȴ@�@��@��@�+@v�@v�@v�@ff@V@V@E�@5?@$�@$�@{@{@@@��@@��@`B@V@�@z�@Z@�@��@�
@��@��@�@dZ@"�@o@�@�@��@~�@-@�@�#@�^@�7@G�@G�@G�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB�
B�B�B�ZB�fB�fB�ZB�ZB�TB�TB�ZB�NB�;B�
B��BŢB�XB�!B��B��B�+B|�B�B� B|�Bw�B�B�oB�oB�VB~�Bm�B^5BS�BJ�BP�BF�B;dB9XB:^B;dB;dB;dB8RB2-B+B%�B�B�B�B	7B�5BÖB�^B�B��B�hB�Bu�Bl�BdZB_;BS�BI�B<jB2-B+B#�B�BJB
��B
�B
�mB
�B
ɺB
B
�XB
�B
��B
��B
�\B
�%B
|�B
r�B
m�B
hsB
_;B
VB
I�B
9XB
49B
2-B
0!B
,B
#�B
bB

=B
B	��B	�B	�B	�`B	�BB	�)B	��B	��B	�}B	�FB	�B	��B	��B	�VB	�B	q�B	ffB	`BB	\)B	XB	VB	R�B	N�B	G�B	@�B	9XB	5?B	49B	33B	2-B	1'B	0!B	/B	.B	-B	-B	,B	+B	(�B	%�B	#�B	�B	�B	{B	DB��B��B��B��B��B��B�B�B�sB�fB�TB�;B�#B�B�
B�B��B��BɺBŢB��B�jB�XB�RB�FB�9B�3B�-B�B�B��B��B��B��B��B��B��B��B�{B�hB�VB�DB�+B�B�B�B�B~�B}�Bz�By�Bx�Bw�Bv�Bt�Br�Bq�Bo�Bm�Bk�BiyBffBcTBaHB_;B_;B]/B[#BYBXBS�BQ�BO�BM�BL�BL�BJ�BH�BE�BC�BB�BA�B?}B=qB;dB8RB6FB5?B5?B49B5?B49B2-B1'B0!B/B.B.B-B,B)�B'�B&�B%�B%�B$�B#�B"�B �B�B!�B#�B#�B"�B!�B!�B"�B"�B"�B#�B#�B#�B$�B#�B"�B"�B"�B$�B%�B%�B%�B&�B'�B&�B&�B&�B'�B'�B(�B,B/B/B0!B/B/B2-B2-B33B49B49B5?B5?B6FB6FB8RB9XB9XB:^B<jB=qB?}BC�BD�BE�BD�BG�BL�BO�BP�BQ�BR�BYB[#B`BBbNBcTBe`BdZBdZBbNBbNBaHB_;B`BBgmBjBl�Bo�Bp�Bp�Br�Bt�Bt�Bu�Bv�Bv�B{�B�B�%B�+B�1B�=B�JB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�9B�LB�XB�dB�}B��BÖBƨBǮBǮBɺB��B��B��B��B��B��B��B��B��B��B�#B�5B�BB�HB�ZB�`B�fB�yB�B�B�B�B��B��B��B��B	B	B	B	B	%B	+B	1B		7B	DB	PB	\B	bB	hB	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	(�B	.B	/B	1'B	2-B	33B	33B	5?B	6FB	6FB	7LB	8RB	9XB	<jB	?}B	@�B	B�B	C�B	C�B	D�B	G�B	H�B	K�B	M�B	M�B	O�B	R�B	S�B	VB	W
B	W
B	XB	YB	YB	ZB	[#B	]/B	_;B	`BB	bNB	cTB	hsB	iyB	jB	l�B	m�B	m�B	m�B	m�B	n�B	p�B	s�B	t�B	w�B	z�B	|�B	~�B	�B	�B	�1B	�JB	�PB	�VB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�3B	�FB	�XB	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�;B	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
PB
PB
PB
VB
VB
VB
\B
bB
bB
bB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
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
/B
/B
/B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
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
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
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
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
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
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
XB
XB
XB
XB
XB
XB
YB
YB
YB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
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
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
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
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�bB�hB��B׍B�FB��B�UB�B�B�B��B�sB�B�tB�B�B��B��B՛B�B��B�MB��B�+B��B~�B�?B�[B�Bx�B��B�B��B� B�oBo�B`vBU�BMBS�BH�B<�B9�B;B;�B<B=<B9�B3�B,�B'RB!�B�B�BdB��B�SB�PB��B��B��B�3Bw�Bm�Be`Ba-BU�BK�B=�B33B,"B%�B�B�B
��B
�-B
�B
ܒB
�)B
�MB
��B
�CB
��B
��B
�4B
��B
~wB
s�B
n�B
j0B
aB
XyB
LdB
:*B
4�B
2�B
1B
.B
'�B
�B
xB
�B
 B	�B	��B	�fB	�B	�~B	��B	�6B	��B	�8B	��B	��B	��B	��B	��B	s�B	g8B	aB	\�B	X�B	W
B	TB	P�B	IlB	CaB	:�B	5�B	4�B	3�B	2�B	1�B	0oB	/OB	.}B	-]B	-]B	,qB	+�B	)�B	&�B	%,B	!|B		B	�B	�B	 B�qB�jB��B��B��B�B�B�_B�B�ZB��B�)BٴB��B�$B�uB̳BʦBƎB�UB��B��B�	B��B�B��B��B��B��B�B��B�XB�8B��B��B�5B�7B��B��B��B��B��B��B��B��B�B� B~�B{�BzDBy>Bx�Bw�ButBs3Br�Bq�BncBl=Bj�Bh�Bd�BbNB_�B_�B^jB\�BZBY1BT�BR�BQBOBM�BM�BK�BJXBF�BDgBC{BB�B@4B>BB<�B9�B8B6+B5�B4�B5�B4�B3�B2aB0�B0B/ B.�B-�B-B,=B)�B($B&�B&�B%FB$&B#nB!�B �B#�B%B$�B$&B# B"�B#�B#�B#�B$�B$�B$�B%FB$@B#nB#�B#�B%`B&2B&LB&�B'�B(>B'RB'mB'�B(�B(�B)�B,�B/OB/iB0�B0;B0�B2�B2�B3�B4�B4�B5�B5�B6�B6�B8�B9�B:B;JB=<B>(B@BC�BEBFYBE�BIBM�BPbBQNBRoBS�BY�B\]Ba-Bc�Bd�Bf2Bd�Bd�Bb�Bb�BbB`�Ba�BhsBkBmBo�Bp�Bp�Br�Bt�Bt�Bu�BwBw�B|�B�9B�YB�_B��B��B�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�8B�B�CB�OB�vB��B��B��B��B��B��B��B��B��B��B��B��B��B�0B�B� B��B� B�B�TB�2B՛B�qBބB��B�B�B��B��B��B�B��B��B�B�B�2B�RB�VB	;B	GB	MB	SB	?B	_B	KB		lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 'B	"�B	)_B	./B	/iB	1AB	2GB	3MB	3hB	5ZB	6`B	6`B	7fB	8�B	9�B	<�B	?�B	@�B	B�B	C�B	C�B	D�B	G�B	H�B	K�B	M�B	N"B	P.B	SB	TB	VB	W$B	W$B	XEB	YKB	YB	ZQB	[WB	]~B	_pB	`vB	b�B	c�B	hXB	iyB	jB	l�B	m�B	m�B	m�B	m�B	n�B	p�B	s�B	t�B	xB	{B	}"B	.B	�UB	��B	��B	�~B	��B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�
B	�*B	�B	�CB	�OB	�'B	�'B	�AB	�-B	�GB	�-B	�GB	�GB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ðB	ĶB	ĶB	żB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�<B	�B	�B	�B	�B	��B	�B	�+B	�#B	�~B	ބB	ߊB	��B	�bB	�B	�B	�B	�zB	�zB	�`B	�zB	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�(B	�HB
 B
'B
-B
-B
3B
MB
�B
KB
	RB
	RB

XB

XB
DB
DB
DB
^B
xB
dB
jB
jB
�B
pB
pB
pB
�B
}B
}B
}B
�B
�B
uB
�B
�B
�B
�B
�B
�B
mB
sB
�B
�B
�B
�B
�B
�B
�B
	B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
%B
&B
'B
(
B
(
B
)*B
)B
*B
+B
+B
+6B
+B
,"B
-)B
-)B
-)B
.B
./B
./B
/B
/5B
/OB
0UB
1[B
2GB
2-B
3MB
3MB
3MB
3MB
4TB
4TB
4TB
5tB
5ZB
6`B
6`B
6`B
6`B
7�B
7fB
7fB
8lB
8lB
9rB
9rB
9rB
9rB
9rB
9�B
:xB
:^B
:xB
;B
;B
;B
<�B
<�B
=qB
=qB
=qB
=�B
=qB
=qB
=�B
>�B
>wB
>wB
>]B
>�B
>wB
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C{B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
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
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
OB
PB
P�B
P�B
Q B
Q B
RB
S&B
S&B
S&B
T,B
T,B
UB
VB
VB
VB
VB
VB
VB
VB
V9B
XB
X+B
X+B
XB
XB
X+B
YB
Y1B
Y1B
ZQB
[=B
[=B
[=B
\CB
\CB
\xB
]dB
^5B
^5B
^OB
^OB
_VB
_VB
_VB
`vB
`vB
`\B
abB
abB
abB
bhB
bhB
bhB
bhB
cnB
cnB
cnB
cnB
d�B
dtB
e`B
eFB
e`B
e`B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iyB
i�B
i_B
jB
jB
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
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
n�B
n}B
n}B
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
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<}�<c��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910010032192019100100321920191001003219202211182140272022111821402720221118214027201910020017132019100200171320191002001713  JA  ARFMdecpA19c                                                                20190921003715  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190920153723  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190920153725  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190920153726  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190920153727  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190920153727  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190920153727  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190920153727  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190920153728  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190920153728                      G�O�G�O�G�O�                JA  ARUP                                                                        20190920155402                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190920153127  CV  JULD            G�O�G�O�F���                JM  ARSQJMQC2.0                                                                 20190924000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARSQJMQC2.0                                                                 20190924000000  CF  TEMP_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20190930153219  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190930153219  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191001151713  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124027  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                