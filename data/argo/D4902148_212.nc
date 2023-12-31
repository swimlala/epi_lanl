CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-06T18:37:49Z creation;2020-06-06T18:37:54Z conversion to V3.1;2022-11-21T05:26:54Z update;     
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
resolution        =���   axis      Z        |  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  M`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200606183749  20221123114512  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_212                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�"�G� 1   @�#��À@;Xr� Ĝ�d��2�W�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�Q�A (�A (�A@(�A^�\A�{A�{A�{A�{A�{A�{A�{A��HB 
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
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D/�=D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Da�=Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Drz=Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD�D� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�=D�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�ffAʹ9AʮAĥ�A���A�r�A�ȴA�t�A�v�A��A��-A�^5A��A�l�A��A���A��+A��TA��A��A���A�+A��^A���A��FA��\A�I�A���A��jA���A�VA�ȴA�O�A��#A�v�A�oA���A��jA�bA�"�A��-A� �A�l�A�Q�A���A�7LA�x�A��A�"�A���A��!A�~�A�K�A��A�VA���A���A�v�A��A��A�1'A��wA�(�A�?}A�p�A�bA��mA��
A�~�A�A��HA�
=A�/A�VA��A���A�O�A��#A�%A���A�|�A�%A~ĜA}`BA|^5Az�Ax�uAv5?Atv�Ar�HArAq�Ap�`Ao/Am�^Am�Al�Al��Al5?Ak��Aj��Ai�#Ahn�Ag�Af�9Ael�Ae/Ad��AdVAb��A`ĜA^�A^M�A]�;A]�A[A[dZAZ��AZ�AZAYx�AXM�AW��AW�AVbNAU��AT�uAS/AQ�FAP^5ANn�ALjAKG�AJZAIl�AH��AHffAH(�AG�AG;dAE�-AEAD��AD=qAC��AC��AB�RAA�mAA�A@�A?C�A>Q�A=%A<^5A;t�A;C�A:�A:~�A: �A9|�A8��A8A7G�A6��A6�+A5p�A4�A4JA3�A3p�A2��A1�A1x�A0�uA05?A01A/��A/%A.�HA.JA,��A,ZA+��A+XA+A*=qA)��A)�A'�wA'XA&�A&n�A&  A%VA$^5A#�mA#O�A"��A"1'A!dZA �+A�wA+A~�A{A��AXA"�A�HA~�A�FAbA�jA;dA�HA �A�hAjAVA �A�PA5?A��A33A�Av�AE�At�A~�A��A
�A	�
A	t�A�jAA�A�A�mAA�hA?}A��A�PAĜA\)A1AVA �A ��A �u@�;d@��@�Z@��@���@��@��/@�D@�1@�o@���@�@���@��m@�+@�^5@蛦@�o@旍@�-@�j@��@�"�@⟾@�V@�-@�A�@��T@� �@�-@��`@��m@��@�5?@�Q�@�33@�5?@�7L@϶F@�~�@��@�C�@ʟ�@�=q@ɲ-@��@���@�Q�@��@���@ă@���@�M�@�O�@���@�E�@��-@��@�`B@�?}@�V@��9@�bN@�t�@���@�E�@��@�Ĝ@�A�@���@���@�`B@���@��w@��\@�X@�j@�
=@��T@�/@�Q�@�b@���@��m@�ƨ@��@��!@�v�@�$�@��T@�`B@��@��
@��@���@�^5@�J@�G�@��D@�1@��@�l�@�o@��@��^@�p�@�X@�V@��`@���@��@��!@��@��h@���@�r�@�|�@�K�@�33@�
=@�~�@�J@���@���@�p�@�O�@���@��D@�Q�@�l�@���@�5?@�@�hs@���@���@�Z@�  @���@�"�@��H@���@���@�hs@�?}@�&�@���@�  @��w@�C�@�ȴ@���@�^5@��@���@�x�@��@��@��@���@���@�t�@�K�@�
=@���@�5?@���@���@���@���@��7@�G�@�7L@�/@�&�@��@��u@��@���@�l�@�@���@���@�ff@���@��T@��^@��h@�/@�Ĝ@��@��@���@���@�j@��F@�33@��@��@�M�@�{@�@���@��T@��#@��#@�@���@��h@�X@��`@�(�@|�@~�R@~�+@~�+@~ff@~V@~$�@~5?@~5?@~@}��@|�j@|9X@{��@z��@yX@x��@xA�@xA�@x �@w��@vȴ@u�@t��@t��@t9X@s�@s33@so@s@r�@r�H@r��@q��@q�#@qhs@q7L@q&�@q�@p�`@p��@p�9@pbN@p �@o�;@o��@o+@n�@n�R@n$�@m��@l�@l�@l��@l��@l�D@lz�@lZ@kƨ@k"�@j��@ihs@h�`@h�@hb@g��@g�@f�@fȴ@fȴ@f��@f5?@f$�@e�@e/@d��@dz�@ct�@c@b��@bM�@bJ@ahs@a%@`Ĝ@`�@`r�@`1'@` �@`b@_�@_|�@^��@^$�@]@]�-@]�-@]��@]?}@\��@\9X@[�m@[ƨ@[t�@[@Z�H@Z��@Z~�@Z�@Y�^@Y��@Y��@X��@X�9@X��@XQ�@W�@W�@W��@Wl�@W�@Vȴ@VE�@V{@U?}@T9X@T�@S��@SS�@So@R�H@R��@R��@Rn�@RM�@R�@Q��@P��@P�u@P�@PA�@Pb@O�w@Ol�@N�y@Nv�@NE�@M�@M�h@L�@LZ@L(�@Kƨ@KS�@J�@Jn�@I��@I&�@I%@HĜ@H1'@G�w@G�P@G�P@G\)@F�R@F�+@Fff@FV@FE�@E�T@E/@D�@D�j@Dz�@D1@C�@C"�@B��@B-@A��@AX@A7L@A�@@�9@@bN@@  @?�@?|�@?�@?�@?�@?�@>��@>ȴ@>��@>5?@=�-@<�@<�D@<1@;�@;C�@;@:��@:~�@:^5@:M�@:J@9��@9hs@97L@8Ĝ@81'@8b@7�@7�P@7K�@7
=@6�@6v�@6E�@6E�@65?@6$�@6@5�T@5@5�-@5�h@5`B@4�j@4I�@4�@3��@3�m@3ƨ@3��@3S�@3"�@2�H@2M�@1��@1G�@0��@0r�@01'@/�@/�w@/�P@/\)@/+@.��@.�y@.�@.�@.�R@.��@.V@.@-�@-��@-?}@-�@-�@,�@,�@,Z@+�
@+t�@+"�@+@*�@*��@*�!@*��@*~�@)��@)�^@)��@)�7@)X@)7L@)%@(�`@(Q�@(b@'�@'�;@'��@'�w@'�w@'�w@'��@'l�@'\)@'\)@'
=@&��@%�@%/@$�@$�D@$z�@$9X@$�@$�@$1@#�m@#�F@#��@#dZ@#C�@"�H@"��@"�\@"^5@"=q@!��@!�^@!��@!X@ ��@ A�@ b@�;@�@;d@�y@��@v�@E�@5?@$�@�T@�-@�@/@/@��@�j@�D@j@1@1@1@��@�m@t�@o@�H@��@�\@�\@^5@�@J@�#@�7@hs@X@�@�`@�9@�u@�@r�@bN@A�@ �@�@��@l�@+@��@�R@�+@E�@�T@�-@�@�@�@�@�@�@`B@/@�@��@�j@�@��@z�@j@I�@�@1@�m@�
@ƨ@t�@C�@C�@"�@o@��@=q@�@�@��@�7@G�@�@��@�9@��@��@�u@A�@b@\)@��@v�@ff@E�@{@@�h@�@p�@p�@/@�/@�D@Z@�@��@�m@�
@�@t�@dZ@dZ@S�@C�@"�@@
��@
��@
��@
��@
M�@	��@	��@	�7@	X@	G�@	&�@��@Ĝ@�9@�@r�@A�@1'@ �@�@�;@��@��@|�@K�@�@��@��@��@�y@ȴ@��@��@�+@�@O�@/@/@/@�/@�@��@�@z�@I�@I�@I�@9X@�@1@��@�m@��@�m@�
@�F@��@�@t�@dZ@dZ@C�@C�@"�@��@��@�\@=q@-@J@��@��@�@�7@x�@G�@ ��@ �9@ �@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�ffAʹ9AʮAĥ�A���A�r�A�ȴA�t�A�v�A��A��-A�^5A��A�l�A��A���A��+A��TA��A��A���A�+A��^A���A��FA��\A�I�A���A��jA���A�VA�ȴA�O�A��#A�v�A�oA���A��jA�bA�"�A��-A� �A�l�A�Q�A���A�7LA�x�A��A�"�A���A��!A�~�A�K�A��A�VA���A���A�v�A��A��A�1'A��wA�(�A�?}A�p�A�bA��mA��
A�~�A�A��HA�
=A�/A�VA��A���A�O�A��#A�%A���A�|�A�%A~ĜA}`BA|^5Az�Ax�uAv5?Atv�Ar�HArAq�Ap�`Ao/Am�^Am�Al�Al��Al5?Ak��Aj��Ai�#Ahn�Ag�Af�9Ael�Ae/Ad��AdVAb��A`ĜA^�A^M�A]�;A]�A[A[dZAZ��AZ�AZAYx�AXM�AW��AW�AVbNAU��AT�uAS/AQ�FAP^5ANn�ALjAKG�AJZAIl�AH��AHffAH(�AG�AG;dAE�-AEAD��AD=qAC��AC��AB�RAA�mAA�A@�A?C�A>Q�A=%A<^5A;t�A;C�A:�A:~�A: �A9|�A8��A8A7G�A6��A6�+A5p�A4�A4JA3�A3p�A2��A1�A1x�A0�uA05?A01A/��A/%A.�HA.JA,��A,ZA+��A+XA+A*=qA)��A)�A'�wA'XA&�A&n�A&  A%VA$^5A#�mA#O�A"��A"1'A!dZA �+A�wA+A~�A{A��AXA"�A�HA~�A�FAbA�jA;dA�HA �A�hAjAVA �A�PA5?A��A33A�Av�AE�At�A~�A��A
�A	�
A	t�A�jAA�A�A�mAA�hA?}A��A�PAĜA\)A1AVA �A ��A �u@�;d@��@�Z@��@���@��@��/@�D@�1@�o@���@�@���@��m@�+@�^5@蛦@�o@旍@�-@�j@��@�"�@⟾@�V@�-@�A�@��T@� �@�-@��`@��m@��@�5?@�Q�@�33@�5?@�7L@϶F@�~�@��@�C�@ʟ�@�=q@ɲ-@��@���@�Q�@��@���@ă@���@�M�@�O�@���@�E�@��-@��@�`B@�?}@�V@��9@�bN@�t�@���@�E�@��@�Ĝ@�A�@���@���@�`B@���@��w@��\@�X@�j@�
=@��T@�/@�Q�@�b@���@��m@�ƨ@��@��!@�v�@�$�@��T@�`B@��@��
@��@���@�^5@�J@�G�@��D@�1@��@�l�@�o@��@��^@�p�@�X@�V@��`@���@��@��!@��@��h@���@�r�@�|�@�K�@�33@�
=@�~�@�J@���@���@�p�@�O�@���@��D@�Q�@�l�@���@�5?@�@�hs@���@���@�Z@�  @���@�"�@��H@���@���@�hs@�?}@�&�@���@�  @��w@�C�@�ȴ@���@�^5@��@���@�x�@��@��@��@���@���@�t�@�K�@�
=@���@�5?@���@���@���@���@��7@�G�@�7L@�/@�&�@��@��u@��@���@�l�@�@���@���@�ff@���@��T@��^@��h@�/@�Ĝ@��@��@���@���@�j@��F@�33@��@��@�M�@�{@�@���@��T@��#@��#@�@���@��h@�X@��`@�(�@|�@~�R@~�+@~�+@~ff@~V@~$�@~5?@~5?@~@}��@|�j@|9X@{��@z��@yX@x��@xA�@xA�@x �@w��@vȴ@u�@t��@t��@t9X@s�@s33@so@s@r�@r�H@r��@q��@q�#@qhs@q7L@q&�@q�@p�`@p��@p�9@pbN@p �@o�;@o��@o+@n�@n�R@n$�@m��@l�@l�@l��@l��@l�D@lz�@lZ@kƨ@k"�@j��@ihs@h�`@h�@hb@g��@g�@f�@fȴ@fȴ@f��@f5?@f$�@e�@e/@d��@dz�@ct�@c@b��@bM�@bJ@ahs@a%@`Ĝ@`�@`r�@`1'@` �@`b@_�@_|�@^��@^$�@]@]�-@]�-@]��@]?}@\��@\9X@[�m@[ƨ@[t�@[@Z�H@Z��@Z~�@Z�@Y�^@Y��@Y��@X��@X�9@X��@XQ�@W�@W�@W��@Wl�@W�@Vȴ@VE�@V{@U?}@T9X@T�@S��@SS�@So@R�H@R��@R��@Rn�@RM�@R�@Q��@P��@P�u@P�@PA�@Pb@O�w@Ol�@N�y@Nv�@NE�@M�@M�h@L�@LZ@L(�@Kƨ@KS�@J�@Jn�@I��@I&�@I%@HĜ@H1'@G�w@G�P@G�P@G\)@F�R@F�+@Fff@FV@FE�@E�T@E/@D�@D�j@Dz�@D1@C�@C"�@B��@B-@A��@AX@A7L@A�@@�9@@bN@@  @?�@?|�@?�@?�@?�@?�@>��@>ȴ@>��@>5?@=�-@<�@<�D@<1@;�@;C�@;@:��@:~�@:^5@:M�@:J@9��@9hs@97L@8Ĝ@81'@8b@7�@7�P@7K�@7
=@6�@6v�@6E�@6E�@65?@6$�@6@5�T@5@5�-@5�h@5`B@4�j@4I�@4�@3��@3�m@3ƨ@3��@3S�@3"�@2�H@2M�@1��@1G�@0��@0r�@01'@/�@/�w@/�P@/\)@/+@.��@.�y@.�@.�@.�R@.��@.V@.@-�@-��@-?}@-�@-�@,�@,�@,Z@+�
@+t�@+"�@+@*�@*��@*�!@*��@*~�@)��@)�^@)��@)�7@)X@)7L@)%@(�`@(Q�@(b@'�@'�;@'��@'�w@'�w@'�w@'��@'l�@'\)@'\)@'
=@&��@%�@%/@$�@$�D@$z�@$9X@$�@$�@$1@#�m@#�F@#��@#dZ@#C�@"�H@"��@"�\@"^5@"=q@!��@!�^@!��@!X@ ��@ A�@ b@�;@�@;d@�y@��@v�@E�@5?@$�@�T@�-@�@/@/@��@�j@�D@j@1@1@1@��@�m@t�@o@�H@��@�\@�\@^5@�@J@�#@�7@hs@X@�@�`@�9@�u@�@r�@bN@A�@ �@�@��@l�@+@��@�R@�+@E�@�T@�-@�@�@�@�@�@�@`B@/@�@��@�j@�@��@z�@j@I�@�@1@�m@�
@ƨ@t�@C�@C�@"�@o@��@=q@�@�@��@�7@G�@�@��@�9@��@��@�u@A�@b@\)@��@v�@ff@E�@{@@�h@�@p�@p�@/@�/@�D@Z@�@��@�m@�
@�@t�@dZ@dZ@S�@C�@"�@@
��@
��@
��@
��@
M�@	��@	��@	�7@	X@	G�@	&�@��@Ĝ@�9@�@r�@A�@1'@ �@�@�;@��@��@|�@K�@�@��@��@��@�y@ȴ@��@��@�+@�@O�@/@/@/@�/@�@��@�@z�@I�@I�@I�@9X@�@1@��@�m@��@�m@�
@�F@��@�@t�@dZ@dZ@C�@C�@"�@��@��@�\@=q@-@J@��@��@�@�7@x�@G�@ ��@ �9@ �@ r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B6FB5?B1'B)�B �B	7B��B�B�B�NB�/B�
B��B��BȴBŢB�}B�jB�XB�9B��B��B�{B�=Bz�Bs�Br�Bp�Bm�BffBaHB`BB]/BZBVBP�BK�BG�B@�B7LB(�B�B\B1BBB��B�B�`B��BǮBÖB��B�qB�^B�9B�B��B��B�bB�Bw�Bl�BW
BM�BA�B6FB/B�BbB
=BB
�B
�
B
ȴB
�^B
�'B
��B
��B
��B
�=B
�%B
�B
}�B
t�B
iyB
`BB
T�B
K�B
?}B
49B
(�B
!�B
�B
�B
VB
+B
B
B
  B	��B	��B	�B	�B	�`B	�NB	�)B	��B	��B	��B	��B	ĜB	�jB	�3B	�!B	�B	��B	��B	��B	��B	��B	��B	�{B	�bB	�JB	�7B	�B	�B	|�B	v�B	o�B	iyB	`BB	XB	R�B	N�B	J�B	G�B	F�B	E�B	C�B	?}B	9XB	6FB	5?B	33B	1'B	/B	+B	'�B	#�B	�B	�B	�B	oB	\B	DB	
=B	1B	%B	B	B��B��B��B�B�B�B�fB�ZB�NB�HB�5B�#B�B��B��B��B��B��B��BǮBB��B�jB�^B�XB�FB�3B�!B�B��B��B��B��B��B��B��B��B�{B�oB�VB�JB�7B�+B�B�B�B�B�B~�B}�By�Bt�Bp�Bm�Bl�BjBgmBdZBaHB_;B\)BZBXBW
BVBT�BS�BQ�BO�BM�BK�BI�BG�BF�BF�BE�BE�BD�BC�BB�B@�B>wB<jB:^B8RB7LB7LB6FB5?B49B33B1'B/B/B.B.B.B-B,B+B)�B)�B(�B(�B'�B&�B&�B%�B%�B$�B$�B$�B#�B#�B"�B!�B!�B!�B!�B!�B!�B!�B �B!�B!�B!�B"�B#�B#�B%�B%�B&�B&�B&�B&�B&�B'�B&�B(�B)�B)�B-B-B0!B2-B33B33B33B33B49B49B49B6FB7LB7LB9XB:^B:^B;dB=qBB�BC�BE�BE�BF�BG�BI�BL�BN�BP�BQ�BQ�BQ�BR�BT�BW
B\)B^5B_;B`BB_;B^5B_;B`BBaHBbNBe`BgmBiyBjBk�Bl�Bq�Br�Bs�Bs�Bu�Bu�Bv�Bz�B~�B�B�B�%B�+B�DB�DB�DB�JB�VB�bB�oB�oB�uB�uB��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�9B�?B�dB�wB�wB�}BBǮBɺB��B��B��B��B�B�B�B�/B�BB�TB�ZB�ZB�`B�fB�mB�yB�B�B�B�B�B�B��B��B��B��B��B��B	B	B		7B	PB	\B	bB	oB	�B	�B	�B	�B	�B	!�B	!�B	!�B	"�B	"�B	#�B	'�B	+B	+B	,B	0!B	1'B	1'B	2-B	33B	33B	49B	5?B	7LB	8RB	9XB	;dB	?}B	C�B	E�B	F�B	G�B	G�B	H�B	I�B	J�B	L�B	M�B	O�B	Q�B	T�B	XB	^5B	dZB	gmB	hsB	hsB	iyB	k�B	n�B	s�B	u�B	u�B	v�B	x�B	z�B	z�B	{�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�DB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�'B	�3B	�9B	�?B	�XB	�^B	�dB	�qB	�wB	��B	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�fB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
1B
1B

=B

=B

=B
DB

=B
DB
PB
VB
VB
\B
bB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
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
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
H�B
I�B
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
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
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
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
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
ffB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
o�B
p�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
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
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B7B7fB6FB2�B-�B,B B�B�B�B��B��B��B�}BʌB��B�iB��B�B��B��B��B��B�B|�BtBs3Bq�Bo5Bh
Ba�BabB^B[=BW
BQ�BL�BIBBuB8�B*�B�B�B	�B�B�B�$B��B�XBӏB�fB��B�B�(B�B�tB�IB�B�:B�TB�By�BoiBX�BO�BCaB7�B1�B!�B�B�BB
�nB
�eB
�B
��B
��B
�yB
�B
�+B
�DB
��B
�SB
�B
vzB
k6B
b�B
W?B
NpB
A�B
5�B
)�B
"�B
�B
�B
�B
�B
{B
oB
 �B	��B	�B	��B	�B	�LB	�B	�dB	ՁB	ԕB	�&B	��B	��B	�]B	�B	��B	�/B	�DB	�NB	�\B	�jB	�]B	�B	��B	�NB	�B	�=B	�YB	��B	~�B	x�B	q�B	k�B	b�B	Y�B	T,B	O�B	K�B	HB	F�B	F?B	D�B	A B	:DB	6�B	5�B	3�B	1�B	0;B	,"B	)B	%,B	 �B	�B	B	[B	bB	�B	
�B	�B	�B	B	'B��B��B�ZB�B��B�B�8B��B��B�NB�;B��B�BՁB�[B҉BϑB�jB��B�B�aB�UB��B�B�^B�2B�9B��B��B��B��B��B��B��B�WB�eB�YB��B��B��B�PB�#B�B��B��B��B�oB��B�BcB|Bv�BraBnIBm�Bk�BiBfBb�B`\B]�BZ�BX�BW�BV�BU�BU2BS@BQBOBBL�BJrBH�BGEBF�BFBE�BEBDMBC�BBB?�B>]B<B9rB7�B7�B6�B6�B5ZB4�B2�B1B/�B.�B.}B.�B.B-�B,WB+B+B)�B)�B)*B'�B'mB&�B&�B%`B%zB%FB$@B$�B$B#nB# B# B"�B"�B"�B"�B!�B"�B"�B"�B#�B$�B%FB&�B&fB'RB'mB'mB'RB'mB(�B(XB)�B*�B+B-�B./B1B2�B3hB3hB3hB3�B4�B4�B4�B6�B7�B7�B9�B:�B:�B<6B>]BCBDgBF�BF�BGzBH�BJ�BMPBOvBQBR BR BR:BSuBUMBWYB\xB^�B_�B`�B_�B^�B_�B`�Ba�Bb�Be�Bg�Bi�Bj�Bk�BmCBq�Br�Bs�BtBu�Bv+Bw�B{BcB�uB��B��B��B�xB�xB��B��B��B��B��B��B��B��B��B��B�KB�B�'B�:B�&B�LB�*B�kB�qB�}B�vB��B��B��B��B��B��B��B�B��B�#B�B�B�4B�&B�SB�KBچBݘB��B�B�B�tB�zB�B��B��B��B�B�B��B��B��B��B��B��B��B�JB�cB	aB	mB		lB	�B	�B	�B	�B	�B	�B	�B	�B	B	!�B	!�B	!�B	"�B	# B	$@B	(>B	+B	+6B	,qB	0UB	1AB	1AB	2-B	3MB	3MB	4TB	5tB	7fB	8�B	9�B	;�B	?�B	C�B	E�B	F�B	G�B	G�B	H�B	I�B	J�B	L�B	NB	P.B	R:B	UMB	XyB	^�B	d�B	g�B	hsB	h�B	i�B	k�B	o B	s�B	u�B	vB	wB	x�B	z�B	z�B	|B	|B	}"B	.B	�B	�;B	�-B	�-B	�B	�3B	�9B	�SB	�?B	�KB	�RB	�XB	�xB	�PB	�pB	��B	��B	��B	��B	��B	�sB	��B	��B	��B	��B	��B	�B	�'B	�B	�B	�$B	�0B	�=B	�)B	�/B	�5B	�5B	�OB	�'B	�[B	�vB	�MB	��B	��B	�rB	�xB	��B	��B	��B	��B	��B	ªB	ªB	ªB	ðB	ðB	ÖB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	� B	�B	� B	��B	�B	�B	�9B	�
B	�1B	�1B	�KB	�=B	�CB	�]B	�dB	�;B	�VB	�\B	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	��B	�<B	�.B
 4B
 B
[B
3B
3B
9B
mB
YB
EB
KB
1B
fB

XB

XB

XB
^B

rB
xB
jB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 �B
!�B
"�B
"�B
$B
$�B
$�B
$�B
%�B
&B
'B
'B
'B
(
B
)B
)B
*B
*B
+B
+B
,B
,"B
-B
-B
-)B
-)B
-)B
./B
./B
./B
.IB
.IB
/OB
0;B
0;B
0;B
1AB
1AB
1AB
2-B
2-B
2aB
3hB
4nB
5tB
6zB
6`B
7fB
7fB
7fB
8RB
8RB
8lB
9XB
9rB
9XB
9XB
9rB
9rB
:xB
:xB
:xB
;B
<�B
<jB
<�B
<�B
<�B
=�B
>�B
>�B
?}B
?�B
?�B
?}B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
H�B
I�B
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
L�B
L�B
L�B
M�B
M�B
OB
N�B
OB
OB
O�B
O�B
Q B
QB
P�B
RB
RB
SB
R�B
R�B
SB
SB
TB
TB
S�B
S�B
T,B
UB
UB
UB
VB
VB
VB
VB
VB
W$B
W$B
X+B
XB
XB
X+B
X+B
YB
Y1B
Y1B
Z7B
Z7B
Z7B
ZQB
[=B
[=B
[#B
[#B
[=B
[=B
\CB
\CB
\CB
\)B
]IB
]IB
]IB
^5B
^jB
^OB
_VB
_VB
_;B
_;B
`BB
`BB
`BB
`\B
`\B
`BB
`BB
`\B
aHB
aHB
abB
aHB
abB
abB
bNB
bhB
bNB
bNB
bhB
bNB
cTB
cnB
cnB
cnB
c�B
dtB
dtB
dtB
ezB
ezB
e�B
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
h�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
o�B
p�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y	B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{B
z�B
z�B
{�B
|B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<P�</O<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006170035132020061700351320200617003513202211182143222022111821432220221118214322202006180017332020061800173320200618001733  JA  ARFMdecpA19c                                                                20200607033747  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200606183749  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200606183752  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200606183752  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200606183753  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200606183753  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200606183753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200606183753  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200606183753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200606183753  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200606183754  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200606183754                      G�O�G�O�G�O�                JA  ARUP                                                                        20200606185413                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200606153604  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200606153535  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20200616153513  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200616153513  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200617151733  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124322  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114512                      G�O�G�O�G�O�                