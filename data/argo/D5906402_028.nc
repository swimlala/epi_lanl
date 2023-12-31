CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-08-19T05:38:38Z creation; 2022-04-26T16:07:00Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  Z�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  |   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d )P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 0�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 d N<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � U�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` s(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   s�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   y�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210819053838  20220426232408  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_028                 8138_008904_028                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @ٌ�NP��@ٌ�NP��11  @ٌ�}Vl�@ٌ�}Vl�@-x�@��@-x�@���d�p
�d�p
11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @�  @�G�@�  @��RA\)A\)A+�A@  AaG�A�Q�A�  A�Q�A�Q�A�  AϮA�Q�A�  B   B  B(�B  B   B((�B0  B8  B@  BH(�BP  BW�
B_�Bg�
Bp(�Bx(�B�  B�  B�  B�  B��B��B�  B�{B�  B��B��B�  B�=qB�{B��B��B��B�  B�  B�{B�  B�  B�  B�  B�  B��B��B��B�  B�(�B�{B�  C   C  C  C
=C
=C
  C��C  C��C�C  C
=C  C
=C  C  C   C"
=C$  C%��C'��C*  C,
=C.  C/��C2  C4  C5��C8  C:  C;��C=��C@  CB  CD
=CF
=CH  CJ  CL
=CN  CP  CR  CT  CV  CX  CZ  C\
=C]��C_��Cb
=Cd
=Cf
=Ch  Ci��Cl  Cm��Co��Cq��Cs��Cv  Cw��Cz  C|  C}��C�C�C�  C�  C���C���C���C���C���C���C���C���C���C���C�  C�  C�
=C�\C�  C���C�  C�  C�  C�C�  C���C�  C�C�C�  C���C�  C�  C�  C���C���C���C���C�  C�  C�  C�C�  C�  C�C�C�C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�C�  C�  C�C���C�  C�  C���C���C���C�  C���C�  C�C�C�
=C�C�C�C�  C�  C���C���C�  C�  C�  C�  C�C�C�C�C�C�  C���C�  C�C�  C���C�  C�C�  C�  C�  C�  C�  C�C���C���C���C�  C�C���C���C���C�  C�C�C�  C���C�  C�  C���C�C�  C�  C�  C�C�
=C�  C���C�  D �D ��D�D}qD�qD� D�qD� D  D}qD  D�D�D}qD  D��D�D��D	�D	}qD
  D
�D�D� D  D� D�qD}qD�qD� D�D��D  D}qD  D��D  D}qD  D� D�D� D  D� D�qD� D�D��D�D�D  Dz�D�qD� D  D� D�D� D  D� D  D� D  D��D �D �D!D!� D"  D"� D"��D#z�D#�qD$}qD%  D%��D%�qD&z�D&�qD'}qD'�qD(}qD)  D)��D*  D*}qD+  D+�D,�D,� D-�D-��D.�D.� D/�D/� D/�qD0� D1  D1� D1�qD2}qD3  D3� D3�qD4}qD4�qD5��D6  D6}qD6�qD7}qD8�D8�D9�D9� D:  D:��D;  D;� D<  D<� D=�D=��D>  D>� D?  D?� D@�D@��DA�DA� DB  DB��DC�DC��DD  DD� DE  DE}qDF  DF��DG  DG� DH�DH� DI  DI� DI�qDJ}qDK  DK��DL�DL}qDM  DM� DN  DN� DO  DO��DP�DP}qDP�qDQ}qDR  DR}qDR�qDS}qDS�qDT� DT�qDU}qDU�qDV}qDW  DW� DX�DX}qDX�qDY� DZ�DZ}qD[  D[��D\  D\� D]  D]}qD^  D^��D_D_��D`  D`� Da�Da� Da�qDb}qDc  Dc��Dd  Dd��De  De� Df�Df� Dg�Dg��Dh  Dh}qDi  Di� Dj  Dj� Dk  Dk}qDk�qDl� Dm  Dm� Dn  Dn� Do�Do� Dp  Dp��Dq�Dq� Dq�qDr� Ds  Ds� Dt�Dt��Du  Duz�Du�qDv� Dv�qDw� DxDx� Dx�qDy� Dz�Dz� D{  D{��D{�qD|}qD}  D}��D~�D~��D  D� D�  D�AHD�� D��HD�HD�@ D�}qD���D�  D�@ D��HD��HD�HD�B�D��HD��HD�  D�>�D�~�D�� D���D�AHD�� D���D�  D�>�D�}qD�� D�  D�AHD�� D���D���D�@ D�~�D��qD���D�AHD�� D���D��qD�>�D��HD�� D�  D�AHD�� D���D���D�@ D��HD��HD�HD�@ D�� D���D���D�=qD�� D��HD�  D�AHD���D�� D���D�@ D�� D��HD�  D�=qD�� D��HD���D�>�D��HD�� D���D�@ D��HD��HD�  D�>�D�}qD���D���D�@ D��HD�� D���D�@ D�� D��qD���D�@ D��HD��HD�HD�@ D�~�D�� D�  D�>�D��HD���D��qD�=qD�~�D�� D�  D�>�D�}qD���D���D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D�� D���D�  D�>�D�~�D���D�  D�@ D�~�D���D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D��HD�� D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�@ D�� D��HD�HD�@ D�� D�� D��qD�>�D��HD�D�  D�>�D�~�D���D�  D�@ D�� D��HD��D�B�D�~�D���D�  D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D��HD�� D�  D�>�D�� D�D�  D�@ D�� D���D�HD�@ D�~�D�� D��D�AHD�� D�� D�  D�AHDHD¾�D�  D�@ DÀ D�� D���D�@ DĀ D�� D�HD�>�Dŀ D�� D���D�=qD�}qDƾ�D�  D�AHD�~�DǽqD��qD�=qDȀ D��HD�  D�AHDɁHD�� D���D�@ Dʀ Dʾ�D�HD�AHD�~�D�� D�  D�@ D̀ D�� D�HD�@ D̀ D��HD�  D�@ D�~�D�� D��D�@ Dπ DϾ�D���D�@ DЀ D��HD�HD�@ D�~�D�� D�HD�B�DҁHDҾ�D�  D�@ DӀ D��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDւ�D�D�HD�>�D�~�D�� D�  D�@ D؀ D�� D���D�>�Dـ D�� D�  D�>�D�~�D�� D�HD�>�D�~�D۾�D�  D�AHD܁HD�� D��qD�=qD�}qD�� D�  D�=qDހ D��HD�  D�>�D�~�D��HD��D�AHD�~�D�qD�  D�AHD� DᾸD���D�@ D� D⾸D�  D�AHD� D�� D�HD�>�D� D��HD���D�=qD�~�D�� D���D�@ D�~�D�� D���D�>�D�~�D��HD�  D�@ D�~�D�qD��qD�@ D� D龸D�  D�AHDꂏD�=?#�
?W
=?�\)?�{?\?�G�@�\@��@(�@+�@8Q�@J=q@Tz�@c�
@u@�  @��@�\)@�z�@�(�@��@�=q@�33@��H@�G�@Ǯ@�{@�@�(�@��
@�=q@��@���@��RA�\A
=A	��Ap�AG�Az�A��A��A   A$z�A(Q�A+�A0��A3�
A7�A<(�A@  AC�
AHQ�AL(�AP  AU�AX��A\��AaG�AeAi��An{Ar�\AvffA{�A�  A��A�(�A�ffA�Q�A�=qA���A�
=A���A�33A�p�A�\)A�G�A��
A�{A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?��@   @@  @�  @�  @�G�@�  @��RA\)A\)A+�A@  AaG�A�Q�A�  A�Q�A�Q�A�  AϮA�Q�A�  B   B  B(�B  B   B((�B0  B8  B@  BH(�BP  BW�
B_�Bg�
Bp(�Bx(�B�  B�  B�  B�  B��B��B�  B�{B�  B��B��B�  B�=qB�{B��B��B��B�  B�  B�{B�  B�  B�  B�  B�  B��B��B��B�  B�(�B�{B�  C   C  C  C
=C
=C
  C��C  C��C�C  C
=C  C
=C  C  C   C"
=C$  C%��C'��C*  C,
=C.  C/��C2  C4  C5��C8  C:  C;��C=��C@  CB  CD
=CF
=CH  CJ  CL
=CN  CP  CR  CT  CV  CX  CZ  C\
=C]��C_��Cb
=Cd
=Cf
=Ch  Ci��Cl  Cm��Co��Cq��Cs��Cv  Cw��Cz  C|  C}��C�C�C�  C�  C���C���C���C���C���C���C���C���C���C���C�  C�  C�
=C�\C�  C���C�  C�  C�  C�C�  C���C�  C�C�C�  C���C�  C�  C�  C���C���C���C���C�  C�  C�  C�C�  C�  C�C�C�C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�C�  C�  C�C���C�  C�  C���C���C���C�  C���C�  C�C�C�
=C�C�C�C�  C�  C���C���C�  C�  C�  C�  C�C�C�C�C�C�  C���C�  C�C�  C���C�  C�C�  C�  C�  C�  C�  C�C���C���C���C�  C�C���C���C���C�  C�C�C�  C���C�  C�  C���C�C�  C�  C�  C�C�
=C�  C���C�  D �D ��D�D}qD�qD� D�qD� D  D}qD  D�D�D}qD  D��D�D��D	�D	}qD
  D
�D�D� D  D� D�qD}qD�qD� D�D��D  D}qD  D��D  D}qD  D� D�D� D  D� D�qD� D�D��D�D�D  Dz�D�qD� D  D� D�D� D  D� D  D� D  D��D �D �D!D!� D"  D"� D"��D#z�D#�qD$}qD%  D%��D%�qD&z�D&�qD'}qD'�qD(}qD)  D)��D*  D*}qD+  D+�D,�D,� D-�D-��D.�D.� D/�D/� D/�qD0� D1  D1� D1�qD2}qD3  D3� D3�qD4}qD4�qD5��D6  D6}qD6�qD7}qD8�D8�D9�D9� D:  D:��D;  D;� D<  D<� D=�D=��D>  D>� D?  D?� D@�D@��DA�DA� DB  DB��DC�DC��DD  DD� DE  DE}qDF  DF��DG  DG� DH�DH� DI  DI� DI�qDJ}qDK  DK��DL�DL}qDM  DM� DN  DN� DO  DO��DP�DP}qDP�qDQ}qDR  DR}qDR�qDS}qDS�qDT� DT�qDU}qDU�qDV}qDW  DW� DX�DX}qDX�qDY� DZ�DZ}qD[  D[��D\  D\� D]  D]}qD^  D^��D_D_��D`  D`� Da�Da� Da�qDb}qDc  Dc��Dd  Dd��De  De� Df�Df� Dg�Dg��Dh  Dh}qDi  Di� Dj  Dj� Dk  Dk}qDk�qDl� Dm  Dm� Dn  Dn� Do�Do� Dp  Dp��Dq�Dq� Dq�qDr� Ds  Ds� Dt�Dt��Du  Duz�Du�qDv� Dv�qDw� DxDx� Dx�qDy� Dz�Dz� D{  D{��D{�qD|}qD}  D}��D~�D~��D  D� D�  D�AHD�� D��HD�HD�@ D�}qD���D�  D�@ D��HD��HD�HD�B�D��HD��HD�  D�>�D�~�D�� D���D�AHD�� D���D�  D�>�D�}qD�� D�  D�AHD�� D���D���D�@ D�~�D��qD���D�AHD�� D���D��qD�>�D��HD�� D�  D�AHD�� D���D���D�@ D��HD��HD�HD�@ D�� D���D���D�=qD�� D��HD�  D�AHD���D�� D���D�@ D�� D��HD�  D�=qD�� D��HD���D�>�D��HD�� D���D�@ D��HD��HD�  D�>�D�}qD���D���D�@ D��HD�� D���D�@ D�� D��qD���D�@ D��HD��HD�HD�@ D�~�D�� D�  D�>�D��HD���D��qD�=qD�~�D�� D�  D�>�D�}qD���D���D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D�� D���D�  D�>�D�~�D���D�  D�@ D�~�D���D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D���D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D��HD�� D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�@ D�� D��HD�HD�@ D�� D�� D��qD�>�D��HD�D�  D�>�D�~�D���D�  D�@ D�� D��HD��D�B�D�~�D���D�  D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D��HD�� D�  D�>�D�� D�D�  D�@ D�� D���D�HD�@ D�~�D�� D��D�AHD�� D�� D�  D�AHDHD¾�D�  D�@ DÀ D�� D���D�@ DĀ D�� D�HD�>�Dŀ D�� D���D�=qD�}qDƾ�D�  D�AHD�~�DǽqD��qD�=qDȀ D��HD�  D�AHDɁHD�� D���D�@ Dʀ Dʾ�D�HD�AHD�~�D�� D�  D�@ D̀ D�� D�HD�@ D̀ D��HD�  D�@ D�~�D�� D��D�@ Dπ DϾ�D���D�@ DЀ D��HD�HD�@ D�~�D�� D�HD�B�DҁHDҾ�D�  D�@ DӀ D��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDւ�D�D�HD�>�D�~�D�� D�  D�@ D؀ D�� D���D�>�Dـ D�� D�  D�>�D�~�D�� D�HD�>�D�~�D۾�D�  D�AHD܁HD�� D��qD�=qD�}qD�� D�  D�=qDހ D��HD�  D�>�D�~�D��HD��D�AHD�~�D�qD�  D�AHD� DᾸD���D�@ D� D⾸D�  D�AHD� D�� D�HD�>�D� D��HD���D�=qD�~�D�� D���D�@ D�~�D�� D���D�>�D�~�D��HD�  D�@ D�~�D�qD��qD�@ D� D龸D�  D�AHDꂏG�O�?#�
?W
=?�\)?�{?\?�G�@�\@��@(�@+�@8Q�@J=q@Tz�@c�
@u@�  @��@�\)@�z�@�(�@��@�=q@�33@��H@�G�@Ǯ@�{@�@�(�@��
@�=q@��@���@��RA�\A
=A	��Ap�AG�Az�A��A��A   A$z�A(Q�A+�A0��A3�
A7�A<(�A@  AC�
AHQ�AL(�AP  AU�AX��A\��AaG�AeAi��An{Ar�\AvffA{�A�  A��A�(�A�ffA�Q�A�=qA���A�
=A���A�33A�p�A�\)A�G�A��
A�{A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AܓuAܕ�Aܗ�A܋DA܍PAܗ�Aܛ�Aܕ�A�~�A�|�A�K�A��TAۓuAۇ+A�|�A�~�A�|�A�z�A�t�A�p�A�n�A�hsA�bNA�\)A�XA�O�A�K�A�I�A�I�A�E�A�-A��A�oA�1A�A���A��A��;AڼjAڙ�A�O�A��A�bA�=qA��A�/A�bNA��#A�ffA�x�A�7LA�+A�XAǣ�Aŧ�A��#A�$�A���A��A���A�{A��FA��A���A�XA�p�A���A�r�A�jA��A���A���A��A���A�t�A���A��7A�K�A�ƨA���A���A�{A�hsA���A�1A�"�A�(�A��HA�/A��A���A���A��wA�K�A��jA��^A�t�A��mA"�A}��A{�TAz�DAy&�Aw�TAv��At�RAp�AnVAk�-AiAh�Af�HAd�/AbM�A`{A]S�A[�AV$�ARĜAQ�AO��AF�yA@{A?&�A>-A;
=A9�TA9�FA9��A8��A6�jA6VA6I�A5�A5�^A5��A5S�A5/A3%A1ƨA1x�A17LA0��A0ffA/�A-p�A,�RA,=qA+`BA+dZA+�FA+��A+p�A*��A)�mA(��A({A&�A$��A$I�A$1A#��A"�`A"�A"�9A!�^A!"�A �!A E�A (�A�AƨA��AbNA$�A��A�-Al�AȴAVA�HA~�AVA5?A�AhsA��AbNA �A{A��A�A�
A;dA�yAffA-A��Av�A9XA�FAp�A��AbA�;A�hA%A��A��A/A�A�uA^5A9XA(�A  A��Ax�A&�A
�`A
�A	�A�+An�AVAA�A �A��AhsA"�A�\AG�AQ�AJA�wAt�AC�AVA�yA�A��A9XA$�A9XA�A�
A�A ��A �@���@�S�@�33@��R@�@�p�@�%@���@��@���@��u@�9X@�1@��@���@�$�@���@�O�@�/@���@�1'@�ƨ@�@�^5@�hs@�7L@���@��@�ƨ@@�J@���@�Z@��@���@띲@�dZ@���@ꗍ@�M�@�h@��@�P@�S�@�33@�o@��@�ȴ@�+@�E�@�$�@�@���@�x�@��@�1'@㝲@⟾@�X@�Q�@ߍP@�;d@�33@ް!@��@�&�@�Ĝ@��@ۅ@�+@���@�J@١�@�`B@�/@���@��/@ج@�Q�@� �@ץ�@�@ְ!@�v�@�V@��@���@���@�@�A�@�o@ҧ�@�M�@�{@���@с@�p�@���@�+@��@���@ΰ!@�v�@�$�@�O�@�ȴ@ɑh@�O�@�O�@�/@���@ȓu@�Z@�(�@�b@Ǖ�@���@��#@ř�@�%@�bN@�1@��
@�S�@���@§�@�@+@�~�@�v�@�V@�-@�@�%@���@���@�-@��#@��7@�V@��9@��@�(�@��w@�l�@��y@���@���@���@�^5@��T@�&�@�Ĝ@� �@��P@�ȴ@�{@�O�@�(�@�@��+@�M�@�5?@��@��@��@��`@��j@��u@�I�@� �@���@�ƨ@�t�@��+@�-@��@��T@�O�@��D@���@���@�l�@�K�@�
=@���@�v�@�^5@�V@�M�@�E�@�5?@�J@�p�@��/@��D@��@��@��@�r�@��@��P@�t�@�C�@�o@�@�ȴ@���@��\@�n�@�ff@�^5@�M�@�$�@�J@���@��T@�X@�A�@���@�v�@��#@��-@��@�?}@�V@���@�z�@��@��m@��
@��w@��F@���@��@�S�@�+@�o@���@��!@���@���@��\@�~�@�=q@�p�@��9@� �@��@�b@�  @��w@�l�@�33@�o@��H@��\@�=q@�x�@�7L@���@�Z@���@�dZ@�;d@�"�@��@�@���@�~�@�-@��@���@��7@�7L@��@���@���@���@�Ĝ@�z�@�j@�Z@�Q�@�A�@��@��@�J@��7@��@��/@��j@���@�A�@�1@��m@��
@��@��P@�C�@��@��R@�n�@��@�@��T@�x�@�O�@�7L@���@��
@�dZ@��@�~�@��@���@��-@���@�hs@�?}@�V@�%@���@��@��@�A�@��@��@�t�@�l�@�\)@�C�@��+@���@���@�hs@�`B@�X@�X@�X@�X@�G�@�7L@��j@��@�(�@�ƨ@�+@�ȴ@���@��+@�{@��@��^@���@�x�@�7L@���@��@��@l�@;d@�@
=@~�@~��@~ff@}@}`B@}V@|�@|(�@{ƨ@{S�@z��@yhs@x��@xQ�@w+@vV@u�@up�@t��@t�/@t��@t�@tz�@tZ@t�@r�H@r�@q7L@p��@pQ�@p1'@o�@o�@o
=@nȴ@n$�@m�T@m��@m��@lz�@k��@j�H@j�\@jn�@j�@jJ@i�@i�#@i�^@hĜ@h �@gK�@f��@f5?@e`B@eO�@d�/@d1@cdZ@c@bn�@bM�@b-@a��@a��@aX@`�`@_�;@^�+@]�h@\��@\�j@\�@\�@\�@[��@[33@Z�H@Z�!@Z~�@Zn�@Z=q@Y��@Y�#@Yx�@Y&�@Xr�@WK�@V��@Vv�@V5?@U�-@T��@T��@Tj@SC�@R��@R��@R�\@R�\@R=q@Q��@PA�@N��@NV@NE�@NE�@N$�@N{@N@M�T@M�h@MO�@M?}@M�@L�j@Lj@L1@K"�@J�H@J�!@J=q@I�7@Ix�@Ix�@Ihs@IX@I7L@I�@G�@Gl�@G;d@F�@F��@F5?@E��@E�h@E�@Ep�@E`B@EO�@EO�@D�@DZ@D(�@C��@C�@Ct�@CdZ@B��@B~�@BM�@A�#@AX@@��@@�u@@bN@@1'@?��@?l�@?l�@?l�@?;d@?�@>�R@>5?@=�-@=?}@=?}@<�j@<1@;�@;dZ@;o@:�\@:^5@9�^@9x�@9&�@9&�@9%@8�`@8��@8  @7K�@6�@6�R@6��@6�+@5�@5/@5�@5V@4��@4�/@4j@4(�@3�m@3�@3t�@3dZ@3S�@3o@2��@2J@1�^@1��@1G�@0�9@0A�@01'@0  @/�@/�;@/�;@/�;@/�w@/��@/�P@/|�@/|�@/l�@/\)@/K�@/�@.�y@.E�@.5?@.{@.@-�@-�T@-��@-�-@-��@-��@-�h@-`B@-�@,�/@,�j@,�@,�D@,Z@,I�@,(�@+�m@+ƨ@+��@+S�@+"�@+o@*�@*�H@*�!@*�\@*~�@*=q@*�@*J@)��@)�@)�#@)��@)�7@)hs@)7L@(�`@(��@(A�@(  @'�;@'�@'|�@'l�@'�@&�@&�R@&��@&��@&v�@&E�@&$�@%�@%�-@%�-@%��@%��@%�h@%�@%`B@%O�@%�@$�/@$�@$��@$��@$��@$�D@$�D@$�D@$z�@$j@$9X@$(�@#�m@#�F@#��@#"�@#@"��@"��@"�!@"n�@"n�@"M�@"=q@"-@!��@!�^@!hs@!hs@!%@ Ĝ@ �u@ r�@ �@ r�@ A�@��@\)@;dA܋DAܑhAܓuAܕ�Aܕ�Aܗ�Aܕ�Aܗ�Aܗ�AܓuAܗ�Aܗ�Aܗ�Aܕ�A܉7A܉7A܋DA܇+A܉7A܍PA܏\A܍PAܑhAܗ�Aܕ�Aܙ�Aܛ�Aܙ�Aܛ�Aܝ�Aܗ�Aܙ�Aܙ�Aܗ�Aܙ�A�v�A�v�A܁A܁A�~�A�v�A�z�A�z�A܃A�|�A�1'A�Q�A��A�A���A��A���A۬Aە�Aە�Aۗ�Aە�AۓuAە�AۓuAۑhAۓuAۉ7Aۉ7AۍPAۉ7AۃAۇ+AۃA�~�A�~�A�~�A�z�A�z�A�~�A�|�A�z�A�|�A�~�A�|�A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  AܓuAܕ�Aܗ�A܋DA܍PAܗ�Aܛ�Aܕ�A�~�A�|�A�K�A��TAۓuAۇ+A�|�A�~�A�|�A�z�A�t�A�p�A�n�A�hsA�bNA�\)A�XA�O�A�K�A�I�A�I�A�E�A�-A��A�oA�1A�A���A��A��;AڼjAڙ�A�O�A��A�bA�=qA��A�/A�bNA��#A�ffA�x�A�7LA�+A�XAǣ�Aŧ�A��#A�$�A���A��A���A�{A��FA��A���A�XA�p�A���A�r�A�jA��A���A���A��A���A�t�A���A��7A�K�A�ƨA���A���A�{A�hsA���A�1A�"�A�(�A��HA�/A��A���A���A��wA�K�A��jA��^A�t�A��mA"�A}��A{�TAz�DAy&�Aw�TAv��At�RAp�AnVAk�-AiAh�Af�HAd�/AbM�A`{A]S�A[�AV$�ARĜAQ�AO��AF�yA@{A?&�A>-A;
=A9�TA9�FA9��A8��A6�jA6VA6I�A5�A5�^A5��A5S�A5/A3%A1ƨA1x�A17LA0��A0ffA/�A-p�A,�RA,=qA+`BA+dZA+�FA+��A+p�A*��A)�mA(��A({A&�A$��A$I�A$1A#��A"�`A"�A"�9A!�^A!"�A �!A E�A (�A�AƨA��AbNA$�A��A�-Al�AȴAVA�HA~�AVA5?A�AhsA��AbNA �A{A��A�A�
A;dA�yAffA-A��Av�A9XA�FAp�A��AbA�;A�hA%A��A��A/A�A�uA^5A9XA(�A  A��Ax�A&�A
�`A
�A	�A�+An�AVAA�A �A��AhsA"�A�\AG�AQ�AJA�wAt�AC�AVA�yA�A��A9XA$�A9XA�A�
A�A ��A �@���@�S�@�33@��R@�@�p�@�%@���@��@���@��u@�9X@�1@��@���@�$�@���@�O�@�/@���@�1'@�ƨ@�@�^5@�hs@�7L@���@��@�ƨ@@�J@���@�Z@��@���@띲@�dZ@���@ꗍ@�M�@�h@��@�P@�S�@�33@�o@��@�ȴ@�+@�E�@�$�@�@���@�x�@��@�1'@㝲@⟾@�X@�Q�@ߍP@�;d@�33@ް!@��@�&�@�Ĝ@��@ۅ@�+@���@�J@١�@�`B@�/@���@��/@ج@�Q�@� �@ץ�@�@ְ!@�v�@�V@��@���@���@�@�A�@�o@ҧ�@�M�@�{@���@с@�p�@���@�+@��@���@ΰ!@�v�@�$�@�O�@�ȴ@ɑh@�O�@�O�@�/@���@ȓu@�Z@�(�@�b@Ǖ�@���@��#@ř�@�%@�bN@�1@��
@�S�@���@§�@�@+@�~�@�v�@�V@�-@�@�%@���@���@�-@��#@��7@�V@��9@��@�(�@��w@�l�@��y@���@���@���@�^5@��T@�&�@�Ĝ@� �@��P@�ȴ@�{@�O�@�(�@�@��+@�M�@�5?@��@��@��@��`@��j@��u@�I�@� �@���@�ƨ@�t�@��+@�-@��@��T@�O�@��D@���@���@�l�@�K�@�
=@���@�v�@�^5@�V@�M�@�E�@�5?@�J@�p�@��/@��D@��@��@��@�r�@��@��P@�t�@�C�@�o@�@�ȴ@���@��\@�n�@�ff@�^5@�M�@�$�@�J@���@��T@�X@�A�@���@�v�@��#@��-@��@�?}@�V@���@�z�@��@��m@��
@��w@��F@���@��@�S�@�+@�o@���@��!@���@���@��\@�~�@�=q@�p�@��9@� �@��@�b@�  @��w@�l�@�33@�o@��H@��\@�=q@�x�@�7L@���@�Z@���@�dZ@�;d@�"�@��@�@���@�~�@�-@��@���@��7@�7L@��@���@���@���@�Ĝ@�z�@�j@�Z@�Q�@�A�@��@��@�J@��7@��@��/@��j@���@�A�@�1@��m@��
@��@��P@�C�@��@��R@�n�@��@�@��T@�x�@�O�@�7L@���@��
@�dZ@��@�~�@��@���@��-@���@�hs@�?}@�V@�%@���@��@��@�A�@��@��@�t�@�l�@�\)@�C�@��+@���@���@�hs@�`B@�X@�X@�X@�X@�G�@�7L@��j@��@�(�@�ƨ@�+@�ȴ@���@��+@�{@��@��^@���@�x�@�7L@���@��@��@l�@;d@�@
=@~�@~��@~ff@}@}`B@}V@|�@|(�@{ƨ@{S�@z��@yhs@x��@xQ�@w+@vV@u�@up�@t��@t�/@t��@t�@tz�@tZ@t�@r�H@r�@q7L@p��@pQ�@p1'@o�@o�@o
=@nȴ@n$�@m�T@m��@m��@lz�@k��@j�H@j�\@jn�@j�@jJ@i�@i�#@i�^@hĜ@h �@gK�@f��@f5?@e`B@eO�@d�/@d1@cdZ@c@bn�@bM�@b-@a��@a��@aX@`�`@_�;@^�+@]�h@\��@\�j@\�@\�@\�@[��@[33@Z�H@Z�!@Z~�@Zn�@Z=q@Y��@Y�#@Yx�@Y&�@Xr�@WK�@V��@Vv�@V5?@U�-@T��@T��@Tj@SC�@R��@R��@R�\@R�\@R=q@Q��@PA�@N��@NV@NE�@NE�@N$�@N{@N@M�T@M�h@MO�@M?}@M�@L�j@Lj@L1@K"�@J�H@J�!@J=q@I�7@Ix�@Ix�@Ihs@IX@I7L@I�@G�@Gl�@G;d@F�@F��@F5?@E��@E�h@E�@Ep�@E`B@EO�@EO�@D�@DZ@D(�@C��@C�@Ct�@CdZ@B��@B~�@BM�@A�#@AX@@��@@�u@@bN@@1'@?��@?l�@?l�@?l�@?;d@?�@>�R@>5?@=�-@=?}@=?}@<�j@<1@;�@;dZ@;o@:�\@:^5@9�^@9x�@9&�@9&�@9%@8�`@8��@8  @7K�@6�@6�R@6��@6�+@5�@5/@5�@5V@4��@4�/@4j@4(�@3�m@3�@3t�@3dZ@3S�@3o@2��@2J@1�^@1��@1G�@0�9@0A�@01'@0  @/�@/�;@/�;@/�;@/�w@/��@/�P@/|�@/|�@/l�@/\)@/K�@/�@.�y@.E�@.5?@.{@.@-�@-�T@-��@-�-@-��@-��@-�h@-`B@-�@,�/@,�j@,�@,�D@,Z@,I�@,(�@+�m@+ƨ@+��@+S�@+"�@+o@*�@*�H@*�!@*�\@*~�@*=q@*�@*J@)��@)�@)�#@)��@)�7@)hs@)7L@(�`@(��@(A�@(  @'�;@'�@'|�@'l�@'�@&�@&�R@&��@&��@&v�@&E�@&$�@%�@%�-@%�-@%��@%��@%�h@%�@%`B@%O�@%�@$�/@$�@$��@$��@$��@$�D@$�D@$�D@$z�@$j@$9X@$(�@#�m@#�F@#��@#"�@#@"��@"��@"�!@"n�@"n�@"M�@"=q@"-@!��@!�^@!hs@!hs@!%@ Ĝ@ �u@ r�@ �@ r�@ A�@��@\)G�O�A܋DAܑhAܓuAܕ�Aܕ�Aܗ�Aܕ�Aܗ�Aܗ�AܓuAܗ�Aܗ�Aܗ�Aܕ�A܉7A܉7A܋DA܇+A܉7A܍PA܏\A܍PAܑhAܗ�Aܕ�Aܙ�Aܛ�Aܙ�Aܛ�Aܝ�Aܗ�Aܙ�Aܙ�Aܗ�Aܙ�A�v�A�v�A܁A܁A�~�A�v�A�z�A�z�A܃A�|�A�1'A�Q�A��A�A���A��A���A۬Aە�Aە�Aۗ�Aە�AۓuAە�AۓuAۑhAۓuAۉ7Aۉ7AۍPAۉ7AۃAۇ+AۃA�~�A�~�A�~�A�z�A�z�A�~�A�|�A�z�A�|�A�~�A�|�A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
l�B
lWB
l"B
l�B
l"B
k�B
k�B
l�B
jB
jB
pB
m�B
j�B
j�B
jB
kQB
k�B
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
lWB
lWB
lWB
lWB
lWB
l�B
lWB
lWB
kQB
j�B
j�B
kQB
n/B
o B
n/B
g�B
`�B
GB
B
B
@B
eB
B
�B
A B
k�B
��B
�CB
ԕB
��B�B+BB3hBU�B_�Ba�Bh
Bo�Bq�BsBrBpBu�BzB}�B|�B.B�Bx8BzBrGBj�BdZBR BE�B<�B6�B!�B
�B;B
��B
�B
�yB
�[B
�-B
�B
��B
��B
w2B
l�B
W?B
#nB
B

�B
�B	��B	�GB	�QB	��B	�)B	�#B	�0B	�IB	�LB	�~B	�FB	��B	cB	x�B	lWB	c B	[�B	H�B	?}B	7�B	>B	 'B	�B	 \B	-B	/B	0�B	1'B	<�B	FtB	E�B	EmB	H�B	I�B	K)B	L�B	K�B	f�B	n�B	pB	qB	sMB	v+B	|B	��B	�4B	��B	�B	��B	��B	�B	��B
	B
�B
YB
�B
�B
DB
�B
�B
�B
uB
%B
�B
B
VB
�B
B
�B
FB
@B
B
(B
�B
�B
�B
(B
VB
VB
�B
	�B
	B
fB
DB
B
�B
�B
�B
 \B
%�B
+B
0�B
3hB
0�B
5?B
6B
:�B
6�B
?HB
?B
=qB
:^B
5tB
3�B
3�B
3�B
1�B
2�B
33B
5B
4�B
5�B
5�B
5�B
8RB
;dB
;0B
8RB
5�B
1�B
.�B
'�B
'�B
'�B
'�B
'RB
%B
"�B
!-B
OB
CB
�B
B
4B
�B
"B
�B
\B
�B
�B
�B
_B
B
~B
 'B
!bB
B
eB
�B
B
{B
�B
B
�B
"B
�B

�B
�B

=B

rB

=B

�B

�B
	B
1B
+B
�B
�B
%B
�B
�B
_B
�B
SB
B
�B
�B
uB
�B
;B
  B	��B	�cB	��B	��B
  B
 iB	��B
�B
�B
�B
�B
�B
�B
�B
�B
%B
YB
YB
�B
�B
�B
%B
�B
B
�B
YB
�B
�B
�B
GB
�B
{B
�B
GB
�B
uB
B
uB
B
B
�B
�B
uB
AB
uB
uB
B
�B
B
�B
uB
AB
�B
AB
oB
 �B
�B
{B
B
B
B
GB
B
B
%B
B
�B
�B
�B
MB
GB
�B
�B
�B
B
�B
B
MB
�B
�B
�B
B
�B
�B
�B
MB
�B
�B
+B
+B
�B
fB
fB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	B
	7B
	B
	B
�B
�B
fB
�B
�B
	B
�B
fB
1B
�B
	B
	7B
�B

	B

	B

�B

�B
xB
B
PB
PB
�B
"B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
.B
�B
�B
\B
�B
�B
hB
�B
4B
�B
�B
oB
:B
�B
:B
�B
:B
:B
:B
B
�B
�B
uB
�B
@B
B
FB
FB
B
�B
{B
FB
�B
�B
B
�B
B
�B
B
B
B
{B
B
B
�B
B
�B
�B
�B
+B
�B
�B
�B
eB
B
7B
�B
B
�B
�B
�B
B
7B
kB
	B
�B
	B
�B
�B
kB
7B
�B
xB
~B
B
~B
B
�B
�B
B
�B
�B
�B
�B
VB
�B
�B
 'B
 �B
 �B
 \B
 �B
 \B
 \B
 �B
 \B
 �B
 �B
 �B
 �B
!bB
!�B
!�B
!�B
!�B
!�B
"hB
!�B
!�B
!bB
!-B
 �B
$tB
$�B
%zB
%�B
&LB
%�B
&�B
&�B
'B
'B
&�B
'B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(XB
)�B
(�B
)*B
(XB
+6B
+kB
+�B
+�B
,�B
,�B
,�B
,�B
-B
-B
,�B
,�B
,qB
-B
-B
-CB
.�B
.IB
.}B
.IB
.B
.IB
/�B
0�B
0�B
0�B
0�B
0�B
0UB
0UB
0UB
0!B
0UB
0�B
1'B
1�B
2-B
3�B
4�B
4�B
5�B
5�B
5�B
6B
5�B
6FB
6zB
6zB
7B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
8�B
9XB
9�B
9�B
9�B
9�B
:�B
;0B
;0B
;�B
<jB
=<B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=qB
=�B
>�B
?B
?�B
?�B
@�B
?�B
@�B
@�B
@�B
@�B
A�B
A B
A B
A B
B[B
B�B
CaB
C-B
CaB
C�B
C�B
C�B
C�B
CaB
DgB
D3B
EB
EmB
E�B
FB
FB
F?B
F�B
GEB
GEB
G�B
HB
G�B
HB
G�B
HKB
HKB
H�B
JXB
J�B
J�B
J�B
J�B
JXB
K^B
K^B
K�B
K�B
K�B
K�B
K�B
LdB
L�B
L�B
L�B
MB
MjB
NpB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
OvB
OvB
OvB
OB
N�B
OBB
P�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S&B
R�B
R�B
S&B
S[B
S[B
TaB
T�B
T�B
UgB
U2B
UgB
U2B
U2B
U2B
U2B
T�B
W
B
W?B
W
B
W�B
W�B
XyB
X�B
X�B
YB
X�B
X�B
X�B
XyB
YB
X�B
YB
Y�B
ZB
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[�B
\)B
\)B
\]B
\�B
\�B
]/B
]/B
]/B
]dB
]/B
]�B
^5B
^jB
^�B
^5B
_B
_�B
_�B
_�B
_�B
`B
`B
`�B
`�B
`�B
`�B
aB
aB
a|B
a�B
b�B
cTB
cTB
c B
cTB
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e`B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
g8B
gB
g8B
g�B
h
B
hsB
hsB
h�B
hsB
h�B
h�B
h�B
h�B
iB
h�B
iB
h�B
h�B
iDB
iB
iDB
iDB
i�B
jB
jKB
jB
jKB
jKB
jKB
jKB
jB
jB
jKB
jB
j�B
j�B
kB
kB
kQB
k�B
kQB
k�B
k�B
k�B
lWB
lWB
l�B
lWB
l�B
l�B
l�B
l�B
l�B
m]B
m]B
m]B
m]B
m]B
m]B
m]B
m�B
m�B
m�B
n/B
ncB
o B
n�B
o B
o5B
o B
o5B
o�B
o�B
pB
o�B
o�B
p;B
p;B
p;B
poB
qB
p�B
p�B
p�B
p�B
p�B
qB
p�B
qAB
qvB
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
q�B
rB
r|B
rGB
sMB
sB
sB
r�B
sMB
s�B
sMB
sMB
s�B
sMB
s�B
s�B
tB
s�B
t�B
t�B
t�B
u%B
t�B
u%B
u%B
u�B
u�B
u�B
m]B
l�B
l�B
k�B
l�B
kQB
m)B
lWB
k�B
l�B
kQB
k�B
l�B
k�B
m]B
m)B
kB
l�B
l�B
k�B
kB
m�B
jB
k�B
m)B
iyB
k�B
l�B
j�B
kQB
l�B
j�B
kQB
l�B
kB
s�B
i�B
i�B
jKB
k�B
m�B
l�B
iyB
i�B
o�B
i�B
e�B
pB
k�B
l"B
k�B
rB
o�B
l�B
kQB
jKB
j�B
kQB
jB
k�B
k�B
iyB
lWB
j�B
iB
jKB
k�B
jB
j�B
l"B
kB
i�B
j�B
kB
jB
j�B
g8B
kB
j�B
k�B
l"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  B
l�B
lVB
lTB
l�B
k�B
k�B
lB
m~B
kB
m~B
s*B
ohB
kB
j�B
j�B
k`B
k�B
lB
lB
lB
lMB
l�B
l�B
l�B
l�B
luB
liB
laB
l�B
l�B
l�B
l�B
l�B
ksB
j�B
k%B
k�B
n�B
o�B
o�B
j\B
lmB
V�B
*B
�B
AB
�B
!
B
$5B
H"B
p.B
��B
��B
ܾB'B�B�B"�BAyB\;BalBe�Bm�BuIBu�Bu�Bs�Bt�B�>B�nB��B�B�lB�B�!B��B}}B}�BwBZvBL�BI�BIsB.�BgB�B eB
�B
��B
�B
�ZB
��B
��B
��B
��B
��B
pXB
.�B
kB
�B
B	�iB	�jB	��B	�B	�GB	��B	ūB	� B	��B	�qB	��B	�0B	��B	��B	w�B	v:B	hPB	PB	I�B	Z�B	W�B	%B	$)B	,mB	1wB	0B	1lB	6B	C�B	H
B	FEB	F�B	I�B	J�B	L�B	NlB	TkB	kkB	o�B	qCB	r�B	u~B	zB	��B	��B	�~B	�?B	��B	��B	ۆB	�*B	�]B
�B
�B
�B
�B
EB
�B
	�B
B
3B
�B
�B
�B
>B
�B
B
�B
yB
qB
oB
B
B
�B
@B
�B
VB
?B
'B
MB

wB
	�B
	sB
@B
6B
�B
�B
�B
 �B
%�B
,B
2�B
4�B
2TB
6B
7FB
?DB
7�B
@�B
@FB
@3B
<1B
66B
4�B
5�B
5;B
4jB
5�B
42B
6RB
5�B
6IB
6CB
6�B
9aB
<�B
<�B
9�B
8B
6DB
2�B
(tB
(1B
(aB
(�B
(�B
&�B
$]B
$WB
#�B
 CB
\B
zB
|B
�B
B
�B
�B
EB
eB
�B
4B
�B
�B
"B
%]B
_B
�B
�B
�B
�B
GB
QB
�B
:B
�B
0B
B
*B

�B
7B
WB
�B

B
�B
�B
�B
�B
'B
�B
B
	VB
B
�B
QB
B
�B
�B
[B
dB
 �B
 FB	��B	��B	��B
 �B
LB
�B
�B
�B
CB
B
DB
GB
cB
2B
�B
�B
�B
'B
B
�B
gB
7B
�B
�B
�B
UB
zB
LB
�B
�B
EB
�B
*B
�B
pB
<B
B
B
�B
cB
4B
�B
�B
UB
!B
UB
(B
�B
oB
�B
�B
JB
mB
BB
FB
VB
�B
�B
�B
�B
B
�B
�B
�B
�B
HB
�B
,B
qB
B

�B
	<B
B
8B
RB
�B
�B
~B
eB
#B
yB
�B
�B
�B
�B
GB
xB
�B
xB
	!B
�B
�B
.B
B
�B
�B
B
aB
	#B
	�B
B

zB
	�B
	�B
	�B
	�B
	3B
	uB
	-B
	lB
	�B
	SB
�B
�B
�B
	�B

RB
	�B
	�B
 B
lB
 B
VB
�B
B
5B
�B
B
UB
GB
�B
�B
FB
�B
uB
B
BB
%B
�B
�B
�B
.B
0B
qB
"B
�B
B
�B
{B
B
tB
B
wB
�B
WB
�B
mB
�B
�B
8B
MB
�B
}B
�B
�B
B
B
�B
�B
IB
�B
�B
�B
�B
[B
�B
/B
�B
mB
SB
GB
�B
hB
GB
�B
�B
�B
ZB
jB
�B
B
_B
�B
2B
jB
bB
B
B
�B
B
=B
]B
{B
�B
QB
�B
B
�B
�B
B
�B
;B
�B
�B
1B
�B
�B
�B
7B
tB
8B
~B
�B
 0B
�B
�B
 jB
!�B
!cB
!'B
 �B
 �B
 �B
! B
!5B
!#B
![B
!�B
!B
!�B
"B
"B
!�B
!�B
!�B
"8B
"�B
!�B
!�B
!�B
!�B
#�B
&AB
%�B
&jB
&�B
&�B
&NB
'SB
'sB
'lB
'SB
'B
'�B
'�B
({B
(�B
(�B
)FB
(�B
)(B
)MB
)�B
)!B
)�B
*�B
,^B
,�B
,B
,�B
-^B
-B
-B
-`B
-xB
-{B
,�B
-B
-3B
-�B
-�B
.~B
/B
.zB
.�B
.�B
.�B
0B
0�B
1�B
0�B
0�B
0�B
0�B
0\B
0aB
0�B
0oB
1nB
1�B
2B
2�B
3�B
4�B
5B
5|B
6�B
6B
6B
6fB
6B
6�B
7B
7]B
8wB
9\B
8�B
8�B
8�B
8�B
8�B
9RB
9zB
:B
9bB
9�B
:1B
:yB
:�B
:�B
<B
;�B
<B
=-B
=`B
=�B
=xB
>%B
>B
=�B
=�B
=�B
=�B
=�B
?B
?�B
@&B
@�B
@PB
@�B
@�B
AWB
@�B
AB
AtB
BB
AJB
A�B
B}B
CiB
C�B
C�B
CdB
C�B
C�B
C�B
C�B
C�B
D:B
D�B
D�B
E�B
E�B
FVB
F&B
FuB
F�B
GoB
G�B
G�B
HB
H9B
HB
HHB
HNB
H�B
I2B
JB
K0B
KB
J�B
KB
J�B
J�B
K�B
K�B
K�B
K�B
K�B
LB
L+B
L�B
L�B
L�B
MB
M�B
NlB
OB
N�B
OB
OQB
OsB
OB
OB
O�B
PB
O�B
O�B
O�B
ObB
O�B
P�B
R~B
SB
R�B
R�B
R�B
R�B
R�B
R�B
SB
SmB
S@B
S!B
S]B
S�B
S�B
TEB
T�B
T�B
UQB
VB
UNB
UlB
UIB
ULB
U]B
UvB
V+B
W�B
WB
WuB
XB
X+B
X�B
X�B
X�B
Y+B
X�B
X�B
X�B
X�B
Y�B
X�B
ZB
ZB
Z4B
ZB
Z�B
[9B
[2B
[=B
[�B
[�B
\�B
\hB
\�B
]3B
]`B
]7B
]:B
]hB
]�B
]�B
^.B
^�B
^�B
^�B
^�B
_�B
`AB
_�B
`B
`wB
`fB
`�B
aB
a<B
`�B
aB
aKB
a}B
bNB
b�B
csB
c�B
cuB
ctB
d{B
eYB
d�B
d�B
d�B
d�B
eNB
e�B
e�B
f9B
e�B
fB
f!B
e�B
f�B
gB
g�B
g2B
g�B
hWB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iB
i(B
h�B
iB
h�B
h�B
ibB
iVB
i�B
i�B
i�B
j@B
jcB
j1B
jdB
jfB
jtB
jcB
j�B
j�B
j�B
j�B
k6B
kB
k8B
kLB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m1B
l�B
mB
mDB
m�B
mvB
mvB
mvB
mwB
m~B
m�B
m�B
nB
n`B
n�B
n�B
oNB
n�B
o?B
opB
o'B
o�B
o�B
o�B
pB
o�B
pB
p|B
pkB
p~B
p�B
qB
p�B
p�B
p�B
p�B
qB
q,B
qB
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
rB
rB
r3B
rUB
r�B
r�B
s{B
sUB
s3B
s	B
s�B
s�B
swB
shB
s�B
s�B
t=B
tKB
t6B
thB
uB
t�B
uB
uB
t�B
unB
u�B
vxB
v+G�O�B
m]B
l�B
l�B
k�B
l�B
kQB
m)B
lWB
k�B
l�B
kQB
k�B
l�B
k�B
m]B
m)B
kB
l�B
l�B
k�B
kB
m�B
jB
k�B
m)B
iyB
k�B
l�B
j�B
kQB
l�B
j�B
kQB
l�B
kB
s�B
i�B
i�B
jKB
k�B
m�B
l�B
iyB
i�B
o�B
i�B
e�B
pB
k�B
l"B
k�B
rB
o�B
l�B
kQB
jKB
j�B
kQB
jB
k�B
k�B
iyB
lWB
j�B
iB
jKB
k�B
jB
j�B
l"B
kB
i�B
j�B
kB
jB
j�B
g8B
kB
j�B
k�B
l"G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<JT<�-<\�B<�d�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<mg�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:s�<5��<#�
<#�
<e�<#�
<.�<��N<B:B<�Qp<��*<#�
<#�
<bh�<�k<^Ғ<#�
<#�
<-�-<#�
<��<���<#�
<#�
<#�
<#�
<D{�=��<̦"<A��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<n*�<9��<7k�<#�
<#�
<#�
<#�
<3-�<)̆<>?�<A�S<�rR<Yu<#�
<2�=�<���<#�
<#�
<N�{<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021081905383820210819053838IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021082907005820210829070058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021082907005820210829070058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364920220422133649IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                