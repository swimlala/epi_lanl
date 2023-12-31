CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-31T02:39:41Z creation; 2022-04-26T16:06:55Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  Z    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       ah   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H %�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      -   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H J0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      Qx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` n�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   n�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   t�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   z�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20210131023941  20220426232401  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_008                 8138_008904_008                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�Z���@�Z���11  @�Z����@�Z����@,��f�I=@,��f�I=�dS�QC�r�dS�QC�r11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @=p�@�G�@�G�@�  @�G�@��RA\)A   A,��A@��A_\)A�  A�  A�  A�  A��AϮA�Q�A��B   B�B�B  B�
B'�
B0  B7�
B?�
BH  BP(�BX(�B`(�Bh  Bo�Bx(�B�{B�  B�  B�  B�  B�  B�{B�  B�  B�  B��B�  B�{B�  B�  B�  B�{B�{B�  B�{B�  B�  B�{B�  B�  B�{B�  B�{B�  B�  B�  B�  C   C  C
=C  C��C	��C  C  C  C
=C  C  C
=C��C�C��C 
=C"
=C$  C&
=C(
=C*  C,  C-��C0  C2
=C4
=C6
=C8
=C:  C<  C>  C@{CB  CD  CF  CH  CJ  CK��CN  CP  CR  CT  CV  CX  CZ
=C\
=C^
=C_��Cb  Cd
=Cf
=Ch
=Cj  Cl  Cn  Cp  Cq��Ct  Cv  Cx  Cz
=C|{C~
=C�C�  C�  C�C���C���C���C���C�  C�C�C�  C�C�
=C�C���C�  C���C���C���C���C���C���C�  C�C�  C���C���C���C���C�  C�C�  C���C���C�  C�C�  C���C���C���C�  C���C���C�  C�  C�  C���C���C���C���C�  C�C�  C�  C���C���C�  C�C�  C�  C�  C�
=C�C�C�C�  C�  C�C���C�  C�  C�  C�C�  C���C���C���C�C�  C�  C�C���C���C���C���C���C�  C���C���C���C���C�C�  C�  C���C���C���C�  C�  C�C�  C�  C�  C�C�C�  C���C���C�C�C�  C�  C�  C�C�C���C���C���C���C���C���C�  C�  C�  C�  C�  C�  C���D � D�D��D�D� D  D� D  D��D�qD��DD��D�qD}qD  D��D	�D	� D
  D
� D  D� D  D� D  D� D�D� D��D}qD  D}qD��D}qD  D��D�D��D  D� D�qD}qD  D� D  D� D  D� D  D� D  D� D�qD}qD  D� D  Dz�D  D��D  D}qD   D � D!  D!� D"  D"� D#  D#��D$  D$� D%  D%� D%�qD&� D'�D'� D'�qD(� D)�D)� D*  D*� D+  D+��D+�qD,}qD,�qD-� D.  D.}qD.�qD/� D0�D0� D1  D1��D1�qD2� D3  D3� D4�D4� D4��D5}qD5�qD6� D7�D7� D8�D8��D9�D9��D:  D:� D;  D;��D<  D<}qD=  D=� D>  D>��D?  D?}qD@  D@� D@�qDA}qDB  DB}qDC  DC� DD  DD� DE  DE}qDE�qDF� DG  DGz�DH  DH� DH�qDI��DJ�DJ� DK�DK��DL  DL� DM  DM}qDN�DN��DO  DO}qDO�qDP}qDQ  DQ� DR  DR� DR�qDS� DT  DT}qDT��DU}qDV  DV� DW�DW�DX�DX� DX�qDYz�DY�qDZ� D[  D[� D[�qD\��D]  D]}qD]�qD^� D_�D_��D`  D`� Da  Da��Db  Db}qDc  Dc��Dd  Dd}qDe  De� Df  Df}qDg  Dg� Dg�qDh� Di�Di� Di�qDj}qDj�qDk� Dl  Dl}qDl�qDm��DnDn��Do  Do}qDo�qDp}qDq  Dq� Dq�qDr� Ds  Ds}qDt  Dt� Dt�qDu}qDv�Dv� Dw  Dw� Dx�Dx�Dy�Dy� Dy�qDz}qD{  D{}qD|�D|��D|�qD}}qD~  D~�D�D��D�HD�@ D��HD���D��qD�>�D�~�D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�HD�@ D�� D���D�HD�@ D�� D���D���D�@ D�� D�� D��qD�>�D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�}qD���D�HD�@ D�� D���D���D�=qD�~�D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD���D��HD�  D�>�D�~�D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D��HD�  D�=qD�~�D���D���D�>�D�� D�� D���D�>�D�~�D�� D���D�>�D�� D�� D�  D�>�D�~�D��HD�  D�@ D�� D�� D�  D�AHD�� D��qD���D�>�D�� D�� D�  D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�~�D�� D���D�>�D�}qD��qD�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD�� D��HD�  D�>�D�~�D���D���D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�~�D��qD���D�>�D�� D�D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�@ D��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�@ D�~�D���D���D�>�D�� D���D�  D�@ D�~�D���D���D�>�D�� D�� D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD�� D���D�=qD�� D��HD�HD�@ D�~�D�� D�HD�AHD��HD�� D�  D�AHD�~�D�� D�  D�@ D�~�D¾�D���D�@ DÁHD�� D���D�>�D�~�D�� D�  D�@ Dŀ D�� D�  D�@ DƁHD��HD�  D�>�D�~�D�� D�HD�@ DȀ D��HD�  D�@ DɁHD��HD�  D�@ D�~�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��HD�HD�AHD͂�D�� D�  D�@ D΁HD�� D�HD�AHDπ DϾ�D�  D�B�DЂ�D�D��D�AHDсHD�� D���D�>�D�~�DҾ�D��qD�=qD�~�DӾ�D�  D�@ DԀ D��HD�HD�AHDՁHD�D�HD�@ Dր D�� D�  D�AHDׁHD�� D���D�=qD�~�Dؾ�D���D�@ DفHD�� D���D�>�Dڀ D�� D�  D�@ Dۀ D��HD�HD�>�D܀ D�� D�HD�B�D݀ D�� D���D�>�D�~�D޾�D���D�>�D�}qD߽qD���D�AHD�� DྸD���D�>�D�}qD�qD�  D�AHD� D⾸D���D�@ D� D�qD�  D�AHD�HD��HD�  D�AHD�HD��HD�  D�@ D� D澸D���D�/\?\)?��?W
=?�\)?��
?�p�?�
=@   @
=q@
=@+�@333@=p�@O\)@^�R@k�@}p�@��
@�{@�33@���@�G�@�ff@�\)@�z�@��H@�G�@Ǯ@�\)@�@��H@�\@���@�\)@�@�(�A ��Az�AQ�A�A\)A33AffA�HAp�A"�\A&ffA(��A.�RA1�A5A:=qA=p�AAG�AEAI��AMp�AP��AU�AXQ�A\(�A`��Ac�
AhQ�Al��AqG�Au�Ax��A}p�A���A��\A���A�ffA���A�33A���A�\)A���A��HA��A��RA�G�A��HA���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?�=q@   @=p�@�G�@�G�@�  @�G�@��RA\)A   A,��A@��A_\)A�  A�  A�  A�  A��AϮA�Q�A��B   B�B�B  B�
B'�
B0  B7�
B?�
BH  BP(�BX(�B`(�Bh  Bo�Bx(�B�{B�  B�  B�  B�  B�  B�{B�  B�  B�  B��B�  B�{B�  B�  B�  B�{B�{B�  B�{B�  B�  B�{B�  B�  B�{B�  B�{B�  B�  B�  B�  C   C  C
=C  C��C	��C  C  C  C
=C  C  C
=C��C�C��C 
=C"
=C$  C&
=C(
=C*  C,  C-��C0  C2
=C4
=C6
=C8
=C:  C<  C>  C@{CB  CD  CF  CH  CJ  CK��CN  CP  CR  CT  CV  CX  CZ
=C\
=C^
=C_��Cb  Cd
=Cf
=Ch
=Cj  Cl  Cn  Cp  Cq��Ct  Cv  Cx  Cz
=C|{C~
=C�C�  C�  C�C���C���C���C���C�  C�C�C�  C�C�
=C�C���C�  C���C���C���C���C���C���C�  C�C�  C���C���C���C���C�  C�C�  C���C���C�  C�C�  C���C���C���C�  C���C���C�  C�  C�  C���C���C���C���C�  C�C�  C�  C���C���C�  C�C�  C�  C�  C�
=C�C�C�C�  C�  C�C���C�  C�  C�  C�C�  C���C���C���C�C�  C�  C�C���C���C���C���C���C�  C���C���C���C���C�C�  C�  C���C���C���C�  C�  C�C�  C�  C�  C�C�C�  C���C���C�C�C�  C�  C�  C�C�C���C���C���C���C���C���C�  C�  C�  C�  C�  C�  C���D � D�D��D�D� D  D� D  D��D�qD��DD��D�qD}qD  D��D	�D	� D
  D
� D  D� D  D� D  D� D�D� D��D}qD  D}qD��D}qD  D��D�D��D  D� D�qD}qD  D� D  D� D  D� D  D� D  D� D�qD}qD  D� D  Dz�D  D��D  D}qD   D � D!  D!� D"  D"� D#  D#��D$  D$� D%  D%� D%�qD&� D'�D'� D'�qD(� D)�D)� D*  D*� D+  D+��D+�qD,}qD,�qD-� D.  D.}qD.�qD/� D0�D0� D1  D1��D1�qD2� D3  D3� D4�D4� D4��D5}qD5�qD6� D7�D7� D8�D8��D9�D9��D:  D:� D;  D;��D<  D<}qD=  D=� D>  D>��D?  D?}qD@  D@� D@�qDA}qDB  DB}qDC  DC� DD  DD� DE  DE}qDE�qDF� DG  DGz�DH  DH� DH�qDI��DJ�DJ� DK�DK��DL  DL� DM  DM}qDN�DN��DO  DO}qDO�qDP}qDQ  DQ� DR  DR� DR�qDS� DT  DT}qDT��DU}qDV  DV� DW�DW�DX�DX� DX�qDYz�DY�qDZ� D[  D[� D[�qD\��D]  D]}qD]�qD^� D_�D_��D`  D`� Da  Da��Db  Db}qDc  Dc��Dd  Dd}qDe  De� Df  Df}qDg  Dg� Dg�qDh� Di�Di� Di�qDj}qDj�qDk� Dl  Dl}qDl�qDm��DnDn��Do  Do}qDo�qDp}qDq  Dq� Dq�qDr� Ds  Ds}qDt  Dt� Dt�qDu}qDv�Dv� Dw  Dw� Dx�Dx�Dy�Dy� Dy�qDz}qD{  D{}qD|�D|��D|�qD}}qD~  D~�D�D��D�HD�@ D��HD���D��qD�>�D�~�D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�HD�@ D�� D���D�HD�@ D�� D���D���D�@ D�� D�� D��qD�>�D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�}qD���D�HD�@ D�� D���D���D�=qD�~�D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD���D��HD�  D�>�D�~�D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D��HD�  D�=qD�~�D���D���D�>�D�� D�� D���D�>�D�~�D�� D���D�>�D�� D�� D�  D�>�D�~�D��HD�  D�@ D�� D�� D�  D�AHD�� D��qD���D�>�D�� D�� D�  D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�~�D�� D���D�>�D�}qD��qD�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD�� D��HD�  D�>�D�~�D���D���D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�~�D��qD���D�>�D�� D�D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�@ D��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�@ D�~�D���D���D�>�D�� D���D�  D�@ D�~�D���D���D�>�D�� D�� D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD�� D���D�=qD�� D��HD�HD�@ D�~�D�� D�HD�AHD��HD�� D�  D�AHD�~�D�� D�  D�@ D�~�D¾�D���D�@ DÁHD�� D���D�>�D�~�D�� D�  D�@ Dŀ D�� D�  D�@ DƁHD��HD�  D�>�D�~�D�� D�HD�@ DȀ D��HD�  D�@ DɁHD��HD�  D�@ D�~�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��HD�HD�AHD͂�D�� D�  D�@ D΁HD�� D�HD�AHDπ DϾ�D�  D�B�DЂ�D�D��D�AHDсHD�� D���D�>�D�~�DҾ�D��qD�=qD�~�DӾ�D�  D�@ DԀ D��HD�HD�AHDՁHD�D�HD�@ Dր D�� D�  D�AHDׁHD�� D���D�=qD�~�Dؾ�D���D�@ DفHD�� D���D�>�Dڀ D�� D�  D�@ Dۀ D��HD�HD�>�D܀ D�� D�HD�B�D݀ D�� D���D�>�D�~�D޾�D���D�>�D�}qD߽qD���D�AHD�� DྸD���D�>�D�}qD�qD�  D�AHD� D⾸D���D�@ D� D�qD�  D�AHD�HD��HD�  D�AHD�HD��HD�  D�@ D� D澸D���G�O�?\)?��?W
=?�\)?��
?�p�?�
=@   @
=q@
=@+�@333@=p�@O\)@^�R@k�@}p�@��
@�{@�33@���@�G�@�ff@�\)@�z�@��H@�G�@Ǯ@�\)@�@��H@�\@���@�\)@�@�(�A ��Az�AQ�A�A\)A33AffA�HAp�A"�\A&ffA(��A.�RA1�A5A:=qA=p�AAG�AEAI��AMp�AP��AU�AXQ�A\(�A`��Ac�
AhQ�Al��AqG�Au�Ax��A}p�A���A��\A���A�ffA���A�33A���A�\)A���A��HA��A��RA�G�A��HA���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��`A��`A��`A��HA��TA��`A��TA��`A��mA��mA��yA��yA��yA��yA��A��mA��TA��TA��mA��#A��A���A���A���A�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A��A�=qA�^5A� �A�+A�(�Aˣ�A��/A��HA�bNA�M�A�  A�JA�C�A��A��A���A�\)A�`BA��/A��!A��A�bNA�ƨA��A��A�A��FA�M�A�XA�VA�I�A��
A�VA���A���A��DA�  A�
=A���A�ƨA���A�
=A���A���A���A���A�(�A�JA��A���A|�Aw�AuXAoVAj{AeVA^z�AV��ASG�AQ;dAO�^AN��AM�TAM��AMS�AK��AI��AG/AF��AF��AFĜAF~�AE�AC�^ABjAAC�A>r�A<jA;�A;��A<n�A<1'A<I�A;�;A;hsA;`BA;VA:$�A8�/A8�A7�FA7S�A7+A6��A57LA4bA3��A2��A2Q�A1S�A0��A/�A/C�A.9XA-�A.JA,�A+��A*�A(��A(Q�A&��A&JA%VA#��A"��A!�;A!/A �/A �jA M�A��A�+A-A=qAQ�A�A~�A�A��A|�An�A�A��AƨA�-Al�A�AZA�FAS�A��A�DAJA��A�A��AQ�A1'AbA��A�#A%A�+A  AoAA��A�A�^A��AhsA�A�\A=qAJA��A��AdZA��A|�A7LA7LA;dA
=A�!AJAƨA��A�hAl�A/A33A&�A�A
��A
�A
5?A	ƨA	�FA	�-A	�wA	�FA	�FA	�
A	�wA	��A	��A	;dA��A��A�!A�\AffAE�A1AhsA��A�/A��Az�AbNAJA�-AhsA��A��A�!AQ�A�TAx�A�An�A=qA1'A{A�mA��AG�A%A ��@���@��@��y@�{@�G�@��@�z�@�  @��@��@�^5@�&�@��@��P@�33@��!@�5?@���@��@�1@�@��@���@�1@���@�$�@���@�hs@�%@�I�@�dZ@�\@��T@�u@���@�C�@���@�\@噚@��@㝲@�\)@�;d@��H@�+@���@�x�@�I�@�33@��@ް!@�ff@���@ݡ�@�G�@���@ܣ�@܃@�Q�@�1'@�(�@�b@�C�@�@���@���@�C�@ְ!@�~�@�{@�x�@��@�bN@���@ӕ�@�l�@Ұ!@��@љ�@�hs@�/@���@�z�@ϝ�@�dZ@�
=@��@�@ͩ�@�x�@̓u@˶F@ˍP@�dZ@�+@�ȴ@��@�?}@ȋD@��@�l�@ư!@�v�@�$�@��T@�p�@�/@�V@�Ĝ@ă@Ý�@��@��@���@�x�@�/@��j@�A�@��m@���@�dZ@�K�@��@�n�@��^@���@�`B@��/@�I�@���@�K�@�ȴ@�^5@�-@�p�@���@�(�@�ƨ@��H@��#@�@���@��7@�&�@�Z@��@��@��\@�=q@���@��h@��@��@�bN@�A�@�1'@�(�@��@��
@��@���@��\@�E�@��@���@�&�@���@���@���@��P@�l�@�
=@���@���@�ff@�=q@�{@��#@��7@�X@��9@�A�@�1@��;@��@�o@�v�@��@�O�@�%@���@��`@��`@���@��9@���@�A�@��w@�\)@�"�@�@��@�ȴ@���@���@���@��h@�p�@�O�@�%@��@���@��@�A�@��;@���@�l�@�+@�@���@��!@�=q@���@��#@���@�?}@���@���@�z�@�Q�@�b@��w@���@�|�@�dZ@�33@���@���@���@�v�@�V@�-@���@�j@�ƨ@��@�dZ@��+@�5?@��@���@�`B@��@��
@���@�\)@�;d@�o@��H@���@�v�@�5?@���@��h@�/@���@��u@�bN@�b@��
@��P@��@��y@�ȴ@�ff@�=q@�J@���@�?}@���@�Ĝ@�bN@�A�@��@�1@��@��F@��@�33@���@��@���@�v�@�ff@�V@�E�@�-@���@��h@�G�@�7L@��@��j@�z�@�b@�ƨ@��@�K�@��@��y@���@�~�@�ff@�=q@�{@��T@���@�/@���@��`@��/@���@���@�9X@� �@�b@��m@��
@���@���@�dZ@��@�^5@�=q@��@�p�@�?}@�/@��@��j@���@�z�@�A�@�b@��@�P@;d@~��@}O�@{��@{o@z��@z��@zM�@y�#@y&�@x1'@w��@w;d@vȴ@v5?@v@u�@u�h@uO�@uO�@u?}@u�@u�@t��@t�@tZ@sƨ@s�@r�!@qhs@p�9@pbN@o�;@o|�@o
=@nv�@n5?@m�-@m�@l�j@kƨ@j�\@i��@i��@i��@i&�@h�`@h�9@h1'@g�w@g�P@gK�@g
=@f�@fff@e�@e�@d��@d�/@d�j@dZ@c�
@c��@c"�@b��@bn�@bJ@a��@aX@`�`@`��@`1'@` �@_�;@_��@_;d@^v�@]�-@]��@]��@]V@\��@\�j@\z�@\9X@\�@[��@[��@[33@[o@Z��@Zn�@ZJ@Yx�@X�`@X��@XbN@XA�@XA�@X �@W�P@WK�@Vȴ@Vff@V5?@V@U�@U�@U@U�@U?}@UV@T��@T��@Tz�@TZ@TI�@T(�@T1@S�
@S�@So@R�@R�!@R-@Q��@Q�#@Q��@Q&�@Pr�@O�@O;d@O
=@N�y@N�R@N��@N{@M�h@M?}@MV@L�@L�/@L��@L��@Lz�@L(�@K�F@K33@J�@J�!@JM�@I��@I��@I�7@IX@I7L@I&�@I�@H�`@HĜ@H1'@G;d@F5?@F@E�T@E��@E/@D�/@D��@DZ@D�@C�F@CS�@Co@B��@B��@B~�@A�7@A�@@��@@bN@?�@?�P@?�P@?K�@>�y@>��@>5?@=�T@=��@=p�@<��@<�/@<��@<�D@<I�@;�m@;S�@:��@:^5@:�@:J@:J@9��@9��@9x�@9X@9%@8 �@7�w@7\)@7+@7
=@6��@6�y@6�R@6ff@5�@5�@5�@5V@4�@4��@4�@4�D@4I�@4(�@3�
@333@2M�@1hs@1�@0��@0��@0�@01'@/|�@/�@.��@.��@.��@.v�@.V@.@-�@-��@-�@-/@,�@,�j@,�@,I�@,1@+�F@+"�@*M�@)�7@)hs@)&�@(��@(��@(��@(Ĝ@(�9@(�9@(�9@(�9@(��@(�@(  @'l�@';d@'�@&��@&�@&ȴ@&��@&$�@%@%�@%?}@$��@$��@$Z@$9X@$�@#��@#�F@#�@#S�@#o@"��@"M�@!��@!�7@!X@!7L@!�@ ��@ r�@ bN@ 1'@��@+@��@�@�R@v�@V@5?@�@@O�@��@�@I�@I�@9X@�A��;A��;A��`A��TA��mA��TA��`A��TA��`A��TA��TA��`A��TA��mA��`A��yA��mA��`A��HA��;A��TA��HA��HA��TA��HA��`A��TA��TA��`A��TA��yA��TA��`A��mA��TA��mA��HA��HA��`A��HA��mA��mA��mA��yA��`A��yA��mA��mA��yA��mA��yA��`A��mA��A��`A��A��mA��mA��A��yA��A��yA��mA��yA��`A��yA��mA��mA��A��mA��yA��yA��mA��A��mA��yA��A��mA��A��yA��yA��A��yA��A��yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A��TA��`A��`A��`A��HA��TA��`A��TA��`A��mA��mA��yA��yA��yA��yA��A��mA��TA��TA��mA��#A��A���A���A���A�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A��A�=qA�^5A� �A�+A�(�Aˣ�A��/A��HA�bNA�M�A�  A�JA�C�A��A��A���A�\)A�`BA��/A��!A��A�bNA�ƨA��A��A�A��FA�M�A�XA�VA�I�A��
A�VA���A���A��DA�  A�
=A���A�ƨA���A�
=A���A���A���A���A�(�A�JA��A���A|�Aw�AuXAoVAj{AeVA^z�AV��ASG�AQ;dAO�^AN��AM�TAM��AMS�AK��AI��AG/AF��AF��AFĜAF~�AE�AC�^ABjAAC�A>r�A<jA;�A;��A<n�A<1'A<I�A;�;A;hsA;`BA;VA:$�A8�/A8�A7�FA7S�A7+A6��A57LA4bA3��A2��A2Q�A1S�A0��A/�A/C�A.9XA-�A.JA,�A+��A*�A(��A(Q�A&��A&JA%VA#��A"��A!�;A!/A �/A �jA M�A��A�+A-A=qAQ�A�A~�A�A��A|�An�A�A��AƨA�-Al�A�AZA�FAS�A��A�DAJA��A�A��AQ�A1'AbA��A�#A%A�+A  AoAA��A�A�^A��AhsA�A�\A=qAJA��A��AdZA��A|�A7LA7LA;dA
=A�!AJAƨA��A�hAl�A/A33A&�A�A
��A
�A
5?A	ƨA	�FA	�-A	�wA	�FA	�FA	�
A	�wA	��A	��A	;dA��A��A�!A�\AffAE�A1AhsA��A�/A��Az�AbNAJA�-AhsA��A��A�!AQ�A�TAx�A�An�A=qA1'A{A�mA��AG�A%A ��@���@��@��y@�{@�G�@��@�z�@�  @��@��@�^5@�&�@��@��P@�33@��!@�5?@���@��@�1@�@��@���@�1@���@�$�@���@�hs@�%@�I�@�dZ@�\@��T@�u@���@�C�@���@�\@噚@��@㝲@�\)@�;d@��H@�+@���@�x�@�I�@�33@��@ް!@�ff@���@ݡ�@�G�@���@ܣ�@܃@�Q�@�1'@�(�@�b@�C�@�@���@���@�C�@ְ!@�~�@�{@�x�@��@�bN@���@ӕ�@�l�@Ұ!@��@љ�@�hs@�/@���@�z�@ϝ�@�dZ@�
=@��@�@ͩ�@�x�@̓u@˶F@ˍP@�dZ@�+@�ȴ@��@�?}@ȋD@��@�l�@ư!@�v�@�$�@��T@�p�@�/@�V@�Ĝ@ă@Ý�@��@��@���@�x�@�/@��j@�A�@��m@���@�dZ@�K�@��@�n�@��^@���@�`B@��/@�I�@���@�K�@�ȴ@�^5@�-@�p�@���@�(�@�ƨ@��H@��#@�@���@��7@�&�@�Z@��@��@��\@�=q@���@��h@��@��@�bN@�A�@�1'@�(�@��@��
@��@���@��\@�E�@��@���@�&�@���@���@���@��P@�l�@�
=@���@���@�ff@�=q@�{@��#@��7@�X@��9@�A�@�1@��;@��@�o@�v�@��@�O�@�%@���@��`@��`@���@��9@���@�A�@��w@�\)@�"�@�@��@�ȴ@���@���@���@��h@�p�@�O�@�%@��@���@��@�A�@��;@���@�l�@�+@�@���@��!@�=q@���@��#@���@�?}@���@���@�z�@�Q�@�b@��w@���@�|�@�dZ@�33@���@���@���@�v�@�V@�-@���@�j@�ƨ@��@�dZ@��+@�5?@��@���@�`B@��@��
@���@�\)@�;d@�o@��H@���@�v�@�5?@���@��h@�/@���@��u@�bN@�b@��
@��P@��@��y@�ȴ@�ff@�=q@�J@���@�?}@���@�Ĝ@�bN@�A�@��@�1@��@��F@��@�33@���@��@���@�v�@�ff@�V@�E�@�-@���@��h@�G�@�7L@��@��j@�z�@�b@�ƨ@��@�K�@��@��y@���@�~�@�ff@�=q@�{@��T@���@�/@���@��`@��/@���@���@�9X@� �@�b@��m@��
@���@���@�dZ@��@�^5@�=q@��@�p�@�?}@�/@��@��j@���@�z�@�A�@�b@��@�P@;d@~��@}O�@{��@{o@z��@z��@zM�@y�#@y&�@x1'@w��@w;d@vȴ@v5?@v@u�@u�h@uO�@uO�@u?}@u�@u�@t��@t�@tZ@sƨ@s�@r�!@qhs@p�9@pbN@o�;@o|�@o
=@nv�@n5?@m�-@m�@l�j@kƨ@j�\@i��@i��@i��@i&�@h�`@h�9@h1'@g�w@g�P@gK�@g
=@f�@fff@e�@e�@d��@d�/@d�j@dZ@c�
@c��@c"�@b��@bn�@bJ@a��@aX@`�`@`��@`1'@` �@_�;@_��@_;d@^v�@]�-@]��@]��@]V@\��@\�j@\z�@\9X@\�@[��@[��@[33@[o@Z��@Zn�@ZJ@Yx�@X�`@X��@XbN@XA�@XA�@X �@W�P@WK�@Vȴ@Vff@V5?@V@U�@U�@U@U�@U?}@UV@T��@T��@Tz�@TZ@TI�@T(�@T1@S�
@S�@So@R�@R�!@R-@Q��@Q�#@Q��@Q&�@Pr�@O�@O;d@O
=@N�y@N�R@N��@N{@M�h@M?}@MV@L�@L�/@L��@L��@Lz�@L(�@K�F@K33@J�@J�!@JM�@I��@I��@I�7@IX@I7L@I&�@I�@H�`@HĜ@H1'@G;d@F5?@F@E�T@E��@E/@D�/@D��@DZ@D�@C�F@CS�@Co@B��@B��@B~�@A�7@A�@@��@@bN@?�@?�P@?�P@?K�@>�y@>��@>5?@=�T@=��@=p�@<��@<�/@<��@<�D@<I�@;�m@;S�@:��@:^5@:�@:J@:J@9��@9��@9x�@9X@9%@8 �@7�w@7\)@7+@7
=@6��@6�y@6�R@6ff@5�@5�@5�@5V@4�@4��@4�@4�D@4I�@4(�@3�
@333@2M�@1hs@1�@0��@0��@0�@01'@/|�@/�@.��@.��@.��@.v�@.V@.@-�@-��@-�@-/@,�@,�j@,�@,I�@,1@+�F@+"�@*M�@)�7@)hs@)&�@(��@(��@(��@(Ĝ@(�9@(�9@(�9@(�9@(��@(�@(  @'l�@';d@'�@&��@&�@&ȴ@&��@&$�@%@%�@%?}@$��@$��@$Z@$9X@$�@#��@#�F@#�@#S�@#o@"��@"M�@!��@!�7@!X@!7L@!�@ ��@ r�@ bN@ 1'@��@+@��@�@�R@v�@V@5?@�@@O�@��@�@I�@I�@9XG�O�A��;A��;A��`A��TA��mA��TA��`A��TA��`A��TA��TA��`A��TA��mA��`A��yA��mA��`A��HA��;A��TA��HA��HA��TA��HA��`A��TA��TA��`A��TA��yA��TA��`A��mA��TA��mA��HA��HA��`A��HA��mA��mA��mA��yA��`A��yA��mA��mA��yA��mA��yA��`A��mA��A��`A��A��mA��mA��A��yA��A��yA��mA��yA��`A��yA��mA��mA��A��mA��yA��yA��mA��A��mA��yA��A��mA��A��yA��yA��A��yA��A��yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�}B�}B�HB�}B�HB�HB�}B�}B�B�HB�HB�}B�}B�}B�}B�}B�}B�B�B�HB�B��B�wB�BB�B��B��B�B��B�wB��B�}B��B�[B��B�DB	ŢB
�B
.�B
E�B
_�B
|�B
��B
�B
�"B�B�BB,B@�BCaBI�BXEBb�B`BBiyBo BrB��B��B�kB��B��B�BÖB�3BǮB�hB�B��BiyBd�BaBUgB_�BW
B8RB�B
��B
�SB
j�B
Q�B
3�B
�B	��B	�B	��B	�}B	��B	}"B	\)B	6B	5B	D�B	\�B	l"B	p�B	z�B	x�B	w2B	v�B	�%B	�=B	�?B	�EB	�)B	՛B	�B	�B	خB	�TB	�B	�,B	خB	�/B
'�B
<�B
S[B
_;B
poB
|B
�%B
�+B
}VB
~]B
�lB
��B
�B
��B
��B
}VB
�iB
|B
y�B
rGB
m]B
g�B
iB
f�B
d&B
e,B
f�B
Z�B
R�B
IRB
F�B
@�B
9XB
:*B
2�B
/B
*�B
)�B
'�B
&�B
%�B
%FB
%�B
%B
*�B
,�B
5B
:�B
8�B
7LB
7B
4nB
0�B
/�B
/OB
.�B
.}B
,�B
*�B
)*B
&�B
%�B
!�B
#nB
$B
'�B
)�B
+B
-B
.�B
1[B
4nB
2-B
0!B
.�B
(�B
#nB
!B
�B
!�B
!�B
!�B
 �B
 �B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
 �B
�B
�B
�B
qB
�B
�B
!�B
#:B
$@B
'RB
+kB
.�B
-�B
,qB
,�B
2�B
5?B
6B
=qB
AUB
@�B
@�B
@B
>�B
>�B
>�B
?}B
A�B
A B
A�B
B'B
@B
?�B
?HB
>�B
=�B
>wB
<�B
<6B
;0B
9�B
:*B
9�B
8�B
7�B
7B
5�B
49B
3�B
3�B
2�B
2aB
1'B
/�B
/�B
.�B
,�B
,=B
-B
+�B
+6B
+kB
*eB
*0B
*0B
+B
+B
($B
&�B
%�B
%zB
$@B
#B
!�B
 �B
xB
=B
�B
B
�B
�B
�B
�B
�B
�B
7B
�B
_B
�B
�B
�B
SB
MB
B
B
�B
�B
uB
@B
�B
�B
hB
@B
�B
hB
 B
4B
4B
 B
4B
 B
�B
�B
�B
.B
�B
(B
�B
.B
bB
.B
.B
\B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
B
�B
B
�B
JB
~B
�B
�B
DB

�B
B
	�B
	lB
	B
�B
fB
�B
+B
1B
�B
fB
�B
+B
�B
_B
_B
�B
�B
	7B
	�B

�B
B
xB
�B
DB
�B
B
JB
�B
�B
B
�B
�B
�B
PB
�B
�B
�B
B
�B
�B
�B
�B
�B
"B
�B
�B
�B
�B
VB
�B
�B
�B
"B
�B
�B
"B
�B
�B
�B
VB
�B
(B
\B
\B
(B
(B
�B
(B
�B
bB
�B
�B
4B
�B
:B
B
B
uB
uB
uB
B
�B
�B
B
�B
B
{B
{B
MB
�B
�B
�B
�B
$B
�B
�B
_B
�B
�B
1B
�B
�B
�B
�B
�B
�B
B
7B
kB
kB
7B
7B
kB
=B
=B
=B
qB
�B
B
�B
�B
CB
B
�B
IB
�B
!�B
!�B
"4B
#:B
#B
$@B
$tB
$�B
%FB
%zB
%�B
%zB
%�B
&LB
&�B
&�B
&�B
&�B
&�B
&B
&B
&LB
%�B
%�B
%B
%FB
$B
!bB
!�B
#�B
$@B
#�B
#B
"4B
!�B
"4B
 �B
!�B
!-B
!bB
!bB
!bB
!�B
"hB
#:B
#�B
#�B
$�B
%FB
%B
%FB
%�B
$�B
%zB
&LB
&�B
&�B
'�B
'�B
'�B
(XB
(�B
)�B
*eB
*eB
*�B
*eB
*0B
*0B
)�B
)�B
*eB
*eB
*eB
+B
+B
+B
*�B
+B
+B
+�B
,=B
,�B
.}B
.IB
.IB
.}B
/B
.B
.�B
.�B
.�B
/B
0�B
0�B
0�B
0�B
1'B
1[B
1'B
2�B
1�B
2�B
2aB
2�B
49B
5B
4�B
4�B
5B
4�B
4�B
5B
5?B
6FB
5?B
5?B
5�B
5�B
5B
5B
5?B
5B
4�B
5?B
5?B
5tB
5tB
5tB
5�B
6FB
6�B
7�B
7LB
7LB
7B
7LB
7�B
7�B
8�B
8�B
8�B
9�B
;0B
;0B
;0B
;�B
;0B
;dB
;�B
<B
<B
<B
;�B
<�B
<jB
<jB
=<B
>�B
>wB
>wB
?B
?HB
?�B
@B
@OB
@�B
@�B
@�B
B�B
C�B
C�B
C�B
C�B
DgB
DgB
D�B
EB
E9B
E9B
EmB
E�B
EmB
E�B
F�B
F�B
F�B
F�B
F�B
GzB
G�B
G�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
JXB
J�B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
K^B
LdB
L0B
K�B
LdB
LdB
LdB
LdB
MB
M6B
MB
M6B
MjB
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
OBB
OBB
O�B
PB
P}B
P}B
P}B
P}B
P�B
P�B
QNB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RTB
R B
R�B
R�B
R�B
R�B
S&B
S[B
T,B
TaB
T�B
T�B
T�B
U2B
T�B
U�B
U�B
VB
V9B
V9B
V9B
V9B
V9B
V9B
VmB
V�B
W?B
W
B
WsB
W�B
XB
XEB
XEB
XEB
XyB
XyB
XEB
XyB
XEB
YB
Y�B
ZB
ZB
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[WB
[WB
\�B
\)B
\)B
\�B
]dB
]�B
^B
^jB
^�B
^�B
_;B
_;B
_pB
_pB
_�B
_�B
`B
_�B
`B
`B
`vB
`�B
a|B
a�B
a|B
a|B
aHB
a�B
a�B
a�B
a�B
bNB
bNB
b�B
b�B
bNB
b�B
bNB
bNB
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
e`B
e`B
e`B
e`B
e`B
e�B
e�B
f2B
ffB
ffB
ffB
ffB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
g�B
h
B
h
B
h�B
i�B
jB
i�B
jB
jB
jB
jB
j�B
j�B
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
l�B
m)B
m)B
m�B
m�B
m�B
m�B
n/B
ncB
ncB
n�B
n�B
o5B
o�B
o�B
p;B
poB
p;B
p;B
poB
qAB
p�B
qAB
q�B
rB
rGB
r|B
r�B
r�B
r�B
sB
sMB
s�B
tB
tTB
u%B
u�B
u�B
v+B
v`B��B��B�B��B��B��B��B��B�B��B��B��B��B�wB��B��B�B�}B�B��B�B�B��B��B��B��B��B��B�wB��B��B�B��B��B�OB�B�B�B��B��B�wB�}B��B��B�B��B�}B�}B��B��B��B��B��B�wB��B�B��B��B��B�OB��B�B��B��B��B��B�HB��B��B��B��B�}B��B��B��B�B�B�B�B��B��B�HB�B�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B��B��B�gB��B�OB�TB��B��B�B�ZB�TB��B��B��B��B��B��B�,B�B��B�@B��B��B�gB�<B��B��B�#B��B��B��B��B��B��B��B�8B	��B
B
1�B
JB
d�B
�-B
�EB
�(B
�BoB
B$B7	BC
BF�BPBa;BjQBg�Bp7Bs(Bw�B�B��B�<B�B�,BčBʴB��B҇B�	B��B��Bm�Bk}BhFB^Bl�Bi�BT�B-�B
ĸB
��B
{B
_�B
=�B
$�B
�B	��B	��B	�1B	�B	��B	p�B	MuB	?�B	K1B	a�B	o~B	sB	{�B	y�B	{�B	}nB	�KB	��B	�FB	�|B	�cB	�B	�B	�B	��B	�B	��B	��B	؏B	��B
'�B
<�B
TyB
`�B
p�B
}EB
�B
�B
�B
�B
��B
�iB
�sB
��B
�uB
~�B
��B
~RB
|�B
t�B
p�B
h�B
l=B
g�B
d9B
h�B
jAB
`B
V�B
KKB
K2B
C�B
=�B
?6B
7nB
2dB
-aB
*�B
(�B
(�B
(�B
)UB
'B
$�B
*TB
+�B
5�B
<&B
9�B
9*B
;@B
6cB
1WB
/�B
/�B
06B
0�B
/RB
-`B
*�B
(@B
'�B
$B
%qB
&0B
)�B
*�B
+�B
-�B
/ZB
2TB
7�B
4qB
2�B
2�B
-B
%!B
�B
�B
"B
"�B
#B
"�B
"MB
�B
�B
B
!iB
"NB
!CB
�B
�B
�B
�B
 �B
#1B
�B
LB
B
&B
�B
�B
!�B
#�B
$�B
(�B
-oB
0cB
-�B
,�B
,�B
2�B
51B
5�B
=�B
A�B
AvB
A�B
ADB
?MB
?�B
?|B
@@B
BjB
B~B
D!B
C�B
@�B
@�B
@B
?GB
?YB
@B
>B
=�B
<B
:�B
;�B
;�B
:�B
:@B
93B
6xB
4�B
4cB
4zB
3�B
3�B
2tB
1�B
3+B
0B
-�B
.B
.�B
,�B
,8B
,sB
+EB
+�B
,B
-�B
-tB
)B
'�B
'B
&�B
%GB
$�B
$B
#�B
�B
�B
B
�B
B
sB
�B
B
�B
�B
B
�B
LB
�B
B
�B
�B
�B
\B
JB
OB
MB
VB
:B
AB
�B
.B
�B
ZB
B
�B
3B
B
�B
-B
vB
(B
B
�B
ZB
MB
AB
�B
�B
mB
�B
{B
�B
�B
WB
�B
�B
B
�B
�B
�B
�B
�B
B
HB
�B
;B
B
�B
WB
wB
�B
B
�B
�B
�B

#B
	�B
	�B
	�B

ZB

NB
�B
	�B
.B

B
4B
�B
DB
dB
B
-B
�B

B
�B
uB
�B
B
�B
�B
�B
�B
�B
oB
QB
TB
(B
B
�B
�B
.B
�B
�B
�B
�B
�B
IB
0B
B
�B
�B
�B
�B
=B
�B
$B
B
NB
�B
8B
�B
�B
WB
qB
�B
%B
�B
�B
�B
�B
@B
TB
�B
vB
WB
�B
B
(B
B
�B
?B
-B
�B
�B
�B
DB
�B
CB
kB
nB
?B
�B
'B
	B
�B
�B
B
B
BB
fB
DB
+B
�B
�B
�B
TB
�B
B
5B
B
uB
�B
�B
�B
�B
�B
nB
�B
�B
�B
B
�B
�B
;B
JB
B
JB
&B
�B
bB
�B
�B
"RB
"(B
"�B
$%B
#�B
$�B
$�B
%�B
&1B
%�B
%�B
%�B
&vB
&�B
'DB
'$B
&�B
&�B
&�B
&�B
&wB
&�B
&:B
&cB
&�B
'�B
%TB
!aB
#MB
%nB
$�B
$nB
#sB
#QB
#DB
#�B
!7B
"#B
!�B
!�B
!�B
!�B
"B
#B
$)B
$=B
$�B
%�B
%�B
%�B
&B
&;B
%�B
&jB
&�B
'JB
'�B
(VB
(iB
(]B
)�B
)�B
*$B
+5B
*�B
*�B
*�B
*yB
*�B
*|B
*�B
*�B
*�B
*�B
+lB
+.B
+-B
*�B
+IB
+�B
,�B
,�B
,�B
/B
.�B
.�B
/pB
/�B
.�B
/6B
/*B
/0B
/�B
0�B
1B
1!B
0�B
1�B
1�B
2�B
3B
2?B
2�B
2�B
3�B
5B
5LB
4�B
5B
58B
5B
59B
5�B
6dB
7�B
5�B
6B
7B
6XB
5MB
5�B
5�B
5VB
5HB
5�B
5�B
5�B
5�B
5�B
6�B
7�B
8CB
8�B
7�B
7�B
7�B
7�B
8rB
9B
9=B
9?B
9XB
:�B
;oB
;ZB
;�B
<B
;?B
;�B
;�B
<B
<1B
<5B
<�B
=KB
<�B
=�B
>�B
?�B
>�B
?B
?�B
?�B
@�B
@�B
@�B
A=B
A�B
B(B
D(B
DyB
C�B
DB
D"B
D�B
D�B
EB
EtB
EqB
E}B
E�B
E�B
E�B
F�B
GB
GB
GB
GB
GB
G�B
HB
H3B
H�B
H�B
IB
I2B
I�B
J&B
J9B
J�B
J�B
J�B
KB
J�B
KB
LB
K�B
K�B
K�B
L�B
LIB
L>B
L�B
L�B
L�B
L�B
MiB
M^B
MIB
M�B
M�B
N/B
N`B
N�B
N�B
N�B
N�B
OB
OgB
O�B
O�B
P?B
PIB
P�B
P�B
P�B
P�B
P�B
Q,B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
R�B
RpB
SB
S*B
SB
SCB
S�B
TB
T�B
UB
U6B
T�B
U6B
UeB
U�B
VXB
V(B
V:B
V^B
VPB
VRB
VoB
VfB
V�B
V�B
W]B
W�B
WYB
W�B
X`B
X8B
XnB
X{B
XjB
X�B
X�B
X~B
X�B
X�B
ZB
Z�B
ZYB
ZJB
ZkB
Z�B
Z�B
Z�B
[B
[B
[[B
[�B
[�B
[�B
[�B
[�B
\[B
]B
\{B
\�B
]B
]�B
]�B
^OB
^�B
^�B
_NB
_�B
_�B
_�B
_�B
`B
`B
`+B
`*B
`�B
`�B
aB
aJB
a�B
a�B
a�B
a�B
a�B
bB
a�B
b B
b�B
b�B
b�B
b�B
b�B
bgB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
ftB
f�B
f|B
fnB
f�B
f�B
f�B
f�B
gB
geB
gfB
g�B
hB
g�B
hGB
h+B
hwB
h�B
i�B
j�B
jHB
j3B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
kB
l!B
k�B
k�B
k�B
lB
lB
l4B
l�B
l�B
mB
mDB
myB
mqB
nB
n%B
n%B
n*B
n~B
n�B
n�B
oB
oB
o�B
pB
pRB
pxB
p�B
pfB
pqB
p�B
q^B
q%B
q�B
r+B
rPB
rqB
r�B
r�B
sB
sB
sfB
s�B
tB
t�B
t�B
u�B
u�B
u�B
vUG�O�B��B��B�B��B��B��B��B��B�B��B��B��B��B�wB��B��B�B�}B�B��B�B�B��B��B��B��B��B��B�wB��B��B�B��B��B�OB�B�B�B��B��B�wB�}B��B��B�B��B�}B�}B��B��B��B��B��B�wB��B�B��B��B��B�OB��B�B��B��B��B��B�HB��B��B��B��B�}B��B��B��B�B�B�B�B��B��B�HB�B�B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0]�<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<]��<?*�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<t.G<Qp<#�
<#�
<)�w<<��<#�
<Un<#�
<#�
<#�
<#�
<#�
<`�~<�g�<��$=��<�i<��V<�T�<s2b<2�<`/?<��}<kx<@o<��<�,G<�y<�n<�`<;y�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021013102394120210131023941IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021020021720210210200217QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021020021720210210200217QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364520220422133645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064020220426160640IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064020220426160640IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064020220426160640IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                