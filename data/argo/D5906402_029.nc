CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-08-29T05:22:33Z creation; 2022-04-26T16:07:00Z DMQC;      
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
_FillValue        G�O�     X  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  Z`   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     X  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X     PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     X  �h   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     X  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     X  �p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     X  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     X 
x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X '�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     X /(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X L�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     X S�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` q0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   w�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   }�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210829052233  20220426232408  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_029                 8138_008904_029                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @ُK��w�@ُK��w�11  @ُK���@ُK���@-�F�V@-�F�V�d��`k�d��`k11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @�  @�  @�  @��RA  A   A,(�A@  A^�RA�  A�Q�A�  A��A�  AϮA�  A�  A��B  B(�B  B   B((�B0(�B8  B@(�BHQ�BPQ�BXQ�B`  Bg�
Bp  Bw�
B�  B�  B�  B�{B�{B�  B�{B�{B�{B�  B��B�  B�  B�{B��B�  B�{B�{B�  B�  B��B�  B�(�B�(�B�  B��B�  B�  B�  B�{B�{B�{C   C��C  C
=C  C
  C��C��C  C��C  C
=C  C  C��C��C   C"  C$  C&
=C(
=C*
=C,
=C.
=C/��C1��C4  C5��C7�C9��C;��C=�C@  CB  CC��CF  CH{CJ
=CL
=CN  CP  CR
=CT
=CV
=CX  CZ  C\  C^  C`  Cb
=Cd
=Cf
=Ch  Ci��Ck��Cm��Co��Cq��Ct  Cv  Cx
=Cz
=C|
=C~  C��C���C�  C�  C���C���C�C�C�  C�  C�C�  C�C�  C�  C�C�C���C���C���C���C�C�C�  C�C�  C�  C���C�  C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C�
=C�C�  C���C�  C�C�  C�  C�C�C�C�C�C�C�C�  C���C���C�  C�C�
=C�  C���C���C�  C�C�C�C���C���C���C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�C���C�  C�C�C�C���C���C�  C���C���C�C�C���C�  C�C�  C���C���C���C���C���C���C���C���C�  C�C�  C�C�C�  C���C���C���C�  C�C�  C���C���C�  C�C�  C���C���C�  C�
=D �D ��D�D��D  D� D�qD}qD  D�DD�D�D��D�D��DD� D�qD	� D	�qD
}qD  D� D  D��D�D}qD�qD}qD  D� D  D}qD��D}qD�D� D�qD��D�D}qD�qD}qD�qD� D  D��D  D��D�D�D�D� D�qD}qD  D� D  D}qD�qD��D�D��D   D � D!�D!��D"D"�D#  D#��D$�D$� D%�D%�D%�qD&}qD'  D'��D(  D(� D)D)� D*  D*}qD*�qD+� D+�qD,� D-�D-��D-�qD.z�D/  D/� D/�qD0z�D1  D1��D2D2� D3  D3�D4�D4��D4�qD5� D6  D6}qD7  D7��D8�D8� D9�D9� D9��D:}qD;�D;� D;�qD<��D=  D=}qD>�D>}qD>��D?z�D?�qD@� DA  DA� DA�qDB}qDC  DC� DD�DD��DE  DE� DF  DF��DG  DG� DH  DH� DI  DI��DJ  DJ� DJ�qDK}qDL�DL��DM  DM��DN�DN��DO  DO� DO�qDP� DQ  DQ� DR  DR� DS  DS� DT  DT��DU  DU}qDV  DV� DW  DW� DX  DX��DY  DY� DZ  DZ� D[�D[� D[�qD\� D]�D]��D^  D^� D_  D_}qD_�qD`}qD`�qDa}qDa�qDb}qDc  Dc}qDc�qDd� Dd�qDe� Df�Df��Dg�Dg� Dh  Dh� Di  Di� Dj�Dj� Dk  Dk}qDl  Dl��Dm�Dm� Dn  Dn��Do  Do� Dp  Dp� Dq�Dq� Dr  Dr� Dr�qDs}qDs�qDt��Du  Du}qDv  Dv� Dw  Dw}qDw�qDx� Dy  Dy� Dz�Dz� Dz�qD{}qD{�qD|��D}�D}� D~  D~}qD~�qD� D�  D�>�D�� D���D���D�>�D�~�D�� D�HD�@ D�~�D�� D���D�@ D��HD���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�AHD���D�� D�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD���D�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D�� D���D�AHD��HD��HD�HD�@ D���D�� D�HD�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D���D���D�AHD��HD�� D�HD�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D�� D�� D�HD�AHD�� D���D�  D�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�~�D�� D�  D�>�D�� D�� D�  D�@ D�~�D���D���D�>�D�~�D�� D���D�@ D��HD�� D���D�AHD���D��HD�HD�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D���D�>�D�~�D�� D��D�AHD���D��HD�HD�B�D�� D�� D�  D�@ D�~�D��qD���D�@ D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��qD�>�D�~�D���D���D�>�D�� D�� D�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D��HD�HD�@ D�� D���D�  D�AHD��HD��HD�HD�AHD��HD�� D���D�>�D�� D��HD�  D�AHD��HD�� D�  D�@ D�~�D�� D�  D�AHDHD�� D�  D�@ DÁHD��HD�HD�@ DĀ D�� D���D�=qD�~�D�� D���D�=qD�~�D�� D�  D�@ DǁHD�� D���D�>�DȀ DȽqD���D�@ Dɀ D��HD�HD�@ Dʀ D�� D�  D�@ DˁHD�� D���D�>�D�~�D̾�D��qD�@ D́HD�� D���D�>�D�}qD��HD�HD�@ DρHD�D�HD�@ D�~�D�� D�HD�@ Dр D�� D�  D�@ DҀ DҾ�D�  D�>�D�~�DӾ�D��qD�@ DԁHD��HD�HD�AHDՁHD��HD�HD�B�Dր DֽqD���D�=qD�~�D׾�D�  D�@ D؀ D��HD�HD�AHDفHD��HD�  D�@ Dڀ Dھ�D���D�>�D�~�D۽qD�  D�AHD܁HD��HD�  D�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD�� D���D�@ D�� D�� D���D�>�D�~�D��HD�HD�AHD�HD��HD�  D�=qD�~�D�� D�  D�@ D�HD��HD�HD�B�D� D徸D���D�@ D�~�D�qD�  D�@ D�~�D羸D��qD�>�D� D�� D��D�"�?��?#�
?W
=?�  ?��
?�p�?�
=?��@   @�@�R@+�@8Q�@B�\@Q�@^�R@c�
@u@�  @��@���@�33@��H@��\@��@���@�
=@�  @Ǯ@�{@�@�p�@��@�{@�@�p�A33AffA
=qA�RA�AffA=qAp�A!G�A$z�A(Q�A,(�A/\)A3�
A7
=A:=qA>�RAB�\AE�AH��AMp�AP��AS�
AXQ�A[�A_\)Ac�
Ag�Ak�Ap  As�
Aw
=A|(�A�  A��A��
A�{A�  A��A��
A�A�\)A���A��
A�A��A��A��
A�p�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?��@   @@  @�  @�  @�  @�  @��RA  A   A,(�A@  A^�RA�  A�Q�A�  A��A�  AϮA�  A�  A��B  B(�B  B   B((�B0(�B8  B@(�BHQ�BPQ�BXQ�B`  Bg�
Bp  Bw�
B�  B�  B�  B�{B�{B�  B�{B�{B�{B�  B��B�  B�  B�{B��B�  B�{B�{B�  B�  B��B�  B�(�B�(�B�  B��B�  B�  B�  B�{B�{B�{C   C��C  C
=C  C
  C��C��C  C��C  C
=C  C  C��C��C   C"  C$  C&
=C(
=C*
=C,
=C.
=C/��C1��C4  C5��C7�C9��C;��C=�C@  CB  CC��CF  CH{CJ
=CL
=CN  CP  CR
=CT
=CV
=CX  CZ  C\  C^  C`  Cb
=Cd
=Cf
=Ch  Ci��Ck��Cm��Co��Cq��Ct  Cv  Cx
=Cz
=C|
=C~  C��C���C�  C�  C���C���C�C�C�  C�  C�C�  C�C�  C�  C�C�C���C���C���C���C�C�C�  C�C�  C�  C���C�  C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C�
=C�C�  C���C�  C�C�  C�  C�C�C�C�C�C�C�C�  C���C���C�  C�C�
=C�  C���C���C�  C�C�C�C���C���C���C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�C���C�  C�C�C�C���C���C�  C���C���C�C�C���C�  C�C�  C���C���C���C���C���C���C���C���C�  C�C�  C�C�C�  C���C���C���C�  C�C�  C���C���C�  C�C�  C���C���C�  C�
=D �D ��D�D��D  D� D�qD}qD  D�DD�D�D��D�D��DD� D�qD	� D	�qD
}qD  D� D  D��D�D}qD�qD}qD  D� D  D}qD��D}qD�D� D�qD��D�D}qD�qD}qD�qD� D  D��D  D��D�D�D�D� D�qD}qD  D� D  D}qD�qD��D�D��D   D � D!�D!��D"D"�D#  D#��D$�D$� D%�D%�D%�qD&}qD'  D'��D(  D(� D)D)� D*  D*}qD*�qD+� D+�qD,� D-�D-��D-�qD.z�D/  D/� D/�qD0z�D1  D1��D2D2� D3  D3�D4�D4��D4�qD5� D6  D6}qD7  D7��D8�D8� D9�D9� D9��D:}qD;�D;� D;�qD<��D=  D=}qD>�D>}qD>��D?z�D?�qD@� DA  DA� DA�qDB}qDC  DC� DD�DD��DE  DE� DF  DF��DG  DG� DH  DH� DI  DI��DJ  DJ� DJ�qDK}qDL�DL��DM  DM��DN�DN��DO  DO� DO�qDP� DQ  DQ� DR  DR� DS  DS� DT  DT��DU  DU}qDV  DV� DW  DW� DX  DX��DY  DY� DZ  DZ� D[�D[� D[�qD\� D]�D]��D^  D^� D_  D_}qD_�qD`}qD`�qDa}qDa�qDb}qDc  Dc}qDc�qDd� Dd�qDe� Df�Df��Dg�Dg� Dh  Dh� Di  Di� Dj�Dj� Dk  Dk}qDl  Dl��Dm�Dm� Dn  Dn��Do  Do� Dp  Dp� Dq�Dq� Dr  Dr� Dr�qDs}qDs�qDt��Du  Du}qDv  Dv� Dw  Dw}qDw�qDx� Dy  Dy� Dz�Dz� Dz�qD{}qD{�qD|��D}�D}� D~  D~}qD~�qD� D�  D�>�D�� D���D���D�>�D�~�D�� D�HD�@ D�~�D�� D���D�@ D��HD���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�AHD���D�� D�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD���D�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D�� D���D�AHD��HD��HD�HD�@ D���D�� D�HD�@ D�� D��HD�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D���D���D�AHD��HD�� D�HD�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D�� D�� D�HD�AHD�� D���D�  D�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�~�D�� D�  D�>�D�� D�� D�  D�@ D�~�D���D���D�>�D�~�D�� D���D�@ D��HD�� D���D�AHD���D��HD�HD�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D���D�>�D�~�D�� D��D�AHD���D��HD�HD�B�D�� D�� D�  D�@ D�~�D��qD���D�@ D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D��qD�>�D�~�D���D���D�>�D�� D�� D�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D��HD�HD�@ D�� D���D�  D�AHD��HD��HD�HD�AHD��HD�� D���D�>�D�� D��HD�  D�AHD��HD�� D�  D�@ D�~�D�� D�  D�AHDHD�� D�  D�@ DÁHD��HD�HD�@ DĀ D�� D���D�=qD�~�D�� D���D�=qD�~�D�� D�  D�@ DǁHD�� D���D�>�DȀ DȽqD���D�@ Dɀ D��HD�HD�@ Dʀ D�� D�  D�@ DˁHD�� D���D�>�D�~�D̾�D��qD�@ D́HD�� D���D�>�D�}qD��HD�HD�@ DρHD�D�HD�@ D�~�D�� D�HD�@ Dр D�� D�  D�@ DҀ DҾ�D�  D�>�D�~�DӾ�D��qD�@ DԁHD��HD�HD�AHDՁHD��HD�HD�B�Dր DֽqD���D�=qD�~�D׾�D�  D�@ D؀ D��HD�HD�AHDفHD��HD�  D�@ Dڀ Dھ�D���D�>�D�~�D۽qD�  D�AHD܁HD��HD�  D�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD�� D���D�@ D�� D�� D���D�>�D�~�D��HD�HD�AHD�HD��HD�  D�=qD�~�D�� D�  D�@ D�HD��HD�HD�B�D� D徸D���D�@ D�~�D�qD�  D�@ D�~�D羸D��qD�>�D� D�� D��G�O�?��?#�
?W
=?�  ?��
?�p�?�
=?��@   @�@�R@+�@8Q�@B�\@Q�@^�R@c�
@u@�  @��@���@�33@��H@��\@��@���@�
=@�  @Ǯ@�{@�@�p�@��@�{@�@�p�A33AffA
=qA�RA�AffA=qAp�A!G�A$z�A(Q�A,(�A/\)A3�
A7
=A:=qA>�RAB�\AE�AH��AMp�AP��AS�
AXQ�A[�A_\)Ac�
Ag�Ak�Ap  As�
Aw
=A|(�A�  A��A��
A�{A�  A��A��
A�A�\)A���A��
A�A��A��A��
A�p�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AۑhAۓuAە�AۓuAۛ�Aۙ�A۟�Aۡ�Aۡ�Aۡ�Aۡ�Aۡ�Aۡ�Aۡ�Aۛ�A�|�A�ffA�?}A�&�A�  A���A�ȴA���AڼjAں^AڸRAڲ-AڮAڮAڥ�Aڛ�Aڕ�A�z�A�1'A���AٶFA٧�Aه+A�G�A���A�~�A�I�A��A֛�A�G�A��;AՇ+A�A�A��A�1A��/A�ȴAѸRA�~�A��AͅA�VAȁAǰ!A��A�S�A���A��HA�&�A��A��HA�p�A��\A�S�A�5?A���A�t�A�9XA��wA���A��^A���A���A���A�(�A�bNA��jA�n�A�ĜA��A���A���A�A��A�JA���A�(�A�ȴA�dZA���A�A�A�ȴA���A�33Az�Avv�AtAq��An�yAl��AkoAi�PAg��Ag�Ag
=Af��Ag%Af�`Af=qAaA[%AWt�AUANȴALffAK
=AH�HAE��AC��AA�A@^5A=�TA;��A:(�A8r�A6$�A4��A3dZA2�HA2bA0-A/�hA/�A0�`A1�A0~�A/�A/�A/?}A.�A-�A*�uA)x�A)��A*��A(1'A%C�A$1'A#;dA"�DA!�A!�wA!�;A!A!�-A!�A!�A ��A 1'A (�A�A�A�+A�TAC�A��AE�AbA�FAhsA��A��Ap�A&�A�/Az�A�A{A��A��A/AĜA9XA�wA;dA��A(�A�#Ax�AO�A"�A�A�RAZAbA�A�A�#A�
A��Al�A7LAAz�A�A�A$�A�A{A�A�
A��A\)A�AVAn�A;dAE�A �AbAbAA��A��A�A
�A
��A
ĜA
��A
$�A
bA	��A	+A��AVAbA��A�PA\)A�+AA�A�A7LA��A�AA��A7LA��Ar�A��AG�A �A ��@��m@���@���@���@��@�v�@�J@��`@��/@��9@�+@�$�@���@�x�@�V@�(�@�M�@�V@�u@�9X@��@�+@��@��^@���@�A�@�@�=q@���@�-@�7@�p�@�7L@���@�u@�j@�  @�;d@�\@�M�@��@�&�@�9X@�P@�p�@��;@�33@�@��@ޟ�@�n�@�E�@�=q@�-@��@�z�@��y@���@�ȴ@ڟ�@�ff@��@ّh@��@��@ם�@�C�@���@��`@Լj@�9X@�
=@�n�@��#@�hs@��@мj@Ͼw@�n�@�-@��#@�G�@���@�A�@�dZ@���@�M�@�J@��@���@Ɂ@�r�@��H@�`B@��@��`@�Q�@�(�@���@Õ�@�"�@���@�v�@�M�@�5?@�5?@�$�@�J@��#@��7@��/@��;@��@�~�@�=q@��@�J@��T@�@���@���@��@�;d@��\@�=q@�J@���@�&�@��@��F@�t�@�\)@�
=@��@��!@��+@�@��@�j@�9X@� �@�  @��w@�dZ@�;d@�+@�
=@��y@�ȴ@���@��@��@�p�@�?}@��@��/@�bN@��@��P@�v�@��@��T@�/@��m@�"�@��@���@��+@�^5@�=q@�$�@�J@��#@��^@��h@�`B@�7L@�&�@�&�@��@��@�V@�%@��@�Ĝ@���@�bN@��w@�33@�@��H@�M�@���@�hs@��`@�j@�Z@�1'@�b@��m@��m@��;@��
@��
@���@���@��w@��F@��@���@���@���@�|�@�@�ff@�J@���@�x�@�Ĝ@�9X@�  @��F@�l�@��@�$�@��#@��^@���@�O�@���@��9@�z�@�I�@�9X@�1'@�(�@� �@���@�C�@���@�~�@�5?@���@�@��@���@��9@�z�@�1@���@���@�|�@�l�@�C�@���@�M�@�@��-@�hs@��9@�I�@���@�\)@�
=@��\@��@��-@���@�`B@��@���@���@�bN@�(�@��@��m@��
@�ƨ@��F@���@�dZ@���@�v�@�E�@�$�@�{@�@��@��T@���@�@��h@��`@��u@�j@�(�@�ƨ@�l�@�C�@��H@��+@�$�@���@��^@��@�x�@�`B@�V@���@��D@�j@�9X@��m@��w@��@���@�t�@�dZ@�+@��H@��R@�^5@�-@��@�J@��@��#@�X@�Ĝ@��9@��@�1@�l�@�K�@�;d@�+@�@�@�x�@�7L@��@��j@��D@�j@�bN@�Z@�I�@� �@��@�@~v�@~$�@}��@}�h@}`B@}/@}V@|��@|�/@|�@|I�@{C�@yx�@xA�@w�@w\)@vȴ@vv�@u�T@t��@tI�@s�F@st�@r�@r�H@r=q@qx�@p��@pA�@o�P@o+@n�y@nE�@m?}@lz�@lI�@l(�@k��@kt�@ko@j��@jn�@j�@i�#@i�7@iX@iG�@iG�@iX@i7L@i�@hĜ@hbN@h �@g�@g�@g
=@f�R@f��@f�+@fff@f@e�h@eO�@e/@d�j@d�D@d9X@d1@c��@b�@bn�@b�@ahs@_��@_�@^�y@^�+@^{@]��@\�@[��@[��@[dZ@[33@["�@[@Z�@Z��@Z~�@Z^5@ZJ@Y�^@X�`@X��@XQ�@Xb@W�;@W|�@W;d@V��@V��@V�+@V@UO�@U/@T�@T�D@Tz�@Tj@TI�@T(�@T1@S��@S�m@S�
@S�@SS�@S33@So@R��@Q��@Q7L@P��@PĜ@PA�@O��@O;d@Nff@M�@M��@M�-@Mp�@M/@L(�@K��@J�!@JM�@I�#@IX@H��@H�@H  @G�P@F�y@Fȴ@F�R@F�R@F��@F��@Fv�@F5?@E�@E?}@D�@D�D@DZ@C�m@C��@Ct�@Ct�@CS�@B�H@B�@AG�@@1'@?��@?l�@?l�@?;d@>�@>��@>5?@=?}@<�@<j@;��@;��@;33@:�@:��@:�\@9�@9&�@8�@8bN@81'@8b@8  @8  @7��@7�P@7\)@6�y@6ȴ@6��@6v�@6v�@6V@6V@6E�@6{@5O�@4��@4�@3�m@3��@3��@3C�@2��@2^5@2M�@2=q@2�@1��@1x�@1%@0�`@0��@0Ĝ@0�u@0r�@0Q�@/�;@/��@/+@.�@.��@.v�@.E�@.{@-�@-�@-?}@-/@-�@,��@,�@,�/@,��@,�@,��@,�D@,z�@,j@,Z@,9X@,(�@+��@+�m@+�F@+o@*=q@*J@)�@)�7@)7L@)%@)%@(Ĝ@(bN@(Q�@(b@'�@'K�@&�@&�+@&ff@&{@&{@&@%�@%�@%�@%�T@%�T@%��@%��@%��@%@%�-@%�h@%�@%`B@%O�@%?}@%/@$��@$�@$�/@$�j@$��@$z�@$j@$I�@$9X@#��@#��@#�@#dZ@#33@"��@"��@"n�@"^5@"�@!��@!��@!G�@!%@ ��@ r�@ bN@ Q�@ A�@ b@�@��@\)@��@V@E�@E�@$�@�@@?}@V@�j@��AۑhAۑhAۍPAۑhAۓuAۏ\AۓuAۑhAۏ\AۓuAە�Aۗ�AۓuAۓuAە�Aە�AۓuAۗ�Aە�AۓuAۑhAۏ\A۟�A۝�A۝�A۟�Aۙ�Aۙ�Aە�Aۙ�A۝�A۝�A۟�Aۡ�A۟�Aۣ�A۟�A۝�Aۡ�A۟�Aۡ�Aۡ�A۝�Aۡ�Aۡ�A۟�Aۣ�A۟�A۝�A۟�A۝�A۝�Aۡ�Aۡ�Aۡ�Aۣ�Aۡ�A۟�Aۣ�Aۡ�A۝�Aۡ�Aۣ�A۟�Aۣ�Aۥ�A۟�A۟�Aۣ�A۟�A۝�A۟�A۟�Aۡ�Aۡ�Aۥ�Aۣ�Aۡ�Aۥ�Aۙ�AہAہA�~�A�z�A�t�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       AۑhAۓuAە�AۓuAۛ�Aۙ�A۟�Aۡ�Aۡ�Aۡ�Aۡ�Aۡ�Aۡ�Aۡ�Aۛ�A�|�A�ffA�?}A�&�A�  A���A�ȴA���AڼjAں^AڸRAڲ-AڮAڮAڥ�Aڛ�Aڕ�A�z�A�1'A���AٶFA٧�Aه+A�G�A���A�~�A�I�A��A֛�A�G�A��;AՇ+A�A�A��A�1A��/A�ȴAѸRA�~�A��AͅA�VAȁAǰ!A��A�S�A���A��HA�&�A��A��HA�p�A��\A�S�A�5?A���A�t�A�9XA��wA���A��^A���A���A���A�(�A�bNA��jA�n�A�ĜA��A���A���A�A��A�JA���A�(�A�ȴA�dZA���A�A�A�ȴA���A�33Az�Avv�AtAq��An�yAl��AkoAi�PAg��Ag�Ag
=Af��Ag%Af�`Af=qAaA[%AWt�AUANȴALffAK
=AH�HAE��AC��AA�A@^5A=�TA;��A:(�A8r�A6$�A4��A3dZA2�HA2bA0-A/�hA/�A0�`A1�A0~�A/�A/�A/?}A.�A-�A*�uA)x�A)��A*��A(1'A%C�A$1'A#;dA"�DA!�A!�wA!�;A!A!�-A!�A!�A ��A 1'A (�A�A�A�+A�TAC�A��AE�AbA�FAhsA��A��Ap�A&�A�/Az�A�A{A��A��A/AĜA9XA�wA;dA��A(�A�#Ax�AO�A"�A�A�RAZAbA�A�A�#A�
A��Al�A7LAAz�A�A�A$�A�A{A�A�
A��A\)A�AVAn�A;dAE�A �AbAbAA��A��A�A
�A
��A
ĜA
��A
$�A
bA	��A	+A��AVAbA��A�PA\)A�+AA�A�A7LA��A�AA��A7LA��Ar�A��AG�A �A ��@��m@���@���@���@��@�v�@�J@��`@��/@��9@�+@�$�@���@�x�@�V@�(�@�M�@�V@�u@�9X@��@�+@��@��^@���@�A�@�@�=q@���@�-@�7@�p�@�7L@���@�u@�j@�  @�;d@�\@�M�@��@�&�@�9X@�P@�p�@��;@�33@�@��@ޟ�@�n�@�E�@�=q@�-@��@�z�@��y@���@�ȴ@ڟ�@�ff@��@ّh@��@��@ם�@�C�@���@��`@Լj@�9X@�
=@�n�@��#@�hs@��@мj@Ͼw@�n�@�-@��#@�G�@���@�A�@�dZ@���@�M�@�J@��@���@Ɂ@�r�@��H@�`B@��@��`@�Q�@�(�@���@Õ�@�"�@���@�v�@�M�@�5?@�5?@�$�@�J@��#@��7@��/@��;@��@�~�@�=q@��@�J@��T@�@���@���@��@�;d@��\@�=q@�J@���@�&�@��@��F@�t�@�\)@�
=@��@��!@��+@�@��@�j@�9X@� �@�  @��w@�dZ@�;d@�+@�
=@��y@�ȴ@���@��@��@�p�@�?}@��@��/@�bN@��@��P@�v�@��@��T@�/@��m@�"�@��@���@��+@�^5@�=q@�$�@�J@��#@��^@��h@�`B@�7L@�&�@�&�@��@��@�V@�%@��@�Ĝ@���@�bN@��w@�33@�@��H@�M�@���@�hs@��`@�j@�Z@�1'@�b@��m@��m@��;@��
@��
@���@���@��w@��F@��@���@���@���@�|�@�@�ff@�J@���@�x�@�Ĝ@�9X@�  @��F@�l�@��@�$�@��#@��^@���@�O�@���@��9@�z�@�I�@�9X@�1'@�(�@� �@���@�C�@���@�~�@�5?@���@�@��@���@��9@�z�@�1@���@���@�|�@�l�@�C�@���@�M�@�@��-@�hs@��9@�I�@���@�\)@�
=@��\@��@��-@���@�`B@��@���@���@�bN@�(�@��@��m@��
@�ƨ@��F@���@�dZ@���@�v�@�E�@�$�@�{@�@��@��T@���@�@��h@��`@��u@�j@�(�@�ƨ@�l�@�C�@��H@��+@�$�@���@��^@��@�x�@�`B@�V@���@��D@�j@�9X@��m@��w@��@���@�t�@�dZ@�+@��H@��R@�^5@�-@��@�J@��@��#@�X@�Ĝ@��9@��@�1@�l�@�K�@�;d@�+@�@�@�x�@�7L@��@��j@��D@�j@�bN@�Z@�I�@� �@��@�@~v�@~$�@}��@}�h@}`B@}/@}V@|��@|�/@|�@|I�@{C�@yx�@xA�@w�@w\)@vȴ@vv�@u�T@t��@tI�@s�F@st�@r�@r�H@r=q@qx�@p��@pA�@o�P@o+@n�y@nE�@m?}@lz�@lI�@l(�@k��@kt�@ko@j��@jn�@j�@i�#@i�7@iX@iG�@iG�@iX@i7L@i�@hĜ@hbN@h �@g�@g�@g
=@f�R@f��@f�+@fff@f@e�h@eO�@e/@d�j@d�D@d9X@d1@c��@b�@bn�@b�@ahs@_��@_�@^�y@^�+@^{@]��@\�@[��@[��@[dZ@[33@["�@[@Z�@Z��@Z~�@Z^5@ZJ@Y�^@X�`@X��@XQ�@Xb@W�;@W|�@W;d@V��@V��@V�+@V@UO�@U/@T�@T�D@Tz�@Tj@TI�@T(�@T1@S��@S�m@S�
@S�@SS�@S33@So@R��@Q��@Q7L@P��@PĜ@PA�@O��@O;d@Nff@M�@M��@M�-@Mp�@M/@L(�@K��@J�!@JM�@I�#@IX@H��@H�@H  @G�P@F�y@Fȴ@F�R@F�R@F��@F��@Fv�@F5?@E�@E?}@D�@D�D@DZ@C�m@C��@Ct�@Ct�@CS�@B�H@B�@AG�@@1'@?��@?l�@?l�@?;d@>�@>��@>5?@=?}@<�@<j@;��@;��@;33@:�@:��@:�\@9�@9&�@8�@8bN@81'@8b@8  @8  @7��@7�P@7\)@6�y@6ȴ@6��@6v�@6v�@6V@6V@6E�@6{@5O�@4��@4�@3�m@3��@3��@3C�@2��@2^5@2M�@2=q@2�@1��@1x�@1%@0�`@0��@0Ĝ@0�u@0r�@0Q�@/�;@/��@/+@.�@.��@.v�@.E�@.{@-�@-�@-?}@-/@-�@,��@,�@,�/@,��@,�@,��@,�D@,z�@,j@,Z@,9X@,(�@+��@+�m@+�F@+o@*=q@*J@)�@)�7@)7L@)%@)%@(Ĝ@(bN@(Q�@(b@'�@'K�@&�@&�+@&ff@&{@&{@&@%�@%�@%�@%�T@%�T@%��@%��@%��@%@%�-@%�h@%�@%`B@%O�@%?}@%/@$��@$�@$�/@$�j@$��@$z�@$j@$I�@$9X@#��@#��@#�@#dZ@#33@"��@"��@"n�@"^5@"�@!��@!��@!G�@!%@ ��@ r�@ bN@ Q�@ A�@ b@�@��@\)@��@V@E�@E�@$�@�@@?}@V@�jG�O�AۑhAۑhAۍPAۑhAۓuAۏ\AۓuAۑhAۏ\AۓuAە�Aۗ�AۓuAۓuAە�Aە�AۓuAۗ�Aە�AۓuAۑhAۏ\A۟�A۝�A۝�A۟�Aۙ�Aۙ�Aە�Aۙ�A۝�A۝�A۟�Aۡ�A۟�Aۣ�A۟�A۝�Aۡ�A۟�Aۡ�Aۡ�A۝�Aۡ�Aۡ�A۟�Aۣ�A۟�A۝�A۟�A۝�A۝�Aۡ�Aۡ�Aۡ�Aۣ�Aۡ�A۟�Aۣ�Aۡ�A۝�Aۡ�Aۣ�A۟�Aۣ�Aۥ�A۟�A۟�Aۣ�A۟�A۝�A۟�A۟�Aۡ�Aۡ�Aۥ�Aۣ�Aۡ�Aۥ�Aۙ�AہAہA�~�A�z�A�t�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
� B
cB
�B
�B
�B
� B
�4B
�4B
� B
� B
cB
.B
~]B
~�B
}�B
{�B
|B
{B
z�B
z�B
zxB
zxB
zxB
zB
y�B
zB
zB
x�B
x8B
v`B
sB
m]B
l�B
lWB
jB
e�B
f2B
Z�B
T�B
V�B
W
B
T,B
R�B
N�B
K�B
HB
E�B
?HB
3�B
1[B
6�B
<6B
<�B
g�B
yrB
��B
��B
��B
��B
�'B
�B�B�B0�B6�B@�BFBMjB_;Bt�Bw2Bz�B|�B�uB��B��Bv�Bm�Bi�BQBB�B<�B4nB \BB
�)B
�*B
��B
�xB
�B
��B
�7B
�uB
rB
bNB
6�B	�+B	�9B	��B	�$B	�'B	�bB	��B	�B	�~B	��B	��B	�1B	��B	��B	��B	r|B	Z�B	QNB	JXB	D�B	>B	:�B	8�B	7B	1�B	3�B	0�B	5B	1�B	/�B	.�B	-wB	+6B	)�B	($B	)�B	.�B	:�B	I�B	`B	pB	qvB	q�B	qvB	q�B	v`B	�\B	�:B	�uB	�XB	�,B	��B	�$B	�'B	�IB	�B	��B	ȀB	�TB	��B	��B	�sB	��B	�sB	��B	�EB	�B	ޞB	�B
GB

rB
 B
�B
B
B
uB
B
�B
FB
B
�B
{B
�B
�B
�B
�B
oB
 B
�B
�B

rB
DB
�B
�B
"B
�B
�B
�B
�B
B
+B
�B
�B
�B
�B
�B
#�B
$B
%B
$@B
$B
($B
+�B
-�B
2�B
3�B
2�B
1�B
/�B
.�B
/�B
1�B
,=B
"�B
"hB
&B
(XB
)�B
)_B
*�B
1�B
2aB
2�B
2aB
2�B
0�B
0UB
1�B
0�B
/OB
-CB
.B
,=B
+6B
*eB
*�B
(XB
&�B
&�B
$�B
 �B
!B
CB
kB
�B
�B
�B
B
B
�B
4B
VB
�B
	lB
�B
�B
YB
�B
B
uB
�B
;B
;B
 �B
 4B
 �B
�B	��B	��B	��B	�]B	��B	�VB	��B	��B	��B	��B	��B	�(B	�]B	�]B	�]B	��B	��B	��B	��B	��B	��B	�JB	��B	�B	��B	�xB	�	B	��B	��B	��B	�rB	��B	�B	�xB	�B	�B	�B	��B
B
�B
B
B
 �B
 4B
  B
  B	�.B
  B	�]B	��B
 �B	��B	�(B	�.B	�.B
 4B	��B
 4B	��B	��B
B
oB
�B
oB
AB
�B
�B
�B
�B
�B
�B
{B
�B
B
MB
�B
�B
SB
�B
%B
�B
�B
�B
_B
�B
�B
�B
�B
+B
�B
�B
+B
_B
�B

	B

	B
	7B
	lB
	B
�B
fB
1B
�B
	B
fB

	B
	�B
	7B
	B
	B
	�B

�B
DB
xB
xB
xB
DB

�B

�B
xB
JB
�B
�B
JB
B
�B
PB
B
�B
B
�B
PB
�B
�B
VB
"B
�B
�B
�B
VB
VB
�B
bB
�B
\B
�B
:B
:B
:B
�B
�B
�B
�B
B
B
B
�B
@B
@B
@B
B
B
@B
uB
�B
@B
@B
�B
�B
@B
B
�B
�B
B
SB
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
1B
�B
_B
�B
1B
�B
eB
1B
�B
	B
=B
qB
�B
�B
xB
OB
�B
�B
~B
�B
�B
�B
�B
VB
VB
VB
�B
!B
�B
 \B
�B
�B
�B
�B
!B
VB
�B
�B
 �B
!�B
!�B
!bB
!bB
 �B
 �B
 �B
!�B
!-B
!�B
!bB
"�B
"�B
#�B
#nB
$B
$�B
%zB
%FB
%FB
%�B
%�B
&�B
&B
&�B
'B
&�B
'B
'B
'RB
'RB
'B
'�B
(�B
(�B
)*B
)*B
)*B
)*B
(�B
)*B
)*B
(�B
)_B
)�B
*�B
*0B
*�B
+�B
+6B
+6B
+kB
+�B
,�B
-wB
-�B
-wB
-�B
-�B
.�B
/B
/B
/OB
/�B
0UB
0UB
0UB
0UB
0�B
0�B
1'B
0�B
0�B
1�B
2-B
1�B
2-B
1�B
1�B
2-B
2�B
2�B
33B
4�B
4�B
5B
5B
4�B
4�B
6zB
6�B
6�B
7B
7�B
7�B
8B
7�B
8B
7�B
8B
8RB
8�B
9$B
9$B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
;�B
<B
<6B
<jB
<�B
<�B
=B
=�B
>BB
>wB
>wB
>�B
>wB
?B
?}B
?�B
@B
@OB
@�B
@�B
@�B
A�B
B'B
B'B
A�B
B'B
B�B
B�B
B�B
B�B
C-B
CaB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
C�B
DgB
EB
E9B
EB
EB
EB
E9B
E�B
E�B
E�B
FB
FB
F?B
F?B
FtB
F�B
GB
F�B
GEB
H�B
HKB
HKB
H�B
H�B
IB
K)B
K^B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
L0B
LdB
LdB
M6B
MB
M�B
MjB
M�B
M�B
M�B
NpB
NB
M�B
NpB
N�B
NpB
N�B
OBB
OBB
OBB
OBB
O�B
O�B
OvB
OvB
O�B
PB
O�B
O�B
O�B
P}B
Q�B
Q�B
Q�B
Q�B
RTB
RTB
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
UgB
VB
V9B
VmB
W?B
W?B
WsB
W�B
XEB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
XyB
YKB
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
ZQB
ZQB
Z�B
[#B
[#B
[�B
[�B
[�B
[�B
[�B
\)B
[�B
[�B
]/B
]dB
]�B
^B
^5B
^�B
^�B
^�B
_B
_;B
`vB
`�B
`�B
aB
aB
aB
aB
aHB
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
d�B
d�B
d�B
e,B
d�B
e`B
e�B
e�B
e�B
e`B
e�B
e�B
e�B
ffB
f2B
f2B
ffB
ffB
ffB
ffB
gB
hsB
iDB
iyB
i�B
i�B
i�B
jB
jB
j�B
kB
kB
kB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l"B
l"B
lWB
lWB
l�B
m�B
ncB
ncB
ncB
o B
o5B
o5B
o5B
o�B
pB
pB
p;B
p�B
qB
q�B
rB
rB
r�B
r|B
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
r�B
sB
sB
sB
sMB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
v+B
v+B
v+B
v�B
v�B
w2B
w2B
wfB
w2B
w2B
wfB
wfB
w�B
wfB
yrB
yrB
yrB
yrB
y�B
y�B
zDB
z�B
z�B
{B
{B
�B
�B
��B
~�B
cB
�4B
~�B
� B
�4B
~�B
��B
cB
� B
��B
~(B
�4B
�4B
�B
��B
�B
�B
��B
|�B
�4B
� B
~]B
�oB
�B
��B
~�B
�B
�B
~�B
cB
�iB
~�B
�B
��B
.B
�;B
� B
�B
�oB
cB
�B
�iB
�B
��B
��B
~�B
��B
�4B
�B
�B
��B
.B
� B
��B
�B
�4B
�B
�B
.B
��B
� B
cB
�iB
�B
~(B
�4B
�4B
~�B
~�B
� B
~�B
~]B
.B
� B
}�B
��B
cB
|PB
�B
|B
~�B
zxG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B
�B
�B
�B
�B
qB
�B
�B
�B
�B
�9B
�8B
�B
�B
�B
�B
~�B
]B
~(B
|�B
}+B
{�B
{B
z�B
z�B
z�B
z�B
z-B
y�B
zEB
zLB
yB
x�B
x8B
uOB
n
B
mbB
m3B
l-B
h�B
mB
\B
VMB
Y�B
YB
V�B
T�B
PPB
L�B
H�B
GDB
F�B
?�B
9MB
@CB
E�B
LwB
vVB
~�B
�B
�?B
��B
��B
ЏB
��B�B&TB5�B=�BG7BNXBZ�BpYB}B{�B�DB�gB�3B��B�JB|;BxdB{�BU�BD�B@�BAB<�B�B
֯B
��B
��B
��B
��B
��B
�TB
��B
{�B
�
B
O�B
LB	ݷB	��B	�2B	��B	�-B	��B	�3B	��B	�B	�B	�%B	�?B	�B	�
B	�B	eB	X�B	Z�B	K\B	BB	AB	A�B	<�B	7�B	7B	7{B	;B	5�B	4^B	5B	1LB	/FB	++B	*�B	.�B	0�B	9�B	GB	_qB	q�B	s%B	sB	r?B	sB	z�B	�WB	�RB	�B	��B	��B	�/B	�mB	��B	�]B	��B	�vB	�+B	ҿB	�"B	صB	�B	ھB	�
B	�/B	كB	�MB	�(B	��B
�B
�B
gB
�B
^B
jB
�B
�B
B
vB
@B
?B
�B
�B
oB
[B
xB
&B
"B
�B
�B
�B
NB
B
 B
�B
�B
�B
�B
B
&B
�B
�B
*B
�B
 	B
!B
$zB
%B
'B
%�B
$B
(B
+�B
.B
3B
4B
3�B
2�B
0�B
/@B
2FB
6�B
/�B
#nB
"�B
&&B
(�B
*vB
*fB
-&B
2�B
2�B
2�B
3XB
4�B
1cB
2;B
3�B
2�B
0�B
.�B
/MB
-gB
,tB
-�B
,(B
*B
)�B
)�B
'=B
!�B
 B
+B
�B
|B
B
�B
�B
GB
�B
�B
IB
BB
�B
�B
�B
�B
�B
�B
�B
�B
#B
B
B
�B
�B
FB
 �B	��B	��B	�wB	��B	��B
 �B	�LB	�XB
 �B	��B	��B	��B	��B	��B	��B	�YB	�uB	��B	�<B	��B	�B	��B	�WB	��B	��B	�	B	��B	��B	�hB	��B	�?B	��B	��B	��B	��B	��B	��B
�B
�B
8B
�B
IB
"B
qB
eB
�B
UB	��B
 B
�B	��B	��B
�B
 �B
�B
 �B
mB
 �B
�B
#B
QB
�B
�B
ZB
�B
�B
_B
�B
�B
JB
�B
B
B
IB
	.B
�B
B
�B
�B
�B
�B
�B
YB
OB
iB
�B
�B
jB
�B
mB
9B
	TB

pB
=B
+B
	�B
	�B
	DB
	DB
�B
�B
	�B

�B

�B
�B

�B
	�B
	�B

�B
rB
�B
�B
�B
/B
�B
�B
CB
�B
rB
@B
B
B
�B
�B
YB
�B
EB
�B
^B
1B
�B
?B
YB
�B
�B
�B
;B
�B
�B
|B
�B
B
!B
�B
QB
�B
�B
�B
B
�B
B
B
EB
oB
TB
.B
�B
�B
dB
B
"B
VB
�B
�B
yB
�B
&B
oB
tB
B
JB
AB
3B
BB
eB
�B
�B
-B
 B
�B
B
B
�B
�B
B
�B
B
UB
B
�B
B
HB
B
�B
�B
oB
aB
�B
B
GB
#B
�B
B
�B
�B
B
�B
B
	B
#B
mB
VB
B
WB
~B
nB
nB
B
�B
 �B
!jB
 �B
 cB
 vB
 AB
�B
 jB
 �B
 �B
!�B
"ZB
!�B
!�B
!�B
!B
!�B
"B
">B
!�B
"UB
"�B
#�B
#�B
$MB
$4B
%!B
%�B
%�B
%�B
%�B
&SB
&|B
&�B
&�B
'gB
'QB
'RB
'IB
'JB
'B
'�B
'�B
(�B
)QB
)2B
)uB
)VB
)TB
)TB
)"B
)cB
)PB
) B
*�B
*�B
+B
*�B
+|B
,�B
+�B
,B
,<B
,uB
-B
.	B
.YB
-�B
-�B
.nB
/�B
/_B
/rB
/�B
05B
0�B
0�B
0�B
0�B
0�B
1B
1�B
11B
1�B
2eB
2XB
2&B
2mB
1�B
2�B
3ZB
2�B
3TB
4IB
5�B
5'B
57B
5BB
5]B
6gB
7QB
7PB
7 B
7�B
7�B
8!B
8/B
7�B
8=B
81B
8B
8�B
9pB
9kB
9iB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:hB
;�B
<�B
<KB
<�B
<�B
=B
=B
=�B
>jB
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@@B
@�B
@�B
@�B
A<B
A�B
B�B
BRB
BGB
B/B
B�B
B�B
B�B
CPB
CPB
CwB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DbB
DHB
DkB
DQB
E	B
EYB
E_B
EB
E2B
EpB
E�B
E�B
E�B
FB
FEB
FcB
FB
F�B
GB
G4B
GwB
G�B
H�B
I9B
H�B
H�B
I+B
IIB
JKB
LB
KxB
K�B
K�B
K�B
K�B
LB
K�B
LPB
L]B
L�B
L�B
M4B
M�B
MZB
M�B
M�B
NB
N$B
NbB
N�B
N,B
NeB
O!B
N�B
N�B
O=B
OYB
OYB
OjB
OjB
O�B
O�B
O�B
O�B
PB
PJB
PB
PB
PlB
QoB
Q�B
RB
R-B
RyB
R�B
R�B
S�B
T6B
S�B
S�B
S�B
T!B
T�B
U�B
VPB
VsB
V�B
V�B
W�B
W�B
XB
X*B
X�B
Y	B
X�B
X�B
X�B
X�B
X�B
YB
Y-B
Y�B
ZB
ZNB
Y�B
ZaB
Z�B
Z�B
Z�B
Z�B
Z�B
[YB
\B
\;B
\�B
[�B
\ B
\B
\7B
\uB
\cB
]B
]�B
]�B
^"B
^mB
^�B
^�B
_B
^�B
_�B
`*B
a*B
`�B
`�B
a:B
a*B
a!B
aSB
a�B
a�B
bfB
b�B
b�B
b�B
b�B
b�B
b�B
cB
b�B
c�B
d�B
e$B
e	B
eDB
ePB
edB
e�B
fB
e�B
e�B
e�B
fB
fB
f�B
f�B
fLB
fQB
f�B
f�B
f�B
f�B
gcB
h�B
i�B
i�B
i�B
j"B
j#B
jMB
j�B
k4B
k5B
k6B
kEB
k�B
k�B
k�B
k�B
k�B
k�B
lB
lB
l<B
lMB
lAB
laB
l{B
l�B
miB
n�B
n�B
n�B
n�B
odB
orB
oGB
o�B
pB
p-B
pfB
p�B
q*B
q�B
rB
rIB
rsB
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
r�B
sB
sB
s5B
sDB
s4B
sfB
slB
s�B
s�B
s�B
s�B
s�B
tB
tB
tB
tCB
t|B
t�B
t�B
t�B
u
B
u�B
u�B
v4B
u�B
vKB
v^B
v|B
v�B
w'B
wAB
woB
wNB
w~B
wSB
wuB
w�B
w�B
xB
xVB
y�B
y�B
y~B
y�B
y�B
z,B
z�B
z�B
{B
{LG�O�B
�B
�B
��B
~�B
cB
�4B
~�B
� B
�4B
~�B
��B
cB
� B
��B
~(B
�4B
�4B
�B
��B
�B
�B
��B
|�B
�4B
� B
~]B
�oB
�B
��B
~�B
�B
�B
~�B
cB
�iB
~�B
�B
��B
.B
�;B
� B
�B
�oB
cB
�B
�iB
�B
��B
��B
~�B
��B
�4B
�B
�B
��B
.B
� B
��B
�B
�4B
�B
�B
.B
��B
� B
cB
�iB
�B
~(B
�4B
�4B
~�B
~�B
� B
~�B
~]B
.B
� B
}�B
��B
cB
|PB
�B
|B
~�B
zxG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<L��<#�
<*=G<+
?<�Ȝ<t�<#�
<#�
<#�
<#�
<#�
<r�;<#�
<#�
<#�
<#�
<#�
<#�
<#�
<b
<�1�<#�
<#�
<#�
<���<٠w<_�s<q�><#�
<6d�<�Z�<#�
<#�
<#�
<X<���<�� <F�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-�t=,AJ<ʷ�<P�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<P�k<��W<1�V<#�
<�-�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021082905223320210829052233IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021090809011720210908090117QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021090809011720210908090117QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364920220422133649IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                