CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-06-30T03:25:59Z creation; 2022-04-26T16:06:59Z DMQC;      
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
_FillValue        G�O�     P  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ZX   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     P  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     P  �P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     P  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P 
8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T '�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P .�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T L,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     P S�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` p�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   q0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   w0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   }0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210630032559  20220426232406  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_023                 8138_008904_023                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @ـD�{��@ـD�{��11  @ـD����@ـD����@,_��#x�@,_��#x��d�w[�0�d�w[�011  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�  @��R@��R@�  A   A  A ��A,(�A?\)A_\)A\)A�  A�  A�Q�A�Q�A�Q�A�  A�\)A��B  B(�B  B   B'�
B0  B8(�B?�
BG�BO�BW�B`  Bh  Bp  Bx  B�
B��B�  B�{B�{B�{B�  B��B�  B�  B�{B�{B�{B�{B�  B�  B��B�  B�{B�{B�  B��
B�  B�{B�=qB�  B�{B�  B��B�  B�  B�  C   C��C  C��C�C	��C
=C  C�C��C
=C  C�C  C  C  C   C"  C$
=C&
=C(  C*  C,  C-��C0  C2
=C3��C6  C8
=C:  C<
=C>{C?��CA��CC��CF
=CH  CJ
=CL  CN
=CP  CR  CT  CV  CX  CZ  C\  C^
=C`
=Cb  Cc��Ce��Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu��Cw��Cz  C|  C~  C�C���C���C�C�C�  C���C���C���C�C�  C�  C�C�  C���C���C���C�  C�  C�  C�C�  C�  C�  C���C�  C�
=C�C�  C�  C���C�  C�  C���C���C���C�  C�
=C�C�  C�  C�  C�  C�  C�  C�C�C�  C�  C���C���C�C�C�  C�C�  C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C�  C���C���C���C�  C�  C���C�  C�
=C�  C�  C�C�C�C�  C���C�  C�  C�  C�  C�  C���C���C���C�  C�C�C�  C�  C�  C�C�C�  C���C���C�  C�
=C�C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C�  D   D ��D  D� D  D� D�D}qD�D��D�qD� D�D� D  D� D  D� D	  D	��D	�qD
� D  D� D  D� D�qD}qD�D� D  D� D  D� D�qD� D�D� D  D��D�qD� D  D� D�qD}qD�qD}qD�qD}qD�qD� D  D��D�D� D  D� D  D� D  D� D�qD}qD�qD z�D �qD!��D"  D"� D#  D#� D$  D$� D%  D%}qD&  D&��D'  D'� D(�D(� D(�qD)��D*  D*}qD+  D+}qD+�qD,� D,�qD-}qD-�qD.}qD/�D/��D0�D0� D0��D1}qD2  D2��D3�D3��D4�D4� D4�qD5� D6  D6� D7�D7}qD7�qD8}qD8��D9� D9�qD:� D;�D;��D<  D<� D=  D=� D>  D>� D?�D?��D@�D@� DA  DA� DA�qDB��DC�DC��DD  DD}qDD�qDE� DF�DF� DG  DG� DH  DH��DI  DI� DJ  DJ��DK�DK��DL�DL� DL�qDM� DN  DN��DO�DO� DO�qDP� DQ  DQ� DR  DR� DS  DS��DT�DT��DU  DU� DV�DV��DW  DW� DX  DX� DX�qDY}qDZ�DZ}qD[  D[� D[�qD\}qD]  D]��D^�D^� D_�D_� D`  D`��Da�Da��Db  Db� Dc  Dc� Dd  Dd� De  De� De�qDf� Dg  Dgz�Dh  Dh� Dh��Di� Dj�Dj� Dj��Dk� Dl  Dl�Dm�Dm��Dn�Dn� Dn�qDo� Dp�Dp� Dp�qDq}qDr  Dr}qDr�qDs}qDs�qDt� Du  Du��Dv�Dv�Dw  Dw}qDx�Dx��Dy�Dy� Dz�Dz��D{  D{� D|  D|��D}�D}��D~�D~��D�D}qD��D�@ D��HD���D���D�>�D�� D��HD���D�@ D��HD���D�  D�AHD�� D�� D�  D�AHD��HD�� D���D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D��HD�� D�  D�AHD��HD���D�  D�>�D��HD��HD�HD�AHD�� D��qD���D�>�D�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D��HD��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D��HD�� D�  D�>�D�}qD��qD���D�>�D�� D���D���D�@ D�� D���D�  D�>�D�~�D���D�HD�@ D�� D��HD�HD�AHD�� D��HD�HD�AHD���D��HD�HD�@ D�~�D�� D�HD�AHD��HD���D�  D�AHD�� D���D�  D�>�D�� D��HD���D�>�D�~�D�� D�  D�=qD�~�D���D�HD�AHD���D��HD���D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D���D�  D�@ D�� D���D��qD�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�� D���D�  D�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�� D��HD�HD�@ D��HD�� D���D�@ D��HD��HD�HD�AHD��HD�� D�  D�AHD���D��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�AHD�� D���D���D�@ D��HD�� D���D�AHD��HD��HD�  D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D��HD�� D�  D�@ D�~�D�� D���D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D���D�  D�@ D�� D���D�  D�AHD��HD��HD�HD�AHD���D�� D���D�@ DHD�� D�HD�AHDÀ Dþ�D���D�>�D�~�Dľ�D���D�AHDŀ Dž�D���D�@ DƁHD�� D���D�AHDǀ DǾ�D�  D�AHDȁHDȾ�D���D�@ Dɀ Dɾ�D�  D�>�D�~�D�� D�  D�@ Dˀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�D;�D�HD�AHD΀ D�� D���D�@ DρHD�� D���D�@ DЁHD��HD�HD�@ DсHD�� D��qD�>�DҀ D�� D�HD�@ DӀ DӾ�D���D�@ DԁHD�D�HD�@ DՀ Dվ�D�  D�AHDցHD�� D���D�@ D׀ D׾�D���D�>�D؀ D�� D�  D�AHDـ Dپ�D���D�AHDڀ Dھ�D�  D�AHDۀ D�� D�HD�@ D܀ D��HD�  D�@ D݀ D��HD�  D�>�D�~�D޾�D�HD�@ D߀ D�� D�  D�AHD��HD�� D�  D�>�D� D��HD��D�AHD�HD�� D�  D�@ D�HD��HD�  D�@ D� D侸D�  D�@ D� D��HD���D�@ D�HD��HD�HD�B�D� D�� D�  D�@ D�HD���D��?��?#�
?L��?�=q?��
?\?�G�?��H@\)@(�@.{@:�H@Q�@^�R@n{@�  @��@��@�Q�@�G�@���@��@��H@\@˅@�33@޸R@�ff@�\)@���A ��AffA
=qA�RA�
A�A��A ��A%�A)��A.{A333A7
=A<(�A@��ADz�AI��AN{AS33AW
=A\(�AaG�Ae�Ai��An�RAr�\AxQ�A|(�A�Q�A��\A���A�\)A�G�A��
A�ffA�Q�A��HA�p�A�
=A��A��
A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?�  @   @B�\@�  @��R@��R@�  A   A  A ��A,(�A?\)A_\)A\)A�  A�  A�Q�A�Q�A�Q�A�  A�\)A��B  B(�B  B   B'�
B0  B8(�B?�
BG�BO�BW�B`  Bh  Bp  Bx  B�
B��B�  B�{B�{B�{B�  B��B�  B�  B�{B�{B�{B�{B�  B�  B��B�  B�{B�{B�  B��
B�  B�{B�=qB�  B�{B�  B��B�  B�  B�  C   C��C  C��C�C	��C
=C  C�C��C
=C  C�C  C  C  C   C"  C$
=C&
=C(  C*  C,  C-��C0  C2
=C3��C6  C8
=C:  C<
=C>{C?��CA��CC��CF
=CH  CJ
=CL  CN
=CP  CR  CT  CV  CX  CZ  C\  C^
=C`
=Cb  Cc��Ce��Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu��Cw��Cz  C|  C~  C�C���C���C�C�C�  C���C���C���C�C�  C�  C�C�  C���C���C���C�  C�  C�  C�C�  C�  C�  C���C�  C�
=C�C�  C�  C���C�  C�  C���C���C���C�  C�
=C�C�  C�  C�  C�  C�  C�  C�C�C�  C�  C���C���C�C�C�  C�C�  C�  C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C�  C���C���C���C�  C�  C���C�  C�
=C�  C�  C�C�C�C�  C���C�  C�  C�  C�  C�  C���C���C���C�  C�C�C�  C�  C�  C�C�C�  C���C���C�  C�
=C�C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C�  D   D ��D  D� D  D� D�D}qD�D��D�qD� D�D� D  D� D  D� D	  D	��D	�qD
� D  D� D  D� D�qD}qD�D� D  D� D  D� D�qD� D�D� D  D��D�qD� D  D� D�qD}qD�qD}qD�qD}qD�qD� D  D��D�D� D  D� D  D� D  D� D�qD}qD�qD z�D �qD!��D"  D"� D#  D#� D$  D$� D%  D%}qD&  D&��D'  D'� D(�D(� D(�qD)��D*  D*}qD+  D+}qD+�qD,� D,�qD-}qD-�qD.}qD/�D/��D0�D0� D0��D1}qD2  D2��D3�D3��D4�D4� D4�qD5� D6  D6� D7�D7}qD7�qD8}qD8��D9� D9�qD:� D;�D;��D<  D<� D=  D=� D>  D>� D?�D?��D@�D@� DA  DA� DA�qDB��DC�DC��DD  DD}qDD�qDE� DF�DF� DG  DG� DH  DH��DI  DI� DJ  DJ��DK�DK��DL�DL� DL�qDM� DN  DN��DO�DO� DO�qDP� DQ  DQ� DR  DR� DS  DS��DT�DT��DU  DU� DV�DV��DW  DW� DX  DX� DX�qDY}qDZ�DZ}qD[  D[� D[�qD\}qD]  D]��D^�D^� D_�D_� D`  D`��Da�Da��Db  Db� Dc  Dc� Dd  Dd� De  De� De�qDf� Dg  Dgz�Dh  Dh� Dh��Di� Dj�Dj� Dj��Dk� Dl  Dl�Dm�Dm��Dn�Dn� Dn�qDo� Dp�Dp� Dp�qDq}qDr  Dr}qDr�qDs}qDs�qDt� Du  Du��Dv�Dv�Dw  Dw}qDx�Dx��Dy�Dy� Dz�Dz��D{  D{� D|  D|��D}�D}��D~�D~��D�D}qD��D�@ D��HD���D���D�>�D�� D��HD���D�@ D��HD���D�  D�AHD�� D�� D�  D�AHD��HD�� D���D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D��HD�� D�  D�AHD��HD���D�  D�>�D��HD��HD�HD�AHD�� D��qD���D�>�D�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D��HD��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D��HD�� D�  D�>�D�}qD��qD���D�>�D�� D���D���D�@ D�� D���D�  D�>�D�~�D���D�HD�@ D�� D��HD�HD�AHD�� D��HD�HD�AHD���D��HD�HD�@ D�~�D�� D�HD�AHD��HD���D�  D�AHD�� D���D�  D�>�D�� D��HD���D�>�D�~�D�� D�  D�=qD�~�D���D�HD�AHD���D��HD���D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D���D�  D�@ D�� D���D��qD�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�� D���D�  D�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�� D��HD�HD�@ D��HD�� D���D�@ D��HD��HD�HD�AHD��HD�� D�  D�AHD���D��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�AHD�� D���D���D�@ D��HD�� D���D�AHD��HD��HD�  D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D��HD�� D�  D�@ D�~�D�� D���D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D���D�  D�@ D�� D���D�  D�AHD��HD��HD�HD�AHD���D�� D���D�@ DHD�� D�HD�AHDÀ Dþ�D���D�>�D�~�Dľ�D���D�AHDŀ Dž�D���D�@ DƁHD�� D���D�AHDǀ DǾ�D�  D�AHDȁHDȾ�D���D�@ Dɀ Dɾ�D�  D�>�D�~�D�� D�  D�@ Dˀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�D;�D�HD�AHD΀ D�� D���D�@ DρHD�� D���D�@ DЁHD��HD�HD�@ DсHD�� D��qD�>�DҀ D�� D�HD�@ DӀ DӾ�D���D�@ DԁHD�D�HD�@ DՀ Dվ�D�  D�AHDցHD�� D���D�@ D׀ D׾�D���D�>�D؀ D�� D�  D�AHDـ Dپ�D���D�AHDڀ Dھ�D�  D�AHDۀ D�� D�HD�@ D܀ D��HD�  D�@ D݀ D��HD�  D�>�D�~�D޾�D�HD�@ D߀ D�� D�  D�AHD��HD�� D�  D�>�D� D��HD��D�AHD�HD�� D�  D�@ D�HD��HD�  D�@ D� D侸D�  D�@ D� D��HD���D�@ D�HD��HD�HD�B�D� D�� D�  D�@ D�HD���G�O�?��?#�
?L��?�=q?��
?\?�G�?��H@\)@(�@.{@:�H@Q�@^�R@n{@�  @��@��@�Q�@�G�@���@��@��H@\@˅@�33@޸R@�ff@�\)@���A ��AffA
=qA�RA�
A�A��A ��A%�A)��A.{A333A7
=A<(�A@��ADz�AI��AN{AS33AW
=A\(�AaG�Ae�Ai��An�RAr�\AxQ�A|(�A�Q�A��\A���A�\)A�G�A��
A�ffA�Q�A��HA�p�A�
=A��A��
A�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A� �A��HAُ\AمA���A�M�A��/A�^5A�;dA�7LA�-A�+A�$�A� �A��A��A�oA�oA�{A��A��A� �A�(�A�33A�A�A�K�A�^5A�t�A�v�A�t�A�t�A�t�A�t�A�z�A�|�AׁA�r�A�ZA�?}A�&�A�bA��A֋DA�jA�{A���A�ffA�5?A��A���A��yA���AԼjA�n�AӲ-A҉7A���A�I�A�M�A��`A��A�O�A�VAǸRA�^5A�K�A�"�A�z�A��7A�-A�n�A�{A�-A��TA�bNA��`A��jA�n�A��yA���A�r�A�ȴA��RA� �A��mA�dZA�|�A��PA��FA�"�A�bNA�;dA��A�dZA��mA�hsA�O�A��/A�7LA�ĜA�bNA���A���A��`A�ffA��yA��
A�dZA�1'A�t�A%Ayx�Ao�Ai;dAfA`��A]dZA[��AWoAS�7AO�#AN�\AK�
AG��AEK�ACoAA`BA>��A<ZA;A;`BA;/A;"�A;�A:M�A7O�A7
=A6��A6A�A5�mA5
=A49XA3�A2�A2bA1��A1�hA0�DA.�HA.=qA-��A-S�A,1A+�A++A+/A*��A*~�A*5?A)K�A(v�A'��A'K�A'+A&��A%��A$ȴA#�A#t�A#hsA#C�A#
=A"�/A"�\A"r�A"Q�A"1A!|�A!7LA �A��A��A��A�A�;A�FA`BA
=A�DAA�A�TA|�AC�A�A�^AA�RAv�A-A�
AS�A�yA�+AZAbA�hA`BA\)A��AA�A��A��AO�A
=A�\A�A|�A��A=qA�#AdZA+A%A�HA�DA^5A  AƨAp�A��A�DAjA~�Ar�A1A\)A
�uA
I�A
A	�-A	�PA	7LA�yA~�AQ�A�A|�A?}A��A�FAO�AI�Ax�AXAXAA��AVAA�A�AJA�;A�7A ��A �jA j@�5?@�^5@�M�@���@�z�@��;@�ȴ@���@�Q�@�I�@�Z@�j@��j@�?}@�J@�$�@��@��/@�t�@��@���@�@�hs@�I�@�P@��H@��@�ff@�=q@���@��@���@�D@��@�o@��y@��y@�$�@�V@�I�@�S�@���@�{@�@��@��@�j@��
@�@�K�@���@��@�M�@��T@��@��@��u@�\)@��@��H@��y@��y@�@�"�@�@���@�`B@���@���@܃@�1@�t�@�l�@���@���@��@٩�@�J@�-@�M�@�@�x�@�&�@�j@��@���@�E�@��@ՙ�@�O�@���@�9X@���@�+@��@ѩ�@��@��@��@ЋD@ϥ�@�"�@��@�$�@�?}@�r�@�t�@�+@���@���@�p�@�`B@�G�@��/@�r�@�1'@��;@���@�v�@�E�@ř�@�/@ļj@�A�@î@¸R@�M�@���@�p�@��@���@�Ĝ@�bN@��m@��@�\)@�;d@�
=@�ȴ@��@�hs@�V@���@�Q�@�b@�33@�J@�@��h@��/@�|�@�C�@�;d@�+@�V@��h@�?}@��@��`@��9@�bN@�  @���@�S�@�;d@�o@��@��\@�5?@���@�x�@�V@��@���@�b@��m@�K�@�v�@���@��@�X@���@�Z@�1@��F@�\)@��R@�v�@��@��@�7L@���@�r�@�1@�\)@��@��y@���@���@��\@�^5@�/@�Z@�  @���@�ƨ@�ƨ@��F@�K�@��+@�V@�=q@��T@��@�?}@�V@�Ĝ@��@�bN@�I�@�1@��w@��@�n�@�@��h@�X@�&�@��`@�r�@�I�@�ƨ@�o@��!@�@�x�@��@��j@�r�@�j@�bN@�bN@�Q�@�(�@���@�|�@�+@�"�@���@�v�@�V@�J@���@��@�%@�%@��@�bN@�  @�ƨ@�|�@�\)@�33@��@�
=@��y@��@���@���@��R@��!@��\@�v�@�=q@�-@��@�x�@�7L@�V@��9@�  @��
@���@�S�@�@��H@��\@�@���@�x�@�O�@�/@��@���@��u@�I�@��@�dZ@�K�@�"�@���@���@��R@��\@�5?@���@�O�@�?}@�V@���@��u@��@�9X@��;@��@���@�M�@��@��-@��h@��@�O�@�?}@�/@��@�%@���@���@�bN@�9X@� �@�  @��F@�t�@�K�@�"�@���@���@�@�?}@��@���@��j@��u@�b@��@;d@~�y@~E�@}@}�-@}��@}?}@|I�@{t�@z��@z=q@zJ@y�@y�7@y�@x�u@x1'@xb@x  @x  @w�;@w�P@v��@v{@u��@u�@u`B@u?}@t�@t��@t�j@tz�@tZ@t9X@s�F@s33@r�@q�7@q7L@q%@pĜ@p��@p�u@p�u@p�u@pbN@o�@nv�@m`B@mV@lz�@k�m@kdZ@k"�@k@j��@j��@j��@j^5@j-@iX@hbN@g��@g�P@g
=@f�@f�R@f�+@fE�@e�@e��@eO�@d��@d��@dz�@d�@c�
@c��@cS�@cC�@b��@bM�@a�#@ax�@aG�@`��@`bN@_�@_+@^V@]�h@]?}@]V@\��@\�@[�
@[��@[C�@["�@["�@Z�\@Y��@Yhs@X��@XA�@W|�@W
=@V�@Vȴ@VE�@U@U`B@T��@Tz�@T9X@S�m@St�@SdZ@So@R�H@R�!@R^5@RJ@Q�7@Qx�@Qhs@Qhs@QX@Q&�@P�9@P1'@O�@O��@O��@O�P@O|�@O
=@N��@N��@Nv�@Nff@NE�@M�@M`B@L�/@L��@KC�@J~�@J-@JJ@JJ@I��@I�^@H �@F�R@F$�@Ep�@D�/@D�D@D(�@Cƨ@C��@Ct�@CC�@C@B��@B��@B��@Bn�@Bn�@B^5@B^5@B=q@B=q@BM�@B-@A�@A��@Ahs@@Ĝ@@bN@@bN@?�@>��@>$�@<z�@;��@;��@;o@:^5@9��@9�7@9X@9%@8�9@8r�@7�;@7\)@7�@7�@7
=@6�y@6�R@6��@6v�@6E�@5O�@4I�@3�
@3�@3t�@3dZ@3C�@2�\@2J@1�#@1hs@17L@0Ĝ@0�@0r�@0Q�@0A�@0 �@/�@/l�@/\)@/\)@/K�@/;d@/+@/�@.��@.�R@.��@.��@.v�@.ff@.V@.5?@-��@-/@,��@,��@,�j@,I�@,�@,�@,1@+�@*^5@*^5@*-@)��@)�@)x�@)%@(��@(��@(��@(Ĝ@(��@(b@'��@'�P@'�@&�+@&5?@%�T@%��@%��@%/@$��@$��@$I�@$(�@$1@#��@#�
@#�F@#�F@#��@#��@#"�@"��@"-@!�#@!��@!�@ �9@ �@ Q�@   @\)@
=@�R@V@@�-@�@`B@�@�/@�j@�D@9X@��@��@�@dZ@33@@�!@^5@M�@=q@=q@=q@M�@M�@=q@=q@M�@=q@=qA�5?A�5?A�7LA�=qA�A�A�M�A�?}A�
=A��A���A�A��#A��Aٰ!Aُ\AًDAُ\Aُ\AًDAٍPAفA�ffA�?}AخA؉7A؇+A�I�A�?}A�bA���A��Aש�A�r�A�ZA�M�A�E�A�A�A�9XA�9XA�7LA�7LA�1'A�-A�-A�+A�/A�+A�+A�-A�(�A�-A�&�A�(�A�-A�&�A�(�A�(�A�"�A�$�A�"�A�"�A�"�A��A� �A� �A��A� �A��A�"�A�$�A� �A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A�?}A� �A��HAُ\AمA���A�M�A��/A�^5A�;dA�7LA�-A�+A�$�A� �A��A��A�oA�oA�{A��A��A� �A�(�A�33A�A�A�K�A�^5A�t�A�v�A�t�A�t�A�t�A�t�A�z�A�|�AׁA�r�A�ZA�?}A�&�A�bA��A֋DA�jA�{A���A�ffA�5?A��A���A��yA���AԼjA�n�AӲ-A҉7A���A�I�A�M�A��`A��A�O�A�VAǸRA�^5A�K�A�"�A�z�A��7A�-A�n�A�{A�-A��TA�bNA��`A��jA�n�A��yA���A�r�A�ȴA��RA� �A��mA�dZA�|�A��PA��FA�"�A�bNA�;dA��A�dZA��mA�hsA�O�A��/A�7LA�ĜA�bNA���A���A��`A�ffA��yA��
A�dZA�1'A�t�A%Ayx�Ao�Ai;dAfA`��A]dZA[��AWoAS�7AO�#AN�\AK�
AG��AEK�ACoAA`BA>��A<ZA;A;`BA;/A;"�A;�A:M�A7O�A7
=A6��A6A�A5�mA5
=A49XA3�A2�A2bA1��A1�hA0�DA.�HA.=qA-��A-S�A,1A+�A++A+/A*��A*~�A*5?A)K�A(v�A'��A'K�A'+A&��A%��A$ȴA#�A#t�A#hsA#C�A#
=A"�/A"�\A"r�A"Q�A"1A!|�A!7LA �A��A��A��A�A�;A�FA`BA
=A�DAA�A�TA|�AC�A�A�^AA�RAv�A-A�
AS�A�yA�+AZAbA�hA`BA\)A��AA�A��A��AO�A
=A�\A�A|�A��A=qA�#AdZA+A%A�HA�DA^5A  AƨAp�A��A�DAjA~�Ar�A1A\)A
�uA
I�A
A	�-A	�PA	7LA�yA~�AQ�A�A|�A?}A��A�FAO�AI�Ax�AXAXAA��AVAA�A�AJA�;A�7A ��A �jA j@�5?@�^5@�M�@���@�z�@��;@�ȴ@���@�Q�@�I�@�Z@�j@��j@�?}@�J@�$�@��@��/@�t�@��@���@�@�hs@�I�@�P@��H@��@�ff@�=q@���@��@���@�D@��@�o@��y@��y@�$�@�V@�I�@�S�@���@�{@�@��@��@�j@��
@�@�K�@���@��@�M�@��T@��@��@��u@�\)@��@��H@��y@��y@�@�"�@�@���@�`B@���@���@܃@�1@�t�@�l�@���@���@��@٩�@�J@�-@�M�@�@�x�@�&�@�j@��@���@�E�@��@ՙ�@�O�@���@�9X@���@�+@��@ѩ�@��@��@��@ЋD@ϥ�@�"�@��@�$�@�?}@�r�@�t�@�+@���@���@�p�@�`B@�G�@��/@�r�@�1'@��;@���@�v�@�E�@ř�@�/@ļj@�A�@î@¸R@�M�@���@�p�@��@���@�Ĝ@�bN@��m@��@�\)@�;d@�
=@�ȴ@��@�hs@�V@���@�Q�@�b@�33@�J@�@��h@��/@�|�@�C�@�;d@�+@�V@��h@�?}@��@��`@��9@�bN@�  @���@�S�@�;d@�o@��@��\@�5?@���@�x�@�V@��@���@�b@��m@�K�@�v�@���@��@�X@���@�Z@�1@��F@�\)@��R@�v�@��@��@�7L@���@�r�@�1@�\)@��@��y@���@���@��\@�^5@�/@�Z@�  @���@�ƨ@�ƨ@��F@�K�@��+@�V@�=q@��T@��@�?}@�V@�Ĝ@��@�bN@�I�@�1@��w@��@�n�@�@��h@�X@�&�@��`@�r�@�I�@�ƨ@�o@��!@�@�x�@��@��j@�r�@�j@�bN@�bN@�Q�@�(�@���@�|�@�+@�"�@���@�v�@�V@�J@���@��@�%@�%@��@�bN@�  @�ƨ@�|�@�\)@�33@��@�
=@��y@��@���@���@��R@��!@��\@�v�@�=q@�-@��@�x�@�7L@�V@��9@�  @��
@���@�S�@�@��H@��\@�@���@�x�@�O�@�/@��@���@��u@�I�@��@�dZ@�K�@�"�@���@���@��R@��\@�5?@���@�O�@�?}@�V@���@��u@��@�9X@��;@��@���@�M�@��@��-@��h@��@�O�@�?}@�/@��@�%@���@���@�bN@�9X@� �@�  @��F@�t�@�K�@�"�@���@���@�@�?}@��@���@��j@��u@�b@��@;d@~�y@~E�@}@}�-@}��@}?}@|I�@{t�@z��@z=q@zJ@y�@y�7@y�@x�u@x1'@xb@x  @x  @w�;@w�P@v��@v{@u��@u�@u`B@u?}@t�@t��@t�j@tz�@tZ@t9X@s�F@s33@r�@q�7@q7L@q%@pĜ@p��@p�u@p�u@p�u@pbN@o�@nv�@m`B@mV@lz�@k�m@kdZ@k"�@k@j��@j��@j��@j^5@j-@iX@hbN@g��@g�P@g
=@f�@f�R@f�+@fE�@e�@e��@eO�@d��@d��@dz�@d�@c�
@c��@cS�@cC�@b��@bM�@a�#@ax�@aG�@`��@`bN@_�@_+@^V@]�h@]?}@]V@\��@\�@[�
@[��@[C�@["�@["�@Z�\@Y��@Yhs@X��@XA�@W|�@W
=@V�@Vȴ@VE�@U@U`B@T��@Tz�@T9X@S�m@St�@SdZ@So@R�H@R�!@R^5@RJ@Q�7@Qx�@Qhs@Qhs@QX@Q&�@P�9@P1'@O�@O��@O��@O�P@O|�@O
=@N��@N��@Nv�@Nff@NE�@M�@M`B@L�/@L��@KC�@J~�@J-@JJ@JJ@I��@I�^@H �@F�R@F$�@Ep�@D�/@D�D@D(�@Cƨ@C��@Ct�@CC�@C@B��@B��@B��@Bn�@Bn�@B^5@B^5@B=q@B=q@BM�@B-@A�@A��@Ahs@@Ĝ@@bN@@bN@?�@>��@>$�@<z�@;��@;��@;o@:^5@9��@9�7@9X@9%@8�9@8r�@7�;@7\)@7�@7�@7
=@6�y@6�R@6��@6v�@6E�@5O�@4I�@3�
@3�@3t�@3dZ@3C�@2�\@2J@1�#@1hs@17L@0Ĝ@0�@0r�@0Q�@0A�@0 �@/�@/l�@/\)@/\)@/K�@/;d@/+@/�@.��@.�R@.��@.��@.v�@.ff@.V@.5?@-��@-/@,��@,��@,�j@,I�@,�@,�@,1@+�@*^5@*^5@*-@)��@)�@)x�@)%@(��@(��@(��@(Ĝ@(��@(b@'��@'�P@'�@&�+@&5?@%�T@%��@%��@%/@$��@$��@$I�@$(�@$1@#��@#�
@#�F@#�F@#��@#��@#"�@"��@"-@!�#@!��@!�@ �9@ �@ Q�@   @\)@
=@�R@V@@�-@�@`B@�@�/@�j@�D@9X@��@��@�@dZ@33@@�!@^5@M�@=q@=q@=q@M�@M�@=q@=q@M�@=qG�O�A�5?A�5?A�7LA�=qA�A�A�M�A�?}A�
=A��A���A�A��#A��Aٰ!Aُ\AًDAُ\Aُ\AًDAٍPAفA�ffA�?}AخA؉7A؇+A�I�A�?}A�bA���A��Aש�A�r�A�ZA�M�A�E�A�A�A�9XA�9XA�7LA�7LA�1'A�-A�-A�+A�/A�+A�+A�-A�(�A�-A�&�A�(�A�-A�&�A�(�A�(�A�"�A�$�A�"�A�"�A�"�A��A� �A� �A��A� �A��A�"�A�$�A� �A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
_B
�B
	B
�B
"B
DB
B
xB
	�B
	�B

�B
�B
�B
(B
hB
�B
:B
�B
�B
_B
�B
xB
 \B
&LB
-�B
5tB
?�B
OB
U2B
V�B
XEB
YB
Y�B
[�B
^B
c�B
jB
h�B
ffB
dZB
a|B
]�B
V�B
S�B
N<B
G�B
?�B
<�B
<jB
;�B
;0B
:^B
8�B
5?B
,B
33B
A B
FB
F�B
5?B
2�B
5tB
P�B
[#B
{�B
�B
�_B
�0B
��Bv�BʌB�-B��B�0B�zB�B}VBv�BqBg�Be�Bc�Bl�Bv�BrBl�Be�Ba�B]�BYB]dBJ#BB�B=qB/�BVB�B�B�B
�GB
�B
�}B
��B
��B
�B
�{B
qAB
R�B
/�B
B
{B	�B	��B	��B	��B	�oB	jB	`�B	G�B	49B	/�B	-CB	.IB	0�B	9�B	B�B	IB	`�B	n�B	qB	t�B	xB	y	B	yrB	��B	��B	�\B	�4B	�LB	��B	�$B	ÖB	�vB	�
B	�2B	��B	�"B	��B	�rB
;B
�B
�B
~B
�B
JB
�B
,�B
-�B
0!B
4�B
.�B
+�B
.}B
5B
/B
7�B
9�B
7�B
5B
5�B
9�B
;�B
=B
=B
?HB
B'B
B[B
AUB
?B
>BB
<B
8�B
9XB
9XB
:�B
;0B
<�B
=�B
>�B
>BB
>B
>�B
>�B
GzB
J#B
HB
H�B
HKB
H�B
J�B
K�B
K�B
L0B
K�B
K�B
K^B
J#B
J#B
K�B
L�B
LdB
L0B
LdB
L�B
K)B
H�B
C�B
@�B
>wB
>wB
;�B
<B
<�B
=B
D3B
F?B
F�B
K^B
J�B
J�B
J�B
K�B
NpB
PB
NB
HKB
F?B
C�B
B�B
@�B
?}B
=�B
<jB
:*B
8�B
6B
0�B
.�B
+kB
%zB
�B
OB
SB
�B
�B
�B
�B
B
bB
�B
�B
�B
MB
�B
�B
�B

�B
�B
bB
@B
�B
B
1B
B	��B
�B
B
	7B
PB
oB
B
�B
	B
1B
B
PB
�B
�B	��B	��B	�DB	�rB	��B	��B	�8B	��B	��B	��B	��B	�2B	��B	��B	�8B	��B	��B	��B	��B	�`B	��B	��B	��B	��B	�fB	�`B	�%B	��B	��B	��B	��B	�B	�2B	��B	��B	��B	��B	��B	��B	�lB	��B	�B	�DB	�rB	��B	��B	�"B	��B	��B	�.B
�B
;B
 4B	��B
AB
�B
�B
	7B
JB
~B
�B
�B
DB
DB
B

=B
	�B
	B
�B
�B
	B
�B
�B
_B
	B
	lB

=B

=B
JB

�B

	B
	�B
	lB
	B
�B
_B
�B
1B
+B
�B
�B
�B
�B
�B
�B
_B
YB
YB
�B
YB
�B
%B
�B
�B
+B
�B
�B
�B
�B
�B
�B
1B
1B
1B
1B
�B
�B
	B
�B
�B
fB
�B
�B
	B
	�B
	lB
	B
	�B
DB

�B

rB

	B
xB
�B
�B
�B
JB
B
�B
�B
B
B
�B
B
PB
�B
�B
�B
�B
�B
VB
�B
�B
"B
�B
.B
�B
.B
bB
�B
 B
hB
hB
�B
B
B
�B
�B
B
B
B
B
{B
{B
�B
�B
{B
B
B
�B
�B
�B
$B
�B
�B
�B
�B
1B
1B
�B
�B
�B
�B
B
=B
=B
=B
	B
7B
�B
qB
xB
B
�B
�B
�B
B
�B
�B
�B
VB
!B
�B
VB
VB
�B
�B
!B
�B
!B
!B
�B
�B
�B
 �B
 \B
 �B
!bB
!-B
!�B
#:B
#�B
#nB
#nB
#:B
$B
$B
$B
$@B
$@B
$B
$tB
$tB
$�B
$tB
$�B
$tB
$�B
$�B
$@B
$�B
$tB
$B
$tB
$B
$�B
$�B
$�B
$�B
$tB
$�B
%FB
%zB
%B
%�B
&�B
'B
'B
'RB
'�B
'�B
($B
'�B
(�B
)*B
)�B
)�B
)�B
)�B
*0B
)�B
)�B
*�B
+B
+�B
+B
+kB
,=B
,B
+kB
,=B
,qB
-�B
-�B
.�B
/�B
/�B
/�B
0UB
1'B
0�B
0�B
0�B
1[B
0�B
1�B
2-B
2-B
1�B
1�B
2-B
2�B
2�B
2�B
2�B
2�B
4B
4�B
5?B
5?B
4�B
5B
6FB
6B
6�B
6zB
7LB
7�B
7�B
7LB
7�B
8RB
8�B
9�B
9�B
:^B
9�B
:^B
:�B
;�B
;dB
;�B
;�B
;�B
;dB
<B
<jB
=<B
=�B
=<B
=qB
=�B
>BB
>BB
>B
=�B
=�B
>B
>BB
>BB
?�B
?�B
?�B
@OB
@B
@�B
@�B
@�B
?�B
@B
@�B
A B
A�B
A�B
A�B
B[B
CaB
CaB
C�B
CaB
C-B
CaB
CaB
C�B
C�B
D3B
DgB
D�B
E9B
E9B
EB
EmB
EmB
EmB
FB
FB
E�B
F?B
F?B
F�B
F�B
F�B
GEB
F�B
GEB
G�B
HB
HB
HB
G�B
HKB
H�B
IRB
JXB
JXB
J�B
J�B
JXB
K�B
K^B
K�B
K�B
K�B
K�B
L0B
MB
L�B
L�B
M6B
NB
N<B
N<B
NB
N�B
OB
OB
O�B
O�B
O�B
P}B
P�B
P}B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
Q�B
Q�B
R B
R�B
S[B
R�B
R�B
S&B
R�B
R�B
S�B
S�B
S[B
S�B
S�B
S[B
S�B
S�B
T,B
S�B
VB
V9B
V�B
VmB
VmB
V9B
VB
WsB
W?B
W?B
WsB
W�B
W�B
W�B
X�B
XyB
XyB
X�B
YB
YKB
YKB
Y�B
Y�B
YB
Y�B
Y�B
Y�B
Y�B
YB
Y�B
ZB
Y�B
ZB
Z�B
ZQB
Y�B
Y�B
Z�B
Y�B
\]B
[�B
[�B
\]B
\�B
]dB
]�B
]�B
]dB
]�B
^B
^jB
_B
_pB
_;B
_;B
_;B
_pB
_�B
_pB
_;B
`vB
a|B
bNB
b�B
b�B
b�B
bNB
cTB
cTB
cTB
d&B
c�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
e`B
e�B
f2B
f2B
e�B
f2B
f2B
f2B
f2B
ffB
ffB
f2B
ffB
g8B
gB
f�B
f�B
g8B
g�B
g�B
g�B
g�B
hsB
jKB
iDB
iyB
jB
i�B
jKB
j�B
j�B
kB
j�B
kB
kB
k�B
k�B
k�B
lWB
l�B
l�B
m)B
l�B
m]B
n/B
n�B
n�B
o B
n�B
ncB
n�B
o B
o B
n�B
n�B
n�B
n�B
n�B
p;B
p;B
pB
p�B
poB
p�B
p�B
p�B
rGB
rGB
r�B
r�B
s�B
s�B
s�B
s�B
tTB
tTB
t�B
t�B
u%B
u�B
u�B
u�B
u�B
v`B
w2B
w�B
w�B
xB
x8B
xB
xlB
x8B
x8B
x8B
xlB
xlB
y	B
x�B
	lB
	lB
_B
	�B
�B
�B
�B
uB
B
�B
YB
�B	��B
{B
xB
�B
�B
�B
�B
B
�B	�(B
"B
%B
�B
�B
�B

�B
xB
�B
  B
�B
B
	�B

�B
	�B
	�B

�B
	B

	B

	B

�B

�B
	�B
B
	�B
B
�B
B
JB
DB
VB
PB
�B
�B
�B
B
�B
B
VB
VB
VB
�B
�B
�B
.B
�B
�B
bB
�B
B
 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  B
DB
�B
	1B
	wB

�B
OB
B
�B
�B

9B

4B

�B
�B
B
NB
�B
�B
CB
�B
cB
{B
nB
JB
 %B
&B
-�B
5B
?B
N�B
U8B
V�B
XHB
YB
Y�B
[�B
^B
c�B
j�B
iIB
gB
d�B
b�B
_dB
W�B
U�B
O�B
J/B
AB
=�B
=B
<UB
;�B
;&B
:�B
9�B
3�B
=�B
KB
Q�B
TB
:qB
7�B
:gB
TB
b%B
�CB
��B
��B
��B
�HBzB�+B�SB��B�B�B��B�
B}�Br�Bh�Bh�Bi�By�B�Bz5BviBkBf+Ba]Bg-BhxBN�BH�BJ�B7�B$�BpB6B B
��B
��B
�KB
��B
�dB
��B
�B
��B
a�B
8�B
#]B
�B	��B	ϚB	�B	��B	�B	o.B	k�B	P�B	= B	2�B	4B	8B	6�B	?�B	G-B	PHB	fTB	pB	rB	uCB	x+B	yKB	{�B	��B	�BB	�B	��B	�IB	�B	� B	�sB	��B	�AB	��B	��B	�qB	��B	��B
�B
�B
LB
�B
�B
�B
�B
-�B
/DB
3�B
7�B
1hB
-�B
/MB
6�B
2B
<{B
<�B
9FB
5WB
6NB
:�B
<�B
>(B
=�B
?�B
CYB
D\B
B�B
@�B
CB
?B
9�B
;5B
:LB
;�B
<�B
>DB
?�B
?�B
?�B
?�B
?�B
@hB
K�B
L�B
IYB
I�B
I�B
J(B
L�B
M�B
MAB
L�B
L�B
M�B
L#B
JoB
K�B
N�B
M�B
M�B
MpB
M�B
N�B
MTB
K�B
FnB
C�B
@YB
@\B
<�B
<�B
=gB
>�B
EB
G�B
HB
L�B
MwB
K�B
KB
KkB
N�B
R'B
P�B
KwB
G�B
ENB
D,B
A�B
@�B
?B
>'B
;.B
:{B
7�B
2(B
1�B
/mB
'�B
$mB
!�B
B
1B
BB
�B
�B
xB
B
�B
oB
dB
�B
7B
�B
fB

�B
�B
�B
NB
�B
�B
�B
eB	��B
�B
�B
uB
'B
�B
�B
�B
�B
qB
xB
_B

�B
	"B
WB	�AB	��B	�B	�~B	�7B	�yB	�tB	�B	��B	��B	��B	�B	�3B	�GB	�~B	�B	�AB	�*B	��B	��B	�%B	��B	�B	�gB	�:B	��B	��B	��B	��B	��B	�cB	��B	�CB	�CB	��B	�%B	��B	��B	�8B	��B	��B	��B	��B	��B	�8B	� B	��B	�B	��B
tB
BB
�B	��B
dB
�B
�B

B
�B
�B
�B
�B
JB
qB
�B
B

�B

B

fB
	�B

�B
�B
�B
�B
	lB
	�B
vB
gB
�B
�B
�B
B
�B
AB
�B
�B
	(B
	B
ZB
/B
{B
{B
<B
bB
B
:B
�B
pB
JB
#B
�B
3B
�B
YB
B
2B
UB
B
*B
�B
�B
�B
B
rB
�B
�B
	1B
	�B
	@B
	\B
�B
�B
	B

�B

-B
	�B

FB
	B
�B

�B

�B
yB
�B
qB
�B
HB
�B
�B
[B
�B
�B
QB
6B
�B
�B
�B
�B
�B
`B
B
B
�B
DB
|B
MB
�B
yB
�B
sB
�B
�B
B
CB
B
�B
B
�B
�B
7B
�B
B
iB
B
�B
B
�B
�B
�B
�B
�B
~B
&B
:B
�B
4B
�B
	B
�B
{B
�B
�B
*B
GB
�B
�B
�B
�B
�B
�B
�B
�B
dB
B
4B
YB
PB
B
cB
�B
!B
 XB
 �B
 �B
 <B
 )B
 %B
�B
:B
�B
SB
�B
wB
�B
 =B
 �B
!B
!�B
!�B
!�B
"�B
$]B
#�B
#�B
#�B
$oB
$�B
$�B
$�B
$�B
$�B
$KB
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%UB
$�B
$�B
%yB
$�B
%"B
%�B
&B
%B
%B
%IB
%�B
%�B
%�B
&�B
'GB
'�B
'�B
'�B
(9B
(,B
(�B
(�B
*	B
)�B
*B
)�B
*bB
*wB
*WB
*rB
*�B
+�B
,B
+�B
+~B
,B
,�B
,JB
,/B
-AB
.�B
.=B
.�B
/�B
0~B
0<B
/�B
0�B
1QB
1B
0�B
1B
1�B
1�B
2]B
2�B
2pB
2XB
2�B
2�B
2�B
30B
3B
3yB
4PB
5�B
5�B
5�B
5tB
5[B
6-B
6�B
6�B
7B
7<B
7�B
7�B
7�B
7�B
8�B
9MB
9�B
:!B
:4B
:�B
:>B
:�B
;4B
<B
;�B
;�B
;�B
;�B
;�B
<�B
=fB
=�B
=�B
=iB
=�B
>B
>mB
>aB
>YB
>B
=�B
>�B
>�B
?�B
@�B
@B
@#B
@�B
@@B
@�B
@�B
@�B
@)B
@�B
B B
B"B
A�B
BB
B�B
B�B
C�B
C�B
C�B
CxB
CVB
C�B
C�B
DkB
D�B
D�B
D�B
EB
EnB
E_B
E>B
E�B
E�B
E�B
F]B
FaB
F7B
FkB
F�B
GB
GB
F�B
GcB
G^B
G�B
H"B
HwB
HQB
HqB
H@B
H�B
IJB
J"B
KB
J�B
J�B
J�B
K0B
LAB
K�B
LHB
LB
K�B
L^B
L�B
MrB
M"B
MbB
N	B
N�B
NuB
NaB
N�B
O7B
O�B
O�B
PB
O�B
PBB
P�B
P�B
P�B
Q B
QZB
Q�B
RB
RnB
Q�B
RB
RZB
RB
R.B
R�B
SB
S�B
R�B
S(B
S>B
SB
S?B
T$B
S�B
S�B
S�B
S�B
S�B
ThB
T�B
T�B
U_B
V�B
V�B
V�B
VvB
V�B
V�B
W�B
X�B
W�B
X B
XB
X	B
XB
XCB
Y	B
X�B
X�B
Y*B
YNB
YdB
YwB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z;B
Z�B
[%B
ZoB
Z�B
[YB
[�B
[�B
]:B
\B
\FB
],B
]�B
]�B
]�B
]�B
]�B
^.B
^�B
^�B
_QB
_wB
_UB
_iB
_wB
_�B
_�B
_�B
`bB
a�B
bB
b�B
b�B
b�B
b�B
c#B
c�B
c�B
c�B
dkB
dFB
eAB
d�B
d�B
d�B
e/B
e�B
fB
e�B
e�B
eyB
e�B
fHB
fMB
f/B
f{B
fKB
fLB
fZB
f~B
f�B
fpB
gB
g�B
g<B
f�B
f�B
g�B
g�B
g�B
g�B
hWB
i�B
jWB
i�B
i�B
j=B
jrB
j�B
kB
kB
k"B
j�B
kWB
k�B
lB
k�B
lJB
mB
m[B
m B
mGB
mB
m�B
nqB
n�B
o\B
o,B
n�B
n�B
n�B
o%B
oB
n�B
n�B
o!B
oB
o�B
p�B
p�B
p�B
qB
p�B
p�B
qLB
q�B
r�B
r�B
s(B
sQB
s�B
s�B
s�B
tiB
tpB
t�B
uB
u"B
u|B
u�B
u�B
v&B
v9B
v�B
w�B
w�B
w�B
xB
x;B
xB
x\B
x>B
xLB
x:B
xaB
x�B
y	G�O�B
	lB
	lB
_B
	�B
�B
�B
�B
uB
B
�B
YB
�B	��B
{B
xB
�B
�B
�B
�B
B
�B	�(B
"B
%B
�B
�B
�B

�B
xB
�B
  B
�B
B
	�B

�B
	�B
	�B

�B
	B

	B

	B

�B

�B
	�B
B
	�B
B
�B
B
JB
DB
VB
PB
�B
�B
�B
B
�B
B
VB
VB
VB
�B
�B
�B
.B
�B
�B
bB
�B
B
 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:�<.�A<K�l<b�w<#�
<#�
<#�
<#�
<#�
<)��<#�
<#�
<#�
<#�
<#�
<u��<2�8<JĈ<L��<Vp\<#�
<#�
<#�
<#�
<#�
<#�
<#�
<`F
<C
'<#�
<(h�<#�
<#�
<#�
<m��<@E�<#�
<#�
<`�"<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-o�<#�
<#�
<#�
<#�
<�z<|��<#�
<&�<d��<��<}d<#�
<I�{<#�
<#�
<?K$<#�
<#�
<#�
<#�
<2�n<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021063003255920210630032559IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021071007010020210710070100QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021071007010020210710070100QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364820220422133648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064420220426160644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                