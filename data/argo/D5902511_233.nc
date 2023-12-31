CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-10-16T19:03:05Z creation; 2023-02-10T23:09:45Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 >T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 eP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20221016190305  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_233                 6810_008521_233                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���u��"@���u��"11  @����d��@����d��@2��}�3@2��}�3�d�//�t �d�//�t 11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@�  @�G�@�  @�  @�p�A  A ��A,(�AAG�A_\)A~�RA�\)A��A�Q�A�Q�A�  A�  A�  B (�B  B  B(�B   B(  B0  B7�
B?�BH  BPQ�BX  B_�
Bg�Bo�
Bw�
B�B�  B�{B�  B�  B�  B�  B��B�{B�{B�(�B�{B�  B��
B�  B�  B�  B�  B�  B�  B��B�  B�  B�{B�{B��B��B��B�  B�{B�(�B��C   C  C  C  C��C
  C  C
=C  C
=C
=C�C�C��C��C��C   C!��C$  C&  C(  C*
=C,  C-��C0{C2
=C4  C5��C7��C:  C;��C>
=C@{CB  CD  CF  CG�CI��CL
=CN
=CP  CR  CS��CV  CX
=CZ  C\  C^
=C`
=Cb  Cd  Cf  Cg��Cj
=Cl
=Cm�Co��Cq�Cs��Cv  Cx
=Cz
=C|  C~  C�C�  C���C�  C�C�  C�  C���C�  C���C���C�C�C�  C���C�  C�  C�C�
=C�C�
=C�  C�  C�  C�  C�  C�  C�C�  C�C�  C���C�  C���C�  C�  C�C�C�
=C�C�  C���C���C�  C���C���C�  C�
=C�
=C�C�C�
=C�C�  C�C�C�  C�  C�  C�  C�  C�C�  C�C�C�  C�  C���C���C�  C�  C�C�
=C�C�C�
=C���C���C�  C���C�  C���C���C�C�
=C�C�
=C�  C�C�  C���C���C���C���C�  C�  C�C�  C�  C�  C�
=C�C�C�
=C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�
=C�  C���C�  C���C���C�  C���C���C���C���C�C�
=C�
=D   D � D  D}qD�qD� D  D}qD  D}qD  D��D�qD� DD�D  D}qD	  D	��D
�D
}qD
��D� D  D��D�D�D  D}qD  D�D  D}qD  D�DD��D  D��D  Dz�D�qD}qD�D� D��Dz�D�qDz�D��D}qD  D}qD  D� D�qD� DD� D  D��D�qD}qD �D � D!�D!��D"  D"� D#�D#� D#�qD$z�D%  D%� D%��D&z�D&��D'}qD'�qD(}qD)  D)� D*  D*��D+  D+� D,D,��D-�D-� D.�D.�D.�qD/}qD0�D0� D0�qD1}qD2  D2z�D2��D3��D4  D4}qD4�qD5��D6  D6}qD7�D7��D7�qD8� D9  D9� D:  D:� D;  D;� D<  D<}qD<�qD=z�D=�qD>� D?  D?��D@�D@��DA  DAz�DA�qDB� DB�qDC}qDDDD� DE  DE� DE�RDF}qDG  DG��DG�qDH}qDI�DI� DJ  DJ� DK  DK}qDL  DL��DL�qDM� DN�DN}qDN�qDO}qDO�qDP}qDQ  DQ� DQ��DR}qDSDS��DT�DT}qDU  DU��DV  DVz�DW  DW� DX  DX�DYDY}qDY�qDZ}qDZ��D[z�D[�qD\� D]D]�D^D^��D_�D_� D`  D`� Da  Da�Db  Db}qDc  Dc��Dd�Dd��DeDe�Df�Df}qDg  Dg� Dg�qDh}qDh�qDi� DjDj��Dk  Dk��Dl  Dl��Dm�Dm��Dn�Dn� Do  Do� Dp  Dp��Dq�Dq� Dq�qDr�DsDs� Ds�qDt� Dt�qDu� DvDv��Dw�Dw}qDx  Dx� Dy  Dy� Dz  Dz��D{�D{� D|�D|� D}  D}}qD~  D~��D  D� D�qD�>�D��HD�� D�  D�@ D�� D��HD�  D�AHD��HD�� D���D�>�D�� D��HD�  D�@ D��HD�D�  D�@ D�� D���D�HD�@ D�}qD���D�  D�@ D�~�D���D���D�@ D���D���D���D�>�D�~�D�� D���D�@ D��HD�� D���D�@ D��HD��HD�HD�AHD�~�D���D���D�>�D�~�D�� D��D�AHD�~�D���D�  D�@ D�� D�� D�  D�AHD��HD�� D���D�>�D�}qD�� D�HD�@ D�� D�� D�  D�AHD���D�� D�  D�@ D�~�D���D���D�@ D�� D���D���D�>�D�� D���D��qD�>�D�~�D���D�  D�>�D�~�D���D�HD�AHD�� D���D���D�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�~�D���D�HD�B�D��HD��HD�HD�AHD�� D�� D�HD�AHD�� D�� D���D�@ D���D��HD�  D�>�D�~�D���D���D�@ D�}qD�� D�  D�>�D�~�D���D��qD�<)D�|)D���D�  D�@ D�~�D���D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�~�D���D�HD�AHD�~�D���D���D�@ D�� D���D�  D�>�D�� D��HD�HD�@ D�� D��HD�  D�AHD���D��HD���D�>�D�~�D�� D��D�B�D��HD��HD�  D�@ D�~�D�� D�  D�AHD��HD���D���D�@ D�� D���D���D�@ D�� D���D�  D�@ D�~�D���D�HD�AHD���D��HD���D�@ D��HD��HD�HD�AHD��HD�� D���D�>�D�� D�� D�HD�@ D�� D�D�HD�AHD��HD���D�  D�AHD�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�>�D�� D��HD�  D�>�D�~�D��qD��qD�>�D��HD���D�  D�B�D D¾�D�  D�@ DÀ Dþ�D�  D�@ DāHD��HD�HD�AHD�~�D�� D�HD�AHDƀ D�� D�  D�>�D�~�D��HD�  D�@ DȀ D�� D��qD�>�Dɀ Dɾ�D��qD�=qD�}qDʾ�D�  D�AHDˁHD��HD��D�AHD̀ D�� D���D�@ D�}qD;�D�  D�AHD�~�Dξ�D�  D�AHDς�D�� D��qD�>�DЁHD��HD�  D�AHDсHD�D�HD�@ DҀ D�� D���D�>�DӀ D�� D���D�=qDԀ D��HD�  D�@ DՁHD�� D�  D�>�DցHD���D�HD�>�D׀ D�� D�  D�>�D�~�D�� D�HD�B�Dـ D��HD�  D�@ Dڀ DڽqD���D�>�DہHD�D�HD�AHD�~�D�� D�HD�@ D݀ Dݾ�D�  D�B�DށHD�� D�  D�@ D߀ D߾�D���D�@ D��HD�� D���D�@ D�}qDᾸD�  D�@ D� D��HD��D�AHD�HD��HD���D�=qD� D��HD�HD�AHD� D�� D�  D�AHD悏D��HD�  D�@ D�HD�D�HD�@ D� D��HD���D�=qD�~�D龸D��qD�>�D�~�D�qD���D�AHD�HD�� D�  D�@ D� D��HD�HD�>�D� D��HD�  D�>�D� D�� D��qD�=qD�|)DﾸD�  D�>�D�~�D�D���D�>�D�HD�D�HD�AHD� D��HD�  D�AHD� D�qD��qD�@ D�HD���D�  D�AHD�~�D��)D��qD�@ D���D�D�  D�>�D�~�D��qD��>�Q�>���?8Q�?��?��R?�
=?�@��@#�
@8Q�@O\)@c�
@u@�=q@�33@�p�@�=q@��@��R@Ǯ@У�@޸R@�@��@�p�AG�AQ�A�A�A�A=qA\)A#33A)��A,��A333A5A:�HA>{AC�
AG
=AL(�AO\)AU�AX��A\(�Ab�\AeAj�HAp��As�
Az=qA\)A���A���A�
=A���A�z�A�ffA��A��
A��RA��A�(�A�
=A��A�(�A�\)A���A��A�\)A�=qA�p�A�\)A��HA��A�Q�A�33A�p�A�G�A�33AθRA���A�(�A�
=Aٙ�A��A�
=A�=qA���A�
=A�=qA�(�A�A�A���A��A��A�p�A��B�B�RB�B��B�\B  B	p�B
�RBz�Bp�B
=Bz�Bp�B33BQ�B��B33B(�B�B33Bz�B{B33B ��B"=qB#\)B%G�B&{B'�B)�B*{B+�B,��B.=qB/�B0��B2ffB3�B5�B6ffB7\)B8��B:ffB;\)B<��B=�B?\)B@��BABC33BD��BE��BG33BH(�BIBJ�HBK�
BM��BNffBP(�BQ�BR�\BS�
BT��BV�\BW�BX��BZ=qB[�B\��B]�B_�B`��BaBc�Bd��BeBg�Bh��BiBk�Blz�BmBo\)Bp(�Br{Bs33Btz�Bv{Bv�HBx��By��B{33B|Q�B}B33B�{B��HB��B�{B���B�p�B�(�B���B��B�{B���B��B�{B���B���B�=qB�
=B���B�z�B�
=B��B��\B��B��B�z�B�G�B�B��\B�33B�B���B�33B��B���B�33B�{B���B�G�B�{B��\B�p�B��B���B�G�B��
B���B�G�B�B�Q�B��B��B�{B��RB��B�B�Q�B���B�33B�B�  B���B���B�G�B�B�=qB�ffB���B�
=B��B�B�  B��\B��RB�
=B��B�B�{B�z�B���B��B��B�B��B�z�B��RB��HB�p�B��B�B�=qB�ffB��RB�33B�\)B���B�{B�(�B�z�B���B��B�\)B��
B�  B�Q�B���B�
=B�G�B�B�  B�(�B��\B���B��B�\)B��
B�  B�Q�B��RB��HB�33B���B�B�(�B�z�B���B�
=B�G�B��B��B�=qB�Q�B��RB��B�G�B��B�  B�=qB�ffB��HB�33B�G�B�B�{B�Q�B���B��B�p�B��B�  B�ffB�z�B���B�33B�\)B��B�(�B�ffB���B��B�G�BŮB�{B�=qBƣ�B�
=B�33B�p�B�  B�Q�B�z�B���B�G�Bə�B��
B�{Bʣ�B���B�
=B˙�B��B�=qB�ffB��HB�G�B�p�B��B�Q�B�z�B��HB�\)Bϙ�B��
B�Q�B���B��B�\)BѮB�Q�B�z�B��HB�p�B�B�  Bԏ\B��HB�33B�B�  B�Q�BָRB�G�B�p�B��B�ffBظRB���BمB��B�(�BڸRB�
=B�\)B��B�=qB܏\B�
=B݅B��
B�=qB���B��B�\)B��
B�Q�B��\B��HB�p�B��
B�(�B�z�B���B�p�B�B�{B�\B���B�33B�B�(�B�Q�B���B�\)B�B��B�ffB��HB�33B�B�(�B�z�B���B��B�B�{B�Q�B���B�G�B홚B�{B�z�B���B�p�B�B�  B��\B�
=B�G�B�B�=qB�\B��HB�\)B��B�(�B�z�B��B��B�B�=qB���B�
=B�p�B�  B�Q�B���B�33B���B��B�=qB��HB�G�B��B�  B��\B��HB�33B�B�Q�B��\B��HB�p�B��C {C G�C �\C �RC �
C�C\)C�C��C�HC(�CG�Cp�C�RC��C
=CQ�C�\C�C�HC(�C=qC�CC�HC�CQ�Cz�C�C��C{CQ�C�\C��C�C(�CG�Cp�C�RC�HC{C\)Cz�C�C��C	�C	Q�C	��C	�RC	�C
33C
Q�C
�\C
��C
�C33Cp�C�\C�
C
=C33Cp�C��C��C
=CG�C�CC�C{C\)C��C�RC��C=qCp�C��C�
C{C33C�C�RC�HC(�Cp�C��CC
=CG�CffC�RC�C{CffC��C��C  CG�Cp�C��C��C�CG�C��CC�C=qCffC��C�HC{C=qC�CC�HC(�C\)C�CC  CG�Cp�C��C�
C�CQ�Cz�C�RC  C(�CQ�C��C�
C  C33CffC��C�C�CG�Cz�C�RC  C33CffC��CC
=CQ�C�C�RC�HC (�C p�C �\C C!
=C!G�C!z�C!��C!�C"33C"Q�C"�\C"�
C#  C#(�C#ffC#��C#�HC$�C$Q�C$z�C$�C$�
C%{C%Q�C%�\C%C%�C&{C&\)C&��C&�
C'
=C'33C'p�C'�C'��C(=qC(p�C(�C(�
C)
=C)=qC)�C)C*  C*G�C*�C*��C*�
C+�C+\)C+��C+�HC,
=C,=qC,z�C,C-  C-33C-ffC-��C-��C.{C.\)C.��C.C.��C/�C/\)C/��C/�HC0�C0\)C0�\C0C0�C1�C1\)C1��C1�
C2{C2=qC2ffC2��C2�
C3{C3Q�C3�C3�C3�C4�C4Q�C4�\C4�
C5{C5G�C5p�C5��C5�
C6{C6\)C6��C6��C7
=C7=qC7ffC7��C7�
C8�C8Q�C8�\C8�
C9
=C933C9p�C9��C9�
C:
=C:Q�C:�\C:C;  C;(�C;\)C;�C;�RC;��C<=qC<z�C<�RC<��C=33C=ffC=��C=�HC>{C>G�C>z�C>�C>�HC?{C?Q�C?��C?�
C@{C@Q�C@�C@�RC@�HCA{CAG�CA�CA�RCA�CB�CB\)CB��CB�
CC{CCQ�CC�CC�CC�HCD
=CDG�CDz�CD�CD�CE(�CEffCE��CE�CF(�CFffCF��CF��CG{CG=qCGz�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            ?�  @   @=p�@�  @�G�@�  @�  @�p�A  A ��A,(�AAG�A_\)A~�RA�\)A��A�Q�A�Q�A�  A�  A�  B (�B  B  B(�B   B(  B0  B7�
B?�BH  BPQ�BX  B_�
Bg�Bo�
Bw�
B�B�  B�{B�  B�  B�  B�  B��B�{B�{B�(�B�{B�  B��
B�  B�  B�  B�  B�  B�  B��B�  B�  B�{B�{B��B��B��B�  B�{B�(�B��C   C  C  C  C��C
  C  C
=C  C
=C
=C�C�C��C��C��C   C!��C$  C&  C(  C*
=C,  C-��C0{C2
=C4  C5��C7��C:  C;��C>
=C@{CB  CD  CF  CG�CI��CL
=CN
=CP  CR  CS��CV  CX
=CZ  C\  C^
=C`
=Cb  Cd  Cf  Cg��Cj
=Cl
=Cm�Co��Cq�Cs��Cv  Cx
=Cz
=C|  C~  C�C�  C���C�  C�C�  C�  C���C�  C���C���C�C�C�  C���C�  C�  C�C�
=C�C�
=C�  C�  C�  C�  C�  C�  C�C�  C�C�  C���C�  C���C�  C�  C�C�C�
=C�C�  C���C���C�  C���C���C�  C�
=C�
=C�C�C�
=C�C�  C�C�C�  C�  C�  C�  C�  C�C�  C�C�C�  C�  C���C���C�  C�  C�C�
=C�C�C�
=C���C���C�  C���C�  C���C���C�C�
=C�C�
=C�  C�C�  C���C���C���C���C�  C�  C�C�  C�  C�  C�
=C�C�C�
=C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�
=C�  C���C�  C���C���C�  C���C���C���C���C�C�
=C�
=D   D � D  D}qD�qD� D  D}qD  D}qD  D��D�qD� DD�D  D}qD	  D	��D
�D
}qD
��D� D  D��D�D�D  D}qD  D�D  D}qD  D�DD��D  D��D  Dz�D�qD}qD�D� D��Dz�D�qDz�D��D}qD  D}qD  D� D�qD� DD� D  D��D�qD}qD �D � D!�D!��D"  D"� D#�D#� D#�qD$z�D%  D%� D%��D&z�D&��D'}qD'�qD(}qD)  D)� D*  D*��D+  D+� D,D,��D-�D-� D.�D.�D.�qD/}qD0�D0� D0�qD1}qD2  D2z�D2��D3��D4  D4}qD4�qD5��D6  D6}qD7�D7��D7�qD8� D9  D9� D:  D:� D;  D;� D<  D<}qD<�qD=z�D=�qD>� D?  D?��D@�D@��DA  DAz�DA�qDB� DB�qDC}qDDDD� DE  DE� DE�RDF}qDG  DG��DG�qDH}qDI�DI� DJ  DJ� DK  DK}qDL  DL��DL�qDM� DN�DN}qDN�qDO}qDO�qDP}qDQ  DQ� DQ��DR}qDSDS��DT�DT}qDU  DU��DV  DVz�DW  DW� DX  DX�DYDY}qDY�qDZ}qDZ��D[z�D[�qD\� D]D]�D^D^��D_�D_� D`  D`� Da  Da�Db  Db}qDc  Dc��Dd�Dd��DeDe�Df�Df}qDg  Dg� Dg�qDh}qDh�qDi� DjDj��Dk  Dk��Dl  Dl��Dm�Dm��Dn�Dn� Do  Do� Dp  Dp��Dq�Dq� Dq�qDr�DsDs� Ds�qDt� Dt�qDu� DvDv��Dw�Dw}qDx  Dx� Dy  Dy� Dz  Dz��D{�D{� D|�D|� D}  D}}qD~  D~��D  D� D�qD�>�D��HD�� D�  D�@ D�� D��HD�  D�AHD��HD�� D���D�>�D�� D��HD�  D�@ D��HD�D�  D�@ D�� D���D�HD�@ D�}qD���D�  D�@ D�~�D���D���D�@ D���D���D���D�>�D�~�D�� D���D�@ D��HD�� D���D�@ D��HD��HD�HD�AHD�~�D���D���D�>�D�~�D�� D��D�AHD�~�D���D�  D�@ D�� D�� D�  D�AHD��HD�� D���D�>�D�}qD�� D�HD�@ D�� D�� D�  D�AHD���D�� D�  D�@ D�~�D���D���D�@ D�� D���D���D�>�D�� D���D��qD�>�D�~�D���D�  D�>�D�~�D���D�HD�AHD�� D���D���D�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�~�D���D�HD�B�D��HD��HD�HD�AHD�� D�� D�HD�AHD�� D�� D���D�@ D���D��HD�  D�>�D�~�D���D���D�@ D�}qD�� D�  D�>�D�~�D���D��qD�<)D�|)D���D�  D�@ D�~�D���D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�~�D���D�HD�AHD�~�D���D���D�@ D�� D���D�  D�>�D�� D��HD�HD�@ D�� D��HD�  D�AHD���D��HD���D�>�D�~�D�� D��D�B�D��HD��HD�  D�@ D�~�D�� D�  D�AHD��HD���D���D�@ D�� D���D���D�@ D�� D���D�  D�@ D�~�D���D�HD�AHD���D��HD���D�@ D��HD��HD�HD�AHD��HD�� D���D�>�D�� D�� D�HD�@ D�� D�D�HD�AHD��HD���D�  D�AHD�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�>�D�� D��HD�  D�>�D�~�D��qD��qD�>�D��HD���D�  D�B�D D¾�D�  D�@ DÀ Dþ�D�  D�@ DāHD��HD�HD�AHD�~�D�� D�HD�AHDƀ D�� D�  D�>�D�~�D��HD�  D�@ DȀ D�� D��qD�>�Dɀ Dɾ�D��qD�=qD�}qDʾ�D�  D�AHDˁHD��HD��D�AHD̀ D�� D���D�@ D�}qD;�D�  D�AHD�~�Dξ�D�  D�AHDς�D�� D��qD�>�DЁHD��HD�  D�AHDсHD�D�HD�@ DҀ D�� D���D�>�DӀ D�� D���D�=qDԀ D��HD�  D�@ DՁHD�� D�  D�>�DցHD���D�HD�>�D׀ D�� D�  D�>�D�~�D�� D�HD�B�Dـ D��HD�  D�@ Dڀ DڽqD���D�>�DہHD�D�HD�AHD�~�D�� D�HD�@ D݀ Dݾ�D�  D�B�DށHD�� D�  D�@ D߀ D߾�D���D�@ D��HD�� D���D�@ D�}qDᾸD�  D�@ D� D��HD��D�AHD�HD��HD���D�=qD� D��HD�HD�AHD� D�� D�  D�AHD悏D��HD�  D�@ D�HD�D�HD�@ D� D��HD���D�=qD�~�D龸D��qD�>�D�~�D�qD���D�AHD�HD�� D�  D�@ D� D��HD�HD�>�D� D��HD�  D�>�D� D�� D��qD�=qD�|)DﾸD�  D�>�D�~�D�D���D�>�D�HD�D�HD�AHD� D��HD�  D�AHD� D�qD��qD�@ D�HD���D�  D�AHD�~�D��)D��qD�@ D���D�D�  D�>�D�~�D��qG�O�>�Q�>���?8Q�?��?��R?�
=?�@��@#�
@8Q�@O\)@c�
@u@�=q@�33@�p�@�=q@��@��R@Ǯ@У�@޸R@�@��@�p�AG�AQ�A�A�A�A=qA\)A#33A)��A,��A333A5A:�HA>{AC�
AG
=AL(�AO\)AU�AX��A\(�Ab�\AeAj�HAp��As�
Az=qA\)A���A���A�
=A���A�z�A�ffA��A��
A��RA��A�(�A�
=A��A�(�A�\)A���A��A�\)A�=qA�p�A�\)A��HA��A�Q�A�33A�p�A�G�A�33AθRA���A�(�A�
=Aٙ�A��A�
=A�=qA���A�
=A�=qA�(�A�A�A���A��A��A�p�A��B�B�RB�B��B�\B  B	p�B
�RBz�Bp�B
=Bz�Bp�B33BQ�B��B33B(�B�B33Bz�B{B33B ��B"=qB#\)B%G�B&{B'�B)�B*{B+�B,��B.=qB/�B0��B2ffB3�B5�B6ffB7\)B8��B:ffB;\)B<��B=�B?\)B@��BABC33BD��BE��BG33BH(�BIBJ�HBK�
BM��BNffBP(�BQ�BR�\BS�
BT��BV�\BW�BX��BZ=qB[�B\��B]�B_�B`��BaBc�Bd��BeBg�Bh��BiBk�Blz�BmBo\)Bp(�Br{Bs33Btz�Bv{Bv�HBx��By��B{33B|Q�B}B33B�{B��HB��B�{B���B�p�B�(�B���B��B�{B���B��B�{B���B���B�=qB�
=B���B�z�B�
=B��B��\B��B��B�z�B�G�B�B��\B�33B�B���B�33B��B���B�33B�{B���B�G�B�{B��\B�p�B��B���B�G�B��
B���B�G�B�B�Q�B��B��B�{B��RB��B�B�Q�B���B�33B�B�  B���B���B�G�B�B�=qB�ffB���B�
=B��B�B�  B��\B��RB�
=B��B�B�{B�z�B���B��B��B�B��B�z�B��RB��HB�p�B��B�B�=qB�ffB��RB�33B�\)B���B�{B�(�B�z�B���B��B�\)B��
B�  B�Q�B���B�
=B�G�B�B�  B�(�B��\B���B��B�\)B��
B�  B�Q�B��RB��HB�33B���B�B�(�B�z�B���B�
=B�G�B��B��B�=qB�Q�B��RB��B�G�B��B�  B�=qB�ffB��HB�33B�G�B�B�{B�Q�B���B��B�p�B��B�  B�ffB�z�B���B�33B�\)B��B�(�B�ffB���B��B�G�BŮB�{B�=qBƣ�B�
=B�33B�p�B�  B�Q�B�z�B���B�G�Bə�B��
B�{Bʣ�B���B�
=B˙�B��B�=qB�ffB��HB�G�B�p�B��B�Q�B�z�B��HB�\)Bϙ�B��
B�Q�B���B��B�\)BѮB�Q�B�z�B��HB�p�B�B�  Bԏ\B��HB�33B�B�  B�Q�BָRB�G�B�p�B��B�ffBظRB���BمB��B�(�BڸRB�
=B�\)B��B�=qB܏\B�
=B݅B��
B�=qB���B��B�\)B��
B�Q�B��\B��HB�p�B��
B�(�B�z�B���B�p�B�B�{B�\B���B�33B�B�(�B�Q�B���B�\)B�B��B�ffB��HB�33B�B�(�B�z�B���B��B�B�{B�Q�B���B�G�B홚B�{B�z�B���B�p�B�B�  B��\B�
=B�G�B�B�=qB�\B��HB�\)B��B�(�B�z�B��B��B�B�=qB���B�
=B�p�B�  B�Q�B���B�33B���B��B�=qB��HB�G�B��B�  B��\B��HB�33B�B�Q�B��\B��HB�p�B��C {C G�C �\C �RC �
C�C\)C�C��C�HC(�CG�Cp�C�RC��C
=CQ�C�\C�C�HC(�C=qC�CC�HC�CQ�Cz�C�C��C{CQ�C�\C��C�C(�CG�Cp�C�RC�HC{C\)Cz�C�C��C	�C	Q�C	��C	�RC	�C
33C
Q�C
�\C
��C
�C33Cp�C�\C�
C
=C33Cp�C��C��C
=CG�C�CC�C{C\)C��C�RC��C=qCp�C��C�
C{C33C�C�RC�HC(�Cp�C��CC
=CG�CffC�RC�C{CffC��C��C  CG�Cp�C��C��C�CG�C��CC�C=qCffC��C�HC{C=qC�CC�HC(�C\)C�CC  CG�Cp�C��C�
C�CQ�Cz�C�RC  C(�CQ�C��C�
C  C33CffC��C�C�CG�Cz�C�RC  C33CffC��CC
=CQ�C�C�RC�HC (�C p�C �\C C!
=C!G�C!z�C!��C!�C"33C"Q�C"�\C"�
C#  C#(�C#ffC#��C#�HC$�C$Q�C$z�C$�C$�
C%{C%Q�C%�\C%C%�C&{C&\)C&��C&�
C'
=C'33C'p�C'�C'��C(=qC(p�C(�C(�
C)
=C)=qC)�C)C*  C*G�C*�C*��C*�
C+�C+\)C+��C+�HC,
=C,=qC,z�C,C-  C-33C-ffC-��C-��C.{C.\)C.��C.C.��C/�C/\)C/��C/�HC0�C0\)C0�\C0C0�C1�C1\)C1��C1�
C2{C2=qC2ffC2��C2�
C3{C3Q�C3�C3�C3�C4�C4Q�C4�\C4�
C5{C5G�C5p�C5��C5�
C6{C6\)C6��C6��C7
=C7=qC7ffC7��C7�
C8�C8Q�C8�\C8�
C9
=C933C9p�C9��C9�
C:
=C:Q�C:�\C:C;  C;(�C;\)C;�C;�RC;��C<=qC<z�C<�RC<��C=33C=ffC=��C=�HC>{C>G�C>z�C>�C>�HC?{C?Q�C?��C?�
C@{C@Q�C@�C@�RC@�HCA{CAG�CA�CA�RCA�CB�CB\)CB��CB�
CC{CCQ�CC�CC�CC�HCD
=CDG�CDz�CD�CD�CE(�CEffCE��CE�CF(�CFffCF��CF��CG{CG=qCGz�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�-A�5?A�33A�33A�33A�33A�/A�/A�1'A�-A�/A�33A�7LA�;dA�=qA�?}A�?}A�=qA�=qA�?}A�A�A�A�A�A�A�A�A�?}A�A�A�5?A��A��A��A��A�hsA��A�(�A�1'A׋DA�ȴA���A�p�AҰ!A��A�I�A��A�{A�x�Ạ�A��mA�JAɩ�A���A��A��TA�
=A�l�A���A�+A��
A�G�A���A��mA�x�A�?}A�oA�JA�I�A�-A�K�A���A�bA���A�M�A�
=A�{A�;dA���A��A��wA��mA�  A�|�A�VA�1A�VA��A�
=A��-A���A��wA�z�A��\A��HA�=qA�VA�5?A��HA���A��PA���A��A�%A�;dA���A��PA��jA���A��;A�~�A�+A���A��hA��A��#A���A�A�t�A��hA���A��DA��wA���A�r�A�/A��A�oA��A�ȴA��
A���A�+Ayl�AxjAw?}As�TAm�Aj�/Ai�
Ai%Ah�Ah�+Aep�A]��A\E�AY��AW�#AU�TAS?}APȴAN-AM
=AI&�AF��AC�hA>�A<E�A;7LA9�FA7|�A5�A5&�A4��A4-A3��A3K�A2A�A0��A/��A.�+A,��A,^5A+�wA*�HA)p�A'�A&{A$ĜA$1A!�7A �A =qA�A&�A��A��A�Ax�A��AbNA�A�/Ap�A�#A��AE�Ax�A��AVAAƨA��Ap�AVAr�A�hA�+A�-A�A7LA
=A�A-A  AA�A
z�A	�A	hsA	VA	�A	/AȴA�AA��AVA�AAȴAZAC�A�A �\A   @��+@�@��^@�7L@��@��@�K�@� �@�ȴ@�@�ƨ@��H@�@�7L@��9@��@�p�@�O�@��
@�z�@��@�"�@���@�7@��`@��@�5?@�;d@�K�@�  @�o@旍@�@�@�+@��@�^@�@�Q�@�E�@��u@�1'@�"�@ޟ�@��@�O�@ܼj@�&�@��@ܴ9@��;@�%@�hs@��`@�bN@�K�@�M�@�7L@���@�j@׾w@�v�@�`B@�S�@���@�9X@���@�l�@�=q@͉7@���@��T@ͺ^@�p�@̃@�Z@��@�S�@��y@ʧ�@�M�@���@ɡ�@ɑh@�p�@�&�@�9X@Ǯ@�dZ@��@�v�@ŉ7@���@ļj@�b@�ƨ@��
@ÍP@�33@°!@�=q@���@�G�@���@��@�I�@��@�  @��@��w@�;d@�+@��@���@���@�M�@�=q@�^5@�E�@��@��-@�X@�A�@��w@�t�@�C�@��R@��#@���@��@�7L@��@��9@���@���@�v�@�{@��^@���@��@���@���@��@�Z@�A�@�A�@�9X@�(�@�  @�;d@�E�@�J@���@���@���@�/@���@� �@���@���@���@�;d@�o@���@�v�@�V@�@�`B@��@���@��D@�A�@�1@��;@�t�@�;d@��@�n�@�{@�@��7@�%@���@��D@�  @�dZ@��@��\@�n�@�v�@�M�@���@��@�Ĝ@�z�@�b@��w@�t�@�;d@���@�ȴ@�E�@��T@���@���@�p�@���@��m@�S�@�o@��H@���@���@�v�@�=q@�@���@�hs@�7L@�&�@�%@��/@��@��9@��@� �@���@�;d@���@�ȴ@�M�@�@��#@�p�@��@��@�z�@��
@��@�K�@��@���@�~�@�5?@�-@��T@���@���@���@�O�@�7L@�?}@�7L@�7L@�V@��D@���@��m@��@���@��w@�o@���@�v�@���@�@�x�@�?}@�/@���@���@�V@��@�7L@���@�z�@��@�ƨ@��@�\)@�;d@��@�J@��T@�-@��@��-@��@���@�I�@�Z@� �@���@�dZ@�o@��@�33@�"�@�
=@��y@��+@�5?@���@���@�x�@�G�@���@�bN@�b@�b@�b@��@���@��@�t�@�"�@��@��\@��T@��h@��h@���@��-@��7@�X@��@�Ĝ@��u@��u@�bN@�(�@��
@���@��P@�S�@��@�"�@���@�5?@���@�x�@�?}@���@���@���@�z�@�Z@�  @~��@~��@~ff@}�@}�@}V@}V@|��@|�j@|��@{��@{@z�\@z-@zJ@y�7@x�`@xA�@w�@wK�@v�y@vE�@u@u�h@uO�@tZ@sƨ@s33@r��@r�\@rM�@r=q@q��@q�@q�#@q&�@p�u@o�;@oK�@n��@n�@n��@nV@m��@m?}@mV@l�@l��@l�j@l�D@l9X@k�
@kS�@j�H@j��@jn�@i��@i��@ihs@i%@h�u@hbN@g��@g|�@fȴ@e�T@e�-@eO�@eV@d�j@d(�@c��@c33@b��@b�\@b�@aG�@`�9@_�@_\)@_
=@^ȴ@^E�@]�@]@]`B@\j@[��@[o@Z~�@Y�@Y�7@Y&�@X�9@X �@W��@WK�@V��@V��@V$�@UO�@Tz�@S�
@SS�@R�@R��@R�\@R^5@Q�@Q��@QX@Q�@P�9@O�@O��@O|�@Ol�@O�@N��@M�T@MV@L�j@LZ@L�@L(�@Kƨ@Kt�@K33@J�@J�!@J-@I�7@H�`@H�u@H�@HbN@Hb@G��@Gl�@GK�@G;d@Fȴ@F$�@E�@EV@D�@D1@Cƨ@C��@Co@B�!@B��@B=q@B�@A��@A��@AX@A%@@�u@@1'@@b@?�@?\)@?+@>��@>�@>��@>E�@=��@=�@<��@<��@<(�@;S�@:�@:��@:�\@:=q@9�#@9��@9X@9%@8Ĝ@8�@8 �@7��@7l�@7�@6�@6�+@6$�@5@5�@5/@5V@4�@4I�@3��@3��@3t�@3dZ@3C�@2�H@2��@2n�@2=q@1��@1��@1�^@1��@1&�@0�`@0�u@0A�@0b@/�@/��@/\)@/;d@.��@.�@.��@.E�@.{@-�@-�T@-�T@-�T@-�T@-@-��@-�@,��@,j@,Z@+��@+�F@+�@+"�@*��@*J@)��@)��@)%@( �@'��@'�@'|�@'+@&��@&�R@&ff@&ff@&V@&$�@&@%��@%�-@%�h@%p�@%/@%�@%V@$��@$Z@$I�@$I�@$I�@$9X@$(�@$1@#�
@#��@#C�@#@"�!@"�\@"M�@"-@!��@!��@ Ĝ@ r�@ b@��@|�@K�@�@
=@��@ȴ@��@v�@E�@@@�h@O�@V@��@�D@Z@9X@1@�m@ƨ@�@C�@@��@�\@^5@M�@=q@�@J@�@�^@��@�7@x�@X@G�@7L@&�@%@��@�9@�u@r�@bN@A�@b@�;@��@|�@;d@��@�y@ȴ@��@�+@ff@{@��@��@�@`B@?}@V@��@��@�@�D@z�@z�@9X@�@1@��@�m@ƨ@��@��@�@dZ@S�@33@"�@o@�H@�\@~�@~�@^5@^5@=q@-@J@�#@�^@X@�@�`@��@�9@�@Q�@ �@b@�;@��@K�@�@��@�@ȴ@��@�+@ff@5?@$�@@�@�T@��@@��@�h@�@p�@/@V@�@�j@�D@z�A��A��A�(�A�(�A�"�A��A�1'A�33A�5?A�1'A�9XA�7LA�1'A�5?A�/A�5?A�5?A�/A�7LA�5?A�33A�7LA�33A�/A�33A�+A�1'A�+A�1'A�-A�1'A�33A�-A�/A�+A�1'A�+A�1'A�(�A�1'A�1'A�1'A�/A�5?A�33A�1'A�7LA�1'A�33A�33A�/A�9XA�?}A�9XA�=qA�;dA�;dA�=qA�9XA�?}A�9XA�;dA�?}A�;dA�?}A�=qA�=qA�?}A�=qA�A�A�=qA�?}A�A�A�=qA�C�A�;dA�?}A�?}A�;dA�A�A�;dA�A�A�;dA�=qA�=qA�;dA�?}A�;dA�?}A�?}A�=qA�A�A�=qA�A�A�;dA�C�A�A�A�?}A�E�A�?}A�A�A�E�A�?}A�E�A�?}A�A�A�C�A�?}A�E�A�?}A�A�A�A�A�=qA�E�A�A�A�A�A�E�A�?}A�C�A�A�A�=qA�C�A�=qA�A�A�A�A�?}A�E�A�?}A�C�A�C�A�?}A�C�A�A�A�9XA�33A�7LA�&�A�$�A��A��A��A�"�A��A�{A��A�{A�bA�"�A��A��A�bA���A�1A���A��TA��
A���Aݏ\A�1'A�  A��/A�t�A� �A���Aۡ�A�p�A�;dA� �A���A�|�A�=qA��A���A��TAټjA١�A�K�A�+A�VA��/Aا�A�ffA�I�A�7LA�"�A��A���A��#A���A׮A�l�A�I�A�33A�1A��A��/Aִ9A֙�A։7AփA�~�A�oAոRA�A�A�33A��TA���A�ƨAԣ�A�VAӶFA��A���AҶFAҙ�A�ffA�K�A� �A�JA���A���A�ĜAэPA�t�A�bNA�I�A�7LA�7LA�oA���A��`A���A���AмjAжFAЁA�?}A���A��A�ȴA�VA���AμjA΍PA�n�A�M�A��`A�^5A�"�A��A���A̡�A�~�A�G�A��#A˝�A�z�A�E�A�"�A���A��/Aʰ!Aʙ�Aʕ�AʓuA�~�A�^5A�I�A�1'A�"�A� �A��A�{A�VA���A��mA���A���A�ƨA�Aɺ^Aɴ9AɶFAɲ-Aɰ!Aɰ!Aɣ�Aɡ�Aɟ�Aɕ�AɍPA�r�A�ZA�=qA�1'A�$�A�1A���A��#AȮAȉ7A�^5A�1AǃA�;dA��A�{A�bA�JA�A�1A���A��A��
A�ĜAƼjAƲ-AƗ�A�t�A�C�A�1'A��A�bA���A��yA��A�AŮAš�Aŏ\A�~�A�n�A�ZA�G�A�=qA�-A��A��A�JA���A��`A��/A���A���AĮAę�Aď\AăAāA�r�A�n�A�jA�\)A�\)A�S�A�K�A�I�A�K�A�C�A�=qA�33A�&�A��A�
=A���A��TA���AþwAöFAÝ�A�x�A�n�A�bNA�XA�XA�G�A�1'A�{A�  A��mA��A¼jA�ZA�/A��A�oA�1A�A���A��A��RA���A�z�A�r�A�S�A�Q�A�O�A�K�A�C�A�A�A�33A�1'A�33A�-A��A��A��A�  A�  A���A��\A�l�A�Q�A�?}A�-A��A�  A��A��`A���A���A���A�ȴA��!A���A��PA�~�A�v�A�t�A�r�A�p�A�n�A�p�A�p�A�l�A�ffA�ffA�bNA�^5A�Q�A�K�A�(�A���A��/A���A�dZA�E�A�C�A�?}A�(�A�  A���A�A��A�XA�/A� �A�oA�bA�A���A���A���A��mA���A���A�~�A�\)A�G�A�?}A�33A�$�A�VA��`A���A���A���A��uA�XA�33A�VA���A��A�ffA�hsA�dZA�Q�A�M�A�O�A�E�A�=qA�=qA�=qA�33A�&�A��A�oA�1A���A���A��HA���A���A�;dA�"�A��A�{A�{A�bA�
=A�A���A��#A���A���A�A��A���A���A��PA�z�A�~�A�v�A�r�A�dZA�VA�C�A�G�A�I�A�E�A�C�A�E�A�7LA�5?A�+A��A��A��A���A���A��A��`A��;A�A��A�r�A�?}A�A��
A��A���A��uA��hA�jA�bNA�dZA�S�A�=qA�+A��A�JA�bA�1A���A���A��TA���A��A�~�A�C�A�+A�{A�%A���A��
A��!A��A�ZA�A�A�5?A�$�A� �A��A�
=A��A��
A�A���A��A�jA�E�A�(�A�"�A�%A�A�A���A���A���A��hA�t�A�9XA�oA�1A���A��A��
A���A��^A��FA���A��+A�t�A�l�A�n�A�bNA�ZA�XA�K�A�=qA�33A�-A� �A��A�  A��^A��!A���A�ffA�oA���A��A��A��mA��HA��HA��HA��jA���A��hA�v�A�\)A�?}A�{A��#A���A���A���A���A��\A�x�A�dZA�`BA�\)A�XA�G�A�7LA� �A�bA�1A�%A���A��A��A��
A��
A���A�A���A�A��A��\A�VA�+A��A�
=A��
A��9A���A�dZA�/A�"�A���A��yA��;A��#A���A�ȴA��RA��A���A��A�jA�A�A���A�ƨA�n�A�"�A���A��yA��/A���A��9A���A��PA��A��A�~�A�z�A�p�A�jA�\)A�K�A�
=A���A��uA�hsA�dZA�^5A�XA�O�A�E�A�A�A�7LA�(�A�+A�&�A�$�A�"�A� �A� �A�"�A�$�A�"�A��A��A�ĜA���A���A�v�A�ZA�A�A�"�A���A��TA���A��^A���A�`BA��A�ȴA���A�|�A�XA�I�A�7LA��A�A��yA��;A�ȴA��A�ZA�=qA�oA��#A��A���A��A�v�A�n�A�XA�A�A�/A��A�%A���A��;A�ĜA��A���A�~�A�dZA�I�A�+A�JA���A��A��/A���A���A�t�A�ZA�C�A�?}A�+A�%A���A��HA�ĜA��uA�^5A�G�A�A�A�=qA�;dA�9XA�7LA�33A�+A� �A��A��A�oA�  A��A��/A��A���A�ĜA���A�?}A��RA���A�v�A�G�A���A��hA�=qA�A��A��9A�|�A�l�A�A�A�(�A��A���A��
A��jA��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            A�$�A�-A�5?A�33A�33A�33A�33A�/A�/A�1'A�-A�/A�33A�7LA�;dA�=qA�?}A�?}A�=qA�=qA�?}A�A�A�A�A�A�A�A�A�?}A�A�A�5?A��A��A��A��A�hsA��A�(�A�1'A׋DA�ȴA���A�p�AҰ!A��A�I�A��A�{A�x�Ạ�A��mA�JAɩ�A���A��A��TA�
=A�l�A���A�+A��
A�G�A���A��mA�x�A�?}A�oA�JA�I�A�-A�K�A���A�bA���A�M�A�
=A�{A�;dA���A��A��wA��mA�  A�|�A�VA�1A�VA��A�
=A��-A���A��wA�z�A��\A��HA�=qA�VA�5?A��HA���A��PA���A��A�%A�;dA���A��PA��jA���A��;A�~�A�+A���A��hA��A��#A���A�A�t�A��hA���A��DA��wA���A�r�A�/A��A�oA��A�ȴA��
A���A�+Ayl�AxjAw?}As�TAm�Aj�/Ai�
Ai%Ah�Ah�+Aep�A]��A\E�AY��AW�#AU�TAS?}APȴAN-AM
=AI&�AF��AC�hA>�A<E�A;7LA9�FA7|�A5�A5&�A4��A4-A3��A3K�A2A�A0��A/��A.�+A,��A,^5A+�wA*�HA)p�A'�A&{A$ĜA$1A!�7A �A =qA�A&�A��A��A�Ax�A��AbNA�A�/Ap�A�#A��AE�Ax�A��AVAAƨA��Ap�AVAr�A�hA�+A�-A�A7LA
=A�A-A  AA�A
z�A	�A	hsA	VA	�A	/AȴA�AA��AVA�AAȴAZAC�A�A �\A   @��+@�@��^@�7L@��@��@�K�@� �@�ȴ@�@�ƨ@��H@�@�7L@��9@��@�p�@�O�@��
@�z�@��@�"�@���@�7@��`@��@�5?@�;d@�K�@�  @�o@旍@�@�@�+@��@�^@�@�Q�@�E�@��u@�1'@�"�@ޟ�@��@�O�@ܼj@�&�@��@ܴ9@��;@�%@�hs@��`@�bN@�K�@�M�@�7L@���@�j@׾w@�v�@�`B@�S�@���@�9X@���@�l�@�=q@͉7@���@��T@ͺ^@�p�@̃@�Z@��@�S�@��y@ʧ�@�M�@���@ɡ�@ɑh@�p�@�&�@�9X@Ǯ@�dZ@��@�v�@ŉ7@���@ļj@�b@�ƨ@��
@ÍP@�33@°!@�=q@���@�G�@���@��@�I�@��@�  @��@��w@�;d@�+@��@���@���@�M�@�=q@�^5@�E�@��@��-@�X@�A�@��w@�t�@�C�@��R@��#@���@��@�7L@��@��9@���@���@�v�@�{@��^@���@��@���@���@��@�Z@�A�@�A�@�9X@�(�@�  @�;d@�E�@�J@���@���@���@�/@���@� �@���@���@���@�;d@�o@���@�v�@�V@�@�`B@��@���@��D@�A�@�1@��;@�t�@�;d@��@�n�@�{@�@��7@�%@���@��D@�  @�dZ@��@��\@�n�@�v�@�M�@���@��@�Ĝ@�z�@�b@��w@�t�@�;d@���@�ȴ@�E�@��T@���@���@�p�@���@��m@�S�@�o@��H@���@���@�v�@�=q@�@���@�hs@�7L@�&�@�%@��/@��@��9@��@� �@���@�;d@���@�ȴ@�M�@�@��#@�p�@��@��@�z�@��
@��@�K�@��@���@�~�@�5?@�-@��T@���@���@���@�O�@�7L@�?}@�7L@�7L@�V@��D@���@��m@��@���@��w@�o@���@�v�@���@�@�x�@�?}@�/@���@���@�V@��@�7L@���@�z�@��@�ƨ@��@�\)@�;d@��@�J@��T@�-@��@��-@��@���@�I�@�Z@� �@���@�dZ@�o@��@�33@�"�@�
=@��y@��+@�5?@���@���@�x�@�G�@���@�bN@�b@�b@�b@��@���@��@�t�@�"�@��@��\@��T@��h@��h@���@��-@��7@�X@��@�Ĝ@��u@��u@�bN@�(�@��
@���@��P@�S�@��@�"�@���@�5?@���@�x�@�?}@���@���@���@�z�@�Z@�  @~��@~��@~ff@}�@}�@}V@}V@|��@|�j@|��@{��@{@z�\@z-@zJ@y�7@x�`@xA�@w�@wK�@v�y@vE�@u@u�h@uO�@tZ@sƨ@s33@r��@r�\@rM�@r=q@q��@q�@q�#@q&�@p�u@o�;@oK�@n��@n�@n��@nV@m��@m?}@mV@l�@l��@l�j@l�D@l9X@k�
@kS�@j�H@j��@jn�@i��@i��@ihs@i%@h�u@hbN@g��@g|�@fȴ@e�T@e�-@eO�@eV@d�j@d(�@c��@c33@b��@b�\@b�@aG�@`�9@_�@_\)@_
=@^ȴ@^E�@]�@]@]`B@\j@[��@[o@Z~�@Y�@Y�7@Y&�@X�9@X �@W��@WK�@V��@V��@V$�@UO�@Tz�@S�
@SS�@R�@R��@R�\@R^5@Q�@Q��@QX@Q�@P�9@O�@O��@O|�@Ol�@O�@N��@M�T@MV@L�j@LZ@L�@L(�@Kƨ@Kt�@K33@J�@J�!@J-@I�7@H�`@H�u@H�@HbN@Hb@G��@Gl�@GK�@G;d@Fȴ@F$�@E�@EV@D�@D1@Cƨ@C��@Co@B�!@B��@B=q@B�@A��@A��@AX@A%@@�u@@1'@@b@?�@?\)@?+@>��@>�@>��@>E�@=��@=�@<��@<��@<(�@;S�@:�@:��@:�\@:=q@9�#@9��@9X@9%@8Ĝ@8�@8 �@7��@7l�@7�@6�@6�+@6$�@5@5�@5/@5V@4�@4I�@3��@3��@3t�@3dZ@3C�@2�H@2��@2n�@2=q@1��@1��@1�^@1��@1&�@0�`@0�u@0A�@0b@/�@/��@/\)@/;d@.��@.�@.��@.E�@.{@-�@-�T@-�T@-�T@-�T@-@-��@-�@,��@,j@,Z@+��@+�F@+�@+"�@*��@*J@)��@)��@)%@( �@'��@'�@'|�@'+@&��@&�R@&ff@&ff@&V@&$�@&@%��@%�-@%�h@%p�@%/@%�@%V@$��@$Z@$I�@$I�@$I�@$9X@$(�@$1@#�
@#��@#C�@#@"�!@"�\@"M�@"-@!��@!��@ Ĝ@ r�@ b@��@|�@K�@�@
=@��@ȴ@��@v�@E�@@@�h@O�@V@��@�D@Z@9X@1@�m@ƨ@�@C�@@��@�\@^5@M�@=q@�@J@�@�^@��@�7@x�@X@G�@7L@&�@%@��@�9@�u@r�@bN@A�@b@�;@��@|�@;d@��@�y@ȴ@��@�+@ff@{@��@��@�@`B@?}@V@��@��@�@�D@z�@z�@9X@�@1@��@�m@ƨ@��@��@�@dZ@S�@33@"�@o@�H@�\@~�@~�@^5@^5@=q@-@J@�#@�^@X@�@�`@��@�9@�@Q�@ �@b@�;@��@K�@�@��@�@ȴ@��@�+@ff@5?@$�@@�@�T@��@@��@�h@�@p�@/@V@�@�j@�DG�O�A��A��A�(�A�(�A�"�A��A�1'A�33A�5?A�1'A�9XA�7LA�1'A�5?A�/A�5?A�5?A�/A�7LA�5?A�33A�7LA�33A�/A�33A�+A�1'A�+A�1'A�-A�1'A�33A�-A�/A�+A�1'A�+A�1'A�(�A�1'A�1'A�1'A�/A�5?A�33A�1'A�7LA�1'A�33A�33A�/A�9XA�?}A�9XA�=qA�;dA�;dA�=qA�9XA�?}A�9XA�;dA�?}A�;dA�?}A�=qA�=qA�?}A�=qA�A�A�=qA�?}A�A�A�=qA�C�A�;dA�?}A�?}A�;dA�A�A�;dA�A�A�;dA�=qA�=qA�;dA�?}A�;dA�?}A�?}A�=qA�A�A�=qA�A�A�;dA�C�A�A�A�?}A�E�A�?}A�A�A�E�A�?}A�E�A�?}A�A�A�C�A�?}A�E�A�?}A�A�A�A�A�=qA�E�A�A�A�A�A�E�A�?}A�C�A�A�A�=qA�C�A�=qA�A�A�A�A�?}A�E�A�?}A�C�A�C�A�?}A�C�A�A�A�9XA�33A�7LA�&�A�$�A��A��A��A�"�A��A�{A��A�{A�bA�"�A��A��A�bA���A�1A���A��TA��
A���Aݏ\A�1'A�  A��/A�t�A� �A���Aۡ�A�p�A�;dA� �A���A�|�A�=qA��A���A��TAټjA١�A�K�A�+A�VA��/Aا�A�ffA�I�A�7LA�"�A��A���A��#A���A׮A�l�A�I�A�33A�1A��A��/Aִ9A֙�A։7AփA�~�A�oAոRA�A�A�33A��TA���A�ƨAԣ�A�VAӶFA��A���AҶFAҙ�A�ffA�K�A� �A�JA���A���A�ĜAэPA�t�A�bNA�I�A�7LA�7LA�oA���A��`A���A���AмjAжFAЁA�?}A���A��A�ȴA�VA���AμjA΍PA�n�A�M�A��`A�^5A�"�A��A���A̡�A�~�A�G�A��#A˝�A�z�A�E�A�"�A���A��/Aʰ!Aʙ�Aʕ�AʓuA�~�A�^5A�I�A�1'A�"�A� �A��A�{A�VA���A��mA���A���A�ƨA�Aɺ^Aɴ9AɶFAɲ-Aɰ!Aɰ!Aɣ�Aɡ�Aɟ�Aɕ�AɍPA�r�A�ZA�=qA�1'A�$�A�1A���A��#AȮAȉ7A�^5A�1AǃA�;dA��A�{A�bA�JA�A�1A���A��A��
A�ĜAƼjAƲ-AƗ�A�t�A�C�A�1'A��A�bA���A��yA��A�AŮAš�Aŏ\A�~�A�n�A�ZA�G�A�=qA�-A��A��A�JA���A��`A��/A���A���AĮAę�Aď\AăAāA�r�A�n�A�jA�\)A�\)A�S�A�K�A�I�A�K�A�C�A�=qA�33A�&�A��A�
=A���A��TA���AþwAöFAÝ�A�x�A�n�A�bNA�XA�XA�G�A�1'A�{A�  A��mA��A¼jA�ZA�/A��A�oA�1A�A���A��A��RA���A�z�A�r�A�S�A�Q�A�O�A�K�A�C�A�A�A�33A�1'A�33A�-A��A��A��A�  A�  A���A��\A�l�A�Q�A�?}A�-A��A�  A��A��`A���A���A���A�ȴA��!A���A��PA�~�A�v�A�t�A�r�A�p�A�n�A�p�A�p�A�l�A�ffA�ffA�bNA�^5A�Q�A�K�A�(�A���A��/A���A�dZA�E�A�C�A�?}A�(�A�  A���A�A��A�XA�/A� �A�oA�bA�A���A���A���A��mA���A���A�~�A�\)A�G�A�?}A�33A�$�A�VA��`A���A���A���A��uA�XA�33A�VA���A��A�ffA�hsA�dZA�Q�A�M�A�O�A�E�A�=qA�=qA�=qA�33A�&�A��A�oA�1A���A���A��HA���A���A�;dA�"�A��A�{A�{A�bA�
=A�A���A��#A���A���A�A��A���A���A��PA�z�A�~�A�v�A�r�A�dZA�VA�C�A�G�A�I�A�E�A�C�A�E�A�7LA�5?A�+A��A��A��A���A���A��A��`A��;A�A��A�r�A�?}A�A��
A��A���A��uA��hA�jA�bNA�dZA�S�A�=qA�+A��A�JA�bA�1A���A���A��TA���A��A�~�A�C�A�+A�{A�%A���A��
A��!A��A�ZA�A�A�5?A�$�A� �A��A�
=A��A��
A�A���A��A�jA�E�A�(�A�"�A�%A�A�A���A���A���A��hA�t�A�9XA�oA�1A���A��A��
A���A��^A��FA���A��+A�t�A�l�A�n�A�bNA�ZA�XA�K�A�=qA�33A�-A� �A��A�  A��^A��!A���A�ffA�oA���A��A��A��mA��HA��HA��HA��jA���A��hA�v�A�\)A�?}A�{A��#A���A���A���A���A��\A�x�A�dZA�`BA�\)A�XA�G�A�7LA� �A�bA�1A�%A���A��A��A��
A��
A���A�A���A�A��A��\A�VA�+A��A�
=A��
A��9A���A�dZA�/A�"�A���A��yA��;A��#A���A�ȴA��RA��A���A��A�jA�A�A���A�ƨA�n�A�"�A���A��yA��/A���A��9A���A��PA��A��A�~�A�z�A�p�A�jA�\)A�K�A�
=A���A��uA�hsA�dZA�^5A�XA�O�A�E�A�A�A�7LA�(�A�+A�&�A�$�A�"�A� �A� �A�"�A�$�A�"�A��A��A�ĜA���A���A�v�A�ZA�A�A�"�A���A��TA���A��^A���A�`BA��A�ȴA���A�|�A�XA�I�A�7LA��A�A��yA��;A�ȴA��A�ZA�=qA�oA��#A��A���A��A�v�A�n�A�XA�A�A�/A��A�%A���A��;A�ĜA��A���A�~�A�dZA�I�A�+A�JA���A��A��/A���A���A�t�A�ZA�C�A�?}A�+A�%A���A��HA�ĜA��uA�^5A�G�A�A�A�=qA�;dA�9XA�7LA�33A�+A� �A��A��A�oA�  A��A��/A��A���A�ĜA���A�?}A��RA���A�v�A�G�A���A��hA�=qA�A��A��9A�|�A�l�A�A�A�(�A��A���A��
A��jA��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B2-B0�B1'B1�B1[B1[B1�B1�B1�B1�B1�B1�B1�B1[B1�B1[B1�B1�B1�B1�B1[B1�B1�B1[B1�B1�B1'B0�B/�B.B+�B,=B�B1B!B!-B*�B,�B0�B6FBB'BH�BYB`�BrGB�1B�BǮBбBӏB��B��B��B�B~BhB�B/�B9�B:^B@�BB'BB'BFBEBGEBJ�BG�BH�BI�BG�BF�BE�BHKBEmBF�BEmBDgBB[BB�B@�B?�B=�B<�B8�B7LB5?B6�B0UB*�B#�B'�BxB�B#�B \B�B%B�PB�ZB�B�fB�B�B�dB��B�=B��B|�Bm)B_pBEB4nB&�B.B
�VB
�B
��B
��B
�UB
��B
�:B
��B
�	B
��B
��B
�uB
��B
o�B
c B
�B
kB
B
�B	�B	�6B	�^B	��B	��B	��B	��B	~�B	q�B	bNB	TaB	O�B	A B	8�B	'�B	#B	�B	�B		7B��B��B�BޞB��B�9B�TB҉B��B�BB��B�pB�KB��B�-B��B��B��B��B�zB��B�-B�'B��B��B��B��B��B�<B�6B��B��B��B�XB�hB��B��B�B�sB��B��B��B� B��B�BBߤB�B�2B�QB�B��B��B�B�"B��B��B	 �B	GB	�B	�B	uB	�B	SB	�B	�B	�B	 \B	+�B	+6B	-�B	/OB	0!B	2�B	0!B	0UB	1�B	/�B	$@B	#�B	($B	%�B	%FB	$@B	"hB	!�B	YB	SB	OB	'�B	HB	E9B	L�B	F�B	A�B	c�B	jB	r�B	yrB	t�B	h�B	K)B	EmB	GB	F�B	EmB	GB	N�B	ZB	U�B	C�B	A�B	?}B	AUB	I�B	Y�B	P�B	Q�B	PHB	OB	F�B	G�B	F�B	I�B	J�B	L�B	OvB	S�B	W�B	XyB	[�B	bB	oiB	{�B	z�B	{�B	�B	�B	�B	��B	�AB	��B	�B	z�B	��B	w�B	zB	{JB	}�B	|�B	�iB	�lB	�JB	�~B	�"B	��B	��B	�bB	�B	��B	�B	�oB	�uB	�@B	��B	�SB	��B	��B	��B	��B	��B	��B	��B	�zB	��B	��B	�qB	��B	�OB	��B	��B	��B	��B	��B	�XB	��B	��B	��B	�B	��B	�B	�9B	�?B	ƨB	�?B	�B	��B	�B	�#B	�0B	�B	�jB	�6B	�B	͟B	�BB	�TB	��B	��B	�&B	�,B	�2B	�gB	�mB	��B	רB	�B	�B	�B	�B	ٴB	یB	��B	ܒB	�dB	�jB	�BB	�B	�B	�HB	�B	�B	�B	� B	�B	�,B	��B	�mB	��B	�>B	�B	�B	�QB	�"B	��B	��B	�B	��B	�B	��B	�AB	�B	�B	��B	��B	�+B	��B	��B	�2B	��B	�ZB	��B	�B	��B	��B	��B	�>B	�B	��B	��B
 �B
 �B
AB
�B
;B
 �B
 �B
oB
 4B
  B
 4B
 4B
B
AB
B
AB
�B
�B
+B
�B
_B
�B
�B
�B
1B
�B
	�B
B
�B
�B
�B
�B
�B
VB
�B
\B
VB
�B
PB
�B
B
VB
�B
�B
�B
�B
B
@B
 B
bB
4B
�B
 B
�B
:B
B
�B
�B
�B
�B
�B
�B
B
B
�B
B
�B
eB
1B
eB
=B
qB
�B
B
kB
�B
�B
=B
qB
�B
�B
OB
!-B
!-B
"�B
$�B
%�B
%FB
$�B
%FB
$�B
&�B
$�B
%FB
'B
)�B
,B
+6B
+kB
)�B
(�B
)*B
*eB
+kB
+B
*�B
+kB
/OB
1'B
2-B
2aB
3hB
3hB
3�B
49B
49B
49B
4�B
5B
4�B
4�B
5�B
5�B
5�B
6B
6�B
7LB
7LB
7LB
6�B
5�B
5�B
6�B
8B
8�B
9XB
9$B
9$B
8�B
8�B
9$B
8�B
8�B
8B
8RB
8�B
8�B
9�B
9�B
9XB
8�B
8B
9$B
9�B
:*B
:�B
:^B
:*B
:�B
;dB
;�B
<jB
<�B
<�B
<�B
=B
>wB
?}B
?�B
@B
@�B
A�B
A�B
B�B
B�B
B�B
A�B
A�B
AUB
A�B
A�B
B'B
B�B
CaB
EmB
E�B
F?B
E�B
F?B
FtB
FtB
GB
F�B
FtB
GB
GzB
G�B
H�B
H�B
HKB
HKB
H�B
I�B
I�B
JXB
J#B
J#B
K^B
L�B
L�B
LdB
MB
M6B
MB
M6B
M�B
M�B
M�B
N�B
NpB
N<B
NpB
OBB
PHB
P}B
P}B
P�B
P�B
QB
QNB
Q�B
R B
R�B
R�B
RTB
R�B
R�B
S[B
S&B
S�B
S�B
T�B
T�B
T�B
T�B
U2B
TaB
T�B
V9B
V9B
VB
VB
VB
VmB
V9B
VB
UgB
U�B
W?B
W?B
V9B
VmB
V�B
V�B
WsB
XB
XyB
YB
Y�B
Z�B
Z�B
ZQB
[�B
\�B
\�B
\�B
\�B
]/B
]dB
\�B
\�B
\�B
\�B
^jB
_pB
`B
`BB
_�B
_�B
_�B
`vB
`vB
`�B
aHB
a|B
a�B
a|B
a|B
a|B
aHB
a�B
bNB
b�B
d&B
e�B
e�B
ffB
f2B
g8B
f�B
f�B
f�B
f�B
gmB
g�B
h
B
h>B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
hsB
hsB
iDB
iDB
i�B
i�B
i�B
j�B
kQB
k�B
k�B
k�B
k�B
lWB
l�B
lWB
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
n�B
n�B
oiB
o�B
pB
pB
poB
qB
qAB
qvB
qvB
qvB
q�B
rGB
q�B
rB
rB
r|B
r|B
rGB
sB
sB
s�B
s�B
t�B
t�B
t�B
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
v`B
v`B
v�B
w2B
w�B
wfB
wfB
xB
w�B
xB
x�B
x�B
y>B
y>B
yrB
y�B
{B
z�B
{B
{JB
{B
{�B
{�B
{�B
{B
{JB
{�B
|B
|PB
|PB
|�B
}"B
}VB
}"B
}�B
~(B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
�B
� B
�4B
�4B
�4B
� B
��B
�oB
��B
�B
�AB
��B
��B
�GB
��B
�GB
�GB
�{B
�{B
��B
��B
�B
�B
��B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
�%B
��B
��B
�+B
�+B
�+B
�+B
�_B
�+B
�+B
��B
��B
��B
��B
��B
��B
�1B
�fB
��B
�B
�7B
�lB
�lB
�lB
��B
��B
�	B
�rB
�rB
��B
��B
�B
�B
�DB
�DB
�xB
��B
�B
�JB
�~B
�~B
��B
��B
��B
�PB
�PB
��B
��B
��B
��B
��B
��B
�"B
�"B
�"B
�VB
�VB
��B
��B
��B
��B
��B
��B
�\B
�\B
�\B
�\B
��B
�\B
��B
��B
��B
��B
��B
�bB
��B
��B
��B
��B
� B
�4B
�hB
�hB
�hB
��B
�:B
�oB
��B
��B
��B
��B
��B
�B
�uB
�uB
�uB
��B
��B
��B
��B
�B
�B
�B
�B
�{B
�{B
�{B
��B
�B
�FB1[B1[B.�B1�B49B1'B/OB1�B0UB2�B/�B0�B2-B1�B2�B0!B0�B2�B/�B1[B2-B0!B2-B2�B0!B2�B0�B2aB0�B2aB1�B0�B2�B1�B2aB0�B2�B0�B2�B0UB2�B1�B1�B0�B1�B33B/�B2aB2-B1'B2�B/�B0UB1�B0�B1�B1�B1'B2aB0UB2�B1�B0UB2-B1'B1�B2-B1�B2-B0UB2-B1�B0�B2�B0�B33B1'B1'B2�B0!B2�B0�B2�B1'B1�B2-B0�B2�B0�B1�B2-B0�B1�B0�B2�B0UB1�B2�B0�B2aB1�B0UB2�B0�B2aB1'B1�B2�B0UB2�B1�B1'B2�B0!B2aB1�B0�B2aB0�B1'B2�B0�B2aB0�B1[B1�B0!B1�B0�B0�B2aB0UB1'B3�B.�B-B0UB0!B2�B0!B1[B/B0!B1�B-�B,�B+�B+B.}B.�B-�B.}B)_B,�B*eB)�B)�B'B,B)�B$�B8B �B-wB�B�B�BYB($BxB�BB�B�BSBYB&�B�BqB!�B#B$tBOB 'B!bB�B"hB&LB%FB+�B3�B,B,=B+6B-�B+�B/�B-B*eB(XB(�BA�B1�B;0B(�B3�B)�B)�B1[B8RBK�BA�B?BJ#B<�BE�BB�B?�BCaBAUBD�BR BUgBT�B]�BY�BUgBYKBcTB_�Bc�B_�B`B`�B`�BqvBtBsMBo5Bt�B�=B�4B�oB�%B��B��B��B��B�bB��B��B��B��B��B�gB�qB��BŢB�-BʌBȀB��B˒B�B��B��B�B�BбBϫB�B�B��B�vB��BӏB��B�&BӏB҉BӏB��B��B��B��B҉BԕBӏB�TB��B�2B��B�sB�B��B֡B��B��B��B�BB�;B�|B��B�GB��B�KB�
B�&B��B�>B�B�ZB�2B�B�B�ZB��B��B�B�;B��B�B�B�B�%B��B�>B��B�rB��B��B��B��B;B �B �BGBoB�B�B�B%B%BB�BBxBBxB"BDB
�B�BPB�B"BBxB�B�BB(B�B�B�B�B�BB@B�BqB�B�B�BYB�B�BB�BOB�B#�B0UB*�B(XB($B)�B*0B)_B'�B2aBT�B<jB8�B?B:*B8RB9�B<6B9�B9�B7�B7LB8B9�B9XB5tB:�B4�B@�B:�B>BB?B=B?}B@�B@�BC-B?�B@�B@�B?}B?�BA�BH�BA BC�BB[BB�BA�BB'BB'B@�B?�B@�BB'B?HB?�B@OBA�B>�BB�BI�BB�BGEBH�BC�B@OB@�BB�BK^BB�BC�BM�BGzBE9BD�BE�BA�BEmBDgBCaBC�BD�BEBD�BI�BF�BFBFBE�BF?BH�BJ�BIBGEBI�BGzBM6BJXBJ�BO�BOvBK�BHKBF�BK^BH�BFtBIBG�BF�BEmBG�BHKBFBF?BIBHKBEmBGEBM�BM�BK�BJ�BHKBHKBE�BF�BHKBF?BH�BJ#BG�BGzBH�BJ�BF�BEmBG�BK�BD�BGEBFtBJXBHBEmBFBDgBF�BE�BD�BGzBE�BE�BEBD�BE�BFtBC�BDgBE9BDgBE9BE�BL�BM6BT�BK�BL0BIRBE9BD�BOBF?BB'BH�BGEBEBC�BIRB@OBC�BC-BC-BE�BF?BE�BJ�BN�BCaBGBB[BE�BEmBIRBG�BHKBD�BC�BE9BB[BA�BC�BD�BDgBD3BEmBD�BC-BJ�B?B@B@OBA�B@�BB�BFB?HB@�BDgBK^B?�BAUBC�B>�BD3BC-B@�B?}BCaBB[BA B@�B>wBA�BAUB>�B?�BAUB?B>BB@B=qBAUBI�B<�B?}BF�BC�B>wB;dB:�B9�B;�B:�B:*B@OB=qB;�B;0B<B<�B@OB>�B7B:�B<�B8B:�B9XB8RB6�B7�B8�B9XB7LB7�B7B7B6FB6�B6FB8�B6FB3hB2�B5�B1�B33B5?B8B@OB4�B4nB5�B<jB4nB5B9XB8RB3�B:�B2�B2-B0�B/OB0�B0�B0!B.�B-�B-�B,�B33B/�B4nB&B$�B"�B"4B$�B&�B$B"4B#�B"4B"4B#�B%zB$�B%�B&�B0�B,B.�B!�BVB�BIB!B�B�B�B=B�B�B�BBqB�B�BxBxB�B&�B%FB!�B"�B$�B#B#�B$�B%�B#:B#:B!�B!�B#�B*�B%�B�B�B	B�BYB�B�B�B@BSB�B�BJB�B�B�B�B_B;BAB�B��B�.B�.B��B��B��B��B�B�	B�>B��B�lB�B��B�ZB��B�B�%B�vB�B�WB�B�
B��B��B�8B�mB�B��B�]B��B�B��B�`B��B��B��B��B��B�B�B�ZB��B�B�B��BݘB�jB�;B�B�B�9B��BԕB��BҽB�KBŢBуB��B��B��B��B��B�B�-B�eB��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022101619030520221016190305IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022102614013420221026140134QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022102614013420221026140134QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                