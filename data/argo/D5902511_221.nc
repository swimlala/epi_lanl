CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-07-17T13:06:32Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20220717130632  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_221                 6810_008521_221                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���L�fQ@���L�fQ11  @���|���@���|���@1C�c�H@1C�c�H�d��A [��d��A [�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@�@@  @��\@��\@�p�@�p�@�p�A��A ��A+�A@  A_\)A\)A�  A�  A�Q�A�Q�A�\)A�  A�Q�B   B(�B  B�B   B((�B0(�B8  B@(�BH  BO�
BX  B`(�Bg�
Bo�Bw�
B�{B�  B��
B��B�{B�{B�  B�  B�{B�  B�{B�(�B�(�B��B�  B�(�B�{B�  B��B��B��B��B�  B�{B�=qB�{B��B�{B�{B��B�  B�(�C   C  C  C
=C  C
  C
=C  C��C  C
=C
=C
=C  C��C  C 
=C"
=C$
=C&
=C(  C*  C+��C-��C/��C2{C4
=C6
=C8  C:  C<  C>  C@  CB{CD  CE��CG�CJ  CL
=CN  CP  CR  CS��CV
=CX  CZ
=C\{C^
=C`
=Cb  Cd  Ce��Ch
=Cj  Ck��Cn  Cp  Cr  Cs��Cu�Cw��Cz  C|
=C~  C�  C�C�C�  C���C�C�  C���C���C�  C�  C���C�C�C���C�  C���C���C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C���C���C���C���C�  C�  C���C�  C�
=C�C���C�  C�
=C�
=C�C�C�C�
=C�  C���C�  C�  C�C�C�C�  C���C���C�  C�  C�  C�  C�C�C���C�C�  C���C�C�C�  C�  C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�C�
=C�C�  C�C�
=C�  C���C�  C�  C�  C�  C�  C�C�
=C�C�  C���C�  C���C���C���C�  C�  C���C���C���C���C�  C�  C���C�  C�  C�  C�  C���C�  C���C���D   D ��D  D��D�qDz�D�qD}qD  D�D�D� D  D}qD  D� D��Dz�D�qD	� D	�qD
� D  D� D�D� D��D� D�qD}qD�D�D�D� D�qD� D  D� D  Dz�D��D}qD  D��D  D� D  D}qDD� D�qD� D  D}qD  D�DD}qD  D��D  D}qD  D�D   D � D!  D!��D"D"��D#�D#� D#�qD$� D%  D%��D&�D&��D'�D'��D'�qD(� D)D)��D*�D*}qD+  D+�D,  D,� D-D-� D-��D.}qD.�qD/}qD0  D0}qD1  D1��D2�D2� D3  D3� D3�qD4}qD5�D5}qD5��D6� D7D7� D8  D8��D9D9� D9��D:}qD:�qD;}qD;��D<xRD<��D=� D>  D>}qD>�qD?z�D@�D@��DA�DA��DB�DB�DC  DC��DC�qDD� DE  DE}qDF  DF� DF�qDG}qDH  DH��DI  DI}qDI�qDJ� DK�DK� DL�DL}qDL�qDM}qDM�qDN� DO  DO� DO��DPz�DP�qDQ� DR  DR��DS  DS}qDT  DT��DU  DU}qDV�DV��DW  DW� DX�DX��DY�DY��DZ�DZ��D[�D[��D\  D\}qD\�qD]}qD]��D^��D_�D_� D`  D`� Da�Da��Db�Db��Db�qDc��DdDd��De  De� Df�Df�Dg�Dg� Dg�qDh}qDi  Di� Dj�Dj��Dk�Dk��Dl  Dl}qDm�Dm��Dn  Dn��DoDo��Dp�Dp�DqDq�DrDr��Dr�qDs}qDt  Dt� Du  Du��Dv�Dv}qDv�qDw��Dw�qDx��DyDy��Dz�Dz�D{  D{z�D{��D|}qD}  D}��D~  D~z�D  D��D�HD�>�D�~�D�� D�HD�@ D�~�D���D���D�@ D�� D��HD���D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD���D�D�  D�@ D��HD���D���D�>�D�� D��HD�  D�@ D�� D�� D���D�@ D��HD��HD��D�@ D�~�D��HD�HD�@ D�~�D�� D�HD�B�D�� D�� D�HD�@ D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D�  D�>�D�~�D���D�  D�AHD��HD�� D���D�>�D�� D��HD���D�@ D�� D���D�HD�B�D��HD���D���D�>�D�}qD���D���D�>�D�� D�� D���D�@ D�~�D���D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D�HD�B�D��HD�� D�  D�@ D�~�D�� D�  D�AHD��HD��HD�HD�@ D�� D���D�  D�AHD�� D���D�  D�@ D�� D��HD���D�@ D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D��HD�  D�>�D�� D��HD�HD�>�D�� D�� D���D�=qD�|)D��)D��qD�=qD�~�D�� D�  D�@ D�~�D���D�  D�>�D�~�D���D�  D�>�D�}qD���D�HD�@ D�� D�� D�  D�AHD�� D���D���D�@ D���D��HD�  D�@ D�� D��HD�HD�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD�HD�B�D��HD�� D��qD�@ D��HD�� D�  D�@ D��HD��HD�  D�>�D�� D�� D�  D�>�D�� D�� D��D�AHD�~�D���D���D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�B�D��HD��HD�  D�>�D��HD�� D���D�=qD�~�D�� D�  D�@ D�~�D���D���D�>�D�� D�� D�HD�AHD�� D�� D���D�@ D��HD�� D��qD�>�DHD��HD�  D�@ DÂ�D��HD���D�@ D�~�Dľ�D�  D�@ Dŀ D��HD�HD�>�DƁHD�D�HD�AHDǁHD��HD���D�@ DȀ D�� D�HD�>�D�~�D��HD��D�@ Dʀ D�� D�HD�>�D�|)D˾�D���D�>�D̀ D��HD��D�AHD̀ D�� D�HD�@ D�~�Dξ�D���D�@ Dπ D�� D�  D�AHDЀ D��HD�HD�@ Dр D�� D�  D�@ D�~�D�� D�HD�@ DӀ D�� D���D�>�D�~�D�� D�HD�@ DՁHD��HD���D�@ DցHD�� D�HD�AHDׁHD��HD�  D�AHD؁HD�� D�  D�@ Dـ D��HD���D�>�D�~�D�� D�HD�@ Dۀ D�� D���D�>�D܁HD�D�HD�>�D�~�D��HD�HD�AHDށHD޾�D�  D�@ D߀ D��HD�HD�>�D�~�D�� D�  D�AHD� D�� D�HD�=qD�~�D��HD�  D�@ D� D㾸D�  D�AHD�~�D侸D���D�@ D�HD��HD�  D�@ D�~�D澸D�  D�@ D�~�D��HD�HD�AHD�HD�� D��D�B�D�HD�D�HD�AHD�HD꾸D���D�B�D�HD�� D�  D�@ D�HD��HD���D�@ D� D��HD�HD�@ D� DD�  D�>�D�}qDﾸD���D�>�D�� D�D��D�B�D�HD�D�  D�AHD�HD�� D�HD�AHD�HD�� D���D�>�D�~�D���D��qD�>�D�� D�� D�  D�@ D�~�D���D�  D�AHD�� D�� D���?��?8Q�?�\)?���?�
=@�@��@.{@G�@Y��@p��@��@��@�@��\@���@�z�@�  @���@�@޸R@�=q@�@��RA�A�AG�A�A=qA   A#�
A(Q�A.{A3�
A8Q�A<��AB�\AG�AL(�AP  AUAZ�HA_\)Ac33AhQ�An�RAs33AvffA{�A���A��HA���A�
=A�=qA�z�A�ffA�G�A��
A�A�  A��A�A��A�=qA�p�A�\)A��A��A�
=A���A�(�A�
=A���A�(�A��RA���A�33A�A�Q�A�=qA��A�  A��A�(�A�
=A�G�AۅA�{A���A�33A��A�\)A�=qA�(�A�{A���A�33A��A��A��\A�z�A�ffB ��B{B
=B(�B��B�HB�
B	G�B
�\B\)Bz�B�B33B(�B��B
=B  BG�B�RB�
B��B{B�B��B�B�HB Q�B!B"�RB#�B%�B&ffB'\)B(Q�B)�B*�HB+�B-G�B.�\B/�B0z�B1�B3\)B4(�B5G�B6�RB8(�B9G�B:=qB;\)B<��B>=qB?33B@(�BABC
=BC�
BD��BFffBG�BH��BI��BJ�RBLQ�BMG�BN=qBO33BP��BQ�BS
=BT  BUG�BV�HBX(�BY�BZffB\  B]G�B^ffB_�Ba�Bb�\Bc�Bd��BfffBg�Bh��Bj=qBk�Blz�BmBo33Bpz�Bqp�Br�RBtQ�Bu��BvffBw�By�Bz�RB{�
B|��B~{B�B�z�B��B���B�(�B���B��B�  B���B��B�  B�z�B�G�B�  B�z�B�
=B��B�z�B���B��B�(�B��HB���B�=qB���B�\)B�(�B��HB�p�B��B���B�\)B�  B��\B��B��B���B��B��B�z�B�33B��B�Q�B�
=B�B�Q�B��HB���B�Q�B�
=B��B�  B���B���B�=qB���B�\)B�{B��HB���B�(�B��RB���B�Q�B���B��B�(�B���B��B�ffB���B���B�ffB�33B�B�ffB��B��B��\B�33B��
B��\B�p�B�{B���B�\)B�(�B���B���B�(�B���B���B�ffB�
=B��B�=qB�
=B��
B�z�B��B�B�z�B�G�B�B�ffB�33B��B�z�B�
=BǮB�z�B��B�B�Q�B���BˮB̏\B�33BͮB�Q�B��B��
B�z�B��B�B�Q�B�
=B�Bԏ\B��BծB�z�B�33B��Bأ�B�33B�B�z�B�\)B�{B܏\B�G�B��Bޏ\B�p�B�{B���B�\)B�  B���B�B�{B��B�G�B�(�B���B�G�B�B�=qB��HB�B�{B�z�B��HB�G�B��
B�Q�B���B��B�\)B�B�(�B��B�
=B�33B�B��
B�=qB��B���B�33B�p�B��
B�=qB��B��HB��B�B��B�Q�B�z�B��RB�
=B��B��B�(�B�ffB��RB�33B��B��B�{B�Q�B��RB�33B��B�B�  B�=qB���B�
=B�p�B���B��
B�(�B��\B���B�33B�p�B���B��B�=qB���B���B�
=B�G�B���B��C �C 33C Q�C z�C �C �
C ��C{C33C\)C��CC�HC  C�C\)C�\C�C�
C��C{C=qCz�C��C�
C��C
=C33Cp�C��C�RC�
C  C33CffC�\C�RC�HC��C{CG�C�C��C�RC�
C��C33CffC�C��C�RC�C(�CQ�Cp�C�\C�C��C	  C	33C	ffC	�C	��C	C	�C
�C
Q�C
p�C
�\C
�C
�
C
=C33CQ�Cz�C��C�C�
C
=C33C\)C�C��C�RC�
C  C33C\)C�C��CC�HC{CG�Cz�C��C��C�C{C=qCp�C�C�HC
=C=qC\)C�C�RC�C(�C\)Cz�C��C�
C{CG�Cz�C��C��C��C(�C\)C�\C�
C  C(�CQ�Cz�C��C�
C{CQ�C�C�C�HC  C=qC�C�C�HC{CG�C�CC  C(�C\)C�CC  CG�Cz�C�C�
C{C=qCp�C�C�C�C\)C��C��C  C(�C\)C��C��C
=CG�C�C�RC�HC�CG�Cz�CC  C=qCz�C�RC�HC 
=C =qC p�C �RC!  C!33C!ffC!��C!C!��C"33C"z�C"�RC"�C#{C#=qC#p�C#�RC#��C$33C$p�C$�\C$C$��C%(�C%\)C%�\C%��C&
=C&=qC&z�C&�C&�
C'�C'\)C'z�C'�C'�
C({C(G�C(z�C(�RC(��C)33C)ffC)��C)��C)��C*�C*Q�C*�C*�RC*��C+33C+p�C+��C+C+��C,�C,Q�C,�\C,�
C-
=C-=qC-\)C-��C-�RC-��C.(�C.ffC.��C.��C/
=C/G�C/z�C/��C/�
C0  C0=qC0ffC0��C0��C1
=C1=qC1z�C1�RC1�HC2�C2G�C2z�C2��C2��C3  C333C3\)C3�\C3�RC3��C433C4ffC4��C4��C4��C533C5ffC5��C5��C6  C6(�C6\)C6�C6�C6�
C7
=C7=qC7p�C7�C7�HC8{C8\)C8�C8�RC8��C9(�C9\)C9�\C9�C9�HC:{C:=qC:p�C:��C:�
C;
=C;G�C;�C;�RC;��C<(�C<\)C<��C<�
C=  C==qC=p�C=��C=�
C>  C>(�C>\)C>��C>C>��C?33C?ffC?��C?��C@{C@Q�C@�\C@CA  CA=qCAz�CA�CA�HCB�CB\)CB�\CB��CC  CC33CCffCC��CC�
CD�CD\)CD��CD�
CE
=CEQ�CE�\CECF  CF=qCFz�CF�RCG  CG33CGz�CGCH  CH=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                             ?u@�@@  @��\@��\@�p�@�p�@�p�A��A ��A+�A@  A_\)A\)A�  A�  A�Q�A�Q�A�\)A�  A�Q�B   B(�B  B�B   B((�B0(�B8  B@(�BH  BO�
BX  B`(�Bg�
Bo�Bw�
B�{B�  B��
B��B�{B�{B�  B�  B�{B�  B�{B�(�B�(�B��B�  B�(�B�{B�  B��B��B��B��B�  B�{B�=qB�{B��B�{B�{B��B�  B�(�C   C  C  C
=C  C
  C
=C  C��C  C
=C
=C
=C  C��C  C 
=C"
=C$
=C&
=C(  C*  C+��C-��C/��C2{C4
=C6
=C8  C:  C<  C>  C@  CB{CD  CE��CG�CJ  CL
=CN  CP  CR  CS��CV
=CX  CZ
=C\{C^
=C`
=Cb  Cd  Ce��Ch
=Cj  Ck��Cn  Cp  Cr  Cs��Cu�Cw��Cz  C|
=C~  C�  C�C�C�  C���C�C�  C���C���C�  C�  C���C�C�C���C�  C���C���C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C���C���C���C���C�  C�  C���C�  C�
=C�C���C�  C�
=C�
=C�C�C�C�
=C�  C���C�  C�  C�C�C�C�  C���C���C�  C�  C�  C�  C�C�C���C�C�  C���C�C�C�  C�  C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C�C�
=C�C�  C�C�
=C�  C���C�  C�  C�  C�  C�  C�C�
=C�C�  C���C�  C���C���C���C�  C�  C���C���C���C���C�  C�  C���C�  C�  C�  C�  C���C�  C���C���D   D ��D  D��D�qDz�D�qD}qD  D�D�D� D  D}qD  D� D��Dz�D�qD	� D	�qD
� D  D� D�D� D��D� D�qD}qD�D�D�D� D�qD� D  D� D  Dz�D��D}qD  D��D  D� D  D}qDD� D�qD� D  D}qD  D�DD}qD  D��D  D}qD  D�D   D � D!  D!��D"D"��D#�D#� D#�qD$� D%  D%��D&�D&��D'�D'��D'�qD(� D)D)��D*�D*}qD+  D+�D,  D,� D-D-� D-��D.}qD.�qD/}qD0  D0}qD1  D1��D2�D2� D3  D3� D3�qD4}qD5�D5}qD5��D6� D7D7� D8  D8��D9D9� D9��D:}qD:�qD;}qD;��D<xRD<��D=� D>  D>}qD>�qD?z�D@�D@��DA�DA��DB�DB�DC  DC��DC�qDD� DE  DE}qDF  DF� DF�qDG}qDH  DH��DI  DI}qDI�qDJ� DK�DK� DL�DL}qDL�qDM}qDM�qDN� DO  DO� DO��DPz�DP�qDQ� DR  DR��DS  DS}qDT  DT��DU  DU}qDV�DV��DW  DW� DX�DX��DY�DY��DZ�DZ��D[�D[��D\  D\}qD\�qD]}qD]��D^��D_�D_� D`  D`� Da�Da��Db�Db��Db�qDc��DdDd��De  De� Df�Df�Dg�Dg� Dg�qDh}qDi  Di� Dj�Dj��Dk�Dk��Dl  Dl}qDm�Dm��Dn  Dn��DoDo��Dp�Dp�DqDq�DrDr��Dr�qDs}qDt  Dt� Du  Du��Dv�Dv}qDv�qDw��Dw�qDx��DyDy��Dz�Dz�D{  D{z�D{��D|}qD}  D}��D~  D~z�D  D��D�HD�>�D�~�D�� D�HD�@ D�~�D���D���D�@ D�� D��HD���D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD���D�D�  D�@ D��HD���D���D�>�D�� D��HD�  D�@ D�� D�� D���D�@ D��HD��HD��D�@ D�~�D��HD�HD�@ D�~�D�� D�HD�B�D�� D�� D�HD�@ D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D�  D�>�D�~�D���D�  D�AHD��HD�� D���D�>�D�� D��HD���D�@ D�� D���D�HD�B�D��HD���D���D�>�D�}qD���D���D�>�D�� D�� D���D�@ D�~�D���D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D�HD�B�D��HD�� D�  D�@ D�~�D�� D�  D�AHD��HD��HD�HD�@ D�� D���D�  D�AHD�� D���D�  D�@ D�� D��HD���D�@ D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D��HD�  D�>�D�� D��HD�HD�>�D�� D�� D���D�=qD�|)D��)D��qD�=qD�~�D�� D�  D�@ D�~�D���D�  D�>�D�~�D���D�  D�>�D�}qD���D�HD�@ D�� D�� D�  D�AHD�� D���D���D�@ D���D��HD�  D�@ D�� D��HD�HD�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD�HD�B�D��HD�� D��qD�@ D��HD�� D�  D�@ D��HD��HD�  D�>�D�� D�� D�  D�>�D�� D�� D��D�AHD�~�D���D���D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�B�D��HD��HD�  D�>�D��HD�� D���D�=qD�~�D�� D�  D�@ D�~�D���D���D�>�D�� D�� D�HD�AHD�� D�� D���D�@ D��HD�� D��qD�>�DHD��HD�  D�@ DÂ�D��HD���D�@ D�~�Dľ�D�  D�@ Dŀ D��HD�HD�>�DƁHD�D�HD�AHDǁHD��HD���D�@ DȀ D�� D�HD�>�D�~�D��HD��D�@ Dʀ D�� D�HD�>�D�|)D˾�D���D�>�D̀ D��HD��D�AHD̀ D�� D�HD�@ D�~�Dξ�D���D�@ Dπ D�� D�  D�AHDЀ D��HD�HD�@ Dр D�� D�  D�@ D�~�D�� D�HD�@ DӀ D�� D���D�>�D�~�D�� D�HD�@ DՁHD��HD���D�@ DցHD�� D�HD�AHDׁHD��HD�  D�AHD؁HD�� D�  D�@ Dـ D��HD���D�>�D�~�D�� D�HD�@ Dۀ D�� D���D�>�D܁HD�D�HD�>�D�~�D��HD�HD�AHDށHD޾�D�  D�@ D߀ D��HD�HD�>�D�~�D�� D�  D�AHD� D�� D�HD�=qD�~�D��HD�  D�@ D� D㾸D�  D�AHD�~�D侸D���D�@ D�HD��HD�  D�@ D�~�D澸D�  D�@ D�~�D��HD�HD�AHD�HD�� D��D�B�D�HD�D�HD�AHD�HD꾸D���D�B�D�HD�� D�  D�@ D�HD��HD���D�@ D� D��HD�HD�@ D� DD�  D�>�D�}qDﾸD���D�>�D�� D�D��D�B�D�HD�D�  D�AHD�HD�� D�HD�AHD�HD�� D���D�>�D�~�D���D��qD�>�D�� D�� D�  D�@ D�~�D���D�  D�AHD�� D�� G�O�?��?8Q�?�\)?���?�
=@�@��@.{@G�@Y��@p��@��@��@�@��\@���@�z�@�  @���@�@޸R@�=q@�@��RA�A�AG�A�A=qA   A#�
A(Q�A.{A3�
A8Q�A<��AB�\AG�AL(�AP  AUAZ�HA_\)Ac33AhQ�An�RAs33AvffA{�A���A��HA���A�
=A�=qA�z�A�ffA�G�A��
A�A�  A��A�A��A�=qA�p�A�\)A��A��A�
=A���A�(�A�
=A���A�(�A��RA���A�33A�A�Q�A�=qA��A�  A��A�(�A�
=A�G�AۅA�{A���A�33A��A�\)A�=qA�(�A�{A���A�33A��A��A��\A�z�A�ffB ��B{B
=B(�B��B�HB�
B	G�B
�\B\)Bz�B�B33B(�B��B
=B  BG�B�RB�
B��B{B�B��B�B�HB Q�B!B"�RB#�B%�B&ffB'\)B(Q�B)�B*�HB+�B-G�B.�\B/�B0z�B1�B3\)B4(�B5G�B6�RB8(�B9G�B:=qB;\)B<��B>=qB?33B@(�BABC
=BC�
BD��BFffBG�BH��BI��BJ�RBLQ�BMG�BN=qBO33BP��BQ�BS
=BT  BUG�BV�HBX(�BY�BZffB\  B]G�B^ffB_�Ba�Bb�\Bc�Bd��BfffBg�Bh��Bj=qBk�Blz�BmBo33Bpz�Bqp�Br�RBtQ�Bu��BvffBw�By�Bz�RB{�
B|��B~{B�B�z�B��B���B�(�B���B��B�  B���B��B�  B�z�B�G�B�  B�z�B�
=B��B�z�B���B��B�(�B��HB���B�=qB���B�\)B�(�B��HB�p�B��B���B�\)B�  B��\B��B��B���B��B��B�z�B�33B��B�Q�B�
=B�B�Q�B��HB���B�Q�B�
=B��B�  B���B���B�=qB���B�\)B�{B��HB���B�(�B��RB���B�Q�B���B��B�(�B���B��B�ffB���B���B�ffB�33B�B�ffB��B��B��\B�33B��
B��\B�p�B�{B���B�\)B�(�B���B���B�(�B���B���B�ffB�
=B��B�=qB�
=B��
B�z�B��B�B�z�B�G�B�B�ffB�33B��B�z�B�
=BǮB�z�B��B�B�Q�B���BˮB̏\B�33BͮB�Q�B��B��
B�z�B��B�B�Q�B�
=B�Bԏ\B��BծB�z�B�33B��Bأ�B�33B�B�z�B�\)B�{B܏\B�G�B��Bޏ\B�p�B�{B���B�\)B�  B���B�B�{B��B�G�B�(�B���B�G�B�B�=qB��HB�B�{B�z�B��HB�G�B��
B�Q�B���B��B�\)B�B�(�B��B�
=B�33B�B��
B�=qB��B���B�33B�p�B��
B�=qB��B��HB��B�B��B�Q�B�z�B��RB�
=B��B��B�(�B�ffB��RB�33B��B��B�{B�Q�B��RB�33B��B�B�  B�=qB���B�
=B�p�B���B��
B�(�B��\B���B�33B�p�B���B��B�=qB���B���B�
=B�G�B���B��C �C 33C Q�C z�C �C �
C ��C{C33C\)C��CC�HC  C�C\)C�\C�C�
C��C{C=qCz�C��C�
C��C
=C33Cp�C��C�RC�
C  C33CffC�\C�RC�HC��C{CG�C�C��C�RC�
C��C33CffC�C��C�RC�C(�CQ�Cp�C�\C�C��C	  C	33C	ffC	�C	��C	C	�C
�C
Q�C
p�C
�\C
�C
�
C
=C33CQ�Cz�C��C�C�
C
=C33C\)C�C��C�RC�
C  C33C\)C�C��CC�HC{CG�Cz�C��C��C�C{C=qCp�C�C�HC
=C=qC\)C�C�RC�C(�C\)Cz�C��C�
C{CG�Cz�C��C��C��C(�C\)C�\C�
C  C(�CQ�Cz�C��C�
C{CQ�C�C�C�HC  C=qC�C�C�HC{CG�C�CC  C(�C\)C�CC  CG�Cz�C�C�
C{C=qCp�C�C�C�C\)C��C��C  C(�C\)C��C��C
=CG�C�C�RC�HC�CG�Cz�CC  C=qCz�C�RC�HC 
=C =qC p�C �RC!  C!33C!ffC!��C!C!��C"33C"z�C"�RC"�C#{C#=qC#p�C#�RC#��C$33C$p�C$�\C$C$��C%(�C%\)C%�\C%��C&
=C&=qC&z�C&�C&�
C'�C'\)C'z�C'�C'�
C({C(G�C(z�C(�RC(��C)33C)ffC)��C)��C)��C*�C*Q�C*�C*�RC*��C+33C+p�C+��C+C+��C,�C,Q�C,�\C,�
C-
=C-=qC-\)C-��C-�RC-��C.(�C.ffC.��C.��C/
=C/G�C/z�C/��C/�
C0  C0=qC0ffC0��C0��C1
=C1=qC1z�C1�RC1�HC2�C2G�C2z�C2��C2��C3  C333C3\)C3�\C3�RC3��C433C4ffC4��C4��C4��C533C5ffC5��C5��C6  C6(�C6\)C6�C6�C6�
C7
=C7=qC7p�C7�C7�HC8{C8\)C8�C8�RC8��C9(�C9\)C9�\C9�C9�HC:{C:=qC:p�C:��C:�
C;
=C;G�C;�C;�RC;��C<(�C<\)C<��C<�
C=  C==qC=p�C=��C=�
C>  C>(�C>\)C>��C>C>��C?33C?ffC?��C?��C@{C@Q�C@�\C@CA  CA=qCAz�CA�CA�HCB�CB\)CB�\CB��CC  CC33CCffCC��CC�
CD�CD\)CD��CD�
CE
=CEQ�CE�\CECF  CF=qCFz�CF�RCG  CG33CGz�CGCH  CH=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                             @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�ƨA���A���A���A���A���A�ƨA���A���A�ȴA���A���A���A���A�ȴA���A���A���A���A�ȴA���A��#A���A���A��
A��A���A��
A���A���A�ȴAة�A؃A�dZA�VA�G�A�?}A�&�A׏\A�-A֮A�(�AӇ+A�$�A�M�A�`BA��A�^5A�&�AΝ�A���Aͩ�A�ffA�A��;A�r�A��A��A���A�$�Aȇ+A���A�ZA�&�A�dZA��-A�Q�A���A�S�A���A��A�O�A���A��jA�I�A�(�A��A���A�O�A��A�VA��A�=qA��TA�x�A�1A���A��A��A��DA���A�~�A�ZA�r�A��A��
A�{A���A���A���A�ƨA��A��A��DA��wA���A��A� �A~ �A{�Az�jAxffAw33Atn�Ap(�Am�Aj{AfJAa�^A\��AY
=AV��AT=qASVAP�AM�hAI|�AF�9AE?}AC��AB�DAAG�A?��A;��A:$�A8 �A5A2��A1��A1&�A0��A/%A.��A.~�A-�A-hsA,�RA+�-A*n�A(�`A'�A&��A&�+A%�FA${A#%A"jA!�7At�AĜAhsA�FAoA�A��A�^A+A�DAI�A�TA�uA�DAr�Ap�A1'A
�yA
1A	S�A1A��A�hA;dA��A��A9XA��Ap�A�PAĜA �!A z�A �9A ��A -@��@�bN@�"�@�-@��@�Q�@���@�R@�hs@���@�Z@�w@@�@���@�?}@��@�z�@�K�@@���@��@��@홚@��
@��@�ȴ@��@��@�?}@�D@���@㝲@�S�@◍@ᙚ@�9X@��@���@ܓu@�  @ۅ@�o@���@�v�@�`B@��@؛�@�9X@�ƨ@�ȴ@��@Ձ@�%@Դ9@�1'@Ӯ@�t�@ҏ\@���@Ѓ@�  @Ώ\@��@�x�@�t�@���@ʏ\@ɲ-@���@�ƨ@�l�@�o@��@�ff@�J@ŉ7@���@ēu@�bN@��@þw@�l�@�5?@��@��@���@���@�33@�^5@��-@�j@��m@��w@���@���@�t�@�ȴ@��H@�@��@�K�@���@�J@��^@��h@���@�@���@�p�@�7L@��@��@�%@�b@��@�(�@��;@��H@�~�@�M�@��@���@���@��@�X@��@�Ĝ@�Z@�9X@��m@�"�@��!@�ff@��T@��^@���@���@���@�?}@��D@��F@�@��\@��!@��@�C�@��H@�v�@�V@�=q@�-@�@��^@�7L@���@�z�@�Q�@� �@�A�@�I�@� �@�1@�  @��@��@��@��w@�9X@� �@��@�"�@���@�-@�{@��^@���@���@�1'@���@��@��\@�V@�=q@�-@�J@���@��h@�X@��@���@��j@�Q�@��w@�t�@��@�=q@���@�G�@�z�@�1@���@��w@�t�@�+@���@�
=@�o@��@��@��y@�V@�@���@�x�@�G�@��@��/@�r�@�  @���@�ƨ@�dZ@�+@�"�@�o@�@��y@��H@���@��!@�M�@��@��@���@��`@��u@�bN@�A�@�9X@��@���@�t�@�C�@�ȴ@�~�@�5?@�{@���@��h@�?}@�%@�j@���@�t�@�;d@��y@�-@�{@��@��h@�&�@��`@���@�Q�@�  @���@���@�@��!@�v�@�M�@���@���@�`B@�O�@�7L@���@�I�@�(�@�1@���@��
@�K�@�"�@�ȴ@�V@��@��@���@�p�@�X@���@��D@�j@�(�@�1@�ƨ@��@�\)@�;d@�;d@�33@���@�ff@�M�@�=q@�-@�{@�@���@�p�@�V@��9@��@�I�@�  @��w@��@���@�|�@�dZ@�S�@�33@�+@�o@�@��H@��R@�n�@�^5@�^5@�M�@��@���@��@��@���@��@���@���@�Ĝ@��j@��@��@�j@�bN@�Q�@�(�@��@��@K�@~ȴ@~@}��@}��@}?}@|�D@|(�@{�
@{�@z�!@y�@y�7@y%@xQ�@w�@wK�@v�y@vv�@up�@t��@tZ@s��@sS�@r�\@r^5@r-@q�@q��@q%@p�@o��@o+@n��@n��@m�@m�@m�h@l�@lZ@k�
@k@j�\@j-@i��@iG�@h�`@h�9@h�u@hr�@h1'@g�@g��@g�P@gl�@gl�@gl�@gK�@g+@f��@f�R@fV@e�h@d�j@dZ@c�m@c��@c"�@b��@b�!@bn�@b=q@b-@b-@b�@a�#@a�7@a�@`bN@`  @_�;@^��@^�@^$�@]��@\��@\��@\j@\9X@\1@[t�@[@Z�!@Z~�@Y�@Y�@X�`@X��@XQ�@W�@W\)@Vff@V{@U��@U��@U?}@T��@T��@S��@S33@R��@R^5@R�@Q�@Q�^@QX@Q&�@Q%@P�u@P �@Ol�@Nȴ@Nff@Nff@M�T@MO�@L�@Lj@K��@KdZ@J�\@J~�@JM�@I��@I7L@H�`@H��@Hr�@H �@G+@Fȴ@F��@Fv�@FE�@F{@E�T@E@D�@Dj@C�
@C��@CC�@B�@B�H@Bn�@A�^@Ax�@A%@@�u@@A�@?�w@?|�@?l�@?;d@?+@>�y@>��@>{@=@=�h@=/@<�j@<j@<I�@<1@;��@;33@:��@:n�@9��@9�^@9X@9&�@9%@8��@8�9@8r�@8bN@8Q�@8  @7�w@7l�@7;d@7�@6�@6ȴ@6��@6$�@5��@5O�@4�@4��@4(�@3��@2��@2M�@1��@1�^@1��@1x�@1&�@0��@0b@/�w@/l�@.��@.�@.��@.{@-@-�@-/@,�j@,9X@,�@+ƨ@+�@+dZ@*�@*��@*�\@*^5@*�@)��@)��@)�@(Ĝ@( �@'�@'��@'�@'��@'��@'��@'\)@&�y@&��@&V@&5?@%�h@$�@$��@$�j@$��@$z�@$I�@$�@#�m@#ƨ@#�@#S�@"�@"��@"^5@!��@!��@!7L@ Ĝ@ �@ Q�@ b@�@�;@��@|�@K�@+@�y@E�@��@�h@?}@�@�j@�@�D@I�@1@�
@�
@ƨ@�@"�@��@��@�\@�\@~�@M�@�@�7@hs@G�@&�@Ĝ@�u@bN@1'@1'@b@�@��@�P@+@+@�@
=@��@��@��@�+@�+@V@5?@$�@{@@�T@�-@�h@�@?}@V@��@�/@��@��@z�@Z@(�@1@�
@�@t�@t�@S�@S�@C�@"�@"�@"�@�@��@��@�!@n�@n�@M�@=q@=q@-@J@��@�^@�^@��@��@�7@hs@X@7L@7L@&�@&�@��@��@r�@bN@Q�@A�@ �@�@��@K�@+@��@�@�R@�+@ff@{@�@�@��@@�h@p�@O�@?}@V@�@��@�@��@z�@Z@(�@1@��@�F@��@33@o@
�@
�H@
�H@
�!@
^5@
^5@
M�@
=q@
=q@
-@
J@	�#@	��@	�^@	x�@	G�@	7L@	7L@	&�@	&�@	�@��@Ĝ@�u@�@r�@bN@1'@�@�;@�w@�@�P@\)@;d@�@��@�@ȴA�AؾwAؼjA���Aغ^A�ȴA�ȴA�ȴA���A�ƨA�ȴA�ȴA���A���A���A���A���A�ȴA���A�ȴA���A���A���A�ĜA�ƨA���A���A�ȴA�ȴA���A���A�ȴA���A���A�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA���A���A�ȴA�ĜA�ȴA�ȴA�ĜA�ƨA���A�ƨA�ƨA���A���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA�ȴA�ƨA�ƨA�ȴA�ȴA�ȴA�ȴA���A�ȴA�ĜA���A���A���A��A��#A��#A��
A��/A��;A��A���A��
A��
A���A���A���A���A���A���A���A���A���A��
A��A��
A���A��A��/A��A���A��
A��#A��
A��
A��#A��
A���A��A��A���A���A���A��
A���A���A���A��
A��/A��A��
A��
A��
A���A���A���A��
A���A���A���A���A���A�ƨA���A���A���A�ĜA�A���A���A�AؾwA�AخAؗ�A؛�Aإ�A؟�A؟�A؛�A�n�A�x�A�x�A�hsA�jA�hsA�^5A�^5A�`BA�bNA�\)A�ZA�ZA�VA�O�A�M�A�Q�A�S�A�C�A�?}A�?}A�C�A�A�A�A�A�A�A�A�A�C�A�A�A�9XA�9XA�?}A�;dA�;dA��A���A���Aײ-Aן�AבhAׅA�^5A�\)A�S�A�G�A�33A�"�A�{A�JA��A��A�ƨA֟�A֕�Aև+A�~�A�v�A�n�A�XA�;dA�JA���A�=qA��AӓuA�K�A��AҶFAҕ�A�l�A�A�A��A���Aѥ�Aѥ�AѓuA�\)A�"�A� �A��A���A�`BA�ZA�Q�A�M�A�I�A�A�A�7LA�1'A�{AϾwA�t�A�dZA�dZA�dZA�`BA�XA�M�A�G�A�A�A�1'A�$�A��A��A���Aβ-AΕ�AΉ7A�x�A�1'A�
=A�A���A��A���A�A͸RAͲ-AͮA͟�A͕�AͅA�|�A�v�A�hsA�K�A�7LA��A�JA���A���A��A��A��A��`A��;A��A���A���A̩�A̬A̗�A�XA�"�A��A�bA�%A�A�  A���A˅A�5?A� �A�&�A�(�A��A�ƨA�hsA�7LA�%A��;Aɥ�A�t�A�\)A�XA�5?A�%A��A��
A���Aȝ�A�t�A�n�A�hsA�ffA�I�A�
=A��HA�~�A�K�A�JAƲ-A�v�A�I�A�+A��A�VA�AŁA�VA�K�A�?}A�-A�JA��`A���Aĺ^AĮAĝ�A�|�A�33A��/A�v�A�JA¾wA�~�A�n�A�dZA�\)A�VA�33A���A�ƨA���A���A��\A��A�t�A�^5A�S�A�Q�A�$�A��yA���A�n�A�=qA�"�A�oA�bA�JA�
=A�1A���A���A��mA��HA��;A��
A�ȴA�A��^A��^A��-A��A���A���A���A��hA��\A��7A��A�z�A�|�A�x�A�jA�XA�;dA��A��A��TA�ĜA�p�A�+A��HA���A��7A��A�hsA�XA�Q�A�=qA�7LA�-A��A�
=A��yA���A��A��FA�p�A�S�A�?}A�"�A���A�ȴA��!A���A���A���A�x�A�ZA�=qA�33A�/A�1'A�33A�33A�1'A�(�A�$�A�(�A�&�A�$�A� �A��A�JA��A���A��\A�bNA�;dA��A�%A��A��`A��HA���A��jA��-A���A��A���A���A��uA��DA��7A�|�A�z�A�hsA�`BA�XA�M�A�I�A�K�A�I�A�?}A�5?A��A�bA��/A�A�A�S�A��/A�z�A�-A��yA���A��7A�~�A�VA�JA��yA��9A��uA�z�A�Q�A�/A� �A�bA��A��A�r�A�$�A��A�{A�{A��A�{A���A���A��PA�|�A�dZA�S�A�9XA��A��A�Q�A�/A�1A��yA��;A��A�A�ƨA�G�A��wA�bNA��A��hA�?}A���A��9A��uA�t�A�dZA�I�A�A��A�bNA�1A�G�A�A�ĜA���A��DA��+A��A�~�A�v�A�t�A�n�A�jA�hsA�hsA�hsA�bNA�VA�G�A��A�
=A��yA�ĜA���A�bNA��A��A��TA���A�O�A�7LA��A�%A��A��HA���A�VA�(�A�oA�  A��mA��^A���A�l�A�;dA���A���A��-A��+A�O�A�(�A�bA���A��A��A��;A��RA��uA�n�A�S�A�?}A�33A�/A�$�A��A�{A�1A���A���A��A��;A�A��A���A��\A�t�A�G�A��A��A��DA��`A���A�t�A�\)A�K�A�C�A�;dA�+A�$�A�{A���A��
A���A��\A��A�v�A�VA�C�A�"�A���A��A���A���A��A�dZA�O�A�7LA��A�x�A�JA�ȴA��\A�hsA�VA��A��;A��^A��!A��A���A���A���A���A��\A��A�M�A�-A��A�A��HA��9A�x�A�ZA�1'A��A��uA�M�A��TA�VA�
=A���A�O�A�A���A�ĜA�t�A� �A��A��/A���A��wA��!A���A�9XA��RA��jA���A�^5A�C�A�+A�  A��A���A��A��\A��A�ZA�(�A���A��HA��7A�oA���A��wA���A��A�jA�M�A�1'A��A��A���A��A��DA�x�A�hsA�Q�A�-A�VA��A�S�A�"�A��A��RA��A�=qA���A��TA��RA�~�A�K�A�&�A���A�ĜA��7A�(�A�A�Q�A��\A��^A�33A���A��;A��!A��\A�hsA�O�A�(�A�A��FA�p�A�A�A�l�A�/A�  A��#A��A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                             A�z�A�ƨA���A���A���A���A���A�ƨA���A���A�ȴA���A���A���A���A�ȴA���A���A���A���A�ȴA���A��#A���A���A��
A��A���A��
A���A���A�ȴAة�A؃A�dZA�VA�G�A�?}A�&�A׏\A�-A֮A�(�AӇ+A�$�A�M�A�`BA��A�^5A�&�AΝ�A���Aͩ�A�ffA�A��;A�r�A��A��A���A�$�Aȇ+A���A�ZA�&�A�dZA��-A�Q�A���A�S�A���A��A�O�A���A��jA�I�A�(�A��A���A�O�A��A�VA��A�=qA��TA�x�A�1A���A��A��A��DA���A�~�A�ZA�r�A��A��
A�{A���A���A���A�ƨA��A��A��DA��wA���A��A� �A~ �A{�Az�jAxffAw33Atn�Ap(�Am�Aj{AfJAa�^A\��AY
=AV��AT=qASVAP�AM�hAI|�AF�9AE?}AC��AB�DAAG�A?��A;��A:$�A8 �A5A2��A1��A1&�A0��A/%A.��A.~�A-�A-hsA,�RA+�-A*n�A(�`A'�A&��A&�+A%�FA${A#%A"jA!�7At�AĜAhsA�FAoA�A��A�^A+A�DAI�A�TA�uA�DAr�Ap�A1'A
�yA
1A	S�A1A��A�hA;dA��A��A9XA��Ap�A�PAĜA �!A z�A �9A ��A -@��@�bN@�"�@�-@��@�Q�@���@�R@�hs@���@�Z@�w@@�@���@�?}@��@�z�@�K�@@���@��@��@홚@��
@��@�ȴ@��@��@�?}@�D@���@㝲@�S�@◍@ᙚ@�9X@��@���@ܓu@�  @ۅ@�o@���@�v�@�`B@��@؛�@�9X@�ƨ@�ȴ@��@Ձ@�%@Դ9@�1'@Ӯ@�t�@ҏ\@���@Ѓ@�  @Ώ\@��@�x�@�t�@���@ʏ\@ɲ-@���@�ƨ@�l�@�o@��@�ff@�J@ŉ7@���@ēu@�bN@��@þw@�l�@�5?@��@��@���@���@�33@�^5@��-@�j@��m@��w@���@���@�t�@�ȴ@��H@�@��@�K�@���@�J@��^@��h@���@�@���@�p�@�7L@��@��@�%@�b@��@�(�@��;@��H@�~�@�M�@��@���@���@��@�X@��@�Ĝ@�Z@�9X@��m@�"�@��!@�ff@��T@��^@���@���@���@�?}@��D@��F@�@��\@��!@��@�C�@��H@�v�@�V@�=q@�-@�@��^@�7L@���@�z�@�Q�@� �@�A�@�I�@� �@�1@�  @��@��@��@��w@�9X@� �@��@�"�@���@�-@�{@��^@���@���@�1'@���@��@��\@�V@�=q@�-@�J@���@��h@�X@��@���@��j@�Q�@��w@�t�@��@�=q@���@�G�@�z�@�1@���@��w@�t�@�+@���@�
=@�o@��@��@��y@�V@�@���@�x�@�G�@��@��/@�r�@�  @���@�ƨ@�dZ@�+@�"�@�o@�@��y@��H@���@��!@�M�@��@��@���@��`@��u@�bN@�A�@�9X@��@���@�t�@�C�@�ȴ@�~�@�5?@�{@���@��h@�?}@�%@�j@���@�t�@�;d@��y@�-@�{@��@��h@�&�@��`@���@�Q�@�  @���@���@�@��!@�v�@�M�@���@���@�`B@�O�@�7L@���@�I�@�(�@�1@���@��
@�K�@�"�@�ȴ@�V@��@��@���@�p�@�X@���@��D@�j@�(�@�1@�ƨ@��@�\)@�;d@�;d@�33@���@�ff@�M�@�=q@�-@�{@�@���@�p�@�V@��9@��@�I�@�  @��w@��@���@�|�@�dZ@�S�@�33@�+@�o@�@��H@��R@�n�@�^5@�^5@�M�@��@���@��@��@���@��@���@���@�Ĝ@��j@��@��@�j@�bN@�Q�@�(�@��@��@K�@~ȴ@~@}��@}��@}?}@|�D@|(�@{�
@{�@z�!@y�@y�7@y%@xQ�@w�@wK�@v�y@vv�@up�@t��@tZ@s��@sS�@r�\@r^5@r-@q�@q��@q%@p�@o��@o+@n��@n��@m�@m�@m�h@l�@lZ@k�
@k@j�\@j-@i��@iG�@h�`@h�9@h�u@hr�@h1'@g�@g��@g�P@gl�@gl�@gl�@gK�@g+@f��@f�R@fV@e�h@d�j@dZ@c�m@c��@c"�@b��@b�!@bn�@b=q@b-@b-@b�@a�#@a�7@a�@`bN@`  @_�;@^��@^�@^$�@]��@\��@\��@\j@\9X@\1@[t�@[@Z�!@Z~�@Y�@Y�@X�`@X��@XQ�@W�@W\)@Vff@V{@U��@U��@U?}@T��@T��@S��@S33@R��@R^5@R�@Q�@Q�^@QX@Q&�@Q%@P�u@P �@Ol�@Nȴ@Nff@Nff@M�T@MO�@L�@Lj@K��@KdZ@J�\@J~�@JM�@I��@I7L@H�`@H��@Hr�@H �@G+@Fȴ@F��@Fv�@FE�@F{@E�T@E@D�@Dj@C�
@C��@CC�@B�@B�H@Bn�@A�^@Ax�@A%@@�u@@A�@?�w@?|�@?l�@?;d@?+@>�y@>��@>{@=@=�h@=/@<�j@<j@<I�@<1@;��@;33@:��@:n�@9��@9�^@9X@9&�@9%@8��@8�9@8r�@8bN@8Q�@8  @7�w@7l�@7;d@7�@6�@6ȴ@6��@6$�@5��@5O�@4�@4��@4(�@3��@2��@2M�@1��@1�^@1��@1x�@1&�@0��@0b@/�w@/l�@.��@.�@.��@.{@-@-�@-/@,�j@,9X@,�@+ƨ@+�@+dZ@*�@*��@*�\@*^5@*�@)��@)��@)�@(Ĝ@( �@'�@'��@'�@'��@'��@'��@'\)@&�y@&��@&V@&5?@%�h@$�@$��@$�j@$��@$z�@$I�@$�@#�m@#ƨ@#�@#S�@"�@"��@"^5@!��@!��@!7L@ Ĝ@ �@ Q�@ b@�@�;@��@|�@K�@+@�y@E�@��@�h@?}@�@�j@�@�D@I�@1@�
@�
@ƨ@�@"�@��@��@�\@�\@~�@M�@�@�7@hs@G�@&�@Ĝ@�u@bN@1'@1'@b@�@��@�P@+@+@�@
=@��@��@��@�+@�+@V@5?@$�@{@@�T@�-@�h@�@?}@V@��@�/@��@��@z�@Z@(�@1@�
@�@t�@t�@S�@S�@C�@"�@"�@"�@�@��@��@�!@n�@n�@M�@=q@=q@-@J@��@�^@�^@��@��@�7@hs@X@7L@7L@&�@&�@��@��@r�@bN@Q�@A�@ �@�@��@K�@+@��@�@�R@�+@ff@{@�@�@��@@�h@p�@O�@?}@V@�@��@�@��@z�@Z@(�@1@��@�F@��@33@o@
�@
�H@
�H@
�!@
^5@
^5@
M�@
=q@
=q@
-@
J@	�#@	��@	�^@	x�@	G�@	7L@	7L@	&�@	&�@	�@��@Ĝ@�u@�@r�@bN@1'@�@�;@�w@�@�P@\)@;d@�@��@�G�O�A�AؾwAؼjA���Aغ^A�ȴA�ȴA�ȴA���A�ƨA�ȴA�ȴA���A���A���A���A���A�ȴA���A�ȴA���A���A���A�ĜA�ƨA���A���A�ȴA�ȴA���A���A�ȴA���A���A�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA���A���A�ȴA�ĜA�ȴA�ȴA�ĜA�ƨA���A�ƨA�ƨA���A���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA�ȴA�ƨA�ƨA�ȴA�ȴA�ȴA�ȴA���A�ȴA�ĜA���A���A���A��A��#A��#A��
A��/A��;A��A���A��
A��
A���A���A���A���A���A���A���A���A���A��
A��A��
A���A��A��/A��A���A��
A��#A��
A��
A��#A��
A���A��A��A���A���A���A��
A���A���A���A��
A��/A��A��
A��
A��
A���A���A���A��
A���A���A���A���A���A�ƨA���A���A���A�ĜA�A���A���A�AؾwA�AخAؗ�A؛�Aإ�A؟�A؟�A؛�A�n�A�x�A�x�A�hsA�jA�hsA�^5A�^5A�`BA�bNA�\)A�ZA�ZA�VA�O�A�M�A�Q�A�S�A�C�A�?}A�?}A�C�A�A�A�A�A�A�A�A�A�C�A�A�A�9XA�9XA�?}A�;dA�;dA��A���A���Aײ-Aן�AבhAׅA�^5A�\)A�S�A�G�A�33A�"�A�{A�JA��A��A�ƨA֟�A֕�Aև+A�~�A�v�A�n�A�XA�;dA�JA���A�=qA��AӓuA�K�A��AҶFAҕ�A�l�A�A�A��A���Aѥ�Aѥ�AѓuA�\)A�"�A� �A��A���A�`BA�ZA�Q�A�M�A�I�A�A�A�7LA�1'A�{AϾwA�t�A�dZA�dZA�dZA�`BA�XA�M�A�G�A�A�A�1'A�$�A��A��A���Aβ-AΕ�AΉ7A�x�A�1'A�
=A�A���A��A���A�A͸RAͲ-AͮA͟�A͕�AͅA�|�A�v�A�hsA�K�A�7LA��A�JA���A���A��A��A��A��`A��;A��A���A���A̩�A̬A̗�A�XA�"�A��A�bA�%A�A�  A���A˅A�5?A� �A�&�A�(�A��A�ƨA�hsA�7LA�%A��;Aɥ�A�t�A�\)A�XA�5?A�%A��A��
A���Aȝ�A�t�A�n�A�hsA�ffA�I�A�
=A��HA�~�A�K�A�JAƲ-A�v�A�I�A�+A��A�VA�AŁA�VA�K�A�?}A�-A�JA��`A���Aĺ^AĮAĝ�A�|�A�33A��/A�v�A�JA¾wA�~�A�n�A�dZA�\)A�VA�33A���A�ƨA���A���A��\A��A�t�A�^5A�S�A�Q�A�$�A��yA���A�n�A�=qA�"�A�oA�bA�JA�
=A�1A���A���A��mA��HA��;A��
A�ȴA�A��^A��^A��-A��A���A���A���A��hA��\A��7A��A�z�A�|�A�x�A�jA�XA�;dA��A��A��TA�ĜA�p�A�+A��HA���A��7A��A�hsA�XA�Q�A�=qA�7LA�-A��A�
=A��yA���A��A��FA�p�A�S�A�?}A�"�A���A�ȴA��!A���A���A���A�x�A�ZA�=qA�33A�/A�1'A�33A�33A�1'A�(�A�$�A�(�A�&�A�$�A� �A��A�JA��A���A��\A�bNA�;dA��A�%A��A��`A��HA���A��jA��-A���A��A���A���A��uA��DA��7A�|�A�z�A�hsA�`BA�XA�M�A�I�A�K�A�I�A�?}A�5?A��A�bA��/A�A�A�S�A��/A�z�A�-A��yA���A��7A�~�A�VA�JA��yA��9A��uA�z�A�Q�A�/A� �A�bA��A��A�r�A�$�A��A�{A�{A��A�{A���A���A��PA�|�A�dZA�S�A�9XA��A��A�Q�A�/A�1A��yA��;A��A�A�ƨA�G�A��wA�bNA��A��hA�?}A���A��9A��uA�t�A�dZA�I�A�A��A�bNA�1A�G�A�A�ĜA���A��DA��+A��A�~�A�v�A�t�A�n�A�jA�hsA�hsA�hsA�bNA�VA�G�A��A�
=A��yA�ĜA���A�bNA��A��A��TA���A�O�A�7LA��A�%A��A��HA���A�VA�(�A�oA�  A��mA��^A���A�l�A�;dA���A���A��-A��+A�O�A�(�A�bA���A��A��A��;A��RA��uA�n�A�S�A�?}A�33A�/A�$�A��A�{A�1A���A���A��A��;A�A��A���A��\A�t�A�G�A��A��A��DA��`A���A�t�A�\)A�K�A�C�A�;dA�+A�$�A�{A���A��
A���A��\A��A�v�A�VA�C�A�"�A���A��A���A���A��A�dZA�O�A�7LA��A�x�A�JA�ȴA��\A�hsA�VA��A��;A��^A��!A��A���A���A���A���A��\A��A�M�A�-A��A�A��HA��9A�x�A�ZA�1'A��A��uA�M�A��TA�VA�
=A���A�O�A�A���A�ĜA�t�A� �A��A��/A���A��wA��!A���A�9XA��RA��jA���A�^5A�C�A�+A�  A��A���A��A��\A��A�ZA�(�A���A��HA��7A�oA���A��wA���A��A�jA�M�A�1'A��A��A���A��A��DA�x�A�hsA�Q�A�-A�VA��A�S�A�"�A��A��RA��A�=qA���A��TA��RA�~�A�K�A�&�A���A�ĜA��7A�(�A�A�Q�A��\A��^A�33A���A��;A��!A��\A�hsA�O�A�(�A�A��FA�p�A�A�A�l�A�/A�  A��#A��A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B5B5�B5tB4�B6B6B5�B5B5tB5�B4nB6B5tB4�B3�B49B4�B5�B4�B3�B3hB6zB5?B49B5?B5?B4nB4�B3�B3hB1�B.�B+6B&B%�B#�B"hB#�B/�B=�BOvB^�Bl�Bh�Bf�B�_B�B� B��B��B�B	7B
�B(B�B�BB49Bd�Be�BffBo BsMBhsBh>Bo5Bp�Br|Bs�BuZB}�B�xB�MB�\B��BgBIBJXB=qB@�B'�B�B
=B�B��B�vB�B�B�B��B��B�B�nB��B�MB_pBB'B(�B{B
��B
�ZB
�[B
��B
|PB
S�B
A�B
2�B
#nB
\B
�B	�"B	��B	��B	��B	�}B	�9B	��B	��B	|�B	m)B	W
B	J�B	:^B	6FB	-�B	$�B	B	
rB	
	B	 4B	�B��B��B�;B�NB�B��B�<BɺB�3B�B��B�qB�6B�dB��B��B�B��B�B��B��B�RB�XB�_B��B�nB��B��B��B��B�B�B�zB��B��B�-B�VB��B��B�B�1B�{B��B�\B��B��B�B�VB��B�B�B�7B�=B��B�B��B�4B�B��B��B�EB��B՛B�aB�,B҉B�TBΥB̘B�BB�KB��B�-B� B�B��B�-BȀB��B��B�PB�xB�lB��B	�B	VB	"hB	%B	!�B	"4B	"4B	$�B	%zB	"�B	"�B	"4B	!�B	"hB	!bB	%FB	&�B	'�B	(�B	-�B	1�B	2�B	3�B	4�B	9�B	:�B	=qB	?}B	@�B	C�B	E9B	FtB	HB	I�B	L0B	L�B	NpB	U�B	\�B	c�B	gmB	g8B	c�B	g8B	e`B	c�B	d�B	e`B	hsB	m]B	oiB	o�B	p�B	s�B	v�B	zB	|�B	�oB	�oB	��B	�fB	�B	�\B	��B	�VB	�.B	� B	��B	�VB	��B	��B	�VB	��B	��B	�oB	��B	��B	��B	�\B	��B	��B	��B	�*B	��B	��B	�OB	�9B	�FB	�zB	��B	�zB	�B	�OB	�B	�B	�BB	�B	��B	�}B	�UB	��B	��B	��B	��B	��B	ƨB	ǮB	ǮB	�B	�XB	��B	�jB	�B	�}B	�NB	��B	��B	�yB	�yB	��B	�B	�WB	ݘB	��B	�TB	�QB	�B	��B	��B	�B	�]B	�)B	�WB	�QB	�B	�sB	��B	�
B	��B	�B	�QB	�B	��B	�;B	��B	��B	�rB	��B
 �B
  B	�]B	��B	�"B	�"B	�VB	��B	�lB	��B	�.B	��B	��B
�B
�B
�B
1B

	B
DB
DB

=B
B

�B
PB
�B
xB

�B

	B
	�B
�B
�B
_B
+B
+B
�B
1B
	7B
DB
�B
�B
PB
"B
 B
�B
�B
4B
hB
4B
uB
�B
�B
B
eB
~B
B
B
B
B
OB
B
�B
OB
VB
"�B
#�B
#�B
$@B
%FB
%zB
%�B
%�B
%�B
'B
&�B
'B
($B
($B
(�B
(XB
(XB
)�B
)*B
)*B
+kB
+�B
+�B
,B
-B
.B
-�B
-�B
/�B
0�B
0�B
0�B
1'B
1�B
1�B
2-B
4B
3�B
3�B
3�B
4�B
5tB
5�B
5tB
5?B
7�B
7LB
7LB
7�B
7LB
7�B
8�B
8�B
8�B
9�B
8�B
8�B
9$B
8�B
9�B
:�B
:^B
:�B
:�B
:�B
;�B
<jB
<jB
<�B
<jB
<6B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>�B
>�B
>wB
>BB
>wB
?HB
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
AUB
AUB
B'B
A�B
B�B
B�B
B�B
CaB
D�B
EmB
EmB
F?B
E�B
FB
FtB
FtB
FtB
FtB
FtB
F�B
F�B
FtB
GB
GEB
GB
G�B
GEB
HB
HKB
HB
H�B
H�B
I�B
I�B
I�B
I�B
K^B
K�B
L0B
MjB
M6B
MB
M�B
M�B
NB
N�B
OvB
OBB
O�B
P}B
P�B
P�B
QB
QB
QB
Q�B
Q�B
R B
RTB
R B
R�B
R�B
R�B
S&B
S�B
TaB
T�B
T�B
U2B
UgB
U�B
U�B
VB
VB
VB
VB
VmB
VmB
V�B
W
B
W
B
W
B
V�B
W
B
V�B
W?B
W?B
WsB
XEB
X�B
YKB
YB
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
Z�B
[WB
\]B
\)B
[�B
\�B
\�B
]�B
]�B
^�B
^B
^5B
^5B
^5B
^�B
^jB
^jB
^jB
_;B
_pB
_pB
_pB
_�B
`BB
`vB
a�B
a|B
a|B
a�B
bB
a�B
bNB
b�B
c�B
c�B
c�B
d&B
d&B
dZB
d�B
d�B
d�B
e,B
e`B
f2B
ffB
f�B
f2B
gB
gmB
g�B
h
B
hsB
h�B
i�B
iyB
i�B
jKB
jB
j�B
j�B
j�B
kQB
lWB
l�B
l�B
l�B
m)B
m)B
m)B
m]B
m�B
ncB
n�B
n�B
o B
o B
o B
oiB
p;B
p;B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
rB
rGB
r|B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tTB
t�B
t�B
u%B
uZB
uZB
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
w�B
w2B
wfB
xB
x�B
y>B
yrB
y�B
y�B
y�B
zDB
z�B
{B
{B
{�B
|PB
|B
|PB
|�B
}"B
}VB
}VB
}�B
~]B
~]B
~�B
~�B
~�B
cB
�B
cB
�B
�4B
� B
��B
��B
�;B
��B
��B
��B
�AB
�AB
�B
�B
�AB
�uB
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
�YB
��B
��B
��B
��B
��B
��B
�1B
�fB
�fB
��B
��B
��B
��B
�B
��B
�=B
�=B
��B
��B
��B
�B
��B
�DB
��B
��B
��B
�xB
��B
�JB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�VB
��B
��B
�(B
�(B
�(B
�\B
��B
��B
��B
�bB
�bB
�bB
��B
��B
� B
� B
� B
�4B
�4B
�hB
��B
��B
�hB
��B
��B
�B
�B
�oB
��B
��B
��B
�B
��B
�uB
�uB
��B
��B
�B
�FB
�{B
��B
��B
��B
��B
�B
��B
�B
�MB
�MB
�MB
��B
��B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�$B
�$B
�$B
�$B
�YB
�YB
��B
��B
�+B
�_B
��B
��B
��B
�1B
�eB
��B
��B
�B
�7B
�7B
�kB
��B
��B
��B
�	B
�=B
�=B
�=B
�=B
�=B
��B
��B
��B
��B
��B
�B
�CB
�CB
�xB
�xB
�xB
��B
��B
��B
��B
�B
�IB
�IB
��B
��B
��B
��B
�B
�OB
�B
�B
�OB
�B
�B
��B
��B
��B
��B
��B
��B
�!B
�VB
�VB
�VB
�VB
��B
�\B
��B
��B
��B
��B
��?�@�҉B4�B7�B8RB3�B6B6FB4nB6zB6FB6�B5�B5�B4nB5�B6B6�B5�B6zB5�B6B4�B6FB5�B5B4B6FB7B49B7�B6�B5B4nB3�B4�B5�B3�B4B7B8�B2aB5�B7�B6�B5�B4B6�B7LB5?B4�B5?B8�B4nB5�B4�B7B3�B49B5B2�B33B4�B4nB2�B49B5�B2-B1�B4�B49B5�B4�B5B49B5tB5tB4�B33B6zB3�B4�B6FB7LB4nB5B5�B33B33B2�B5�B6B1�B4nB4B2�B2�B3�B4B1[B4�B4nB2�B/OB5?B7LB6FB6�B7B4�B5�B7�B8�B4�B4�B6FB5B49B4�B5tB33B3�B5B5?B33B3hB5tB6FB4�B4nB6B6�B4�B3�B5�B6�B4nB5�B7�B5tB4�B4B4�B3�B2�B4�B5?B3hB5tB2aB6zB5�B33B3hB49B4�B2�B2�B4�B5B5�B1�B3hB4�B2�B1[B3hB4�B1�B1�B1�B2�B0�B.�B8B(�B,qB-�B+6B*�B+�B8�B%zB'�B+�B&LB%�B'�B%�B$tB$tB&LB&�B%B%FB%zB&B#�B#:B&�B$�B#B"hB!�B"hB"hB"�B"4B"4B"hB!�BVB!bB"hB.�B)�B'�B,qB/�B-wB0�B6B2�B5tB6B>�BA�BB[BB�BJ#BJ�BN�BQNBP�BR�BTaBS�BS�BV�BY�BbB��BiyBh�Bp;BjBo BiBjBhsBjBiDBlWBdZBaBdZBl�Bg�BffBd�BzB�oB�AB��B�B��B�oB�MB��B�hB�'B�)B��B�B�B�B�ZB�2B��B��B�B�2B��B��B�vB��B��B��B��B%B�BBMBB�B	lB	�B�B�B
=B	�B
	BDBfBfB�B(BB�B�B"B(B"B�B�B�B(B(B4B@BB�B&�B�B�B�B+B�BYB#:B.�B+�B'B4nB4B7BPB]�B`BBl"Bd�BjBjBd&Ba|BgBf2Bg�Bg�BgmBjBe,Bd&Bd�BdZBk�Bl�BncBt�BrBv�BtTBsBs�Bq�Bo5BoiBqABpoBi�BgmBh>BffBh�Be�Bd�Bc�Ba�B^jBb�Bh�Be,Bt�Bc�Bs�BkBiBh>Bh�Bg�Bm�Bo Br�BqBm�Bn�BoiBq�BsBn�Bm�Bv�BrBp�B~]BqBm�Bn�Bo�Bo5Bm�Bm�Bp�Bp�Bs�BqBp�Bp�Br�Bt�Br�Bp;Bn�Br�BsMBs�Bt�Bs�BrBq�BrBtBqABqvBqBr�Bv�By	Bv+BqBxB~(Bx�B��BuZBxBs�Bw�Br�Br�BqABp�Bo�BpoBrGBs�Bv`B�YB|�B��B~(B~�B��B�B�B�MB��B��B��B��B��B�xB��B��B��B�=B�~B��B�.B��B��B��B��B��B��B�B�7B�B��B�OB�B�eB�SB��B��B�(B��B��B��B�bB�B�xB��B�hB�VB��B�	B�lB��B��B��B��B�lB��B��B��B��B��B�{B�~B��B�\Bs�Bs�Bf�B^�BZ�BT�BS�BT�BQNBQBH�BK)BFtBH�BEBE9BF?BK)BR�BHBH�B>wB>BB>�BH�BJ#BZ�BR�BN�BQ�BOBNBK�BQBG�B;�B5�B4�B1'B-CB.BEBT�BQ�BP}BB'B@�BI�B8�B6zB1�B-�B-�B)�B,�B1�B'B$tB%B*�B�BYBhB�B�BPBJB�B�B�BJB
�B	�B	7B
=B�B
rB~B+B	�BfB_B�B�B �B 4B�B��B��B��B�`B�B��B��B�xB��B�%B�B�MB��B�B�B��B��B�iB�B�5B�B�B�B�B��B��B�2B��B��B�fB��B�B�B��B�|BޞB�5BܒB��BیB��B��B��B��B�/B��B�EBیB�
BԕB�&BܒBϫB�0BȀB��B�zBƨB��B�gB��B��B��B�tB�B�qB��B�OB��B��B��B��B�zB��B�zB��B�B��B�B�*B�IB�0B��B��B��B��B�-B�_B��B��B��B��B��B��B�oB��B��B��B�B��B��B��B��B�B��B�MB�BzB��B}"BiyBlWBv�B\�BYB^�B^BY�BQ�BM�BK^BJ�BIRBHBO�B5�B49B=�B5B2aB.B-CB(�B+�B)*B%�B"�B)_B&B!�B�B+�B#nB�B\B�BJB
=B	7B�BSBSB
��B;B
�"B
��B
��B
�	B
��B
�2B
�]B
��B
�>B
��B
��B
�WB
��B
֡B
��B
�<B
�^B
�EB
�UB
�UB
�B
��B
��B
��B
�OB
��B
�nB
�FB
��B
��B
.B
~(B
~�B
{B
��B
u�B
|B
uZB
{�B
��B
n/B
^B
QNB
W�B
R�B
N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022071713063220220717130632IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022072709013620220727090136QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022072709013620220727090136QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                