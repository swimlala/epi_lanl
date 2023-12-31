CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-31T12:58:48Z creation; 2021-03-26T17:01:01Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201231125848  20210326170211  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               =   =AA  AOAO7836_008777_061                 7836_008777_061                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�S��"@�S��"11  @�S��C@�S��C@;�9����@;�9�����d����
R�d����
R11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�@B�\@�  @�p�@�  @�\A ��A��A ��A,��A@��A`  A�  A�  A�Q�A�Q�A�  AϮA�\)A�\)A��B  B�
B  B Q�B(  B0  B7�
B?�
BH  BP  BX(�B`(�Bg�
Bp  Bx(�B�  B�{B�  B�  B��B��B��
B�B��
B��B�  B��B��B�  B�  B��
B��
B�  B��B��B�{B�{B��B�  B�{B�{B�{B�{B�  B�  B�{B�  B��C  C
=C  C  C
  C��C
=C  C�C�C��C  C
=C
=C  C 
=C"  C#��C%��C(  C*
=C,
=C.
=C0  C1��C4  C6  C8  C:
=C<  C>
=C@{CB�CD{CF  CH  CJ  CL  CN
=CP  CQ��CS��CV
=CX{CZ  C\  C^
=C`
=Cb  Cd  Cf  Ch  Cj{Cl{Cn
=Cp  Cq��Ct  Cv
=Cx{Cz
=C|  C~  C��C�C�  C���C���C�  C�C�  C���C�  C�  C���C���C�C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C���C�  C�  C�  C�  C���C���C�C�
=C�  C��C��C���C���C���C���C���C���C���C���C�  C�  C�  C�C�  C���C��C�  C���C���C���C���C�  C�C���C�  C�C�
=C�
=C�C�C�C�
=C�
=C�  C���C�  C�  C�  C�C�C�  C�  C�C�  C�C�C���C���C�  C�C�  C���C���C�C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C���C���C�  C�  C�  C�C�C���C���C�C�C���C���C�  C�C�  C���C�  C�C�\C�C���C���C���D ��D  Dz�D�D��D�qDz�D��D� D  D� D  D��D  D}qD�qD��D	D	��D
  D
� D�D� D�qD� D�qD}qD  D� D�D��D�qD� DD��D�D��D�D��D  D}qD�D��D�qD}qD�D��D�qD��D�D}qD�D��D  D��D�RD}qD�qD��D�D� D�qD}qD�qD � D!  D!��D"  D"}qD"�qD#� D$�D$� D%  D%� D&  D&}qD&�qD'��D(D(� D(�RD)z�D)��D*z�D+  D+��D,�D,� D,�qD-z�D.  D.}qD/  D/�D0�D0��D1  D1� D1�qD2� D3  D3� D4�D4��D5D5�D6�D6��D6��D7� D8�D8� D9  D9}qD:  D:� D;  D;� D;�qD<� D=  D=}qD=�qD>z�D?  D?��D@�D@� D@�qDA}qDB  DB� DB�qDC� DD  DDz�DD�qDExRDE��DF��DG�DG�DH�DH� DH�qDI}qDJ  DJ��DK  DK}qDK�qDL� DM�DM��DM�qDN}qDO�DO��DP�DP��DQ  DQ� DR  DR��DR�qDSz�DS�qDT}qDU  DU� DV  DV� DW  DW}qDX  DX��DY  DY��DZ�DZ�D[�D[��D\D\� D\�qD]� D^  D^xRD^�qD_��D`D`� Da�Da�DbDb�Dc  Dc� Dd  Dd��De�De� Df  Df��Df�qDg� Dh  Dh}qDi  Di��Dj�Dj}qDk  Dk��Dl  Dl}qDm  Dm� Dm�qDn� Do  Do}qDo�qDp� Dq  Dq��Dr  Dr� Ds�Ds� Ds�qDt��Du  Du}qDu�qDv� Dv�qDw� Dx�Dx� Dy  Dy� Dz�Dz��D{  D{}qD|  D|��D}  D}}qD}��D~xRD~�qD��D��D�@ D��HD��HD�HD�B�D���D��HD�  D�>�D�}qD�� D��D�@ D�~�D�� D�  D�AHD��HD�� D�  D�B�D�~�D�� D�  D�@ D�� D�� D�HD�@ D��HD�D�  D�>�D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D���D�=qD�� D�D�  D�>�D�~�D�� D���D�=qD�~�D�� D�  D�AHD��HD���D�  D�AHD��HD�� D���D�>�D�~�D���D�  D�B�D��HD���D���D�AHD��HD�� D�HD�AHD�~�D��qD��)D�=qD�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�AHD���D��HD��qD�<)D�}qD�� D��D�AHD�� D��HD��D�AHD�� D���D���D�@ D��HD���D���D�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D�~�D��qD�  D�AHD�� D���D��qD�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D�}qD���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�~�D��HD�HD�@ D��HD�� D�  D�@ D�~�D��qD��qD�>�D�}qD�� D�HD�@ D�� D�� D�  D�>�D��HD��HD�HD�AHD�� D��HD���D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�~�D���D��D�@ D�~�D�� D���D�=qD�� D��HD���D�@ D�~�D���D�  D�AHD�� D���D�HD�AHD��HD�� D�HD�B�D��HD�� D���D�@ D�� D���D�  D�AHD���D��HD���D�@ D�~�D���D���D�@ D��HD�� D�  D�AHD��HD�� D�  D�AHD�� D���D�  D�@ D�� D��HD�  D�=qD D�� D���D�@ DÁHD�� D���D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHDƁHDƾ�D��qD�>�Dǀ DǾ�D���D�AHDȂ�D��HD�HD�@ DɁHD�D��D�>�D�~�D�� D�  D�@ Dˀ D�� D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�>�D�~�Dξ�D�  D�>�D�~�D�� D�  D�@ D�~�D��HD�  D�<)D�~�D�� D�HD�AHD�}qDҾ�D�HD�AHDӂ�D��HD���D�@ DԀ DԾ�D�HD�AHDՀ DսqD�HD�B�DցHD��HD�  D�>�D�~�D�� D�  D�AHD؀ Dؾ�D�  D�@ D�~�Dپ�D�  D�@ Dڂ�D��HD�  D�AHDہHD�� D���D�@ D܀ D�D�HD�@ D�~�D�� D��D�AHDށHD�� D���D�@ D�~�D߼)D���D�AHD�� D�qD�  D�B�DႏD�� D���D�>�D�~�D�� D��D�AHD�~�D㾸D�HD�AHD�~�D侸D�  D�AHD傏D��HD�HD�@ D�~�D�� D�  D�@ D�HD��HD�  D�=qD�}qD�� D�HD�B�D� D龸D�  D�AHD�HD�� D���D�>�D� D�� D�  D�>�D�~�D��HD��D�@ D� D��HD���D�>�D�~�DD�  D�@ D�}qDﾸD�HD�AHD�~�D�D�  D�>�D�~�D�� D�HD�AHD�~�D�D�  D�AHD�HD��HD�HD�>�D�~�D���D���D�>�D�}qD���D�  D�>�D�}qD��qD��qD�>�D�� D�� D�HD�@ D�~�D���D��qD�=qD�� D��HD�  D�@ D�� D�� >�G�?8Q�?aG�?�z�?�p�?�@�@��@&ff@5@J=q@^�R@s33@��@��@�33@�p�@�=q@�@��R@Ǯ@�\)@�Q�@�G�@�\)@���A�AffA�A�RA33AQ�Ap�A"�\A'
=A+�A0  A4z�A7�A<(�A?\)AE�AJ=qAL��AP��AR�\AU�AW�AZ�HA^�RAa�Ag
=Ai��Al(�An�RAqG�As�
Aw
=Ax��A|��A���A��\A�(�A�A��RA�Q�A���A�33A�{A�\)A���A�33A�(�A�A��RA���A��\A���A�
=A�  A��A��HA���A�ffA��A��A�33A�p�A��A���A��\A�(�A�p�A�\)A���A�33A��A�  A��A��
A�p�AƸRA�G�Aʏ\A�z�AθRA���A��HA���A�ffA�  Aٙ�A�33A��A�
=A�G�A�33A�A�  A�\A���A�ffA��A�\A���A�ffA�Q�A��\A���A�
=B ��B�B33BQ�BG�BffB33B(�B	�B
{B
=B  B�BffB�B��Bp�B=qB33B  B�B{B\)BQ�B��B�HB�
B��BB�HB   B ��B"{B#33B$(�B%p�B&ffB'�B(z�B*{B+\)B,Q�B-G�B.ffB/\)B0Q�B1p�B2=qB3�B4��B5�B733B8Q�B9��B:�RB<  B=G�B>ffB?33B@Q�BAG�BB=qBC33BDQ�BEG�BF=qBG33BH(�BIG�BJ�\BK�BL��BN{BO33BPQ�BQp�BR�\BS\)BTz�BUp�BV�\BW�BX��BY��BZ�RB[�B\��B^{B_33B`Q�Bap�Bb�\Bc�
Be�Bf=qBg\)Bhz�Bip�Bj=qBk\)BlQ�Bmp�Bn�\Bo\)Bp��BqBr�RBt  Bu�Bv=qBw\)Bxz�By��Bz�HB|  B}�B~=qB\)B�(�B��RB�G�B��
B�Q�B��HB�p�B�  B��\B�
=B��B�  B��\B�
=B���B�{B���B�G�B��B�=qB���B�G�B�B�Q�B���B�\)B��
B�Q�B��HB�p�B��B�Q�B��HB�p�B�  B�z�B�
=B��B�(�B���B�33B��B�=qB��RB�G�B��
B�ffB���B��B�{B��RB��B��B�(�B��\B�
=B��B�  B�z�B�
=B���B�(�B��RB�G�B�B�ffB�
=B��B�  B��\B�33B���B�{B���B��B�B�ffB���B��B�Q�B��HB�p�B��B��\B�
=B���B�(�B���B�p�B�{B��RB�\)B��B�z�B�
=B��B�{B���B�33B�B�ffB�
=B��B�(�B��HB�p�B�{B��HB�\)B��
B�ffB��HB�p�B�  B���B�G�B��
B�z�B��B��B�Q�B���B�G�B��
B�Q�B���BŅB�  Bƣ�B�G�B��B�z�B�
=BɅB�  B�z�B�
=BˮB�Q�B��HB͙�B�=qB���B�\)B��B�Q�B���B�\)B�  Bҏ\B�33B�B�ffB�
=BՅB�  B�z�B�
=B׮B�ffB���BٮB�(�Bڣ�B��BۮB�ffB�
=B�B�Q�B���B�G�B�B�z�B�33B��
B�ffB���B�\)B�{B���B�p�B��B�ffB��HB�B�ffB���B�33B�B�Q�B���B�33B�B��
B�ffB��HB�
=B�33B홚B�{B�z�B���B�
=B�G�B�B�{B�z�B��HB�33B�p�B�B�{B��B���B�
=B�B�{B�Q�B�z�B��HB�\)B��
B�(�B�ffB���B�
=B��B�  B�=qB�z�B��HB�\)B�B�  B�=qB��RB�33B�\)B�B�=qB���B���B�G�B��
B�  B�Q�B��HB�\)B��B��C G�C ffC z�C ��C
=C(�CQ�C��CC�HC�CffC�\C��C�C33CQ�Cz�C��C��C{C\)C�\C�C��C=qCQ�C��C�
C�C33Cp�C�\C�HC{C33C�\C�C�HC(�CQ�C�C��C�C	(�C	p�C	�\C	��C
{C
33C
�\C
�C
�HC33C\)C�C�
C{C=qCp�C�RC  C�C\)C��C�
C��CQ�C�C��C��C(�CG�C��CC  C=qCQ�C�C�
C
=CQ�CffC�RC�C{Cp�C�CC
=C�CffC�CC
=CQ�CffC�C�HC{C\)Cp�CC��C{Cp�C��C�RC
=C=qC\)C�RC�HC
=C\)C�C�RC��C�Cp�C��C�
C
=C33C�\C��C�HC(�C=qC�C�RC�C=qCQ�C�\C�
C�C33Cp�C�\C�HC
=C33Cz�C�\C�HC{C=qC�\C��C�HC {C =qC �\C ��C �C!{C!G�C!�\C!��C!��C"
=C"Q�C"�\C"�C#  C#
=C#\)C#�C#�C$  C${C$Q�C$�C$�C%  C%{C%Q�C%�\C%�C%�HC&(�C&\)C&z�C&��C&�HC'(�C'\)C'p�C'�C(  C({C(G�C(��C(�C(�HC)33C)=qC)�C)��C)�
C*�C*\)C*z�C*C*��C+�C+ffC+�C+�RC+��C,
=C,Q�C,�\C,��C,�C-�C-=qC-�\C-��C-�HC.{C.=qC.�C.��C.��C/
=C/=qC/�C/��C/�C0
=C0G�C0z�C0��C0�C1
=C133C1�C1�\C1�HC2  C233C2z�C2�C2�
C3
=C3(�C3z�C3��C3��C4�C433C4z�C4��C4�HC5�C5=qC5�\C5��C5��C6  C6\)C6p�C6C6�
C7�C7G�C7z�C7�RC7�HC8�C8G�C8�\C8��C9  C9
=C9ffC9p�C9��C9�HC:(�C:G�C:��C:�RC;  C;(�C;p�C;�C;�HC;��C<G�C<ffC<�C<�
C=�C=G�C=z�C=C=�
C>33C>G�C>�\C>C>��C?=qC?Q�C?�RC?�
C@{C@Q�C@p�C@��C@�CA33CA\)CA��CA�HCB  CBQ�CBp�CBCB�CC(�CCffCC��CC�HCD  CD\)CDz�CDCD��CE(�CEz�CE�\CE�CF�CFQ�CF��CF�RCG{CG33CG�CGCG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                       ?�=q@�@B�\@�  @�p�@�  @�\A ��A��A ��A,��A@��A`  A�  A�  A�Q�A�Q�A�  AϮA�\)A�\)A��B  B�
B  B Q�B(  B0  B7�
B?�
BH  BP  BX(�B`(�Bg�
Bp  Bx(�B�  B�{B�  B�  B��B��B��
B�B��
B��B�  B��B��B�  B�  B��
B��
B�  B��B��B�{B�{B��B�  B�{B�{B�{B�{B�  B�  B�{B�  B��C  C
=C  C  C
  C��C
=C  C�C�C��C  C
=C
=C  C 
=C"  C#��C%��C(  C*
=C,
=C.
=C0  C1��C4  C6  C8  C:
=C<  C>
=C@{CB�CD{CF  CH  CJ  CL  CN
=CP  CQ��CS��CV
=CX{CZ  C\  C^
=C`
=Cb  Cd  Cf  Ch  Cj{Cl{Cn
=Cp  Cq��Ct  Cv
=Cx{Cz
=C|  C~  C��C�C�  C���C���C�  C�C�  C���C�  C�  C���C���C�C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C���C�  C�  C�  C�  C���C���C�C�
=C�  C��C��C���C���C���C���C���C���C���C���C�  C�  C�  C�C�  C���C��C�  C���C���C���C���C�  C�C���C�  C�C�
=C�
=C�C�C�C�
=C�
=C�  C���C�  C�  C�  C�C�C�  C�  C�C�  C�C�C���C���C�  C�C�  C���C���C�C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C���C���C�  C�  C�  C�C�C���C���C�C�C���C���C�  C�C�  C���C�  C�C�\C�C���C���C���D ��D  Dz�D�D��D�qDz�D��D� D  D� D  D��D  D}qD�qD��D	D	��D
  D
� D�D� D�qD� D�qD}qD  D� D�D��D�qD� DD��D�D��D�D��D  D}qD�D��D�qD}qD�D��D�qD��D�D}qD�D��D  D��D�RD}qD�qD��D�D� D�qD}qD�qD � D!  D!��D"  D"}qD"�qD#� D$�D$� D%  D%� D&  D&}qD&�qD'��D(D(� D(�RD)z�D)��D*z�D+  D+��D,�D,� D,�qD-z�D.  D.}qD/  D/�D0�D0��D1  D1� D1�qD2� D3  D3� D4�D4��D5D5�D6�D6��D6��D7� D8�D8� D9  D9}qD:  D:� D;  D;� D;�qD<� D=  D=}qD=�qD>z�D?  D?��D@�D@� D@�qDA}qDB  DB� DB�qDC� DD  DDz�DD�qDExRDE��DF��DG�DG�DH�DH� DH�qDI}qDJ  DJ��DK  DK}qDK�qDL� DM�DM��DM�qDN}qDO�DO��DP�DP��DQ  DQ� DR  DR��DR�qDSz�DS�qDT}qDU  DU� DV  DV� DW  DW}qDX  DX��DY  DY��DZ�DZ�D[�D[��D\D\� D\�qD]� D^  D^xRD^�qD_��D`D`� Da�Da�DbDb�Dc  Dc� Dd  Dd��De�De� Df  Df��Df�qDg� Dh  Dh}qDi  Di��Dj�Dj}qDk  Dk��Dl  Dl}qDm  Dm� Dm�qDn� Do  Do}qDo�qDp� Dq  Dq��Dr  Dr� Ds�Ds� Ds�qDt��Du  Du}qDu�qDv� Dv�qDw� Dx�Dx� Dy  Dy� Dz�Dz��D{  D{}qD|  D|��D}  D}}qD}��D~xRD~�qD��D��D�@ D��HD��HD�HD�B�D���D��HD�  D�>�D�}qD�� D��D�@ D�~�D�� D�  D�AHD��HD�� D�  D�B�D�~�D�� D�  D�@ D�� D�� D�HD�@ D��HD�D�  D�>�D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D���D�=qD�� D�D�  D�>�D�~�D�� D���D�=qD�~�D�� D�  D�AHD��HD���D�  D�AHD��HD�� D���D�>�D�~�D���D�  D�B�D��HD���D���D�AHD��HD�� D�HD�AHD�~�D��qD��)D�=qD�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�AHD���D��HD��qD�<)D�}qD�� D��D�AHD�� D��HD��D�AHD�� D���D���D�@ D��HD���D���D�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D�~�D��qD�  D�AHD�� D���D��qD�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D�}qD���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�~�D��HD�HD�@ D��HD�� D�  D�@ D�~�D��qD��qD�>�D�}qD�� D�HD�@ D�� D�� D�  D�>�D��HD��HD�HD�AHD�� D��HD���D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�~�D���D��D�@ D�~�D�� D���D�=qD�� D��HD���D�@ D�~�D���D�  D�AHD�� D���D�HD�AHD��HD�� D�HD�B�D��HD�� D���D�@ D�� D���D�  D�AHD���D��HD���D�@ D�~�D���D���D�@ D��HD�� D�  D�AHD��HD�� D�  D�AHD�� D���D�  D�@ D�� D��HD�  D�=qD D�� D���D�@ DÁHD�� D���D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHDƁHDƾ�D��qD�>�Dǀ DǾ�D���D�AHDȂ�D��HD�HD�@ DɁHD�D��D�>�D�~�D�� D�  D�@ Dˀ D�� D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�>�D�~�Dξ�D�  D�>�D�~�D�� D�  D�@ D�~�D��HD�  D�<)D�~�D�� D�HD�AHD�}qDҾ�D�HD�AHDӂ�D��HD���D�@ DԀ DԾ�D�HD�AHDՀ DսqD�HD�B�DցHD��HD�  D�>�D�~�D�� D�  D�AHD؀ Dؾ�D�  D�@ D�~�Dپ�D�  D�@ Dڂ�D��HD�  D�AHDہHD�� D���D�@ D܀ D�D�HD�@ D�~�D�� D��D�AHDށHD�� D���D�@ D�~�D߼)D���D�AHD�� D�qD�  D�B�DႏD�� D���D�>�D�~�D�� D��D�AHD�~�D㾸D�HD�AHD�~�D侸D�  D�AHD傏D��HD�HD�@ D�~�D�� D�  D�@ D�HD��HD�  D�=qD�}qD�� D�HD�B�D� D龸D�  D�AHD�HD�� D���D�>�D� D�� D�  D�>�D�~�D��HD��D�@ D� D��HD���D�>�D�~�DD�  D�@ D�}qDﾸD�HD�AHD�~�D�D�  D�>�D�~�D�� D�HD�AHD�~�D�D�  D�AHD�HD��HD�HD�>�D�~�D���D���D�>�D�}qD���D�  D�>�D�}qD��qD��qD�>�D�� D�� D�HD�@ D�~�D���D��qD�=qD�� D��HD�  D�@ D�� G�O�>�G�?8Q�?aG�?�z�?�p�?�@�@��@&ff@5@J=q@^�R@s33@��@��@�33@�p�@�=q@�@��R@Ǯ@�\)@�Q�@�G�@�\)@���A�AffA�A�RA33AQ�Ap�A"�\A'
=A+�A0  A4z�A7�A<(�A?\)AE�AJ=qAL��AP��AR�\AU�AW�AZ�HA^�RAa�Ag
=Ai��Al(�An�RAqG�As�
Aw
=Ax��A|��A���A��\A�(�A�A��RA�Q�A���A�33A�{A�\)A���A�33A�(�A�A��RA���A��\A���A�
=A�  A��A��HA���A�ffA��A��A�33A�p�A��A���A��\A�(�A�p�A�\)A���A�33A��A�  A��A��
A�p�AƸRA�G�Aʏ\A�z�AθRA���A��HA���A�ffA�  Aٙ�A�33A��A�
=A�G�A�33A�A�  A�\A���A�ffA��A�\A���A�ffA�Q�A��\A���A�
=B ��B�B33BQ�BG�BffB33B(�B	�B
{B
=B  B�BffB�B��Bp�B=qB33B  B�B{B\)BQ�B��B�HB�
B��BB�HB   B ��B"{B#33B$(�B%p�B&ffB'�B(z�B*{B+\)B,Q�B-G�B.ffB/\)B0Q�B1p�B2=qB3�B4��B5�B733B8Q�B9��B:�RB<  B=G�B>ffB?33B@Q�BAG�BB=qBC33BDQ�BEG�BF=qBG33BH(�BIG�BJ�\BK�BL��BN{BO33BPQ�BQp�BR�\BS\)BTz�BUp�BV�\BW�BX��BY��BZ�RB[�B\��B^{B_33B`Q�Bap�Bb�\Bc�
Be�Bf=qBg\)Bhz�Bip�Bj=qBk\)BlQ�Bmp�Bn�\Bo\)Bp��BqBr�RBt  Bu�Bv=qBw\)Bxz�By��Bz�HB|  B}�B~=qB\)B�(�B��RB�G�B��
B�Q�B��HB�p�B�  B��\B�
=B��B�  B��\B�
=B���B�{B���B�G�B��B�=qB���B�G�B�B�Q�B���B�\)B��
B�Q�B��HB�p�B��B�Q�B��HB�p�B�  B�z�B�
=B��B�(�B���B�33B��B�=qB��RB�G�B��
B�ffB���B��B�{B��RB��B��B�(�B��\B�
=B��B�  B�z�B�
=B���B�(�B��RB�G�B�B�ffB�
=B��B�  B��\B�33B���B�{B���B��B�B�ffB���B��B�Q�B��HB�p�B��B��\B�
=B���B�(�B���B�p�B�{B��RB�\)B��B�z�B�
=B��B�{B���B�33B�B�ffB�
=B��B�(�B��HB�p�B�{B��HB�\)B��
B�ffB��HB�p�B�  B���B�G�B��
B�z�B��B��B�Q�B���B�G�B��
B�Q�B���BŅB�  Bƣ�B�G�B��B�z�B�
=BɅB�  B�z�B�
=BˮB�Q�B��HB͙�B�=qB���B�\)B��B�Q�B���B�\)B�  Bҏ\B�33B�B�ffB�
=BՅB�  B�z�B�
=B׮B�ffB���BٮB�(�Bڣ�B��BۮB�ffB�
=B�B�Q�B���B�G�B�B�z�B�33B��
B�ffB���B�\)B�{B���B�p�B��B�ffB��HB�B�ffB���B�33B�B�Q�B���B�33B�B��
B�ffB��HB�
=B�33B홚B�{B�z�B���B�
=B�G�B�B�{B�z�B��HB�33B�p�B�B�{B��B���B�
=B�B�{B�Q�B�z�B��HB�\)B��
B�(�B�ffB���B�
=B��B�  B�=qB�z�B��HB�\)B�B�  B�=qB��RB�33B�\)B�B�=qB���B���B�G�B��
B�  B�Q�B��HB�\)B��B��C G�C ffC z�C ��C
=C(�CQ�C��CC�HC�CffC�\C��C�C33CQ�Cz�C��C��C{C\)C�\C�C��C=qCQ�C��C�
C�C33Cp�C�\C�HC{C33C�\C�C�HC(�CQ�C�C��C�C	(�C	p�C	�\C	��C
{C
33C
�\C
�C
�HC33C\)C�C�
C{C=qCp�C�RC  C�C\)C��C�
C��CQ�C�C��C��C(�CG�C��CC  C=qCQ�C�C�
C
=CQ�CffC�RC�C{Cp�C�CC
=C�CffC�CC
=CQ�CffC�C�HC{C\)Cp�CC��C{Cp�C��C�RC
=C=qC\)C�RC�HC
=C\)C�C�RC��C�Cp�C��C�
C
=C33C�\C��C�HC(�C=qC�C�RC�C=qCQ�C�\C�
C�C33Cp�C�\C�HC
=C33Cz�C�\C�HC{C=qC�\C��C�HC {C =qC �\C ��C �C!{C!G�C!�\C!��C!��C"
=C"Q�C"�\C"�C#  C#
=C#\)C#�C#�C$  C${C$Q�C$�C$�C%  C%{C%Q�C%�\C%�C%�HC&(�C&\)C&z�C&��C&�HC'(�C'\)C'p�C'�C(  C({C(G�C(��C(�C(�HC)33C)=qC)�C)��C)�
C*�C*\)C*z�C*C*��C+�C+ffC+�C+�RC+��C,
=C,Q�C,�\C,��C,�C-�C-=qC-�\C-��C-�HC.{C.=qC.�C.��C.��C/
=C/=qC/�C/��C/�C0
=C0G�C0z�C0��C0�C1
=C133C1�C1�\C1�HC2  C233C2z�C2�C2�
C3
=C3(�C3z�C3��C3��C4�C433C4z�C4��C4�HC5�C5=qC5�\C5��C5��C6  C6\)C6p�C6C6�
C7�C7G�C7z�C7�RC7�HC8�C8G�C8�\C8��C9  C9
=C9ffC9p�C9��C9�HC:(�C:G�C:��C:�RC;  C;(�C;p�C;�C;�HC;��C<G�C<ffC<�C<�
C=�C=G�C=z�C=C=�
C>33C>G�C>�\C>C>��C?=qC?Q�C?�RC?�
C@{C@Q�C@p�C@��C@�CA33CA\)CA��CA�HCB  CBQ�CBp�CBCB�CC(�CCffCC��CC�HCD  CD\)CDz�CDCD��CE(�CEz�CE�\CE�CF�CFQ�CF��CF�RCG{CG33CG�CGCG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                       @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�M�A�\)A�hsA�hsA�hsA�hsA�hsA�ffA�ffA�hsA�ffA�dZA�jA�^5A�I�A�^5A�l�A�`BA�C�A�E�A�I�A�A�A��A���A��#A��DA�5?A�{A��`A��RA���A���A��+A�v�A�hsA�XA�=qA�
=A��/A���A��A�M�A���A�;dA���A��#A���A�9XA�  A�ZA��A�+A���A�dZA�5?A��
A���A��A��`A���A���A�K�A�9XA�v�A�1'A��+A��A���A��!A�"�A��9A��A�K�A���A��A��A��A���A�1A��^A�jA��/A��
A�I�A��`A"�A~$�A}33A|ĜA{S�Az^5Ax�Av�HAs�wAr��Ar^5Aq�mAp��Am"�Aj�AjZAi��AiC�Ah�jAh^5Ah1Ag�#Ag�PAg/Ae�Aa\)A`�+A`=qA`  A_��A^9XA]7LA\I�A[p�AZv�AYVAX{AV(�AU;dAT��AT��ATn�AT5?AT1AS�-AS&�AQ�wAO�TAN��AN9XAM�AL�!AKhsAJȴAJz�AI�AHffAG�#AGƨAG��AG;dAF��AEAE+AD�\AB��AA\)A?��A=dZA;��A:-A9K�A7��A6ĜA6JA5�7A5�A4�A4�9A4ZA4bA3�PA2��A2$�A1dZA0�yA0��A0M�A/��A/
=A.(�A-��A-�A,�HA,��A,$�A+hsA*�+A)��A)t�A(��A'��A%XA$��A#�PA#+A"5?A ��A A�A��AXAȴA$�Ax�A��A-A��A/A��A�A�RA �A�A�mAVAA�A|�AO�A�A��A�AbNAJA�-A`BA�jA?}A�jAVAA�;A��A+A��A��A
�9A	�A  A�A�A�DA��A33AE�A�hA�AA�A��A��A\)A ȴA 5?@�C�@��@�J@�ȴ@��9@�ȴ@��#@�&�@�bN@�|�@�E�@��@���@���@�R@�  @�+@�@���@��@�=q@�%@�Q�@�C�@�G�@�33@�S�@�7L@���@���@�9X@�^5@д9@� �@�M�@̬@��;@˝�@�
=@�$�@�x�@�hs@�X@��@�Z@���@�t�@��@�v�@ř�@öF@\@��@���@��`@�V@�&�@���@��\@�5?@���@��@� �@�V@�p�@�  @�V@��@��u@��;@�V@���@�O�@��9@���@�n�@��T@��7@�O�@��j@�9X@���@�C�@��@��!@�J@���@� �@���@�@��!@��#@�/@�r�@�  @�S�@��@��T@��7@���@��u@�9X@��@��
@�ȴ@��#@��@�&�@���@�bN@�(�@�  @���@��@��P@�l�@�K�@��H@�=q@�`B@�Ĝ@�b@�dZ@���@�V@��T@�?}@��j@�1'@���@�|�@�l�@�l�@�\)@�C�@�;d@�+@��@���@��R@�n�@��T@�p�@�?}@��@�V@���@�Z@��@�t�@�S�@�K�@�;d@�+@��@�M�@��#@�X@���@��D@�1'@�b@��m@��@�o@���@��R@��!@�n�@�E�@�=q@���@��T@��-@��@�X@���@�r�@�Q�@�1'@��@�1@�  @��m@�ƨ@�l�@�ȴ@�=q@��#@��7@�7L@�&�@���@���@���@��u@\)@~��@~5?@~@}�@}�@}�T@}�T@}��@}�-@}�h@}�@}p�@}p�@}?}@|�D@{��@z�\@zJ@y��@y��@yG�@x��@x��@xQ�@w�;@w�w@w��@wl�@w;d@v��@v�+@v5?@u@u�@t��@t�@s��@s��@s��@sdZ@s@r��@r�@q��@q7L@q%@p��@p�@pQ�@p1'@o�;@o�;@o�w@o+@n��@n5?@m�T@m�T@m��@m@m�@l��@l��@j��@j=q@j-@j=q@j-@j-@j�@i��@i�@i�^@i�7@i&�@i�@h�`@h��@hQ�@g�@f5?@e@e?}@d��@d�D@dj@d9X@d(�@d(�@d�@c�
@bn�@a��@a�7@aX@a7L@a%@`�`@`�9@`A�@_|�@_�@^ȴ@^��@]`B@\��@\�D@\9X@[�
@[t�@Z��@Z�\@Z^5@Y�@Y�@X��@XĜ@Xr�@X  @W�;@W�w@W�P@W
=@V��@U�@U�-@U�@U?}@T�@T�@T�@Sƨ@S��@SdZ@S33@S"�@S33@S33@S33@So@R��@R�\@R^5@Q�^@P��@PĜ@P��@P�@P1'@P  @O�P@O;d@N��@NV@M�@M��@Mp�@MO�@M�@L�@L��@L�j@L��@Lz�@L(�@K��@K��@K��@K��@K�@K�@K��@K��@KC�@J�@Ko@K"�@K"�@Ko@J�!@JM�@J-@J�@I��@I��@I�7@I�@HĜ@H�u@H�@Hr�@HbN@HA�@Hb@G�@G�w@G�w@G�@G�P@Gl�@G\)@G\)@G;d@G+@G+@G�@F��@Fȴ@F��@F�+@FV@FE�@F{@E@E@E�h@EO�@EV@D�@D��@D�@CdZ@B�!@A��@A��@A��@A��@A&�@@�`@@��@@�9@@��@@A�@?l�@?K�@?+@>�@>ff@>V@>E�@>5?@>@=@=`B@=/@<�@<�j@<z�@<I�@;�m@;C�@:J@9�@9��@9X@97L@9�@8��@8�`@8��@8Ĝ@8��@8�@8Q�@7�w@7+@6��@6��@6�+@6ff@6E�@6$�@6{@5�@5�@5/@4��@4�D@4j@3�m@3��@3��@3��@3�@3S�@333@2��@2n�@2^5@2=q@1�@1��@1hs@1X@0��@0bN@0  @/\)@/;d@/;d@/+@/+@/�@.��@.��@.�+@.V@.5?@-�T@-�h@-�@,�/@,��@,�@,9X@,1@+��@+�@+t�@+S�@+"�@+"�@+o@+@+@*�@*^5@*=q@*-@*�@)�@)&�@(1'@'�;@'|�@'+@'�@'�@'
=@&��@&�y@&ȴ@&�+@&v�@&{@%��@%�@$�@$��@$z�@$9X@#��@#�@#S�@#o@"�!@"�\@"~�@"=q@"-@"=q@"=q@"=q@!�@!�^@!x�@!�@ �u@��@\)@+@�y@ȴ@��@�+@V@5?@@�T@��@��@`B@`B@`B@?}@��@��@�j@�D@j@j@Z@9X@��@dZ@33@�@�H@��@n�@�@J@��@�#@��@�7@hs@hs@G�@&�@��@��@r�@Q�@b@��@��@K�@+@�@
=@
=@��@��@�y@�@ȴ@��@E�@��@�-@��@`B@O�@V@�j@Z@�
@�@S�@"�@@��@M�@-@�@�@��@�^@�^@��@��@x�@hs@G�@7L@&�@&�@�@��@��@��@�u@Q�@1'@ �@b@b@b@�;@|�@l�@\)@\)@\)@K�@;d@�@�R@V@$�@@@�h@�h@p�@O�@/@/@V@�/@�@j@I�@1@�
@dZ@S�@"�@
�@
��@
��@
=q@	�#@	��@	x�@	G�@��@��@r�@Q�@Q�@A�@A�@1'@ �@  @�;@��@|�@;d@�@
=@
=@
=@�y@�@�R@��@�+@V@{@@�h@�@p�@/@�/@�@Z@��@�
@��@��@�@dZ@S�@33@�H@n�@�#@�#@�#@��@�^@��@��@�^@��@��@�7@X@&�@�A�K�A�O�A�XA�S�A�K�A�K�A�M�A�Q�A�XA�XA�XA�dZA�hsA�jA�jA�hsA�ffA�ffA�ffA�hsA�jA�jA�hsA�ffA�dZA�ffA�hsA�jA�hsA�ffA�ffA�dZA�ffA�ffA�hsA�jA�jA�hsA�hsA�dZA�ffA�ffA�ffA�hsA�hsA�dZA�dZA�dZA�bNA�bNA�bNA�ffA�hsA�hsA�hsA�p�A�n�A�l�A�ffA�hsA�ffA�ffA�jA�l�A�n�A�l�A�ffA�ffA�dZA�ffA�bNA�ZA�VA�M�A�G�A�I�A�E�A�G�A�I�A�C�A�C�A�C�A�I�A�M�A�`BA�dZA�XA�ZA�\)A�\)A�ffA�dZA�bNA�ZA�ffA�hsA�l�A�n�A�n�A�p�A�l�A�l�A�jA�hsA�hsA�`BA�jA�bNA�dZA�=qA�G�A�E�A�C�A�7LA�1'A�Q�A�Q�A�=qA�?}A�E�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�G�A�I�A�E�A�G�A�G�A�G�A�E�A�?}A�A�A�G�A�A�A�E�A�A�A�9XA�;dA�A�A�9XA� �A��`A��mA��TA��A���A��
A���A���A���A���A���A��HA��`A��;A��/A��/A��A���A�ƨA���A��!A���A�bNA�\)A�^5A�K�A�G�A�?}A�A�A�G�A��A�$�A��A��A�$�A�"�A��A�A���A���A���A��A��A��A��A��A�A���A��wA��^A��FA��-A��-A��!A��!A��A��A��A��A���A���A���A���A���A���A���A���A��uA��hA��7A��7A��DA��A��A��A��A�~�A�|�A�z�A�x�A�t�A�t�A�r�A�p�A�n�A�l�A�jA�hsA�ffA�dZA�bNA�^5A�^5A�\)A�XA�VA�S�A�Q�A�M�A�M�A�K�A�G�A�?}A�;dA�33A�/A�&�A��A�bA�
=A�A�A�A�A�A���A���A��A���A��!A��A�`BA�5?A���A��
A��A��7A�l�A�VA�9XA�/A�&�A��A��A�1A��TA��A��+A�r�A�`BA�M�A�1'A�JA���A��A���A��DA�$�A���A�`BA��
A�^5A��#A�VA�1'A�oA��/A��A���A�ffA�;dA�VA���A��#A�\)A�33A���A�(�A��/A���A���A�ƨA���A���A���A�r�A�Q�A�I�A���A�&�A�r�A�t�A��
A���A��A���A��DA�I�A�/A�"�A��A�bA���A�A���A�x�A�ffA�dZA�bNA�S�A�7LA�oA��`A��A�dZA�bNA�Q�A�Q�A�M�A�G�A�=qA�7LA��A�
=A��A��A�/A��mA��A��A�I�A���A���A��jA��hA�jA�=qA� �A�VA��A���A�ffA�I�A�-A��A��PA���A�C�A���A���A���A�r�A�\)A�M�A�oA��A��A��wA��!A���A�dZA�K�A��A�ƨA�ffA�A�A�/A�{A��A�M�A��!A�v�A�I�A�"�A�VA��A��mA��HA��
A���A��hA�"�A���A�G�A�A���A��A�=qA���A��!A�l�A�+A�oA���A��mA���A��A�1'A��A���A��`A���A��A���A��uA��+A�r�A�`BA�VA�S�A�O�A�Q�A�O�A�M�A�O�A�I�A�C�A�A�A�?}A�=qA�+A�{A���A��`A��#A���A���A�A��RA��A���A�`BA�K�A��A�%A���A��HA�ƨA��wA��^A���A���A���A���A���A��A�|�A�\)A�\)A�M�A�9XA�33A�(�A�1A���A�Q�A�1'A�JA��A��HA��#A���A��A���A�z�A�l�A�p�A�hsA�bNA�\)A�XA�K�A�+A��A�bA��A��A�A��hA�K�A�+A�A�A���A�\)A�$�A�oA�JA���A��A���A��A���A�r�A� �A�A���A���A���A��PA�ffA�XA�?}A�-A��A�1A���A��`A��#A��-A���A��DA�z�A�jA�bNA�A�A�1'A� �A�"�A��A�
=A��wA��hA�?}A�=qA�(�A��A��A�oA�oA�oA�JA�VA�VA�
=A�
=A�
=A�  A���A��mA��
A���A���A���A�ĜA���A��jA��-A���A���A�x�A�XA�5?A�+A�$�A��A�
=A�%A�A��A��;A���A���A���A�ƨA�ƨA���A���A��-A���A��\A��DA�x�A�r�A�p�A�ffA�ffA�jA�bNA�`BA�G�A�/A��A�A��/A��^A��^A��RA��-A���A��PA�ZA�A���A��
A��A��uA��7A��A�x�A�hsA�ZA�O�A�G�A�A�A�?}A�=qA�;dA�;dA�7LA�7LA�5?A�1'A�"�A��A���A��!A��PA�E�A�^Ap�AK�AC�A;dA�AA~��A~ȴA~�RA~��A~�A~^5A~Q�A~A�A~1A}�A}��A}�-A}��A}p�A};dA}33A}/A}+A}�A}�A}�A}"�A}+A}"�A}
=A|��A|��A|�A|��A|��A|��A|�uA|�\A|v�A|�A{|�A{?}A{+A{�A{Az��A{%Az�yAz�yAz��Az�jAz�Az�+AzZAz �Ay��Ay�#Ay�FAy�7Ayx�AyO�Ay"�AyoAx�Ax��Ax��Axz�Ax1'Ax{Ax{AxAw�#Aw7LAv�jAvz�AvAu��Au`BAu?}At�AtZAs�TAs�7AsS�AsS�As7LAs+As�As%As
=AsoAr��Ar�`ArȴArĜAr�!Ar��Ar�uAr�+Ar~�Arr�Arn�ArjAr^5ArQ�ArE�Ar5?Ar$�Ar(�Ar �Ar �Ar�ArbAq�mAq��Aq�FAq��Aq�Aqp�Aqp�AqXAq?}Aq33Ap��Ap�HApȴApQ�Ao�Aol�An�AnA�Am�PAl��Al(�Ak�;Ak��Akx�AkXAk33AkoAj��Aj�Aj�/Aj��Aj��Aj�RAj��Aj��Aj�\Aj�+Ajr�Aj^5AjM�Aj5?Aj$�Aj�Aj  Ai��Ai�;Ai�#Ai��Ai��Ai��Ai�^Ai��Ai�7Ai�Ai|�AihsAiXAi+Ai%Ah�Ah�Ah�HAh�/AhȴAh�jAh�RAh�!Ah��Ah��Ah�+Ah~�Ahr�Ahn�AhbNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                       A�S�A�M�A�\)A�hsA�hsA�hsA�hsA�hsA�ffA�ffA�hsA�ffA�dZA�jA�^5A�I�A�^5A�l�A�`BA�C�A�E�A�I�A�A�A��A���A��#A��DA�5?A�{A��`A��RA���A���A��+A�v�A�hsA�XA�=qA�
=A��/A���A��A�M�A���A�;dA���A��#A���A�9XA�  A�ZA��A�+A���A�dZA�5?A��
A���A��A��`A���A���A�K�A�9XA�v�A�1'A��+A��A���A��!A�"�A��9A��A�K�A���A��A��A��A���A�1A��^A�jA��/A��
A�I�A��`A"�A~$�A}33A|ĜA{S�Az^5Ax�Av�HAs�wAr��Ar^5Aq�mAp��Am"�Aj�AjZAi��AiC�Ah�jAh^5Ah1Ag�#Ag�PAg/Ae�Aa\)A`�+A`=qA`  A_��A^9XA]7LA\I�A[p�AZv�AYVAX{AV(�AU;dAT��AT��ATn�AT5?AT1AS�-AS&�AQ�wAO�TAN��AN9XAM�AL�!AKhsAJȴAJz�AI�AHffAG�#AGƨAG��AG;dAF��AEAE+AD�\AB��AA\)A?��A=dZA;��A:-A9K�A7��A6ĜA6JA5�7A5�A4�A4�9A4ZA4bA3�PA2��A2$�A1dZA0�yA0��A0M�A/��A/
=A.(�A-��A-�A,�HA,��A,$�A+hsA*�+A)��A)t�A(��A'��A%XA$��A#�PA#+A"5?A ��A A�A��AXAȴA$�Ax�A��A-A��A/A��A�A�RA �A�A�mAVAA�A|�AO�A�A��A�AbNAJA�-A`BA�jA?}A�jAVAA�;A��A+A��A��A
�9A	�A  A�A�A�DA��A33AE�A�hA�AA�A��A��A\)A ȴA 5?@�C�@��@�J@�ȴ@��9@�ȴ@��#@�&�@�bN@�|�@�E�@��@���@���@�R@�  @�+@�@���@��@�=q@�%@�Q�@�C�@�G�@�33@�S�@�7L@���@���@�9X@�^5@д9@� �@�M�@̬@��;@˝�@�
=@�$�@�x�@�hs@�X@��@�Z@���@�t�@��@�v�@ř�@öF@\@��@���@��`@�V@�&�@���@��\@�5?@���@��@� �@�V@�p�@�  @�V@��@��u@��;@�V@���@�O�@��9@���@�n�@��T@��7@�O�@��j@�9X@���@�C�@��@��!@�J@���@� �@���@�@��!@��#@�/@�r�@�  @�S�@��@��T@��7@���@��u@�9X@��@��
@�ȴ@��#@��@�&�@���@�bN@�(�@�  @���@��@��P@�l�@�K�@��H@�=q@�`B@�Ĝ@�b@�dZ@���@�V@��T@�?}@��j@�1'@���@�|�@�l�@�l�@�\)@�C�@�;d@�+@��@���@��R@�n�@��T@�p�@�?}@��@�V@���@�Z@��@�t�@�S�@�K�@�;d@�+@��@�M�@��#@�X@���@��D@�1'@�b@��m@��@�o@���@��R@��!@�n�@�E�@�=q@���@��T@��-@��@�X@���@�r�@�Q�@�1'@��@�1@�  @��m@�ƨ@�l�@�ȴ@�=q@��#@��7@�7L@�&�@���@���@���@��u@\)@~��@~5?@~@}�@}�@}�T@}�T@}��@}�-@}�h@}�@}p�@}p�@}?}@|�D@{��@z�\@zJ@y��@y��@yG�@x��@x��@xQ�@w�;@w�w@w��@wl�@w;d@v��@v�+@v5?@u@u�@t��@t�@s��@s��@s��@sdZ@s@r��@r�@q��@q7L@q%@p��@p�@pQ�@p1'@o�;@o�;@o�w@o+@n��@n5?@m�T@m�T@m��@m@m�@l��@l��@j��@j=q@j-@j=q@j-@j-@j�@i��@i�@i�^@i�7@i&�@i�@h�`@h��@hQ�@g�@f5?@e@e?}@d��@d�D@dj@d9X@d(�@d(�@d�@c�
@bn�@a��@a�7@aX@a7L@a%@`�`@`�9@`A�@_|�@_�@^ȴ@^��@]`B@\��@\�D@\9X@[�
@[t�@Z��@Z�\@Z^5@Y�@Y�@X��@XĜ@Xr�@X  @W�;@W�w@W�P@W
=@V��@U�@U�-@U�@U?}@T�@T�@T�@Sƨ@S��@SdZ@S33@S"�@S33@S33@S33@So@R��@R�\@R^5@Q�^@P��@PĜ@P��@P�@P1'@P  @O�P@O;d@N��@NV@M�@M��@Mp�@MO�@M�@L�@L��@L�j@L��@Lz�@L(�@K��@K��@K��@K��@K�@K�@K��@K��@KC�@J�@Ko@K"�@K"�@Ko@J�!@JM�@J-@J�@I��@I��@I�7@I�@HĜ@H�u@H�@Hr�@HbN@HA�@Hb@G�@G�w@G�w@G�@G�P@Gl�@G\)@G\)@G;d@G+@G+@G�@F��@Fȴ@F��@F�+@FV@FE�@F{@E@E@E�h@EO�@EV@D�@D��@D�@CdZ@B�!@A��@A��@A��@A��@A&�@@�`@@��@@�9@@��@@A�@?l�@?K�@?+@>�@>ff@>V@>E�@>5?@>@=@=`B@=/@<�@<�j@<z�@<I�@;�m@;C�@:J@9�@9��@9X@97L@9�@8��@8�`@8��@8Ĝ@8��@8�@8Q�@7�w@7+@6��@6��@6�+@6ff@6E�@6$�@6{@5�@5�@5/@4��@4�D@4j@3�m@3��@3��@3��@3�@3S�@333@2��@2n�@2^5@2=q@1�@1��@1hs@1X@0��@0bN@0  @/\)@/;d@/;d@/+@/+@/�@.��@.��@.�+@.V@.5?@-�T@-�h@-�@,�/@,��@,�@,9X@,1@+��@+�@+t�@+S�@+"�@+"�@+o@+@+@*�@*^5@*=q@*-@*�@)�@)&�@(1'@'�;@'|�@'+@'�@'�@'
=@&��@&�y@&ȴ@&�+@&v�@&{@%��@%�@$�@$��@$z�@$9X@#��@#�@#S�@#o@"�!@"�\@"~�@"=q@"-@"=q@"=q@"=q@!�@!�^@!x�@!�@ �u@��@\)@+@�y@ȴ@��@�+@V@5?@@�T@��@��@`B@`B@`B@?}@��@��@�j@�D@j@j@Z@9X@��@dZ@33@�@�H@��@n�@�@J@��@�#@��@�7@hs@hs@G�@&�@��@��@r�@Q�@b@��@��@K�@+@�@
=@
=@��@��@�y@�@ȴ@��@E�@��@�-@��@`B@O�@V@�j@Z@�
@�@S�@"�@@��@M�@-@�@�@��@�^@�^@��@��@x�@hs@G�@7L@&�@&�@�@��@��@��@�u@Q�@1'@ �@b@b@b@�;@|�@l�@\)@\)@\)@K�@;d@�@�R@V@$�@@@�h@�h@p�@O�@/@/@V@�/@�@j@I�@1@�
@dZ@S�@"�@
�@
��@
��@
=q@	�#@	��@	x�@	G�@��@��@r�@Q�@Q�@A�@A�@1'@ �@  @�;@��@|�@;d@�@
=@
=@
=@�y@�@�R@��@�+@V@{@@�h@�@p�@/@�/@�@Z@��@�
@��@��@�@dZ@S�@33@�H@n�@�#@�#@�#@��@�^@��@��@�^@��@��@�7@X@&�G�O�A�K�A�O�A�XA�S�A�K�A�K�A�M�A�Q�A�XA�XA�XA�dZA�hsA�jA�jA�hsA�ffA�ffA�ffA�hsA�jA�jA�hsA�ffA�dZA�ffA�hsA�jA�hsA�ffA�ffA�dZA�ffA�ffA�hsA�jA�jA�hsA�hsA�dZA�ffA�ffA�ffA�hsA�hsA�dZA�dZA�dZA�bNA�bNA�bNA�ffA�hsA�hsA�hsA�p�A�n�A�l�A�ffA�hsA�ffA�ffA�jA�l�A�n�A�l�A�ffA�ffA�dZA�ffA�bNA�ZA�VA�M�A�G�A�I�A�E�A�G�A�I�A�C�A�C�A�C�A�I�A�M�A�`BA�dZA�XA�ZA�\)A�\)A�ffA�dZA�bNA�ZA�ffA�hsA�l�A�n�A�n�A�p�A�l�A�l�A�jA�hsA�hsA�`BA�jA�bNA�dZA�=qA�G�A�E�A�C�A�7LA�1'A�Q�A�Q�A�=qA�?}A�E�A�G�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�G�A�I�A�E�A�G�A�G�A�G�A�E�A�?}A�A�A�G�A�A�A�E�A�A�A�9XA�;dA�A�A�9XA� �A��`A��mA��TA��A���A��
A���A���A���A���A���A��HA��`A��;A��/A��/A��A���A�ƨA���A��!A���A�bNA�\)A�^5A�K�A�G�A�?}A�A�A�G�A��A�$�A��A��A�$�A�"�A��A�A���A���A���A��A��A��A��A��A�A���A��wA��^A��FA��-A��-A��!A��!A��A��A��A��A���A���A���A���A���A���A���A���A��uA��hA��7A��7A��DA��A��A��A��A�~�A�|�A�z�A�x�A�t�A�t�A�r�A�p�A�n�A�l�A�jA�hsA�ffA�dZA�bNA�^5A�^5A�\)A�XA�VA�S�A�Q�A�M�A�M�A�K�A�G�A�?}A�;dA�33A�/A�&�A��A�bA�
=A�A�A�A�A�A���A���A��A���A��!A��A�`BA�5?A���A��
A��A��7A�l�A�VA�9XA�/A�&�A��A��A�1A��TA��A��+A�r�A�`BA�M�A�1'A�JA���A��A���A��DA�$�A���A�`BA��
A�^5A��#A�VA�1'A�oA��/A��A���A�ffA�;dA�VA���A��#A�\)A�33A���A�(�A��/A���A���A�ƨA���A���A���A�r�A�Q�A�I�A���A�&�A�r�A�t�A��
A���A��A���A��DA�I�A�/A�"�A��A�bA���A�A���A�x�A�ffA�dZA�bNA�S�A�7LA�oA��`A��A�dZA�bNA�Q�A�Q�A�M�A�G�A�=qA�7LA��A�
=A��A��A�/A��mA��A��A�I�A���A���A��jA��hA�jA�=qA� �A�VA��A���A�ffA�I�A�-A��A��PA���A�C�A���A���A���A�r�A�\)A�M�A�oA��A��A��wA��!A���A�dZA�K�A��A�ƨA�ffA�A�A�/A�{A��A�M�A��!A�v�A�I�A�"�A�VA��A��mA��HA��
A���A��hA�"�A���A�G�A�A���A��A�=qA���A��!A�l�A�+A�oA���A��mA���A��A�1'A��A���A��`A���A��A���A��uA��+A�r�A�`BA�VA�S�A�O�A�Q�A�O�A�M�A�O�A�I�A�C�A�A�A�?}A�=qA�+A�{A���A��`A��#A���A���A�A��RA��A���A�`BA�K�A��A�%A���A��HA�ƨA��wA��^A���A���A���A���A���A��A�|�A�\)A�\)A�M�A�9XA�33A�(�A�1A���A�Q�A�1'A�JA��A��HA��#A���A��A���A�z�A�l�A�p�A�hsA�bNA�\)A�XA�K�A�+A��A�bA��A��A�A��hA�K�A�+A�A�A���A�\)A�$�A�oA�JA���A��A���A��A���A�r�A� �A�A���A���A���A��PA�ffA�XA�?}A�-A��A�1A���A��`A��#A��-A���A��DA�z�A�jA�bNA�A�A�1'A� �A�"�A��A�
=A��wA��hA�?}A�=qA�(�A��A��A�oA�oA�oA�JA�VA�VA�
=A�
=A�
=A�  A���A��mA��
A���A���A���A�ĜA���A��jA��-A���A���A�x�A�XA�5?A�+A�$�A��A�
=A�%A�A��A��;A���A���A���A�ƨA�ƨA���A���A��-A���A��\A��DA�x�A�r�A�p�A�ffA�ffA�jA�bNA�`BA�G�A�/A��A�A��/A��^A��^A��RA��-A���A��PA�ZA�A���A��
A��A��uA��7A��A�x�A�hsA�ZA�O�A�G�A�A�A�?}A�=qA�;dA�;dA�7LA�7LA�5?A�1'A�"�A��A���A��!A��PA�E�A�^Ap�AK�AC�A;dA�AA~��A~ȴA~�RA~��A~�A~^5A~Q�A~A�A~1A}�A}��A}�-A}��A}p�A};dA}33A}/A}+A}�A}�A}�A}"�A}+A}"�A}
=A|��A|��A|�A|��A|��A|��A|�uA|�\A|v�A|�A{|�A{?}A{+A{�A{Az��A{%Az�yAz�yAz��Az�jAz�Az�+AzZAz �Ay��Ay�#Ay�FAy�7Ayx�AyO�Ay"�AyoAx�Ax��Ax��Axz�Ax1'Ax{Ax{AxAw�#Aw7LAv�jAvz�AvAu��Au`BAu?}At�AtZAs�TAs�7AsS�AsS�As7LAs+As�As%As
=AsoAr��Ar�`ArȴArĜAr�!Ar��Ar�uAr�+Ar~�Arr�Arn�ArjAr^5ArQ�ArE�Ar5?Ar$�Ar(�Ar �Ar �Ar�ArbAq�mAq��Aq�FAq��Aq�Aqp�Aqp�AqXAq?}Aq33Ap��Ap�HApȴApQ�Ao�Aol�An�AnA�Am�PAl��Al(�Ak�;Ak��Akx�AkXAk33AkoAj��Aj�Aj�/Aj��Aj��Aj�RAj��Aj��Aj�\Aj�+Ajr�Aj^5AjM�Aj5?Aj$�Aj�Aj  Ai��Ai�;Ai�#Ai��Ai��Ai��Ai�^Ai��Ai�7Ai�Ai|�AihsAiXAi+Ai%Ah�Ah�Ah�HAh�/AhȴAh�jAh�RAh�!Ah��Ah��Ah�+Ah~�Ahr�Ahn�AhbNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�lB�B�B��B�fB��B��B��B�+B��B�ZB��B��B�TB��B�TB��B��B��B�TB��B��B�TB�B�%B��B�|B�B�iB��B�vB�AB�B�B�B��B��BںB�XB��B��B��BffBEB�BPB�8B�B��B��B��B��B�B~(Be�BO�BAUB1[BkB�B��B��B�B��B�B�3B��B�qB��B��B� Bm�Bc BZBMjBJ�BGEB?HB;�B6�B/B#nB�BoB��B�2B��B�B�pB՛B��B��B�6B��B�B��B��B�oBm�Bi�Be`Ba�B]�BZ�BW�BT�BQ�BK�BHB-B%�B#:B \B�BYB:B
rBB iB
�`B
�B
�B
�B
��B
�dB
�]B
چB
خB
��B
ҽB
��B
�-B
�jB
��B
�FB
�FB
�-B
��B
�*B
�RB
��B
��B
�~B
��B
��B
��B
��B
�.B
��B
��B
yrB
u�B
k�B
a�B
\�B
T�B
T,B
M�B
G�B
FtB
D�B
D�B
C�B
B�B
?}B
=qB
9XB
5�B
0�B
.�B
,=B
*�B
'�B
&LB
�B
�B
	B
1B
�B
{B
hB
�B
	�B
�B
�B	��B	��B	�B	��B	�yB	�QB	��B	��B	�B	�B	�dB	�]B	�B	��B	ӏB	уB	�vB	�dB	�)B	ǮB	��B	��B	�'B	��B	�qB	��B	��B	��B	��B	��B	��B	�-B	��B	��B	�B	��B	��B	�B	��B	�CB	�=B	�_B	�$B	�B	�:B	�"B	�rB	��B	�SB	��B	�{B	�;B	.B	|�B	{B	zDB	wfB	v`B	u�B	s�B	sMB	pB	m�B	l�B	j�B	kQB	h�B	gB	e�B	d�B	d&B	a|B	a|B	_pB	\�B	^B	\�B	Z�B	X�B	Y�B	W
B	W
B	T�B	R B	Q�B	O�B	P�B	P�B	K�B	I�B	MB	LdB	K^B	K�B	IB	M�B	L�B	K�B	J�B	J�B	J�B	J�B	I�B	IB	I�B	IRB	I�B	IRB	IB	G�B	IB	MB	J�B	J�B	IRB	JXB	M�B	OvB	TaB	T,B	S[B	T�B	T�B	Y�B	YKB	Z�B	_B	aB	b�B	c�B	e�B	i�B	jKB	j�B	k�B	o5B	rB	r�B	sMB	s�B	uZB	v�B	xlB	yrB	zB	zB	|�B	�B	��B	�B	�YB	��B	��B	�~B	��B	��B	�B	�{B	�_B	�eB	�	B	�xB	��B	��B	��B	�B	�B	��B	�0B	�qB	�IB	�OB	��B	�UB	��B	��B	�[B	��B	��B	�B	�6B	��B	��B	��B	ʌB	�0B	ΥB	�&B	�gB	�B	��B	ܒB	��B	��B	�/B	ݘB	�B	�B	�jB	�pB	�B	�B	��B	�B	�B	�B	��B	��B	�vB	�B	��B	�8B	�lB	��B	��B	�B	�(B
B
%B
	�B
DB
�B
VB
\B
oB
�B
_B
�B
�B
	B
CB
xB
OB
�B
�B
!-B
"hB
%FB
)�B
*eB
+kB
,=B
,qB
,qB
,�B
-�B
0!B
5tB
9$B
<jB
?HB
A�B
A�B
FtB
G�B
GzB
IRB
P}B
R�B
T�B
U2B
U�B
VB
V9B
V9B
VmB
V�B
V�B
V�B
V�B
V�B
VmB
Y�B
]�B
bNB
dZB
e`B
e�B
g�B
iDB
i�B
k�B
m�B
n/B
n�B
o�B
p�B
q�B
tB
u�B
x�B
|B
}VB
~�B
.B
� B
� B
��B
�AB
��B
�+B
��B
�xB
�~B
��B
��B
�4B
�B
��B
��B
��B
��B
��B
�B
��B
�OB
�OB
�B
��B
��B
�4B
�B
�CB
�wB
�CB
��B
��B
��B
�IB
�IB
��B
��B
�aB
�aB
��B
��B
��B
�^B
��B
�}B
��B
ÖB
�gB
�B
ŢB
��B
ŢB
��B
ƨB
�B
�vB
�}B
�B
уB
� B
��B
�&B
��B
�B
��B
چB
�#B
��B
�NB
�B
�TB
��B
��B
�
B
�B
�B
�B
�iB
�iB
�;B
�AB
�B
��B
�%B
��B
�B
�B
�"B
�(B
��B
�cB
�cB
��B iB �B�BuB�BMBMBBBMBBSBB�B1B�B�B�B
	B
�BxB~B�B�B BBBuBB{B�BB�BBYB1B�B1B1BeBeB1B�B�BkBkB7B�B	BxBBIB�BB�B�B!bB"hB#B#:B#nB#�B$B$@B$�B%FB%FB%�B%�B&LB&B&B&�B&�B&�B&�B'B'�B'�B($B(XB(�B(�B)_B)*B)�B)�B*0B*eB+6B,�B.B/�B1�B1�B1�B1�B33B3�B3�B3�B3�B5?B6�B7B7B8B8�B9$B9$B9XB9�B:^B:�B;0B;dB;�B;�B;�B<6B>B?�B?�B@�BAUBA�BA�BB'BB[BB[BB�BB�BB�BCaBDgBE�BF�BF�BF�BGBGBGBF�BF�BF�BGEBH�BH�BIBJ�BJ�BJ�BJ�BK)BK)BK)BL0BL�BL�BL�BM�BN<BNpBN<BO�BP}BQ�BS�BS�BS�BS�BT,BT,BT�BU�BU�BVBV9BW
BWsBXEBXyBXyBX�BYBY�BZQBZQBZ�BZ�BZ�BZ�BZ�BZ�BZ�BZ�B[�B[�B[�B[�B[�B]/B^�B_B_�B`B`B`B`B`B`B`BB`vB`vBaBaHBa�Bb�Bb�Bc BcTBc�BdZBdZBd�Be,Be,Bd�Be,Bd�Bd�Bd�Bd&Be,Bd�Be,Be�BffBg�Bh
Bh>BhsBh�BhsBh�Bh�Bh�Bh�BiBh�BiDBiyBi�Bi�Bi�BjKBjKBjBjBjBjBjBj�Bj�Bk�Bk�BlWBl"BlWBl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn/BncBn�Bo BoiBoiBo�BpBp;Bq�Bq�Bq�Bq�Bq�Bq�Bq�BrBr�BsMBr�Br�BsBr�BsBs�Bs�Bt�Bt�BuZBu�BuZBv`Bv`Bv�Bv�Bv�Bw2Bw2Bw2BwfBwfBw�Bw�Bw�Bw�Bw�Bw�Bw�BxBxBx8BxlBx�By>By>By	Bx�Bx�By>By�By�By�By�ByrBy>ByrByrBzBzBzxBzDBzxBzxBz�B{JB{JB{Bz�Bz�B{JB{�B|B|PB|�B|�B}�B}"B}�B}�B}�B}�B~�B~�B~�B~�B.B�iB��B�;B�B��B�B�B�;B�;B�B�;B�oB��B��B��B��B��B��B��B��B��B�oB��B�B�AB��B�uB�uB��B��B�B�GB�{B�MB�B��B�SB��B��B��B��B�B��B��B�YB�%B�YB�YB�YB�YB�YB�YB�%B��B��B��B�+B�B�DB��B��B��B�xB�B�rB�8B�B�rB�rB�8B�fB��B��B�B��B�B��B��B�+B�B��B�8B��B��B�ZB�`B��B��B�B��B��B��B�+B�ZB��B�ZB��B��B��B�ZB�TB��B�ZB�+B��B�`B��B��B��B��B�B�B��B�TB��B��B��B��B��B��B�TB��B�%B�ZB�`B��B�ZB��B�8B�fB�fB��B�%B��B�%B��B�B��B�B�MB�B��B�%B�ZB�`B��B�`B��B�B�TB�rB�B�%B��B�TB�B�B�B��B��B�%B�ZB��B�TB��B��B�;B�B 4B�B�B�VB�AB�B��B�ZB�B��B�B�B��B��B��B�ZB��B��B��B��B�%B�TB�B��B�B�B�B�ZB�B��B��B�B��B�MB�2B��B�|B��B��B��B��B�+B�ZB�+B�B�B�;B��B��B�vB�B�B�B�ZB��B��B�B�8B�;B�vB�;B��B��B�B�B�B�B�B��B�B��B�B�B�`B��B�/B�B�B�iB�B�B�|B�vB�B�B�B�B�B��B�vB�B�B�;B�oB�oB�B�B�;B�oB�oB�oB�B�GB�|B�TB�GB�B�B�B�B�B�B�oB��B�oB�B��B�B��B�/B��B��B��B�"B�B�WB��B�B�QB�B��B�B�B�sB�B�2B�fB�fB�&B��B�B�TB�BߤB�B��B�/B��B�)B��B�#B��B�B�B��B��BӏBбB�)B�BĜBȀB�BB��B��B��B�LB�?B��B��B��B��B��B��B��B��B��B��B��B�xB�B�B�VB�\B�"B�VBw�B�%BncB`�B]�Bb�BW�BP}B\)BM�Bv�B0!BJ�B/�B�B-CB@�B�B(B�BB�B�BB~B�B�B�BoB@B�B�B��B�BٴB�B��B�,BѷB�pB�pB͟B�)B�BB��B��BǮB��BʌB�jB��BɺB�XB�*B��B��B�FB�?B�tB��B�aB��B��B��B��B��B��B��B�!B��B�eB�FB�FB�4B��B�B�SB�YB�oB��BzBx8Bw2B�;B}VB�"Bq�Be�B`B]�B[�BQ�BU�BZBP�BQ�BL�BI�BL0BFBE�BFBK�B>�B<6B5B6zB9$B<�BB�B)�B33BB \B7B7B1B�BB!B'B�BuB�B�BB�B�.B��B�B�TB�B��B�AB�B�B�QB�B�KB�B�8B�`B��B�B�TB� B�BB�B��BߤB�jB�dB��B�WB�]B�)B�B�
B�sB�dBخB�B�BҽBѷB�B�[B��B�B� B��B��B�B��B�B�B�tB��B��B�B��B�-B��B��B�aB��B��B�dB��B�6B��B�$B��B�zB��B�CB�hB�B��B�B�0B��B��B�UB��B��B��B��B��B��B�B��B�B�B�bB�B��B��B��B��B��B�B�4B��B��B��B��B� B��ByrB|�B�YB�%B�fB��Bs�Bp�BoiBo�Bs�Bp�Bi�BncBh�Bh�Bh
Bd�Be`BiyBa�Bc�BaHB^jB_;Bg8B\]B\�BX�BZBZBb�B]/BW?BOBBS�BP}BMjBMBL�BK�BL�BJ�BI�BK^BJ�BI�BL0BJ#BMBI�BIBI�BG�BH�BG�BF�BGEBFtBFBI�BD�BB�B?�B>�B@OB@OB=�B=B@B;dB>B;�B;dB=�B:�B;0B:*B=�B<B8�B9�B8�B6zB6FB5tB5?B4�B6B4�B7�B:�B.}B5?B9�B.B+kB)�B*�B*eB,B/�B!�B#nB&�B"�B�BxB	BCB�BBB�B$B�BMB�B�BB@B�BB�B�B B�B�B�B�BGB��B��B�VB��B�"B��B��B��B�DB�B��B�2B�2B�lB�TB�`B�B��B��B�oB�cB�5B�/B�B��B�B�B�B�WB��B�B�cB�KB�B�B�B��B�`B�8B�)B�B�dBݘB�WB��BچB�BȴB�
BخB�B�mB�2B�BҽB� B�NB�BΥB�<B�B�0BʌB��B�BƨB�pB��B��B�}B�OB�'B��B�HB��B�HB��B��B��B�FB��B�CB�}B��B�zB��B��B�:B��B��B�bB��B��B��B��B�bB�-B�B��B�!B�~B�~B�xB��B��B�B��B�	B��B�kB��B�1B�_B��B��B�MB��B��B��B��B� B�B��B��B�uB�B�@B�B�(B��B�BcB��BzBv�Bs�Bq�Bq�BpoBpBk�Bm�Bm]BkQBn�Bk�Bl�Bi�BiyBjBjBkQBhsBiyBg�Bh
Bg8Be,Bh�Bc�Be,Bc�BdZBffBg8BaHBa�BbBdZBe�Be�B`BBbNB^5B`vB^jB^jB]�B\�B\�B_�B]�B]dBZQB]�BX�BZ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2020123112584820201231125848IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143320210220011433QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143320210220011433QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164420210325101644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                