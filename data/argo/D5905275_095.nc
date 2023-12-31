CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-08-13T19:38:13Z creation; 2023-04-26T19:14:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200813193813  20230426191430  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               _   _AA  AOAO7316_008644_095                 7316_008644_095                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�00}�H@�00}�H11  @�00PH�@�00PH�@'d��
Ri@'d��
Ri�c�P3:~�c�P3:~11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @B�\@��\@��\@�G�@�G�A   A\)A�RA+�A?\)A`��A�Q�A�  A��A�  A��A�  A߮A�  B (�B(�B(�B  B�
B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�
B�  B�{B�{B�{B�(�B�(�B�(�B�{B�{B�(�B�  B��
B�  B�{B�ffB�B��B�  B��B��B�  B��B��B��B��B�  B�  B�  B�{B�  B�  C 
=C
=C��C��C  C	��C  C  C��C
=C  C  C  C��C��C��C�C!��C$  C%��C'��C*  C,  C.  C0  C2  C4
=C6  C7��C9��C;��C=��C?��CB  CD
=CF  CH
=CJ
=CL  CN
=CP
=CR
=CT{CV
=CX
=CZ
=C\  C]��C_��Ca��Cd  Ce��Ch  Cj  Cl  Cn
=Cp
=Cr  Ct  Cv  Cw��Cy��C{�C}��C�  C�C�C�  C�  C���C���C���C�C�  C�  C���C���C���C���C���C�  C�
=C�  C���C��C���C�C���C���C�C�C�C�C�
=C�  C���C���C�  C�C�
=C�  C���C�  C�C�  C���C�  C�  C���C�C�C�  C�C�C���C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C���C���C���C���C�  C�C�C�  C���C���C�  C�
=C�C���C�  C�  C�C�  C���C���C�  C�C�
=C�C�  C�  C���C���C���C�
=C�  C���C���C�  C�C�  C���C���C�C�  C���C���C���C�C���C���C�  C�C�C�C�C�  C�C�
=C�C���C���C�  C�
=C�C�  C�C�
=C�
=D �D � D  D}qD��D� DD��D�D� D�qDz�D  D��D  D}qD  D�D	�D	��D
  D
}qD
��D}qD  D}qD  D��DD��D  D��D�D��D  D}qD  D��D  D� D�D��D  D� D�D� D  D� D  D}qD�qD}qD  D� D  D� D  D� D�D� D�qD� D�D� D�qD � D!�D!�D"  D"� D#�D#��D$  D$� D%�D%� D%�qD&}qD&�qD'}qD(�D(��D)  D)}qD)�qD*� D+  D+� D,�D,��D-�D-�D.�D.��D/D/��D0  D0��D1�D1}qD1�qD2� D3  D3� D3�qD4}qD5  D5� D6  D6��D7  D7� D8  D8��D9�D9��D:  D:}qD:�qD;}qD<  D<� D=  D=��D>�D>� D?  D?}qD?��D@}qDA  DA��DB�DB� DC  DC� DD  DD� DE�DE� DF�DF� DF�qDG� DH  DH� DI  DI� DI�qDJ��DK  DKz�DK�qDL��DM  DM� DN�DN� DN�qDO}qDO�qDP� DQ�DQ� DR  DR� DS�DS��DS�qDT� DT�qDUz�DU�qDV� DW�DW� DX  DX� DY�DY��DZ�DZ��D[�D[��D\�D\��D]  D]� D^  D^}qD_  D_�D`  D`}qD`��Da� Db  Db� Dc  Dc� Dd  Dd}qDd�qDe}qDf  Df��Dg  Dg}qDg�qDh}qDi  Di� Dj�Dj�Dk  Dk}qDl  Dl� Dm  Dm}qDm�qDn� Do  Do��Dp  Dp� Dq�Dq� Dr  Dr��Ds�Ds}qDs��Dt� Du�Du}qDu�qDv��Dw�Dw� Dx  Dx}qDx�qDyz�Dz  Dz��D{�D{��D|  D|� D}  D}� D~  D~� D  D}qD�qD�>�D�~�D�� D�HD�AHD�~�D���D���D�>�D�~�D���D���D�@ D�� D�� D�HD�AHD���D��HD�HD�@ D��HD�� D�  D�>�D�~�D���D���D�@ D��HD��HD�  D�>�D�� D���D���D�@ D�� D�� D��D�AHD�� D�� D�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�AHD�~�D���D�  D�>�D�~�D�� D��D�AHD��HD�� D�  D�AHD��HD�� D�  D�>�D�~�D��qD���D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD��HD�� D��qD�>�D�� D��HD�  D�>�D��HD�� D�  D�B�D��HD��HD�HD�>�D�� D��HD���D�@ D��HD�� D���D�@ D��HD�� D���D�=qD�� D���D���D�AHD�� D�� D���D�>�D�~�D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�B�D���D��HD���D�@ D��HD�� D��qD�=qD�~�D�� D�HD�B�D��HD���D�  D�AHD�� D��qD���D�AHD�� D���D�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�� D���D���D�@ D�� D���D�HD�AHD�~�D���D��D�B�D��HD��qD���D�AHD��HD���D��qD�>�D�~�D��qD���D�>�D�~�D��HD�  D�=qD�~�D�� D��qD�@ D��HD�� D�  D�AHD���D��HD�HD�@ D�� D���D���D�=qD�~�D�� D���D�=qD�� D��HD�  D�>�D�~�D���D���D�>�D�}qD��qD�  D�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��qD�HD�@ D�~�D��HD�HD�@ D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D D�� D�  D�@ D�~�Dþ�D���D�=qD�~�D��HD��D�B�DŁHD�� D���D�AHDƀ DƼ)D���D�AHDǀ D�� D���D�>�D�~�D��HD��D�B�Dɂ�D�D�HD�AHDʀ Dʾ�D���D�=qDˀ D�D�HD�@ D�~�D̾�D���D�>�D�~�D;�D���D�=qD�}qDνqD��)D�@ DρHDϽqD�  D�AHDЁHD�� D���D�>�D�~�DѾ�D�  D�AHDҀ D��HD�HD�@ D�~�DӾ�D�  D�B�DԁHD�� D���D�=qDՀ D��HD���D�=qD�~�D־�D���D�>�D�~�D׾�D���D�@ D؀ D�� D�  D�>�D�}qDپ�D�HD�@ D�~�Dھ�D���D�>�Dۀ D۾�D���D�@ D܁HD��HD���D�>�D݀ D�� D�  D�@ DށHD�� D���D�@ D߁HD߾�D���D�@ D�� D�D�HD�AHD�HD�� D�  D�@ D�HD�D�HD�AHD�HD��HD�HD�@ D�HD��HD�  D�>�D�HD�D�HD�AHD� D�� D�  D�>�D�~�D羸D���D�>�D�~�D�� D�  D�@ D� D龸D���D�>�D�~�D�qD���D�@ D�~�D뾸D���D�>�D�~�D쾸D�  D�@ D� D��HD�HD�@ D� DD�HD�AHD�HD��HD�  D�@ D��HD��HD�  D�>�D�~�D�D���D�>�D� D�� D�  D�@ D�HD�� D���D�AHD�D�D���D�@ D��HD�D�  D�@ D��HD���D���D�@ D�� D�� D���D�@ D��HD�� D�HD�B�D�� D�� D��fD�  ?#�
?L��?��?��R?���?\?�G�?��@�@z�@(�@(��@8Q�@E�@L��@Y��@k�@u@}p�@�ff@��@�\)@�@�p�@��\@�ff@�{@�33@�Q�@��R@��@�=q@�\)@�
=@޸R@�\@�@�\)@�
=@�(�AG�Az�AQ�A
�HA{A�AAQ�A�A\)A"�\A%�A)��A-p�A0��A3�
A7�A:�HA>{AA�AEAH��AMp�AQ�AUAX��A]p�AaG�Ae�Aj=qAn{Aq�Au�Az=qA~{A���A�=qA���A�ffA�  A�=qA�(�A�A�\)A���A��HA�z�A��RA�Q�A��A�33A�p�A�
=A�Q�A�=qA�(�A�p�A�\)A���A�33A��A��RA���A��HA�z�A�ffA���A�=qA�(�A�ffA�Q�A��A�(�A�ffA�  A��A�z�AθRA�Q�A�=qA���A�
=Aأ�A��HA��A�\)A�G�A�A�A�Q�A�=qA�(�A�RA���A�\A���A�\)A�G�A��HA�p�A�\)B ��BB�HB�
B��BB�HB�
B��B	�B33B  B�B=qB\)BQ�BG�B�RB�
B��B{B33B��B�B
=BQ�BB
=B (�B!p�B"�HB$  B%G�B&�RB((�B)�B*ffB+�
B-G�B.ffB/�B1�B2ffB3�B4��B6ffB7�B8��B:{B;�B=�B>ffB?�B@��BB�\BC�
BE�BF�\BH  BI��BJ�HBL(�BM��BO
=BPz�BQ��BS
=BT��BV{BW\)BX��BZ{B[�B\��B^=qB_�Ba�Bb�\Bc�
Be�Bf�RBh(�Bip�Bj�RBl(�BmBo33BpQ�Bq��Bs33Bt��Bu�Bw33Bx��Bz{B{�B|��B}�B\)B�z�B�
=B��B�ffB��B�B�Q�B���B��B�ffB�
=B���B�=qB���B���B�=qB���B�\)B�{B��RB�\)B��B�z�B��B�B�ffB���B��B�(�B���B��B�(�B���B�\)B�  B���B�G�B��B�ffB�
=B���B�Q�B�
=B��B�(�B��RB�G�B��B���B�G�B��B��\B�33B�B�ffB�
=B�B�ffB�
=B��B�ffB���B���B�Q�B���B��B�ffB�
=B�B�Q�B���B���B�Q�B�
=B�B�ffB�
=B��B�Q�B���B���B�=qB���B��B�Q�B��HB��B�{B���B�G�B��B�z�B�
=B�p�B��B�=qB���B�
=B�\)B���B��
B�{B�=qB�Q�B�ffB�z�B��\B��RB��HB���B��B�G�B�p�B���B��B��
B�  B�(�B�Q�B�z�B���B��HB��B�\)B��B��B��B�{B�Q�B£�B��HB�33B�p�B�B�{B�ffBĸRB�
=B�\)BŮB�  B�Q�Bƣ�B���B�\)BǮB�{B�z�B��HB�G�Bə�B��B�Q�Bʣ�B�
=B�\)BˮB�(�B̏\B��HB�G�BͮB�{B�z�B��HB�p�B�B�=qBУ�B��BхB�  B�ffB��HB�G�BӮB�(�Bԏ\B�
=B�p�B��
B�=qBָRB�33B׮B�(�B؏\B�
=BمB�  Bڏ\B�
=BۅB�  B�z�B���B�\)B��B�ffB���B�G�B�B�=qB�RB�33B�B�(�B�RB�33B�B�=qB��B��B噚B�{B�z�B���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�G�B�B�=qB��B��B홚B�{B�\B���B�B�  B�z�B��HB�\)B��
B�=qB��B�
=B�B�  B�ffB���B�G�B��B�(�B��\B�
=B�p�B��B�Q�B��RB�33B���B�{B�z�B���B�\)B�B�(�B��\B���B�\)B��B�(�B�z�B���B�G�B��C   C 33C ffC ��C ��C ��C33CffC�\CC��C(�CQ�C�C�C�HC
=C=qCp�C��CC�C{CG�Cp�C�C�
C{CG�Cp�C��C�
C
=C=qCp�C��C��C��C(�C\)C�C�RC�C�CQ�C�C�RC��C	�C	\)C	�\C	C	��C
(�C
\)C
�C
�RC
�C�CQ�C�C�RC�C�CQ�C�C�RC  C33Cp�C�C�HC�C\)C�\C�RC  C33Cp�C��C�HC�CffC��C�HC�C\)C�\C�
C
=CG�C�\C��C�C\)C��C�
C{CQ�C�CC  CG�C�\C��C{CQ�C��C�
C�CffC��C�C(�Cp�CC
=CffC�C��CG�C�\C�
C�Cp�C�RC  C\)C�C  CQ�C��C�C33Cz�CC{Cp�CC �C p�C C!{C!ffC!�RC"  C"\)C"�RC#{C#p�C#C$
=C$ffC$C%(�C%z�C%�
C&(�C&z�C&��C'�C'�C'�HC(=qC(�\C(�HC)33C)�\C)�C*G�C*��C+  C+Q�C+��C+��C,Q�C,�C-{C-p�C-C.�C.p�C.��C/(�C/�\C/�HC033C0�C0�
C1=qC1��C1��C2G�C2��C2��C3Q�C3�RC4
=C4ffC4�C5{C5z�C5��C633C6�C6�HC733C7�\C7��C8\)C8�RC9
=C9ffC9C:�C:z�C:�HC;G�C;��C;��C<G�C<�C={C=p�C=��C>(�C>z�C>�
C?=qC?��C@  C@Q�C@��CA  CAffCACB�CBffCBCC(�CC�\CC�HCD33CD�CD�
CE33CE��CE��CF=qCF�\CF�CG=qCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  ?�=q@   @B�\@��\@��\@�G�@�G�A   A\)A�RA+�A?\)A`��A�Q�A�  A��A�  A��A�  A߮A�  B (�B(�B(�B  B�
B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�
B�  B�{B�{B�{B�(�B�(�B�(�B�{B�{B�(�B�  B��
B�  B�{B�ffB�B��B�  B��B��B�  B��B��B��B��B�  B�  B�  B�{B�  B�  C 
=C
=C��C��C  C	��C  C  C��C
=C  C  C  C��C��C��C�C!��C$  C%��C'��C*  C,  C.  C0  C2  C4
=C6  C7��C9��C;��C=��C?��CB  CD
=CF  CH
=CJ
=CL  CN
=CP
=CR
=CT{CV
=CX
=CZ
=C\  C]��C_��Ca��Cd  Ce��Ch  Cj  Cl  Cn
=Cp
=Cr  Ct  Cv  Cw��Cy��C{�C}��C�  C�C�C�  C�  C���C���C���C�C�  C�  C���C���C���C���C���C�  C�
=C�  C���C��C���C�C���C���C�C�C�C�C�
=C�  C���C���C�  C�C�
=C�  C���C�  C�C�  C���C�  C�  C���C�C�C�  C�C�C���C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C���C���C���C���C�  C�C�C�  C���C���C�  C�
=C�C���C�  C�  C�C�  C���C���C�  C�C�
=C�C�  C�  C���C���C���C�
=C�  C���C���C�  C�C�  C���C���C�C�  C���C���C���C�C���C���C�  C�C�C�C�C�  C�C�
=C�C���C���C�  C�
=C�C�  C�C�
=C�
=D �D � D  D}qD��D� DD��D�D� D�qDz�D  D��D  D}qD  D�D	�D	��D
  D
}qD
��D}qD  D}qD  D��DD��D  D��D�D��D  D}qD  D��D  D� D�D��D  D� D�D� D  D� D  D}qD�qD}qD  D� D  D� D  D� D�D� D�qD� D�D� D�qD � D!�D!�D"  D"� D#�D#��D$  D$� D%�D%� D%�qD&}qD&�qD'}qD(�D(��D)  D)}qD)�qD*� D+  D+� D,�D,��D-�D-�D.�D.��D/D/��D0  D0��D1�D1}qD1�qD2� D3  D3� D3�qD4}qD5  D5� D6  D6��D7  D7� D8  D8��D9�D9��D:  D:}qD:�qD;}qD<  D<� D=  D=��D>�D>� D?  D?}qD?��D@}qDA  DA��DB�DB� DC  DC� DD  DD� DE�DE� DF�DF� DF�qDG� DH  DH� DI  DI� DI�qDJ��DK  DKz�DK�qDL��DM  DM� DN�DN� DN�qDO}qDO�qDP� DQ�DQ� DR  DR� DS�DS��DS�qDT� DT�qDUz�DU�qDV� DW�DW� DX  DX� DY�DY��DZ�DZ��D[�D[��D\�D\��D]  D]� D^  D^}qD_  D_�D`  D`}qD`��Da� Db  Db� Dc  Dc� Dd  Dd}qDd�qDe}qDf  Df��Dg  Dg}qDg�qDh}qDi  Di� Dj�Dj�Dk  Dk}qDl  Dl� Dm  Dm}qDm�qDn� Do  Do��Dp  Dp� Dq�Dq� Dr  Dr��Ds�Ds}qDs��Dt� Du�Du}qDu�qDv��Dw�Dw� Dx  Dx}qDx�qDyz�Dz  Dz��D{�D{��D|  D|� D}  D}� D~  D~� D  D}qD�qD�>�D�~�D�� D�HD�AHD�~�D���D���D�>�D�~�D���D���D�@ D�� D�� D�HD�AHD���D��HD�HD�@ D��HD�� D�  D�>�D�~�D���D���D�@ D��HD��HD�  D�>�D�� D���D���D�@ D�� D�� D��D�AHD�� D�� D�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�AHD�~�D���D�  D�>�D�~�D�� D��D�AHD��HD�� D�  D�AHD��HD�� D�  D�>�D�~�D��qD���D�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD��HD�� D��qD�>�D�� D��HD�  D�>�D��HD�� D�  D�B�D��HD��HD�HD�>�D�� D��HD���D�@ D��HD�� D���D�@ D��HD�� D���D�=qD�� D���D���D�AHD�� D�� D���D�>�D�~�D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�B�D���D��HD���D�@ D��HD�� D��qD�=qD�~�D�� D�HD�B�D��HD���D�  D�AHD�� D��qD���D�AHD�� D���D�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�� D���D���D�@ D�� D���D�HD�AHD�~�D���D��D�B�D��HD��qD���D�AHD��HD���D��qD�>�D�~�D��qD���D�>�D�~�D��HD�  D�=qD�~�D�� D��qD�@ D��HD�� D�  D�AHD���D��HD�HD�@ D�� D���D���D�=qD�~�D�� D���D�=qD�� D��HD�  D�>�D�~�D���D���D�>�D�}qD��qD�  D�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��qD�HD�@ D�~�D��HD�HD�@ D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D D�� D�  D�@ D�~�Dþ�D���D�=qD�~�D��HD��D�B�DŁHD�� D���D�AHDƀ DƼ)D���D�AHDǀ D�� D���D�>�D�~�D��HD��D�B�Dɂ�D�D�HD�AHDʀ Dʾ�D���D�=qDˀ D�D�HD�@ D�~�D̾�D���D�>�D�~�D;�D���D�=qD�}qDνqD��)D�@ DρHDϽqD�  D�AHDЁHD�� D���D�>�D�~�DѾ�D�  D�AHDҀ D��HD�HD�@ D�~�DӾ�D�  D�B�DԁHD�� D���D�=qDՀ D��HD���D�=qD�~�D־�D���D�>�D�~�D׾�D���D�@ D؀ D�� D�  D�>�D�}qDپ�D�HD�@ D�~�Dھ�D���D�>�Dۀ D۾�D���D�@ D܁HD��HD���D�>�D݀ D�� D�  D�@ DށHD�� D���D�@ D߁HD߾�D���D�@ D�� D�D�HD�AHD�HD�� D�  D�@ D�HD�D�HD�AHD�HD��HD�HD�@ D�HD��HD�  D�>�D�HD�D�HD�AHD� D�� D�  D�>�D�~�D羸D���D�>�D�~�D�� D�  D�@ D� D龸D���D�>�D�~�D�qD���D�@ D�~�D뾸D���D�>�D�~�D쾸D�  D�@ D� D��HD�HD�@ D� DD�HD�AHD�HD��HD�  D�@ D��HD��HD�  D�>�D�~�D�D���D�>�D� D�� D�  D�@ D�HD�� D���D�AHD�D�D���D�@ D��HD�D�  D�@ D��HD���D���D�@ D�� D�� D���D�@ D��HD�� D�HD�B�D�� D�� D��fG�O�?#�
?L��?��?��R?���?\?�G�?��@�@z�@(�@(��@8Q�@E�@L��@Y��@k�@u@}p�@�ff@��@�\)@�@�p�@��\@�ff@�{@�33@�Q�@��R@��@�=q@�\)@�
=@޸R@�\@�@�\)@�
=@�(�AG�Az�AQ�A
�HA{A�AAQ�A�A\)A"�\A%�A)��A-p�A0��A3�
A7�A:�HA>{AA�AEAH��AMp�AQ�AUAX��A]p�AaG�Ae�Aj=qAn{Aq�Au�Az=qA~{A���A�=qA���A�ffA�  A�=qA�(�A�A�\)A���A��HA�z�A��RA�Q�A��A�33A�p�A�
=A�Q�A�=qA�(�A�p�A�\)A���A�33A��A��RA���A��HA�z�A�ffA���A�=qA�(�A�ffA�Q�A��A�(�A�ffA�  A��A�z�AθRA�Q�A�=qA���A�
=Aأ�A��HA��A�\)A�G�A�A�A�Q�A�=qA�(�A�RA���A�\A���A�\)A�G�A��HA�p�A�\)B ��BB�HB�
B��BB�HB�
B��B	�B33B  B�B=qB\)BQ�BG�B�RB�
B��B{B33B��B�B
=BQ�BB
=B (�B!p�B"�HB$  B%G�B&�RB((�B)�B*ffB+�
B-G�B.ffB/�B1�B2ffB3�B4��B6ffB7�B8��B:{B;�B=�B>ffB?�B@��BB�\BC�
BE�BF�\BH  BI��BJ�HBL(�BM��BO
=BPz�BQ��BS
=BT��BV{BW\)BX��BZ{B[�B\��B^=qB_�Ba�Bb�\Bc�
Be�Bf�RBh(�Bip�Bj�RBl(�BmBo33BpQ�Bq��Bs33Bt��Bu�Bw33Bx��Bz{B{�B|��B}�B\)B�z�B�
=B��B�ffB��B�B�Q�B���B��B�ffB�
=B���B�=qB���B���B�=qB���B�\)B�{B��RB�\)B��B�z�B��B�B�ffB���B��B�(�B���B��B�(�B���B�\)B�  B���B�G�B��B�ffB�
=B���B�Q�B�
=B��B�(�B��RB�G�B��B���B�G�B��B��\B�33B�B�ffB�
=B�B�ffB�
=B��B�ffB���B���B�Q�B���B��B�ffB�
=B�B�Q�B���B���B�Q�B�
=B�B�ffB�
=B��B�Q�B���B���B�=qB���B��B�Q�B��HB��B�{B���B�G�B��B�z�B�
=B�p�B��B�=qB���B�
=B�\)B���B��
B�{B�=qB�Q�B�ffB�z�B��\B��RB��HB���B��B�G�B�p�B���B��B��
B�  B�(�B�Q�B�z�B���B��HB��B�\)B��B��B��B�{B�Q�B£�B��HB�33B�p�B�B�{B�ffBĸRB�
=B�\)BŮB�  B�Q�Bƣ�B���B�\)BǮB�{B�z�B��HB�G�Bə�B��B�Q�Bʣ�B�
=B�\)BˮB�(�B̏\B��HB�G�BͮB�{B�z�B��HB�p�B�B�=qBУ�B��BхB�  B�ffB��HB�G�BӮB�(�Bԏ\B�
=B�p�B��
B�=qBָRB�33B׮B�(�B؏\B�
=BمB�  Bڏ\B�
=BۅB�  B�z�B���B�\)B��B�ffB���B�G�B�B�=qB�RB�33B�B�(�B�RB�33B�B�=qB��B��B噚B�{B�z�B���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�G�B�B�=qB��B��B홚B�{B�\B���B�B�  B�z�B��HB�\)B��
B�=qB��B�
=B�B�  B�ffB���B�G�B��B�(�B��\B�
=B�p�B��B�Q�B��RB�33B���B�{B�z�B���B�\)B�B�(�B��\B���B�\)B��B�(�B�z�B���B�G�B��C   C 33C ffC ��C ��C ��C33CffC�\CC��C(�CQ�C�C�C�HC
=C=qCp�C��CC�C{CG�Cp�C�C�
C{CG�Cp�C��C�
C
=C=qCp�C��C��C��C(�C\)C�C�RC�C�CQ�C�C�RC��C	�C	\)C	�\C	C	��C
(�C
\)C
�C
�RC
�C�CQ�C�C�RC�C�CQ�C�C�RC  C33Cp�C�C�HC�C\)C�\C�RC  C33Cp�C��C�HC�CffC��C�HC�C\)C�\C�
C
=CG�C�\C��C�C\)C��C�
C{CQ�C�CC  CG�C�\C��C{CQ�C��C�
C�CffC��C�C(�Cp�CC
=CffC�C��CG�C�\C�
C�Cp�C�RC  C\)C�C  CQ�C��C�C33Cz�CC{Cp�CC �C p�C C!{C!ffC!�RC"  C"\)C"�RC#{C#p�C#C$
=C$ffC$C%(�C%z�C%�
C&(�C&z�C&��C'�C'�C'�HC(=qC(�\C(�HC)33C)�\C)�C*G�C*��C+  C+Q�C+��C+��C,Q�C,�C-{C-p�C-C.�C.p�C.��C/(�C/�\C/�HC033C0�C0�
C1=qC1��C1��C2G�C2��C2��C3Q�C3�RC4
=C4ffC4�C5{C5z�C5��C633C6�C6�HC733C7�\C7��C8\)C8�RC9
=C9ffC9C:�C:z�C:�HC;G�C;��C;��C<G�C<�C={C=p�C=��C>(�C>z�C>�
C?=qC?��C@  C@Q�C@��CA  CAffCACB�CBffCBCC(�CC�\CC�HCD33CD�CD�
CE33CE��CE��CF=qCF�\CF�CG=qCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�?}A�C�A�C�A�E�A�E�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�M�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�=qA�JA݋DA�n�A��Aٝ�A�p�A�S�A�1'A��A�\)A��A��`A׏\A�n�A�\)A�ƨA��A�VA�ƨA�VA̍PA�  A�=qA�ĜA��A���A���A�v�A��/A�Q�A��
A�A���A��A���A���A��TA��uA�{A��#A��A�dZA���A�~�A���A�jA��7A�n�A�A~{Ar�Aj�uAg�hAe`BA_��AZ��AT��AR��AQO�AM�AI��AH��AGoAE`BACC�AB�AA�A>�/A=��A=?}A<�A:�A:I�A8�!A7�PA6��A6�yA6�A6~�A5�
A5�hA5`BA4�!A4-A3K�A2�A2(�A1�TA1�A1A0�!A01'A/�PA/C�A.��A.1A-|�A,Q�A+�A*jA*VA)��A)G�A(�A(��A((�A'�PA'K�A&��A&M�A%dZA$��A#A"�9A"=qA ��A�^A;dA�9A=qA��A�`AM�A=qAJAI�A1'Ap�AĜAv�A�A?}A�A-A�TA�FA\)A%A��AffAbA�;At�A�A�!AM�AA�A�A�A��A�DAbA|�A��A��A^5A��A|�A�A�A&�Az�A�uA��A�!Ar�An�AI�A�^A��Ap�A\)A7LA
�RA
�\A
I�A
bA	�A��A��A�uAM�A�wAXA�DA=qA��A�FA�7A?}A�yAZA �A�
AdZA��AVA��A�^At�A
=A �A b@��@��@�x�@��@�t�@�@��R@�$�@��#@���@��@��9@��@���@�;d@���@�E�@���@��/@��@�P@�C�@�7L@���@��y@�ff@���@���@��`@띲@��@�J@�A�@��@��@�t�@�\@��@���@�^5@߮@�@�l�@�@���@ܣ�@�I�@�b@�z�@��/@ܣ�@�j@�(�@�|�@��y@�V@��T@�G�@���@�I�@�1@�ƨ@�|�@ָR@�-@�p�@ԣ�@��;@�t�@��@ҧ�@�5?@љ�@У�@� �@�b@�+@�ff@ͺ^@̴9@̛�@˥�@�o@�@�+@��@Ɂ@�Q�@�|�@��@Ɵ�@��T@�p�@��@���@ċD@���@��@�@�$�@���@�bN@�ƨ@�@�-@��-@��@��h@���@�O�@���@�A�@���@��@�ȴ@�~�@�@�`B@���@���@�z�@�Q�@�I�@��@�K�@��@���@�V@�$�@���@��T@��#@���@���@���@��u@��@�K�@��@���@�v�@��@��7@�&�@��`@�z�@�A�@��m@�@��@���@��-@���@�X@�&�@��@���@�Ĝ@��u@���@���@���@�|�@���@�5?@��T@�p�@�G�@�&�@���@���@�r�@��@�t�@�\)@�C�@�"�@���@���@�@�hs@�X@�%@��j@��@���@�33@���@�
=@��@��@�~�@�5?@��#@�x�@�X@���@�1'@��m@��;@���@�|�@�C�@���@��R@���@�v�@�v�@�n�@�@�/@�&�@�7L@�7L@�&�@�V@���@�Ĝ@���@���@��u@�bN@��
@�l�@��@��!@�E�@�{@���@���@���@�x�@�X@��u@�  @���@�;d@�
=@���@��+@�5?@��@�7L@���@�ƨ@�|�@��@��@�ȴ@��R@���@�~�@�M�@��T@�x�@�%@�Z@�Q�@�I�@� �@�b@��F@��@�S�@�"�@��@��\@�V@��@���@�?}@�j@�A�@�9X@�(�@�1@��@��P@�K�@��H@�~�@�$�@��@��@��-@���@��7@�X@�7L@���@�bN@��@��
@���@�C�@��H@��H@��!@�^5@�@�x�@�?}@�V@�bN@��;@���@�dZ@��@��H@���@���@��\@�M�@�$�@���@�7L@�V@��@�1'@�b@�@l�@~ȴ@~��@~5?@}�-@}O�@|��@|(�@{�F@{�@{S�@z�@z�\@y��@y%@x��@x  @w�@w�w@w�w@w��@w\)@v�R@v{@u`B@t��@t��@t��@t9X@t�@s�
@s�F@st�@r�@q�#@qX@q%@pr�@p1'@o�@o|�@n�@nE�@m�@l�j@l9X@k�m@kƨ@k��@k�@kdZ@kS�@k33@k@j��@j^5@j=q@i�^@hĜ@hb@gK�@f��@fȴ@fE�@f$�@e�@e�-@e/@eV@d�/@d��@d��@c��@cS�@c33@c"�@co@b�H@b�\@a�@a�7@aG�@a�@`�`@`Ĝ@`�@`1'@_��@_|�@^�y@^�+@^ff@]�@]`B@]?}@\��@\�j@\z�@\I�@\9X@\�@[��@["�@Z�\@Y�^@Yhs@YG�@Y7L@Y&�@Y�@Y%@X�@W��@W�@Vv�@VE�@V@Up�@U?}@T�@T9X@T1@S��@S�F@SS�@S@R��@R=q@Q�7@QG�@Q�@Q%@P��@PĜ@PbN@O��@O|�@O\)@O
=@N�@Nȴ@N�R@NV@N$�@N$�@N@M��@MO�@L�/@L�@L�D@LZ@L�@K�@J��@J~�@J�@IG�@I�@H��@HĜ@Hr�@H1'@G�@Gl�@FV@F{@E�-@EO�@E�@EV@D��@D�@C�F@C��@C��@B��@BM�@BJ@A�@A��@AX@AG�@A�@@��@@�@@ �@?�;@?�w@?��@?|�@?K�@?+@?�@>ȴ@>�+@>V@=�-@=`B@=V@=V@<�@<��@<I�@<1@;�F@;��@;C�@;@:��@:n�@9��@9X@8��@8�u@8A�@8b@7�w@7\)@6��@6�+@65?@6{@5�-@4�@4�D@4�@3�F@3dZ@333@2�H@2~�@2J@1��@1��@1�7@1X@1%@0Ĝ@0�9@0Ĝ@0�9@0�@0r�@01'@/�w@/�P@/\)@/;d@.�@.��@.�+@.5?@.{@-@-��@-��@-�h@-p�@,�/@,Z@,9X@,�@+�F@+��@+�@+33@+o@*�@*��@*�!@*��@*=q@)�@)�^@)x�@)G�@(�`@(��@(bN@(  @'�@'\)@'
=@&��@&�+@&�+@&v�@&E�@&$�@&{@%�T@%�@%�@%`B@%?}@%�@$�@$��@$��@$(�@#�m@#�
@#�F@#�@#dZ@#"�@"�@"�!@"M�@"J@!��@!��@!x�@!%@ Ĝ@ r�@  �@��@��@K�@�@ȴ@�R@��@E�@�@�@O�@��@��@z�@z�@j@j@�@�@o@��@J@��@�^@��@��@��@�7@�7@x�@X@Ĝ@��@�@Q�@b@�@�;@�w@|�@\)@;d@�@
=@��@�y@��@��@��@v�@$�@��@�h@�h@�h@�h@�h@p�@?}@V@�@��@�@z�@9X@1@�F@dZ@C�@@��@��@��@-@�#@�^@��@��@��@�7@x�@x�@X@7L@��@r�@1'@ �@b@��@|�@l�@\)@;d@+@�@
=@��@�y@��@�+@5?@{@�T@�T@��@�T@��@��@O�@?}@��@��@�D@z�@j@Z@I�@�@�F@��@��@�@�@t�@33@@
�@
��@
�!@
�\@
M�@
J@	�@	�#@	�^@	�7@	hs@	X@	G�@	G�@	XA�7LA�5?A�=qA�5?A�;dA�=qA�;dA�;dA�C�A�C�A�?}A�E�A�E�A�C�A�C�A�G�A�E�A�A�A�C�A�G�A�C�A�C�A�E�A�G�A�C�A�E�A�I�A�E�A�C�A�G�A�I�A�E�A�E�A�I�A�G�A�E�A�G�A�I�A�G�A�G�A�K�A�K�A�E�A�G�A�I�A�K�A�G�A�E�A�K�A�K�A�G�A�G�A�K�A�I�A�E�A�I�A�I�A�G�A�G�A�O�A�I�A�K�A�O�A�K�A�I�A�M�A�M�A�I�A�K�A�O�A�M�A�I�A�K�A�O�A�M�A�K�A�Q�A�O�A�M�A�O�A�S�A�M�A�M�A�Q�A�Q�A�M�A�M�A�Q�A�Q�A�M�A�Q�A�S�A�M�A�Q�A�S�A�O�A�M�A�S�A�S�A�O�A�M�A�Q�A�VA�Q�A�M�A�O�A�O�A�K�A�O�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�Q�A�VA�Q�A�M�A�M�A�Q�A�O�A�M�A�Q�A�S�A�Q�A�O�A�Q�A�S�A�O�A�M�A�S�A�S�A�O�A�O�A�S�A�S�A�O�A�Q�A�S�A�5?A�1'A�1'A�1'A�&�A�$�A�&�A� �A��A�%A�A��A��A���AݾwAݙ�AݓuA�jA�Q�A�E�A��A��TAܣ�A�p�A��AۍPAڣ�A��A���A�ȴA�ĜAپwAٰ!Aٰ!A٬A١�AٓuAًDAه+A�z�A�t�A�t�A�r�A�hsA�^5A�\)A�\)A�VA�O�A�O�A�I�A�E�A�=qA�33A�-A�"�A�oA���A���A��`A�ĜAأ�AؓuA؍PA�n�A�I�A�7LA�1'A�+A� �A��A��A�oA�VA�A���A���A��yA�Aס�Aי�AבhA׍PA׉7A׋DA׋DA�|�A�r�A�jA�ffA�`BA�9XA��A�l�A�?}A���AՋDA�+A��A��AԮA�n�A�C�A��A���A��A��TA���A���A���Aӥ�A�dZA���Aҙ�A�ffA�"�A�
=A�A���AѮA�ZA�(�A��/Aв-A�7LA�  Aϩ�A��A�O�A͝�A̺^A�dZA��A��;A���A���A�~�A�oAȴ9A�x�A�5?A��`AǮA�?}A�dZAŮA���Aç�A�K�A�
=A���A�A�jA�{A���A�5?A���A��#A���A�VA���A���A���A��uA��jA�VA��
A�S�A���A�p�A�K�A��yA���A���A��+A�dZA�33A�
=A�ĜA�jA��`A���A�n�A�A���A��A���A�&�A��
A���A��A�dZA�I�A�A�A�9XA��A��^A��DA�ffA�bNA�^5A�XA�S�A�`BA�ZA�I�A�1'A�bA��A�K�A�G�A��A�-A���A�1A��uA�/A�jA���A��A��PA�33A���A��A�^5A��;A���A�E�A�A�7LA���A��A�~�A�bA�9XA��`A�ȴA��RA�A�JA��uA�7LA���A�9XA���A�ZA�oA��!A�XA���A�ȴA��9A��PA�O�A�%A���A���A�|�A�S�A�A���A�dZA�bA���A�dZA�/A���A�"�A��#A��uA�^5A�G�A�/A�{A���A��`A�ĜA�ȴA���A��RA���A��A�z�A��A��+A���A���A���A��RA���A���A��A��A�5?A�;dA�/A��A�1A��A��A��mA��#A���A�ƨA�ĜA�A�ĜA���A���A�ƨA�r�A�n�A�v�A�t�A�t�A�r�A�S�A��RA�VA�JA���A�XA��A���A�=qA���A�"�A�(�A�33A�=qA�G�A�Q�A�XA�XA�\)A���A�jA� �A���A��#A�z�A�VA��A��A��jA���A��+A�ffA�S�A�C�A�1'A���A�A�dZA�C�A���A���A��hA�/A�`BA�  A��`A���A��A�K�A���A���A�p�A�;dA� �A�A�A�p�A�A��hA�ĜA���A��RA��A�n�A���A���A�ZA�&�A��TA���A�?}A��A��9A��DA�dZA��A�-A~��A~n�A}��A}+A|1'A{oAy
=Av5?AtA�At �As�#As�AsO�ArAo?}AnVAnbAm�AmVAl��AkAk%AjVAi��Ail�Ah��Ah��Ah��Ahv�AhbAg��AhJAhJAg�TAg�FAfĜAf=qAe�mAe��Ae��Ae��Ae��AeƨAe�wAex�AeVAdE�Ac�Aa�Aa33A`~�A_�#A_?}A^��A]�wA]\)A\��A\E�A[�TA[O�AZ�AZJAY�-AYXAX�AW�;AV�AU�#AU
=AT^5AS��AS��AS�7ASp�ASl�ASC�AS�AR��AR�AR��AR�\AR~�AQ�mAQx�AQdZAQ7LAP�`AP��APȴAPv�AOO�AM?}AL�+AL1'AK��AKl�AJ�AJM�AJ1AI�mAIAI��AI�hAIp�AIK�AI�AH�AH�RAH�AHQ�AG�mAG\)AG�AF��AFz�AFE�AE��AEƨAEx�AE"�AE
=AD��ADjAC�;ACK�AB�`ABȴAB�jAB�!AB��AB��AB�uABjABA�AB �AA��AA��AAt�AA33A@��A?�mA?hsA>�/A>VA>bA=��A=�TA=��A=��A=t�A=l�A=hsA=`BA=\)A=S�A=?}A=
=A<�9A<��A<bNA<�A;�A;S�A:��A:�yA:�A:�A:��A:�RA:��A:�uA:v�A:A�A9�A9�PA9&�A8ȴA8��A8n�A8VA8(�A7��A7��A7l�A7C�A7"�A7VA7%A7oA6��A6��A6��A6�A7�A7
=A6�A6�A6�RA6�9A6��A6��A6��A6��A6��A6��A6�DA6n�A6-A6{A5�A5��A5�FA5��A5��A5��A5��A5��A5�7A5|�A5|�A5|�A5x�A5dZA5G�A5�A4��A4�A4�RA4��A4~�A4M�A4A�A45?A41'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  A�9XA�?}A�C�A�C�A�E�A�E�A�G�A�G�A�G�A�I�A�I�A�I�A�K�A�M�A�O�A�Q�A�O�A�O�A�Q�A�Q�A�Q�A�=qA�JA݋DA�n�A��Aٝ�A�p�A�S�A�1'A��A�\)A��A��`A׏\A�n�A�\)A�ƨA��A�VA�ƨA�VA̍PA�  A�=qA�ĜA��A���A���A�v�A��/A�Q�A��
A�A���A��A���A���A��TA��uA�{A��#A��A�dZA���A�~�A���A�jA��7A�n�A�A~{Ar�Aj�uAg�hAe`BA_��AZ��AT��AR��AQO�AM�AI��AH��AGoAE`BACC�AB�AA�A>�/A=��A=?}A<�A:�A:I�A8�!A7�PA6��A6�yA6�A6~�A5�
A5�hA5`BA4�!A4-A3K�A2�A2(�A1�TA1�A1A0�!A01'A/�PA/C�A.��A.1A-|�A,Q�A+�A*jA*VA)��A)G�A(�A(��A((�A'�PA'K�A&��A&M�A%dZA$��A#A"�9A"=qA ��A�^A;dA�9A=qA��A�`AM�A=qAJAI�A1'Ap�AĜAv�A�A?}A�A-A�TA�FA\)A%A��AffAbA�;At�A�A�!AM�AA�A�A�A��A�DAbA|�A��A��A^5A��A|�A�A�A&�Az�A�uA��A�!Ar�An�AI�A�^A��Ap�A\)A7LA
�RA
�\A
I�A
bA	�A��A��A�uAM�A�wAXA�DA=qA��A�FA�7A?}A�yAZA �A�
AdZA��AVA��A�^At�A
=A �A b@��@��@�x�@��@�t�@�@��R@�$�@��#@���@��@��9@��@���@�;d@���@�E�@���@��/@��@�P@�C�@�7L@���@��y@�ff@���@���@��`@띲@��@�J@�A�@��@��@�t�@�\@��@���@�^5@߮@�@�l�@�@���@ܣ�@�I�@�b@�z�@��/@ܣ�@�j@�(�@�|�@��y@�V@��T@�G�@���@�I�@�1@�ƨ@�|�@ָR@�-@�p�@ԣ�@��;@�t�@��@ҧ�@�5?@љ�@У�@� �@�b@�+@�ff@ͺ^@̴9@̛�@˥�@�o@�@�+@��@Ɂ@�Q�@�|�@��@Ɵ�@��T@�p�@��@���@ċD@���@��@�@�$�@���@�bN@�ƨ@�@�-@��-@��@��h@���@�O�@���@�A�@���@��@�ȴ@�~�@�@�`B@���@���@�z�@�Q�@�I�@��@�K�@��@���@�V@�$�@���@��T@��#@���@���@���@��u@��@�K�@��@���@�v�@��@��7@�&�@��`@�z�@�A�@��m@�@��@���@��-@���@�X@�&�@��@���@�Ĝ@��u@���@���@���@�|�@���@�5?@��T@�p�@�G�@�&�@���@���@�r�@��@�t�@�\)@�C�@�"�@���@���@�@�hs@�X@�%@��j@��@���@�33@���@�
=@��@��@�~�@�5?@��#@�x�@�X@���@�1'@��m@��;@���@�|�@�C�@���@��R@���@�v�@�v�@�n�@�@�/@�&�@�7L@�7L@�&�@�V@���@�Ĝ@���@���@��u@�bN@��
@�l�@��@��!@�E�@�{@���@���@���@�x�@�X@��u@�  @���@�;d@�
=@���@��+@�5?@��@�7L@���@�ƨ@�|�@��@��@�ȴ@��R@���@�~�@�M�@��T@�x�@�%@�Z@�Q�@�I�@� �@�b@��F@��@�S�@�"�@��@��\@�V@��@���@�?}@�j@�A�@�9X@�(�@�1@��@��P@�K�@��H@�~�@�$�@��@��@��-@���@��7@�X@�7L@���@�bN@��@��
@���@�C�@��H@��H@��!@�^5@�@�x�@�?}@�V@�bN@��;@���@�dZ@��@��H@���@���@��\@�M�@�$�@���@�7L@�V@��@�1'@�b@�@l�@~ȴ@~��@~5?@}�-@}O�@|��@|(�@{�F@{�@{S�@z�@z�\@y��@y%@x��@x  @w�@w�w@w�w@w��@w\)@v�R@v{@u`B@t��@t��@t��@t9X@t�@s�
@s�F@st�@r�@q�#@qX@q%@pr�@p1'@o�@o|�@n�@nE�@m�@l�j@l9X@k�m@kƨ@k��@k�@kdZ@kS�@k33@k@j��@j^5@j=q@i�^@hĜ@hb@gK�@f��@fȴ@fE�@f$�@e�@e�-@e/@eV@d�/@d��@d��@c��@cS�@c33@c"�@co@b�H@b�\@a�@a�7@aG�@a�@`�`@`Ĝ@`�@`1'@_��@_|�@^�y@^�+@^ff@]�@]`B@]?}@\��@\�j@\z�@\I�@\9X@\�@[��@["�@Z�\@Y�^@Yhs@YG�@Y7L@Y&�@Y�@Y%@X�@W��@W�@Vv�@VE�@V@Up�@U?}@T�@T9X@T1@S��@S�F@SS�@S@R��@R=q@Q�7@QG�@Q�@Q%@P��@PĜ@PbN@O��@O|�@O\)@O
=@N�@Nȴ@N�R@NV@N$�@N$�@N@M��@MO�@L�/@L�@L�D@LZ@L�@K�@J��@J~�@J�@IG�@I�@H��@HĜ@Hr�@H1'@G�@Gl�@FV@F{@E�-@EO�@E�@EV@D��@D�@C�F@C��@C��@B��@BM�@BJ@A�@A��@AX@AG�@A�@@��@@�@@ �@?�;@?�w@?��@?|�@?K�@?+@?�@>ȴ@>�+@>V@=�-@=`B@=V@=V@<�@<��@<I�@<1@;�F@;��@;C�@;@:��@:n�@9��@9X@8��@8�u@8A�@8b@7�w@7\)@6��@6�+@65?@6{@5�-@4�@4�D@4�@3�F@3dZ@333@2�H@2~�@2J@1��@1��@1�7@1X@1%@0Ĝ@0�9@0Ĝ@0�9@0�@0r�@01'@/�w@/�P@/\)@/;d@.�@.��@.�+@.5?@.{@-@-��@-��@-�h@-p�@,�/@,Z@,9X@,�@+�F@+��@+�@+33@+o@*�@*��@*�!@*��@*=q@)�@)�^@)x�@)G�@(�`@(��@(bN@(  @'�@'\)@'
=@&��@&�+@&�+@&v�@&E�@&$�@&{@%�T@%�@%�@%`B@%?}@%�@$�@$��@$��@$(�@#�m@#�
@#�F@#�@#dZ@#"�@"�@"�!@"M�@"J@!��@!��@!x�@!%@ Ĝ@ r�@  �@��@��@K�@�@ȴ@�R@��@E�@�@�@O�@��@��@z�@z�@j@j@�@�@o@��@J@��@�^@��@��@��@�7@�7@x�@X@Ĝ@��@�@Q�@b@�@�;@�w@|�@\)@;d@�@
=@��@�y@��@��@��@v�@$�@��@�h@�h@�h@�h@�h@p�@?}@V@�@��@�@z�@9X@1@�F@dZ@C�@@��@��@��@-@�#@�^@��@��@��@�7@x�@x�@X@7L@��@r�@1'@ �@b@��@|�@l�@\)@;d@+@�@
=@��@�y@��@�+@5?@{@�T@�T@��@�T@��@��@O�@?}@��@��@�D@z�@j@Z@I�@�@�F@��@��@�@�@t�@33@@
�@
��@
�!@
�\@
M�@
J@	�@	�#@	�^@	�7@	hs@	X@	G�@	G�G�O�A�7LA�5?A�=qA�5?A�;dA�=qA�;dA�;dA�C�A�C�A�?}A�E�A�E�A�C�A�C�A�G�A�E�A�A�A�C�A�G�A�C�A�C�A�E�A�G�A�C�A�E�A�I�A�E�A�C�A�G�A�I�A�E�A�E�A�I�A�G�A�E�A�G�A�I�A�G�A�G�A�K�A�K�A�E�A�G�A�I�A�K�A�G�A�E�A�K�A�K�A�G�A�G�A�K�A�I�A�E�A�I�A�I�A�G�A�G�A�O�A�I�A�K�A�O�A�K�A�I�A�M�A�M�A�I�A�K�A�O�A�M�A�I�A�K�A�O�A�M�A�K�A�Q�A�O�A�M�A�O�A�S�A�M�A�M�A�Q�A�Q�A�M�A�M�A�Q�A�Q�A�M�A�Q�A�S�A�M�A�Q�A�S�A�O�A�M�A�S�A�S�A�O�A�M�A�Q�A�VA�Q�A�M�A�O�A�O�A�K�A�O�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�Q�A�VA�Q�A�M�A�M�A�Q�A�O�A�M�A�Q�A�S�A�Q�A�O�A�Q�A�S�A�O�A�M�A�S�A�S�A�O�A�O�A�S�A�S�A�O�A�Q�A�S�A�5?A�1'A�1'A�1'A�&�A�$�A�&�A� �A��A�%A�A��A��A���AݾwAݙ�AݓuA�jA�Q�A�E�A��A��TAܣ�A�p�A��AۍPAڣ�A��A���A�ȴA�ĜAپwAٰ!Aٰ!A٬A١�AٓuAًDAه+A�z�A�t�A�t�A�r�A�hsA�^5A�\)A�\)A�VA�O�A�O�A�I�A�E�A�=qA�33A�-A�"�A�oA���A���A��`A�ĜAأ�AؓuA؍PA�n�A�I�A�7LA�1'A�+A� �A��A��A�oA�VA�A���A���A��yA�Aס�Aי�AבhA׍PA׉7A׋DA׋DA�|�A�r�A�jA�ffA�`BA�9XA��A�l�A�?}A���AՋDA�+A��A��AԮA�n�A�C�A��A���A��A��TA���A���A���Aӥ�A�dZA���Aҙ�A�ffA�"�A�
=A�A���AѮA�ZA�(�A��/Aв-A�7LA�  Aϩ�A��A�O�A͝�A̺^A�dZA��A��;A���A���A�~�A�oAȴ9A�x�A�5?A��`AǮA�?}A�dZAŮA���Aç�A�K�A�
=A���A�A�jA�{A���A�5?A���A��#A���A�VA���A���A���A��uA��jA�VA��
A�S�A���A�p�A�K�A��yA���A���A��+A�dZA�33A�
=A�ĜA�jA��`A���A�n�A�A���A��A���A�&�A��
A���A��A�dZA�I�A�A�A�9XA��A��^A��DA�ffA�bNA�^5A�XA�S�A�`BA�ZA�I�A�1'A�bA��A�K�A�G�A��A�-A���A�1A��uA�/A�jA���A��A��PA�33A���A��A�^5A��;A���A�E�A�A�7LA���A��A�~�A�bA�9XA��`A�ȴA��RA�A�JA��uA�7LA���A�9XA���A�ZA�oA��!A�XA���A�ȴA��9A��PA�O�A�%A���A���A�|�A�S�A�A���A�dZA�bA���A�dZA�/A���A�"�A��#A��uA�^5A�G�A�/A�{A���A��`A�ĜA�ȴA���A��RA���A��A�z�A��A��+A���A���A���A��RA���A���A��A��A�5?A�;dA�/A��A�1A��A��A��mA��#A���A�ƨA�ĜA�A�ĜA���A���A�ƨA�r�A�n�A�v�A�t�A�t�A�r�A�S�A��RA�VA�JA���A�XA��A���A�=qA���A�"�A�(�A�33A�=qA�G�A�Q�A�XA�XA�\)A���A�jA� �A���A��#A�z�A�VA��A��A��jA���A��+A�ffA�S�A�C�A�1'A���A�A�dZA�C�A���A���A��hA�/A�`BA�  A��`A���A��A�K�A���A���A�p�A�;dA� �A�A�A�p�A�A��hA�ĜA���A��RA��A�n�A���A���A�ZA�&�A��TA���A�?}A��A��9A��DA�dZA��A�-A~��A~n�A}��A}+A|1'A{oAy
=Av5?AtA�At �As�#As�AsO�ArAo?}AnVAnbAm�AmVAl��AkAk%AjVAi��Ail�Ah��Ah��Ah��Ahv�AhbAg��AhJAhJAg�TAg�FAfĜAf=qAe�mAe��Ae��Ae��Ae��AeƨAe�wAex�AeVAdE�Ac�Aa�Aa33A`~�A_�#A_?}A^��A]�wA]\)A\��A\E�A[�TA[O�AZ�AZJAY�-AYXAX�AW�;AV�AU�#AU
=AT^5AS��AS��AS�7ASp�ASl�ASC�AS�AR��AR�AR��AR�\AR~�AQ�mAQx�AQdZAQ7LAP�`AP��APȴAPv�AOO�AM?}AL�+AL1'AK��AKl�AJ�AJM�AJ1AI�mAIAI��AI�hAIp�AIK�AI�AH�AH�RAH�AHQ�AG�mAG\)AG�AF��AFz�AFE�AE��AEƨAEx�AE"�AE
=AD��ADjAC�;ACK�AB�`ABȴAB�jAB�!AB��AB��AB�uABjABA�AB �AA��AA��AAt�AA33A@��A?�mA?hsA>�/A>VA>bA=��A=�TA=��A=��A=t�A=l�A=hsA=`BA=\)A=S�A=?}A=
=A<�9A<��A<bNA<�A;�A;S�A:��A:�yA:�A:�A:��A:�RA:��A:�uA:v�A:A�A9�A9�PA9&�A8ȴA8��A8n�A8VA8(�A7��A7��A7l�A7C�A7"�A7VA7%A7oA6��A6��A6��A6�A7�A7
=A6�A6�A6�RA6�9A6��A6��A6��A6��A6��A6��A6�DA6n�A6-A6{A5�A5��A5�FA5��A5��A5��A5��A5��A5�7A5|�A5|�A5|�A5x�A5dZA5G�A5�A4��A4�A4�RA4��A4~�A4M�A4A�A45?A41'G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�4B� B�4B�B� B�B�B� B� B�4B� B�iB�4B�4B� B�4B�B�B�B~�B�SB�4B��B�B	[�B	g�B	n�B	q�B	v+B	}�B	�uB	�oB	�B	.B	}�B	�B	uZB	m]B	o�B	d&B	UgB	E9B	0!B	6�B	\�B	�YB	�/B
H�B
Z�B
�\B
��B
��B
�B
.}B
jB
}�B
jKB
_B
�oB
�B
ܒB
�lB�B
�|B
��B
ӏB
��B
iB
+kB	�B	�@B	�+B	jB	X�B	K)B	>�B	%�B	=B	JB		�B	hB	�B	DB	�B	MB	~B	kB	OB	%�B	"�B	-�B	JXB	X�B	c B	�B	��B	�B	�gB	�
B	�B	�B
  B
�B
�B
�B
�B
VB
&B
-�B
-�B
,�B
/�B
5tB
9�B
9�B
D�B
C-B
EmB
E�B
I�B
N�B
M�B
R B
S�B
T,B
T�B
V�B
W�B
YB
Z�B
Z�B
[#B
YKB
^B
[WB
Z�B
W?B
QB
K^B
I�B
J�B
Q�B
OB
M�B
NB
NpB
W
B
YKB
YB
W?B
VmB
U2B
S&B
R�B
QNB
RTB
QNB
S&B
Q�B
S�B
U2B
S�B
W�B
YKB
W�B
VmB
UgB
UgB
U�B
UgB
V9B
S�B
R�B
T�B
VmB
ZB
XEB
W�B
W
B
T,B
Q�B
K)B
G�B
@OB
A�B
HKB
I�B
H�B
K�B
LdB
M�B
MjB
N�B
Q�B
VmB
V�B
W�B
W?B
W
B
W?B
T�B
V�B
UgB
U�B
R�B
PHB
L�B
H�B
HKB
GzB
H�B
HB
F?B
F�B
E9B
CaB
C�B
AUB
@B
>wB
=�B
=<B
=B
;�B
;0B
8�B
7B
9�B
6�B
5tB
5�B
5?B
5?B
49B
4B
4B
3hB
2�B
1�B
1'B
0!B
.�B
.}B
-�B
+�B
)�B
+�B
,=B
%�B
#nB
!-B
!bB
"4B
"�B
 \B
�B
B
�B
�B
1B
�B
�B
�B
�B
$B
~B
�B

�B
bB

�B
	B

=B
JB
�B
B
�B
{B
�B
B
B
�B
�B
VB
.B
�B
4B
oB
B
B
�B
B
�B
�B
�B
�B
VB
�B
PB
xB

rB

=B
	7B
fB
fB
MB
�B
uB
oB
B
�B
�B
MB
 �B
  B
B
uB
�B
oB
B
oB
�B
�B
�B
B
�B
B
�B
B
 �B
uB
�B
GB
fB
JB
�B
~B
JB
B
DB
xB
B

�B
xB
PB
PB
PB
PB
B
�B
JB
�B
�B
�B
�B
�B
\B
\B
(B
�B
�B
�B
�B
�B
�B
VB
"B
�B
"B
VB
�B
(B
(B
�B
bB
�B
�B
�B
�B
�B
"B
VB
VB
�B
�B
(B
�B
\B
\B
B
hB
 B
bB
4B
�B
�B
�B
hB
4B
�B
�B
:B
oB
oB
�B
uB
�B
uB
�B
�B
B
�B
hB
hB
�B
�B
_B
�B
�B
�B
=B
�B
�B
CB
�B
qB
�B
CB
�B
�B
CB
xB
�B
~B
OB
�B
�B
�B
OB
�B
!bB
#:B
#�B
#nB
$tB
$�B
%�B
&B
'B
'B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(XB
)�B
)�B
*eB
*eB
*eB
*eB
*�B
+6B
*�B
+6B
*�B
*�B
,=B
,=B
,qB
,�B
,qB
,=B
,qB
,�B
-�B
/B
.�B
.}B
.}B
.�B
/B
0�B
0�B
0�B
0�B
0�B
1[B
1�B
2-B
2aB
33B
5B
4�B
4�B
4�B
5B
4�B
5tB
5tB
6B
6�B
6�B
6�B
7B
7B
6�B
7B
7B
6�B
7B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
9$B
9$B
9$B
9�B
9XB
9�B
:�B
:^B
:�B
;0B
<B
<B
<B
<B
<6B
<6B
;�B
=qB
=qB
=�B
?HB
?�B
?}B
?}B
@OB
@�B
@OB
@�B
AUB
A B
A�B
A�B
B[B
B[B
B[B
B[B
B�B
C�B
C�B
C�B
D�B
DgB
D�B
DgB
DgB
D�B
EmB
EmB
FtB
F?B
F?B
FtB
F�B
F�B
F�B
F�B
F�B
GEB
HKB
G�B
HKB
HKB
HKB
H�B
H�B
IB
IRB
J#B
JXB
J�B
J�B
J�B
J�B
J�B
K)B
J�B
K)B
K^B
K�B
K�B
K^B
L0B
L�B
M�B
M�B
M�B
M�B
M�B
MjB
M�B
M�B
NpB
N<B
N<B
NpB
N�B
PB
O�B
PB
PB
O�B
O�B
P}B
P�B
P�B
P�B
P�B
QNB
P�B
QNB
QNB
Q�B
Q�B
S&B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
UgB
V�B
VmB
VmB
VmB
VmB
V9B
VB
W
B
W�B
XB
XB
W�B
XB
XyB
W�B
YB
YB
YB
YB
YKB
Y�B
Y�B
ZB
Z�B
[WB
[WB
[WB
[WB
[WB
[WB
[�B
\]B
\]B
\]B
\�B
\�B
\�B
\�B
]�B
]dB
]/B
]dB
]dB
^5B
^jB
^5B
^5B
^B
^jB
_;B
_pB
_pB
_�B
aB
`BB
`�B
`vB
`�B
`�B
`�B
a�B
bNB
bNB
b�B
b�B
b�B
b�B
b�B
d&B
c�B
cTB
cTB
d�B
d�B
d�B
d�B
e`B
e,B
e,B
e`B
e�B
e�B
f2B
ffB
f�B
f�B
f�B
gB
f�B
f�B
gmB
g8B
g8B
hsB
h>B
hsB
h>B
h>B
h�B
h�B
iB
iDB
iB
iyB
iyB
i�B
i�B
jB
j�B
kB
kQB
kB
k�B
k�B
k�B
lWB
lWB
l�B
lWB
l�B
m]B
m�B
m�B
n/B
n�B
n/B
n�B
n�B
o�B
oiB
o�B
o�B
o�B
poB
p�B
poB
poB
p�B
p�B
p�B
qAB
qvB
qvB
qAB
qAB
rB
q�B
rGB
r|B
rGB
r�B
r|B
r|B
r|B
rB
s�B
s�B
sMB
s�B
tB
tTB
tB
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
v+B
v+B
v`B
v�B
v�B
v�B
w�B
w�B
wfB
wfB
w�B
w�B
xB
xB
x�B
x�B
x�B
x�B
y	B
y	B
y>B
y>B
yrB
zB
zB
zB
zB
zxB
zxB
z�B
z�B
{B
{B
{B
{�B
{�B
{�B
|PB
|PB
|�B
|�B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
cB
�B
�B
�B
�B
�B
�B
�iB
��B
��B
��B
��B
��B
��B
�B
�B
�B
�AB
�AB
��B
�GB
�GB
�{B
�{B
��B
�B
�B
��B
��B
��B
��B
��B
�B
��B
�B
�SB
�B
�B
�SB
��B
�YB
��B
��B
��B
��B
��B
��B
��B
�+B
��B
�_B
�_B
��B
��B
��B
��B
��B
�1B
��B
��B
��B
��B
�7B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�	B
�rB
��B
��B
��B
�B
��B
�B
�B
�B
�JB
�B
�~B
�~B
�~B
�~B
��B
��B
�PB
�PB
�"B
��B
��B
��B
��B
�"B
��B
�"B
��B
�\B
�\B
��B
��B
��B
��B
��B
�.B
�.B
��B
�bB
��B
�bB
��B
�.B
�bB
��B
��B
��B
�B
�B
�B
��B
��B
��B
�oB
��B
��B
��B
�hB�4B�oB�B��B�;B.B� B��B~�B� B��BcB�B�B��BcB� B�B�iBcB��B��BcB.B��B�4B~�B�iB�B�B~�B��B��B~�B�B��B�B~�B�B�4B~�B~�B��B��BcB.B�iB��B.B~�B��B�4BcB�B�;B� BcB�B�4B~�B�;B�iB~�B�4B�oB�4B�B�;B�B�B� B�oB�B�B�iB�BcB�B�B� B~�B��B��BcB~�B��B�BcB�B��B�B.B�B� B~�B�iB�;BcBcB�B��B�4B.B�iB�;B�B�B��BcB}�B~�B��B~�B.B��B�B~�B�B��B� B~�B~�B�4B�B~�B�B�4B.B~�BcB� B~(B}�B�BcB}�B}�B.B~]B}VB�1B�lB�7B�7B�JB�JB��B�~B��B�uB�oB�_B��B��B�B��B��B��B�^B��B�EB�TB�B�B�JB	�B	N�B	Q�B	Y�B	b�B	^jB	`B	cTB	c�B	e,B	g�B	jKB	l�B	k�B	m�B	o5B	m�B	m�B	pB	q�B	poB	pB	q�B	sB	r�B	r�B	r�B	uZB	v`B	u�B	w�B	zDB	{�B	zB	}"B	.B	��B	�B	�B	�;B	��B	��B	��B	��B	�AB	��B	�B	��B	�B	��B	��B	��B	�oB	��B	��B	�;B	~�B	~]B	.B	~(B	}�B	|�B	~]B	}�B	|PB	|B	�B	�B	.B	cB	�B	{�B	xlB	v`B	rGB	s�B	v+B	qvB	poB	m]B	m)B	ncB	l"B	j�B	i�B	m�B	rGB	w�B	l"B	l�B	f�B	e`B	a�B	aHB	k�B	bB	^5B	Z�B	V�B	`B	J�B	O�B	S�B	[�B	GzB	H�B	1�B	<�B	1�B	Z�B	4�B	2�B	0UB	-wB	'RB	&�B	!�B	#:B	&�B	4�B	/B	LdB	bNB	N�B	W�B	X�B	^B	a�B	q�B	zDB	�B	��B	�oB	�B	��B	�-B	�CB	�B
�B
!B
 �B
5�B
9�B
O�B
K�B
LdB
Q�B
Q�B
UgB
WsB
[#B
bB
e�B
qvB
}�B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�OB
��B
��B
�$B
�LB
��B
�$B
�B
�B
�XB
��B
�6B
��B
��B
�RB
�RB
��B
��B
�RB
�#B
��B
��B
��B
�hB
��B
��B
��B
��B
�%B
u%B
cTB
a�B
d�B
b�B
dZB
a�B
NB
FtB
B[B
GzB
C�B
?}B
7�B
-CB
VB	�B
%�B
0�B
#nB
#:B
OB
~B
�B
QB
��B
�	B
~]B
~�B
u�B
r|B
m�B
g�B
~(B
�MB
��B
~�B
��B
|B
{JB
{JB
tB
~�B
v�B
u�B
w�B
l"B
h�B
q�B
w�B
\]B
kB
ZQB
ZQB
YKB
Y�B
X�B
W�B
[�B
T�B
gmB
qAB
qvB
tTB
t�B
v�B
{�B
��B
��B
�.B
�MB
��B
�qB
��B
��B
�tB
�/B
�B
�B
��B
ݘB
�5B
�)B
��B
��B
��B
�WB
��B
�B
��B
یB
�]B
�dB&�B!�B$�B$@B#�B*eB0�B	B�BB
�B�B�B	7B  B
��B
��B
��B
�B
��B
�2B
��B
�B�B	B�B�B�BMB
�2B
�B
�pB
�5B
�2B
�mB
��B
��B
ĜB
�OB
��B
�B
�tB
��B
�*B
�qB
��B
�@B
�7B
�B
v�B
c B
`B
aB
kB
\�B
T�B
EB
H�B
:�B
;0B
:*B
-CB
/B
%B
*�B
+6B
fB	��B	��B	�&B	�vB	ϫB	�0B	˒B	�}B	��B	��B	��B	��B	�LB	�B	�kB	�B	�YB	��B	��B	��B	�*B	�B	��B	��B	��B	�MB	�(B	��B	��B	�4B	u�B	w�B	s�B	qAB	rB	v`B	l"B	n�B	f�B	g8B	^�B	_B	[�B	\�B	X�B	W�B	R�B	T�B	T�B	Q�B	b�B	[�B	P�B	K^B	K)B	IRB	I�B	HKB	G�B	OB	K�B	I�B	RTB	\)B	H�B	:�B	F�B	<�B	:*B	6�B	'�B	5tB	#nB	#:B	.IB	'�B	&�B	CB	VB	!�B	.B	$@B	"�B	#nB	�B	xB	.B	�B	B	�B	�B	�B	B	B	�B	�B	
=B	�B	B	�B	DB	B	�B	B	
=B	B	3�B	
�B		B	
	B	�B	hB	�B	xB		�B		�B		B	�B	
rB	xB	�B	�B	B	PB	B	�B	�B	�B	hB	@B	B	�B	�B	1B	�B	:B	�B	�B	OB	#�B	�B	qB	B	kB	eB	+B	7B	=B	�B	�B	B	CB	�B	�B	'�B	)�B	%�B	)�B	'B	!-B	�B	�B	 �B	#:B	$�B	%�B	)�B	*0B	*0B	+B	.B	6B	>wB	<�B	D3B	K�B	M�B	`�B	ZB	W?B	W?B	W?B	YKB	[�B	\�B	\�B	_�B	b�B	j�B	q�B	v�B	|�B	}VB	�SB	��B	��B	�kB	�	B	��B	��B	��B	��B	��B	�B	��B	�B	�RB	��B	�pB	�B	�B	��B	��B	��B	�8B	�DB	��B	�WB	��B	��B	�;B	��B	��B	��B	��B	�JB	�JB	��B	��B	�B	�B
oB
�B
�B
%B
YB
�B
_B
	lB
xB
xB
�B
B
PB
hB
:B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  B�]B�B�B�B�B�B�B�B�B� B�/B�B�\B�)B�+B�B�3B�B�B�BEB��B��B��B��B	\RB	h�B	o�B	r�B	x3B	�2B	��B	��B	��B	�sB	��B	��B	zSB	riB	wB	n�B	h�B	X�B	@�B	J�B	h#B	�B
 kB
P�B
d�B
��B
��B
ΑB
ϭB
X�B
z�B
�B
t�B
`�B
DB
ŁB
��B
��B,�B
�kBB
��B
�mB
}�B
LoB	�7B	��B	�zB	t*B	aLB	^^B	O�B	7wB	"wB	�B	�B	�B	�B	�B	�B	"B	 6B	yB	&�B	)�B	$sB	1�B	N�B	[1B	h�B	��B	��B	��B	��B	�B	��B	�lB
 �B

&B
�B
�B
FB
 �B
'#B
/B
/�B
.+B
1�B
7�B
:�B
<FB
F�B
EbB
IoB
I�B
L@B
O@B
O3B
T�B
T�B
UtB
V�B
X�B
X�B
Z�B
\�B
]�B
\�B
]zB
a�B
]jB
_B
[�B
S
B
MKB
KBB
MNB
TUB
QB
M�B
N�B
M�B
W~B
[�B
[�B
X�B
X�B
W�B
U�B
T&B
RiB
SB
R�B
TQB
R�B
U/B
VXB
T}B
YlB
Z�B
Y%B
W�B
V}B
V�B
V�B
V�B
W�B
TqB
T�B
V�B
XCB
[iB
YEB
YZB
X�B
VJB
UB
NB
I�B
?�B
@�B
H�B
J^B
H�B
L�B
NSB
N_B
M�B
N�B
R�B
XB
W~B
X�B
X)B
X�B
Y�B
U�B
W�B
V�B
W�B
TfB
R�B
M�B
I�B
IDB
H7B
JB
IeB
HDB
G�B
FZB
EB
FCB
B�B
AwB
?oB
>�B
>�B
>|B
=�B
<�B
:�B
8wB
<<B
7�B
6FB
6FB
6SB
5�B
4�B
4�B
4�B
4�B
3�B
2SB
2;B
0�B
/�B
03B
/QB
,�B
*}B
/CB
.�B
'�B
$qB
"B
!�B
#�B
$�B
"%B
B
B
@B
�B
�B
@B
�B
^B
XB
�B
�B
�B
�B
�B
�B
	�B

�B
�B
aB
�B
SB
	B
!B
-B
$B
�B
B
B
iB
UB
�B
B
nB
B
B
�B
cB
�B
{B
�B
0B
"B
B
fB

�B
�B

�B
	�B

1B
�B
|B
|B
�B
�B
UB
	XB
yB
ZB
 �B
B
�B
�B
B
bB
[B
B
B
qB
B
�B
.B
B
�B
_B
bB
�B
�B
RB
B
(B
PB
�B
 B
�B
B
B
B
}B
�B
�B
�B
oB
�B
B
�B
dB
LB
$B
�B
�B
uB
sB
HB
MB
sB
-B
�B
lB
�B
�B
$B
|B
�B
�B
�B
�B
�B
�B
FB
>B
6B
�B
B
�B
�B
�B
�B
mB
�B
�B
�B
�B
�B
B
B
�B
�B
|B
(B
>B
XB
hB
B
B
�B
�B
�B
�B
AB
�B
B
B
yB
2B
1B
#B
�B
]B
�B
WB
8B
eB
�B
UB
�B
�B
B
�B
�B
�B
DB
�B
vB
VB
oB
�B
�B
�B
/B
!0B
�B
�B
XB
�B
!�B
#nB
$B
#�B
$|B
$�B
&"B
'&B
'�B
(B
'wB
(KB
'�B
'�B
'�B
(B
(FB
(B
)5B
)sB
*WB
*�B
*�B
*�B
*�B
+B
+FB
,�B
,B
,�B
+:B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-GB
-�B
.�B
0EB
.�B
.�B
.�B
/B
/�B
0�B
0�B
1&B
1�B
1�B
1�B
2nB
2�B
3�B
4�B
5\B
4�B
4�B
5B
5NB
5�B
6B
6@B
6�B
7TB
7B
7	B
7�B
7>B
7B
7zB
7cB
7AB
84B
8B
8B
8=B
8[B
98B
9B
8�B
9�B
9�B
:+B
:8B
9�B
:�B
;�B
:�B
;sB
;�B
<nB
<*B
<.B
<iB
<�B
<�B
<�B
>KB
=�B
>pB
@(B
?�B
?�B
@B
@�B
@�B
@�B
AlB
A�B
A�B
B'B
BbB
B�B
B�B
B�B
B�B
C}B
D(B
D6B
D�B
D�B
D�B
D�B
D�B
D�B
EBB
FB
F$B
F�B
FvB
FyB
F�B
GB
F�B
GB
F�B
GpB
HNB
H�B
HBB
H�B
H�B
H�B
H�B
I�B
I�B
JB
J�B
J�B
J�B
KB
KB
KB
KB
K?B
KB
KcB
K�B
K�B
K�B
K�B
M"B
M�B
NRB
M�B
M�B
NB
M�B
M�B
M�B
NTB
N�B
NnB
NXB
N�B
O�B
PeB
O�B
P(B
P,B
PB
P=B
QB
QEB
Q*B
QB
QB
QtB
Q,B
Q�B
Q�B
RB
R}B
S�B
R�B
SjB
TB
S�B
S�B
TB
TB
T*B
TB
T%B
T|B
UB
U�B
V3B
V�B
V�B
V�B
V�B
V�B
VZB
V�B
W�B
XYB
X�B
XIB
X+B
X�B
X�B
XmB
Y�B
YKB
Y1B
Y\B
Y�B
ZB
ZKB
Z�B
[4B
[�B
[�B
[lB
[oB
[�B
[�B
\PB
\�B
\�B
\�B
]+B
\�B
\�B
]ZB
]�B
]lB
]VB
]�B
]�B
^�B
^�B
^[B
^kB
^RB
_B
_�B
_�B
_�B
`�B
aEB
`mB
`�B
`�B
a#B
a+B
a>B
b�B
b�B
b�B
cB
b�B
b�B
b�B
cZB
d�B
c�B
cwB
dB
eGB
eB
d�B
eB
e�B
eFB
ecB
e�B
e�B
fbB
fvB
f�B
f�B
f�B
f�B
g&B
f�B
g#B
g�B
gyB
g�B
h�B
h�B
h|B
hgB
h�B
h�B
i$B
iaB
imB
ieB
i�B
i�B
jB
j]B
kB
kPB
k|B
k�B
kUB
k�B
l B
lXB
l�B
l�B
l�B
l�B
m�B
m�B
nB
n,B
n�B
n�B
n�B
o0B
o?B
pB
o�B
o�B
o�B
p&B
p�B
p�B
pfB
p�B
p�B
p�B
p�B
q�B
q�B
q�B
qnB
q�B
rHB
rB
r�B
r�B
r�B
sB
r�B
r�B
r�B
r�B
s�B
s�B
sxB
tB
tDB
tmB
trB
t�B
t�B
t�B
uB
uB
uQB
u�B
u�B
u�B
u�B
v&B
vpB
vtB
v�B
wB
wRB
wWB
w�B
w�B
wmB
w|B
w�B
w�B
xB
x;B
x�B
x�B
x�B
x�B
y.B
y=B
ydB
ywB
y�B
zPB
z'B
z6B
zDB
z�B
z�B
z�B
{*B
{{B
{�B
{�B
{�B
{�B
|[B
|�B
|�B
|�B
}@B
}ZB
}uB
}�B
~B
~
B
}�B
~JB
~LB
B
 B
OB
�B
�B
�B
�B
�B
�B
�]B
��B
��B
�|B
��B
��B
��B
��B
�B
�B
�B
�WB
�nB
�6B
�lB
�mB
��B
��B
�B
�.B
�@B
��B
��B
��B
��B
��B
�3B
��B
�]B
�YB
�5B
�HB
��B
�jB
�nB
��B
��B
��B
��B
��B
�(B
�*B
�OB
�B
��B
��B
��B
�B
��B
�MB
�&B
�uB
��B
��B
��B
�AB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
�nB
��B
�B
��B
��B
�UB
�1B
�)B
�*B
�9B
�^B
�)B
��B
��B
��B
��B
��B
�8B
�uB
��B
�$B
��B
��B
��B
��B
�oB
�B
�fB
�)B
��B
�oB
��B
�B
�	B
�,B
��B
�QB
�2B
��B
�cB
��B
��B
��B
�GB
��B
��B
��B
��B
�IB
�)B
�B
��B
��B
��B
��B
��B
��B
��G�O�B�4B�oB�B��B�;B.B� B��B~�B� B��BcB�B�B��BcB� B�B�iBcB��B��BcB.B��B�4B~�B�iB�B�B~�B��B��B~�B�B��B�B~�B�B�4B~�B~�B��B��BcB.B�iB��B.B~�B��B�4BcB�B�;B� BcB�B�4B~�B�;B�iB~�B�4B�oB�4B�B�;B�B�B� B�oB�B�B�iB�BcB�B�B� B~�B��B��BcB~�B��B�BcB�B��B�B.B�B� B~�B�iB�;BcBcB�B��B�4B.B�iB�;B�B�B��BcB}�B~�B��B~�B.B��B�B~�B�B��B� B~�B~�B�4B�B~�B�B�4B.B~�BcB� B~(B}�B�BcB}�B}�B.B~]B}VB�1B�lB�7B�7B�JB�JB��B�~B��B�uB�oB�_B��B��B�B��B��B��B�^B��B�EB�TB�B�B�JB	�B	N�B	Q�B	Y�B	b�B	^jB	`B	cTB	c�B	e,B	g�B	jKB	l�B	k�B	m�B	o5B	m�B	m�B	pB	q�B	poB	pB	q�B	sB	r�B	r�B	r�B	uZB	v`B	u�B	w�B	zDB	{�B	zB	}"B	.B	��B	�B	�B	�;B	��B	��B	��B	��B	�AB	��B	�B	��B	�B	��B	��B	��B	�oB	��B	��B	�;B	~�B	~]B	.B	~(B	}�B	|�B	~]B	}�B	|PB	|B	�B	�B	.B	cB	�B	{�B	xlB	v`B	rGB	s�B	v+B	qvB	poB	m]B	m)B	ncB	l"B	j�B	i�B	m�B	rGB	w�B	l"B	l�B	f�B	e`B	a�B	aHB	k�B	bB	^5B	Z�B	V�B	`B	J�B	O�B	S�B	[�B	GzB	H�B	1�B	<�B	1�B	Z�B	4�B	2�B	0UB	-wB	'RB	&�B	!�B	#:B	&�B	4�B	/B	LdB	bNB	N�B	W�B	X�B	^B	a�B	q�B	zDB	�B	��B	�oB	�B	��B	�-B	�CB	�B
�B
!B
 �B
5�B
9�B
O�B
K�B
LdB
Q�B
Q�B
UgB
WsB
[#B
bB
e�B
qvB
}�B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�OB
��B
��B
�$B
�LB
��B
�$B
�B
�B
�XB
��B
�6B
��B
��B
�RB
�RB
��B
��B
�RB
�#B
��B
��B
��B
�hB
��B
��B
��B
��B
�%B
u%B
cTB
a�B
d�B
b�B
dZB
a�B
NB
FtB
B[B
GzB
C�B
?}B
7�B
-CB
VB	�B
%�B
0�B
#nB
#:B
OB
~B
�B
QB
��B
�	B
~]B
~�B
u�B
r|B
m�B
g�B
~(B
�MB
��B
~�B
��B
|B
{JB
{JB
tB
~�B
v�B
u�B
w�B
l"B
h�B
q�B
w�B
\]B
kB
ZQB
ZQB
YKB
Y�B
X�B
W�B
[�B
T�B
gmB
qAB
qvB
tTB
t�B
v�B
{�B
��B
��B
�.B
�MB
��B
�qB
��B
��B
�tB
�/B
�B
�B
��B
ݘB
�5B
�)B
��B
��B
��B
�WB
��B
�B
��B
یB
�]B
�dB&�B!�B$�B$@B#�B*eB0�B	B�BB
�B�B�B	7B  B
��B
��B
��B
�B
��B
�2B
��B
�B�B	B�B�B�BMB
�2B
�B
�pB
�5B
�2B
�mB
��B
��B
ĜB
�OB
��B
�B
�tB
��B
�*B
�qB
��B
�@B
�7B
�B
v�B
c B
`B
aB
kB
\�B
T�B
EB
H�B
:�B
;0B
:*B
-CB
/B
%B
*�B
+6B
fB	��B	��B	�&B	�vB	ϫB	�0B	˒B	�}B	��B	��B	��B	��B	�LB	�B	�kB	�B	�YB	��B	��B	��B	�*B	�B	��B	��B	��B	�MB	�(B	��B	��B	�4B	u�B	w�B	s�B	qAB	rB	v`B	l"B	n�B	f�B	g8B	^�B	_B	[�B	\�B	X�B	W�B	R�B	T�B	T�B	Q�B	b�B	[�B	P�B	K^B	K)B	IRB	I�B	HKB	G�B	OB	K�B	I�B	RTB	\)B	H�B	:�B	F�B	<�B	:*B	6�B	'�B	5tB	#nB	#:B	.IB	'�B	&�B	CB	VB	!�B	.B	$@B	"�B	#nB	�B	xB	.B	�B	B	�B	�B	�B	B	B	�B	�B	
=B	�B	B	�B	DB	B	�B	B	
=B	B	3�B	
�B		B	
	B	�B	hB	�B	xB		�B		�B		B	�B	
rB	xB	�B	�B	B	PB	B	�B	�B	�B	hB	@B	B	�B	�B	1B	�B	:B	�B	�B	OB	#�B	�B	qB	B	kB	eB	+B	7B	=B	�B	�B	B	CB	�B	�B	'�B	)�B	%�B	)�B	'B	!-B	�B	�B	 �B	#:B	$�B	%�B	)�B	*0B	*0B	+B	.B	6B	>wB	<�B	D3B	K�B	M�B	`�B	ZB	W?B	W?B	W?B	YKB	[�B	\�B	\�B	_�B	b�B	j�B	q�B	v�B	|�B	}VB	�SB	��B	��B	�kB	�	B	��B	��B	��B	��B	��B	�B	��B	�B	�RB	��B	�pB	�B	�B	��B	��B	��B	�8B	�DB	��B	�WB	��B	��B	�;B	��B	��B	��B	��B	�JB	�JB	��B	��B	�B	�B
oB
�B
�B
%B
YB
�B
_B
	lB
xB
xB
�B
B
PB
hB
:B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<X��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5k�<���<��<�0�<��
<E\�<��<�%�<#�
<-��<q��<-�h=<�=w��=)�$<�9<���<1;<#�
<#�
<#�
<#�
<#�
<���<#�
<�d�<1�<�[<��\=m;<�9�=�R<��<.��<#�
<��b<�A^<�<#�
<#�
<kM�<0�L<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020081319381320200813193813IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020082319003920200823190039QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020082319003920200823190039QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020082411445520200824114455IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                