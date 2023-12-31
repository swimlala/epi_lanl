CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-12-11T00:53:33Z creation; 2022-11-21T21:41:25Z DMQC;      
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
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \p   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  dL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h @,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h gp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191211005333  20221121214125  1902034 1902034 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               r   rAA  AOAO6792_008502_114                 6792_008502_114                 2C  2C  DD  SOLO_II                         SOLO_II                         8502                            8502                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��| ���@��| ���11  @��|-�@��|-��D��7"�D��7"@D�:э&@D�:э&11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 @�\@=p�@�  @�G�@�  @�  A ��A�RAp�A)��A?\)A_\)A\)A�Q�A�
=A�  A�Q�AϮA߮A�Q�A��B�B�
B  B�B'�B0  B7�B?�
BHQ�BP  BX  B`Q�BhQ�BpQ�BxQ�B�=qB�(�B��B��B�  B�  B��
B��B�  B��B�{B�  B�  B�  B��B�{B�(�B�{B�  B�{B�(�B�{B�  B�(�B�  B�  B�(�B�=qB��B��
B�(�B�  B��
C�C��C�C�C

=C  C�C  C
=C
=C
=C  C{C(�C{C 
=C"  C$  C&  C'��C*  C,  C-�C/�C1�C3�HC6  C7�C9��C<�C>
=C?��CB
=CD
=CF
=CH  CI�CK��CN
=CP{CR
=CS��CU�HCW��CY��C\{C^(�C`�Cb  Cc�Ce�Cg�Ci��Ck��Cm��Co��Cq�HCs�HCv  Cx
=Cz  C|  C~
=C�  C���C�  C�C�
=C�  C���C�  C�  C�  C�  C���C��C�  C�C���C���C�C�C�  C�  C�  C�  C���C���C�
=C�  C�  C�
=C�
=C�\C�
=C���C�C�\C�C�  C�  C��C���C�
=C�
=C�
=C�  C���C�C�
=C�  C�C���C���C���C�  C�\C�
=C���C�  C�  C���C�  C�C���C���C���C���C��C���C���C�C�C�C�  C�C���C���C�  C�  C�  C�C�C�  C���C���C���C���C��C�  C�C�  C���C���C�
=C�
=C�C�  C���C�C�C�  C�C���C���C���C�C�C���C��C�  C�
=C�  C���C�  C�  C�  C�  C���C���C���C�  C���C�  C���C���C�  C���C�  C���C�D   D xRD ��D� D  D��D  D� D��DxRD��Dz�D�D��D�D� D  D}qD��D	� D	�qD
}qD  D� D  D��D�Dz�D��Dz�D�qD}qD�qD� D�qD� DD� D  D��D  D}qD  D�DD��D  D��D  D� D  D� D�qD}qD�D��D�D�D  D}qD�qD� D�D��D��D }qD!D!� D!�qD"� D"��D#� D$D$��D%D%��D&�D&��D'  D'� D(  D(� D(�qD)}qD*  D*� D+  D+� D,  D,� D-  D-� D-��D.}qD/  D/}qD0  D0�D1�D1��D2�D2�D3  D3z�D4  D4� D5  D5��D6  D6}qD6��D7� D8  D8}qD9  D9��D:  D:}qD:�qD;� D;�qD<}qD=  D=� D>  D>� D?�D?}qD@�D@�DA�DA� DB  DB� DB��DC� DD�DD��DE�DE� DE�qDF� DG�DG�DHDH��DI  DI� DJ  DJ� DK�DK��DL�DL��DM�DM}qDM�qDN}qDN�qDO}qDO��DP}qDQ�DQ��DR  DR� DS�DS��DS��DTz�DT�qDU� DV  DV� DV�qDW� DX  DX� DY  DY��DZ�DZ}qDZ�qD[}qD[�qD\}qD]�D]� D]�qD^� D_  D_� D`  D`��Da�Da��Db�Db� Dc  Dc� Dc��Dd��De�De� De�qDf}qDg  Dg� Dg�qDh}qDi  Di� Dj�Dj}qDj��Dk� DlDl��Dm  Dm� Dm�qDn}qDn�qDo� Dp  Dpz�Dp�qDq}qDr  Dr� Ds  Ds}qDt�Dt��Du  Du� Dv  Dv��DwDw}qDw�qDx� Dx�qDy� Dz�Dz��D{  D{z�D{�qD|� D}�D}��D}�qD~}qD  D��D�HD�>�D�~�D�� D�HD�AHD��HD�� D�  D�B�D���D�� D���D�@ D��HD�D��D�AHD�|)D��qD�HD�AHD��HD��HD�  D�@ D�~�D��qD���D�AHD�� D���D�  D�@ D�� D�� D��D�B�D��HD��HD�HD�AHD�� D�� D�HD�B�D���D���D���D�AHD���D��HD���D�@ D�� D��HD�HD�>�D�� D���D�  D�>�D�}qD��HD�D�@ D�~�D��HD�HD�>�D��HD��HD�  D�@ D�~�D���D��qD�=qD�~�D��qD��qD�=qD��HD�� D�  D�AHD�~�D��qD��qD�>�D�� D��HD�HD�B�D��HD��qD��qD�=qD�}qD�� D�  D�@ D��HD�� D���D�<)D�|)D��)D���D�@ D�� D��HD���D�:�D�}qD��HD�HD�>�D��HD�D�HD�@ D�~�D�� D��D�@ D�� D�� D��qD�=qD�~�D�� D��qD�>�D���D��D��D�@ D��HD�D��D�AHD�~�D�� D��D�@ D�~�D���D��D�@ D�}qD��qD���D�>�D��HD�D�  D�=qD��HD�D���D�>�D�~�D�� D�HD�>�D��HD�D��qD�>�D��HD��HD��D�C�D���D�D��D�C�D���D��HD���D�>�D�� D�� D�  D�>�D�~�D��qD��)D�>�D���D��HD���D�@ D�� D�� D��)D�=qD�� D�D��D�@ D���D���D��)D�@ D�� D�� D�HD�AHD���D�� D�HD�C�D�� D���D���D�>�D�� D�� D���D�>�D�� D��HD�  D�<)D�}qD�� D�  D�=qD�|)D���D���D�=qD�~�D�� D�  D�AHD��HD�D��qD�=qD�~�D��qD�  D�@ D�}qD���D�  D�C�D�� D��HD�  D�<)D�y�D��qD�  D�AHD��HD���D��)D�<)D�� D��HD��D�B�D�}qDº�D��qD�>�D�~�D�� D��D�AHD�}qDĽqD��qD�>�Dł�D�D��D�B�DƁHDƾ�D�  D�@ D�~�DǾ�D�  D�AHDȀ D�� D��D�>�D�~�Dɾ�D�  D�AHDʀ D��HD�  D�>�DˁHD˾�D��qD�@ D̀ D�� D�HD�@ D�}qD�� D�  D�@ D΁HDξ�D���D�@ Dπ DϾ�D���D�@ DЁHDо�D�  D�AHDр DѾ�D�  D�>�D�}qDҾ�D��qD�>�DӀ D�� D�  D�AHDԀ D�� D�HD�AHDՂ�D���D��D�AHD�~�D�� D�  D�AHDׂ�D�D�  D�=qD؀ D��HD�  D�@ D�~�D�� D�  D�@ D�~�DڽqD�  D�@ Dۀ D�� D�  D�>�D�~�D�� D�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�@ D�~�D�� D��D�C�D�� D�� D�HD�@ D�}qD�qD���D�AHD� D�� D���D�>�D� D��HD�  D�AHD�HD��HD�HD�AHD�~�D徸D�  D�>�D� D�� D��qD�>�D�HD���D��D�B�D�HD�� D�HD�@ D�~�D龸D�  D�@ D�HD�� D���D�=qD�~�D�� D���D�=qD�~�D�� D���D�@ D�~�D�� D�HD�@ D�~�D�)D���D�B�D� D��HD�HD�>�D�� D�� D�  D�AHD�~�D�)D��qD�AHD�~�D�qD���D�@ D� D�qD���D�@ D�~�D�� D���D�AHD��HD�� D���D�=qD��HD�� D���D�AHD��HD���D��qD�=qD�~�D�� D�HD�@ D�~�D���D��\?�(�?�?�G�@z�@��@+�@#�
@G�@B�\@c�
@^�R@�G�@xQ�@���@�=q@��H@���@���@���@�
=@�@�ff@��
@�z�@У�@�\@�  @���@���@�(�@��HAz�AffA
=qAp�AG�Az�A=qA�HA!�A#�
A*=qA)��A333A2�\A<(�A<(�AC�
AE�AMp�AN{AW
=AX��A`  A`��Aj=qAj=qAq�Au�Az�HA�  A���A�z�A�
=A��A�33A�
=A�Q�A��
A��A���A�=qA�{A�
=A��HA��A�Q�A���A���A�{A�=qA��\A�
=A�\)A��
A���A���A���A�{A�{A��HA��HAϮA�  A�(�A���A���A�G�A�A�ffA��HA��HA�ffA�  A�A�z�A�A�A�A��RA�\)A��\A�(�A��RB   B{B�B(�B(�BffB=qBz�Bz�B
�RB
�RB��B�B33B33BG�BB�Bz�B��B33B
=Bp�BG�B�B  Bp�B=qB (�B Q�B"�\B"ffB$z�B$��B&�HB&�HB(��B)��B*�RB+�
B,��B.{B/33B0(�B1p�B2ffB4(�B4��B6{B7
=B8z�B9��B:=qB<(�B<z�B>�RB>�\B@��BA�BC33BC33BEp�BEp�BG�BG�BI�BIBL(�BL  BN=qBN{BPz�BPQ�BRffBR�RBTQ�BT��BV�RBW
=BY�BY�B[�B[\)B]B]��B_�
B_�
Ba�BaBd(�Bc�
Bf{Be�Bh  BhQ�BiBj�\Bk�Bl��Bm�Bo
=Bo33Bqp�BqG�Bs�Bs�
Bu�Bv=qBw\)Bx��ByG�B{\)B{\)B}B}��B�  B�{B���B�p�B��
B�z�B��HB�B�B���B��HB��B�(�B�ffB�p�B�\)B�z�B�ffB���B�p�B��\B�z�B��B�p�B�z�B��RB�p�B��
B�ffB�
=B�G�B�=qB�=qB�\)B�\)B��\B�ffB���B��B���B��\B���B�B�ffB�33B�33B�Q�B�=qB�p�B�\)B�Q�B�ffB�\)B��B�(�B���B��HB�  B��B��HB�G�B��B��\B�z�B��B���B�Q�B���B��B��
B�=qB��HB�
=B�{B�  B��B���B�(�B�  B��B���B�  B��B�
=B��HB��
B�(�B��\B�33B�\)B�Q�B�(�B�G�B�G�B��
B�Q�B��RB�\)B���B�{B��\B�p�B�\)B�ffB�=qB�\)B�33B�Q�B�=qB�
=B��B�  B��\B�
=B��B��
B���B���B��B��B�
=B���B�(�B�  B�33B��B�(�B�Q�B�
=B�\)B�(�B�z�B��BÙ�B�  BĸRB�
=B�B�  B��B�
=B�(�B�(�B�\)B�33B�ffB�ffB�p�B˙�Ḅ�B���B͙�B�BθRB��Bϙ�B�ffBиRBѮBѮB���B���B��B��B�
=B�
=B�(�B�=qB�33B�\)B�Q�B�z�Bٙ�B�Bڣ�B��Bۙ�B�z�Bܣ�B�B�B�
=B���B�{B�=qB�G�B�p�B�z�B��HB�\)B�{B�ffB�\)B�\)B�\B�\B�B�B�RB��HB��B�Q�B��B�B�=qB�
=B�B�  B���B�G�B��
B��B��HB�{B�  B�G�B�G�B�Q�B��RB�p�B�B���B�G�B�p�B�z�B�z�B�B��B���B���B�  B�{B��HB�p�B�  B�z�B��HB��
B�C �C p�C
=C
=C��C�\C�C�C�C�C=qCQ�C�RC�C=qC�C�
C
=CffC�C��C33C��C�RC�CffC��C��C�C��C��C	=qC	(�C	��C	C
Q�C
ffC
��C
��Cp�C��C�
C=qCz�C��C
=CffC��C  C{C��C��C=qC(�CC�RCQ�CG�CC��C=qC�C�
CG�C=qC�
C�
CQ�Cz�C��C=qC=qC��CCffC\)C��C�C�C�C{C{C��C�C33C33CC��C\)C\)C�
C{CQ�CC�
CQ�Cp�C��C�C�C�C  C33C�\CC�C33C��CC\)C\)C��C�C z�C �C!{C!33C!p�C!�C!�C"�C"�C#
=C#{C#�\C#�RC$(�C$p�C$�\C%  C%�C%��C%��C&=qC&=qC&�RC&�HC'(�C'�\C'��C(33C((�C(C(C)G�C)p�C)�HC*�C*ffC*�
C+  C+�C+�\C,(�C,(�C,��C,��C-ffC-ffC.  C.  C.�C.C/
=C/z�C/��C0(�C0(�C0��C0��C1\)C1p�C1�C2{C2�C2��C3  C3p�C3z�C4�C4�C4�RC4�RC533C5p�C5C6�C6=qC6��C6��C7p�C7p�C7��C8  C8��C8��C9{C9p�C9��C9�C:G�C:��C:C;G�C;Q�C;�C;�HC<p�C<�\C<�C=Q�C=ffC=��C=��C>�C>�C?(�C?�C?�C?C@=qC@ffC@��CA{CA33CACACB\)CB\)CB��CB�CC�CC�\CD{CD�CD��CD��CE33CEffCECF
=CF=qCFCF��CGG�CGffCG�CG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                  @�\@=p�@�  @�G�@�  @�  A ��A�RAp�A)��A?\)A_\)A\)A�Q�A�
=A�  A�Q�AϮA߮A�Q�A��B�B�
B  B�B'�B0  B7�B?�
BHQ�BP  BX  B`Q�BhQ�BpQ�BxQ�B�=qB�(�B��B��B�  B�  B��
B��B�  B��B�{B�  B�  B�  B��B�{B�(�B�{B�  B�{B�(�B�{B�  B�(�B�  B�  B�(�B�=qB��B��
B�(�B�  B��
C�C��C�C�C

=C  C�C  C
=C
=C
=C  C{C(�C{C 
=C"  C$  C&  C'��C*  C,  C-�C/�C1�C3�HC6  C7�C9��C<�C>
=C?��CB
=CD
=CF
=CH  CI�CK��CN
=CP{CR
=CS��CU�HCW��CY��C\{C^(�C`�Cb  Cc�Ce�Cg�Ci��Ck��Cm��Co��Cq�HCs�HCv  Cx
=Cz  C|  C~
=C�  C���C�  C�C�
=C�  C���C�  C�  C�  C�  C���C��C�  C�C���C���C�C�C�  C�  C�  C�  C���C���C�
=C�  C�  C�
=C�
=C�\C�
=C���C�C�\C�C�  C�  C��C���C�
=C�
=C�
=C�  C���C�C�
=C�  C�C���C���C���C�  C�\C�
=C���C�  C�  C���C�  C�C���C���C���C���C��C���C���C�C�C�C�  C�C���C���C�  C�  C�  C�C�C�  C���C���C���C���C��C�  C�C�  C���C���C�
=C�
=C�C�  C���C�C�C�  C�C���C���C���C�C�C���C��C�  C�
=C�  C���C�  C�  C�  C�  C���C���C���C�  C���C�  C���C���C�  C���C�  C���C�D   D xRD ��D� D  D��D  D� D��DxRD��Dz�D�D��D�D� D  D}qD��D	� D	�qD
}qD  D� D  D��D�Dz�D��Dz�D�qD}qD�qD� D�qD� DD� D  D��D  D}qD  D�DD��D  D��D  D� D  D� D�qD}qD�D��D�D�D  D}qD�qD� D�D��D��D }qD!D!� D!�qD"� D"��D#� D$D$��D%D%��D&�D&��D'  D'� D(  D(� D(�qD)}qD*  D*� D+  D+� D,  D,� D-  D-� D-��D.}qD/  D/}qD0  D0�D1�D1��D2�D2�D3  D3z�D4  D4� D5  D5��D6  D6}qD6��D7� D8  D8}qD9  D9��D:  D:}qD:�qD;� D;�qD<}qD=  D=� D>  D>� D?�D?}qD@�D@�DA�DA� DB  DB� DB��DC� DD�DD��DE�DE� DE�qDF� DG�DG�DHDH��DI  DI� DJ  DJ� DK�DK��DL�DL��DM�DM}qDM�qDN}qDN�qDO}qDO��DP}qDQ�DQ��DR  DR� DS�DS��DS��DTz�DT�qDU� DV  DV� DV�qDW� DX  DX� DY  DY��DZ�DZ}qDZ�qD[}qD[�qD\}qD]�D]� D]�qD^� D_  D_� D`  D`��Da�Da��Db�Db� Dc  Dc� Dc��Dd��De�De� De�qDf}qDg  Dg� Dg�qDh}qDi  Di� Dj�Dj}qDj��Dk� DlDl��Dm  Dm� Dm�qDn}qDn�qDo� Dp  Dpz�Dp�qDq}qDr  Dr� Ds  Ds}qDt�Dt��Du  Du� Dv  Dv��DwDw}qDw�qDx� Dx�qDy� Dz�Dz��D{  D{z�D{�qD|� D}�D}��D}�qD~}qD  D��D�HD�>�D�~�D�� D�HD�AHD��HD�� D�  D�B�D���D�� D���D�@ D��HD�D��D�AHD�|)D��qD�HD�AHD��HD��HD�  D�@ D�~�D��qD���D�AHD�� D���D�  D�@ D�� D�� D��D�B�D��HD��HD�HD�AHD�� D�� D�HD�B�D���D���D���D�AHD���D��HD���D�@ D�� D��HD�HD�>�D�� D���D�  D�>�D�}qD��HD�D�@ D�~�D��HD�HD�>�D��HD��HD�  D�@ D�~�D���D��qD�=qD�~�D��qD��qD�=qD��HD�� D�  D�AHD�~�D��qD��qD�>�D�� D��HD�HD�B�D��HD��qD��qD�=qD�}qD�� D�  D�@ D��HD�� D���D�<)D�|)D��)D���D�@ D�� D��HD���D�:�D�}qD��HD�HD�>�D��HD�D�HD�@ D�~�D�� D��D�@ D�� D�� D��qD�=qD�~�D�� D��qD�>�D���D��D��D�@ D��HD�D��D�AHD�~�D�� D��D�@ D�~�D���D��D�@ D�}qD��qD���D�>�D��HD�D�  D�=qD��HD�D���D�>�D�~�D�� D�HD�>�D��HD�D��qD�>�D��HD��HD��D�C�D���D�D��D�C�D���D��HD���D�>�D�� D�� D�  D�>�D�~�D��qD��)D�>�D���D��HD���D�@ D�� D�� D��)D�=qD�� D�D��D�@ D���D���D��)D�@ D�� D�� D�HD�AHD���D�� D�HD�C�D�� D���D���D�>�D�� D�� D���D�>�D�� D��HD�  D�<)D�}qD�� D�  D�=qD�|)D���D���D�=qD�~�D�� D�  D�AHD��HD�D��qD�=qD�~�D��qD�  D�@ D�}qD���D�  D�C�D�� D��HD�  D�<)D�y�D��qD�  D�AHD��HD���D��)D�<)D�� D��HD��D�B�D�}qDº�D��qD�>�D�~�D�� D��D�AHD�}qDĽqD��qD�>�Dł�D�D��D�B�DƁHDƾ�D�  D�@ D�~�DǾ�D�  D�AHDȀ D�� D��D�>�D�~�Dɾ�D�  D�AHDʀ D��HD�  D�>�DˁHD˾�D��qD�@ D̀ D�� D�HD�@ D�}qD�� D�  D�@ D΁HDξ�D���D�@ Dπ DϾ�D���D�@ DЁHDо�D�  D�AHDр DѾ�D�  D�>�D�}qDҾ�D��qD�>�DӀ D�� D�  D�AHDԀ D�� D�HD�AHDՂ�D���D��D�AHD�~�D�� D�  D�AHDׂ�D�D�  D�=qD؀ D��HD�  D�@ D�~�D�� D�  D�@ D�~�DڽqD�  D�@ Dۀ D�� D�  D�>�D�~�D�� D�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�@ D�~�D�� D��D�C�D�� D�� D�HD�@ D�}qD�qD���D�AHD� D�� D���D�>�D� D��HD�  D�AHD�HD��HD�HD�AHD�~�D徸D�  D�>�D� D�� D��qD�>�D�HD���D��D�B�D�HD�� D�HD�@ D�~�D龸D�  D�@ D�HD�� D���D�=qD�~�D�� D���D�=qD�~�D�� D���D�@ D�~�D�� D�HD�@ D�~�D�)D���D�B�D� D��HD�HD�>�D�� D�� D�  D�AHD�~�D�)D��qD�AHD�~�D�qD���D�@ D� D�qD���D�@ D�~�D�� D���D�AHD��HD�� D���D�=qD��HD�� D���D�AHD��HD���D��qD�=qD�~�D�� D�HD�@ D�~�D���G�O�?�(�?�?�G�@z�@��@+�@#�
@G�@B�\@c�
@^�R@�G�@xQ�@���@�=q@��H@���@���@���@�
=@�@�ff@��
@�z�@У�@�\@�  @���@���@�(�@��HAz�AffA
=qAp�AG�Az�A=qA�HA!�A#�
A*=qA)��A333A2�\A<(�A<(�AC�
AE�AMp�AN{AW
=AX��A`  A`��Aj=qAj=qAq�Au�Az�HA�  A���A�z�A�
=A��A�33A�
=A�Q�A��
A��A���A�=qA�{A�
=A��HA��A�Q�A���A���A�{A�=qA��\A�
=A�\)A��
A���A���A���A�{A�{A��HA��HAϮA�  A�(�A���A���A�G�A�A�ffA��HA��HA�ffA�  A�A�z�A�A�A�A��RA�\)A��\A�(�A��RB   B{B�B(�B(�BffB=qBz�Bz�B
�RB
�RB��B�B33B33BG�BB�Bz�B��B33B
=Bp�BG�B�B  Bp�B=qB (�B Q�B"�\B"ffB$z�B$��B&�HB&�HB(��B)��B*�RB+�
B,��B.{B/33B0(�B1p�B2ffB4(�B4��B6{B7
=B8z�B9��B:=qB<(�B<z�B>�RB>�\B@��BA�BC33BC33BEp�BEp�BG�BG�BI�BIBL(�BL  BN=qBN{BPz�BPQ�BRffBR�RBTQ�BT��BV�RBW
=BY�BY�B[�B[\)B]B]��B_�
B_�
Ba�BaBd(�Bc�
Bf{Be�Bh  BhQ�BiBj�\Bk�Bl��Bm�Bo
=Bo33Bqp�BqG�Bs�Bs�
Bu�Bv=qBw\)Bx��ByG�B{\)B{\)B}B}��B�  B�{B���B�p�B��
B�z�B��HB�B�B���B��HB��B�(�B�ffB�p�B�\)B�z�B�ffB���B�p�B��\B�z�B��B�p�B�z�B��RB�p�B��
B�ffB�
=B�G�B�=qB�=qB�\)B�\)B��\B�ffB���B��B���B��\B���B�B�ffB�33B�33B�Q�B�=qB�p�B�\)B�Q�B�ffB�\)B��B�(�B���B��HB�  B��B��HB�G�B��B��\B�z�B��B���B�Q�B���B��B��
B�=qB��HB�
=B�{B�  B��B���B�(�B�  B��B���B�  B��B�
=B��HB��
B�(�B��\B�33B�\)B�Q�B�(�B�G�B�G�B��
B�Q�B��RB�\)B���B�{B��\B�p�B�\)B�ffB�=qB�\)B�33B�Q�B�=qB�
=B��B�  B��\B�
=B��B��
B���B���B��B��B�
=B���B�(�B�  B�33B��B�(�B�Q�B�
=B�\)B�(�B�z�B��BÙ�B�  BĸRB�
=B�B�  B��B�
=B�(�B�(�B�\)B�33B�ffB�ffB�p�B˙�Ḅ�B���B͙�B�BθRB��Bϙ�B�ffBиRBѮBѮB���B���B��B��B�
=B�
=B�(�B�=qB�33B�\)B�Q�B�z�Bٙ�B�Bڣ�B��Bۙ�B�z�Bܣ�B�B�B�
=B���B�{B�=qB�G�B�p�B�z�B��HB�\)B�{B�ffB�\)B�\)B�\B�\B�B�B�RB��HB��B�Q�B��B�B�=qB�
=B�B�  B���B�G�B��
B��B��HB�{B�  B�G�B�G�B�Q�B��RB�p�B�B���B�G�B�p�B�z�B�z�B�B��B���B���B�  B�{B��HB�p�B�  B�z�B��HB��
B�C �C p�C
=C
=C��C�\C�C�C�C�C=qCQ�C�RC�C=qC�C�
C
=CffC�C��C33C��C�RC�CffC��C��C�C��C��C	=qC	(�C	��C	C
Q�C
ffC
��C
��Cp�C��C�
C=qCz�C��C
=CffC��C  C{C��C��C=qC(�CC�RCQ�CG�CC��C=qC�C�
CG�C=qC�
C�
CQ�Cz�C��C=qC=qC��CCffC\)C��C�C�C�C{C{C��C�C33C33CC��C\)C\)C�
C{CQ�CC�
CQ�Cp�C��C�C�C�C  C33C�\CC�C33C��CC\)C\)C��C�C z�C �C!{C!33C!p�C!�C!�C"�C"�C#
=C#{C#�\C#�RC$(�C$p�C$�\C%  C%�C%��C%��C&=qC&=qC&�RC&�HC'(�C'�\C'��C(33C((�C(C(C)G�C)p�C)�HC*�C*ffC*�
C+  C+�C+�\C,(�C,(�C,��C,��C-ffC-ffC.  C.  C.�C.C/
=C/z�C/��C0(�C0(�C0��C0��C1\)C1p�C1�C2{C2�C2��C3  C3p�C3z�C4�C4�C4�RC4�RC533C5p�C5C6�C6=qC6��C6��C7p�C7p�C7��C8  C8��C8��C9{C9p�C9��C9�C:G�C:��C:C;G�C;Q�C;�C;�HC<p�C<�\C<�C=Q�C=ffC=��C=��C>�C>�C?(�C?�C?�C?C@=qC@ffC@��CA{CA33CACACB\)CB\)CB��CB�CC�CC�\CD{CD�CD��CD��CE33CEffCECF
=CF=qCFCF��CGG�CGffCG�CG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�;dA�9XA�9XA�5?A�33A�5?A�7LA�9XA�?}A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�VA�XA�ZA�ZA�ZA�VA�S�A�S�A�VA�VA�XA�XA�XA�ZA�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�`BA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�hsA�jA�jA�hsA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�hsA�p�A�l�A�l�A�jA�jA�jA�jA�jA�jA�l�A�l�A�bNA�^5A�^5A�bNA�`BA�^5A�`BA�\)A�XA�XA�\)A�`BA�^5A�`BA�bNA�bNA�dZA�bNA�^5A�bNA�dZA�bNA�dZA�jA�l�A�n�A�n�A�p�A�jA�^5A�Q�A�M�A�=qA�E�A�;dA�G�A�E�A�C�A�I�A�G�A�A�A��A��A���A���A�S�A�1'A�JA���A���A��hA�~�A�l�A�`BA�VA�33A���A��+A�Q�A�+A��A��A\)A+A~�/A~ȴA~�A~JA}�TA}��A}�wA}�-A}��A}p�A}7LA|��A|r�A|I�A|{A{��A{�PA{�A{p�A{dZA{\)A{+Az�9AzI�Az(�Ay�FAyt�AyO�Ay�Ax��AxVAw�mAw�7AwAv�uAvn�Av�Au�Aux�AuO�At��At��AtQ�At(�As�TAs\)Ar�9Aq��Aq+Ap�\ApQ�Ao��AooAnffAm�-Am33Al��Al-Ak�FAk�7AkK�Aj�Aj~�Aj-Ai�FAh�yAhjAg�TAg33Af�Ae�
Ae\)Ae33AeVAe%Ad��Ad�/AdM�Ac��Ac
=Ab��Ab~�Ab5?Aa��Aa�Aa+A`��A`ffA`^5A`E�A_G�A^��A^v�A]��A\��A\n�A\9XA\{A[�mA[�^A[�A[�AZĜAZ��AZffAZ-AY��AY\)AX��AXr�AXbNAXI�AX �AWAWXAW"�AV�yAV�AVffAVE�AU��AUx�AT��AT�jAT��AT^5ATQ�AT �ATAS�#AS��AS`BAR��AR�!ARE�AR{AQ�AQ�#AQ�wAQ�AQ|�AQ�AP�APM�AP1'AO��AO�AOhsAO&�AN��AN��AN5?AM�AM�^AMx�AMVAL�jALI�AK�
AK+AJ�AJĜAJbNAJ�AI��AI7LAH��AG�AGoAF��AF�RAF��AF�+AF9XAE�^AE7LAEoAD��AD�HAD�!AD�AD�AC�FAC��ACS�AB�`ABffAB�AA`BAA
=A@�A?�A?XA>�RA>1'A=��A=�A<VA;x�A;\)A;"�A:��A:��A:�A:(�A9A9��A9;dA8�uA8�A7�mA7��A7l�A7K�A7&�A6ȴA6JA5�A4  A3��A3
=A2��A2�A2�9A1�A1�
A1\)A0�RA0v�A0JA/A/��A/+A.��A.�HA.�A-�;A-l�A-&�A,bNA+dZA*  A)��A(�HA(�DA(=qA(1'A(bA(  A'�mA'��A'��A't�A'&�A&5?A%\)A%oA%A$�9A$E�A$-A#��A"E�A!�;A!��A!S�A!33A �/A ~�A 9XA��A+A�RA�A�FAt�A/A��A^5A5?A�AXAv�AZAA�A5?A�#At�A��A�!A~�AbNA9XA��AdZA+Ar�A�-Al�A�9A�A�A�yA�\A�wA`BA�`A��An�Al�AȴA�A�A�^A�Al�AS�A;dA�;A
��A
�A
n�A
 �A	�A	�PA��A��A�9AVA��AĜA�A��A��AdZA\)A|�AG�A�DA9XAA`BA33An�A�At�A�A ��@��@��R@���@�E�@���@��7@�j@�=q@���@�X@��@���@�ff@�=q@���@�Ĝ@�"�@�@�%@�(�@�?}@��y@�/@�u@�(�@�F@��@�+@���@�G�@�@�l�@��@◍@�-@�@��/@߾w@���@���@���@�z�@ۅ@ٺ^@�Ĝ@؋D@�bN@�1@�;d@֧�@��T@��@�r�@��@�C�@�?}@�  @�;d@�v�@���@�dZ@ʧ�@�=q@�J@�@�`B@��@���@ǝ�@ǅ@�C�@���@�?}@�  @�+@\@��^@��9@�  @�t�@���@���@�$�@�7L@���@�z�@�1@��w@�t�@�"�@��y@���@�M�@��T@�`B@�r�@��!@�@��7@���@��H@�=q@���@�`B@��@�r�@���@�-@�%@�j@�K�@�^5@���@��@�7L@���@��D@�9X@�b@���@��H@��R@���@�~�@�M�@�=q@���@�(�@��;@��@��-@�?}@��9@���@�+@���@�n�@���@��T@���@���@���@�b@�1@�  @��F@�33@���@��@���@�x�@�&�@��D@�1@�ƨ@�K�@��@�o@�
=@�@��@�ȴ@�M�@�@���@�x�@�7L@���@��D@�A�@�1@���@�S�@��R@�n�@���@�hs@�&�@���@��/@�j@��@�K�@��\@�@��h@�&�@��/@��@�9X@� �@��@�  @��@��m@��@�dZ@��H@�ff@�5?@�@��@���@�V@���@��9@�j@��@��@�;d@�o@���@�v�@�5?@��-@�p�@�O�@��@��u@�I�@���@���@�o@��+@�^5@�M�@�$�@��T@���@�/@�Ĝ@�A�@\)@~ff@}��@{t�@zn�@z-@y�@yhs@xr�@w�;@w��@v�R@u@u�h@u?}@t9X@so@r��@r~�@rn�@r=q@q7L@p�9@p��@p1'@o�@o�w@o�P@n��@n{@m��@m@m�@m�@l�/@lZ@lI�@l1@k�F@k�@k@j��@j�\@j^5@jM�@j�@i��@i��@iG�@iG�@i�@h��@hA�@h �@g�@g��@gl�@g+@f��@fV@f5?@f$�@f{@e�T@e�T@e�T@e@e��@e�h@e/@d�D@dZ@dI�@d(�@d1@c�
@c��@ct�@cC�@b�H@b-@a�@a�#@a�#@a�#@a��@a��@aG�@`��@`�9@`r�@_��@_;d@^�y@^v�@^ff@^V@]@]�@]/@\��@\��@\Z@[ƨ@["�@Z~�@Z�@Y�#@Y�^@Y%@X��@Xr�@XA�@W�@W|�@W
=@V�@Vv�@U��@U`B@UV@T�@TI�@T(�@T1@S��@S�@S33@S@R��@RM�@Q�@Q7L@PQ�@O�w@O+@N�R@Nff@N5?@N{@N@M��@M�-@M��@M�h@M�@Mp�@L��@L�j@L��@Lz�@L(�@K��@K�@KdZ@K"�@Ko@J�H@J�\@J-@I��@I�7@Ix�@Ihs@I&�@H��@H��@H��@H�@Hr�@Hr�@Hr�@HQ�@Hb@H  @G|�@F�@F�+@FV@F5?@F5?@F{@E�-@E�-@E��@Ep�@E�@E`B@E�@D��@D�@D��@D�@D�D@Dz�@DZ@D9X@D9X@C��@C�F@Ct�@C33@Co@Bn�@B�@A�@A��@A�^@A��@A�7@Ax�@AX@A%@@Ĝ@@��@@�u@@�@@r�@@bN@@Q�@@A�@@ �@@  @?�;@?�@?�P@?|�@?|�@?\)@?K�@?
=@>�y@>�@>�R@>V@>V@>{@=�T@=��@=�-@=��@=�h@=�@=�@=p�@=`B@=O�@=O�@=/@<��@<z�@<9X@<�@;�
@;�@;S�@;"�@;"�@;o@:�@:�H@:��@:�!@:��@:��@:�\@:~�@:=q@:�@:�@:�@:J@9��@9�#@9�#@9�#@9��@9�^@9��@9��@9��@9�7A�1'A�/A�33A�33A�33A�9XA�;dA�;dA�=qA�;dA�;dA�7LA�9XA�7LA�=qA�=qA�7LA�5?A�7LA�5?A�5?A�5?A�33A�33A�33A�33A�33A�1'A�5?A�33A�5?A�33A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�9XA�9XA�=qA�M�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�S�A�VA�XA�VA�XA�VA�XA�XA�XA�XA�XA�XA�VA�ZA�XA�XA�ZA�XA�ZA�ZA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�ZA�ZA�XA�XA�VA�VA�VA�VA�VA�XA�S�A�S�A�S�A�S�A�S�A�S�A�VA�S�A�VA�S�A�S�A�S�A�S�A�S�A�VA�S�A�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�ZA�XA�XA�VA�XA�VA�XA�XA�XA�VA�XA�VA�XA�VA�XA�XA�XA�XA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�ZA�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�ZA�\)A�ZA�\)A�ZA�\)A�ZA�\)A�`BA�`BA�`BA�`BA�`BA�\)A�^5A�`BA�`BA�\)A�`BA�^5A�^5A�\)A�^5A�\)A�^5A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�dZA�dZA�dZA�dZA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�hsA�ffA�hsA�ffA�hsA�hsA�hsA�ffA�ffA�hsA�ffA�hsA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�jA�jA�jA�jA�jA�jA�hsA�hsA�jA�hsA�jA�jA�jA�jA�jA�hsA�ffA�bNA�dZA�dZA�dZA�bNA�dZA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�dZA�ffA�dZA�hsA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�ffA�ffA�dZA�ffA�dZA�ffA�hsA�n�A�jA�ffA�dZA�ffA�jA�n�A�hsA�hsA�ffA�ffA�jA�jA�jA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�hsA�hsA�l�A�n�A�n�A�n�A�p�A�r�A�p�A�n�A�p�A�r�A�n�A�l�A�l�A�jA�jA�l�A�l�A�l�A�l�A�l�A�jA�l�A�n�A�l�A�l�A�jA�hsA�hsA�hsA�jA�hsA�jA�hsA�l�A�jA�hsA�ffA�jA�jA�jA�l�A�hsA�jA�jA�l�A�jA�jA�jA�jA�jA�l�A�jA�jA�jA�jA�jA�jA�jA�hsA�jA�jA�jA�jA�l�A�jA�l�A�l�A�l�A�l�A�l�A�l�A�l�A�n�A�l�A�l�A�l�A�l�A�l�A�l�A�n�A�l�A�dZA�ffA�`BA�ZA�\)A�^5A�\)A�`BA�\)A�\)A�\)A�^5A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�`BA�bNA�`BA�^5A�dZA�dZA�hsA�bNA�ffA�^5A�dZA�^5A�bNA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�`BA�`BA�`BA�^5A�^5A�`BA�bNA�dZA�`BA�ZA�XA�XA�XA�XA�XA�ZA�XA�XA�XA�XA�ZA�XA�ZA�ZA�ZA�ZA�XA�XA�XA�ZA�\)A�^5A�^5A�`BA�`BA�bNA�`BA�`BA�^5A�`BA�^5A�^5A�`BA�^5A�`BA�^5A�`BA�`BA�^5A�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�dZA�dZA�dZA�^5A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�`BA�`BA�`BA�`BA�dZA�`BA�bNA�`BA�`BA�bNA�ffA�dZA�ffA�hsA�ffA�ffA�bNA�bNA�^5A�bNA�^5A�`BA�dZA�`BA�`BA�ffA�hsA�jA�jA�jA�l�A�jA�l�A�jA�l�A�jA�l�A�l�A�n�A�jA�l�A�p�A�l�A�l�A�p�A�n�A�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                  A�33A�;dA�9XA�9XA�5?A�33A�5?A�7LA�9XA�?}A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�VA�XA�ZA�ZA�ZA�VA�S�A�S�A�VA�VA�XA�XA�XA�ZA�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�`BA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�hsA�jA�jA�hsA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�hsA�p�A�l�A�l�A�jA�jA�jA�jA�jA�jA�l�A�l�A�bNA�^5A�^5A�bNA�`BA�^5A�`BA�\)A�XA�XA�\)A�`BA�^5A�`BA�bNA�bNA�dZA�bNA�^5A�bNA�dZA�bNA�dZA�jA�l�A�n�A�n�A�p�A�jA�^5A�Q�A�M�A�=qA�E�A�;dA�G�A�E�A�C�A�I�A�G�A�A�A��A��A���A���A�S�A�1'A�JA���A���A��hA�~�A�l�A�`BA�VA�33A���A��+A�Q�A�+A��A��A\)A+A~�/A~ȴA~�A~JA}�TA}��A}�wA}�-A}��A}p�A}7LA|��A|r�A|I�A|{A{��A{�PA{�A{p�A{dZA{\)A{+Az�9AzI�Az(�Ay�FAyt�AyO�Ay�Ax��AxVAw�mAw�7AwAv�uAvn�Av�Au�Aux�AuO�At��At��AtQ�At(�As�TAs\)Ar�9Aq��Aq+Ap�\ApQ�Ao��AooAnffAm�-Am33Al��Al-Ak�FAk�7AkK�Aj�Aj~�Aj-Ai�FAh�yAhjAg�TAg33Af�Ae�
Ae\)Ae33AeVAe%Ad��Ad�/AdM�Ac��Ac
=Ab��Ab~�Ab5?Aa��Aa�Aa+A`��A`ffA`^5A`E�A_G�A^��A^v�A]��A\��A\n�A\9XA\{A[�mA[�^A[�A[�AZĜAZ��AZffAZ-AY��AY\)AX��AXr�AXbNAXI�AX �AWAWXAW"�AV�yAV�AVffAVE�AU��AUx�AT��AT�jAT��AT^5ATQ�AT �ATAS�#AS��AS`BAR��AR�!ARE�AR{AQ�AQ�#AQ�wAQ�AQ|�AQ�AP�APM�AP1'AO��AO�AOhsAO&�AN��AN��AN5?AM�AM�^AMx�AMVAL�jALI�AK�
AK+AJ�AJĜAJbNAJ�AI��AI7LAH��AG�AGoAF��AF�RAF��AF�+AF9XAE�^AE7LAEoAD��AD�HAD�!AD�AD�AC�FAC��ACS�AB�`ABffAB�AA`BAA
=A@�A?�A?XA>�RA>1'A=��A=�A<VA;x�A;\)A;"�A:��A:��A:�A:(�A9A9��A9;dA8�uA8�A7�mA7��A7l�A7K�A7&�A6ȴA6JA5�A4  A3��A3
=A2��A2�A2�9A1�A1�
A1\)A0�RA0v�A0JA/A/��A/+A.��A.�HA.�A-�;A-l�A-&�A,bNA+dZA*  A)��A(�HA(�DA(=qA(1'A(bA(  A'�mA'��A'��A't�A'&�A&5?A%\)A%oA%A$�9A$E�A$-A#��A"E�A!�;A!��A!S�A!33A �/A ~�A 9XA��A+A�RA�A�FAt�A/A��A^5A5?A�AXAv�AZAA�A5?A�#At�A��A�!A~�AbNA9XA��AdZA+Ar�A�-Al�A�9A�A�A�yA�\A�wA`BA�`A��An�Al�AȴA�A�A�^A�Al�AS�A;dA�;A
��A
�A
n�A
 �A	�A	�PA��A��A�9AVA��AĜA�A��A��AdZA\)A|�AG�A�DA9XAA`BA33An�A�At�A�A ��@��@��R@���@�E�@���@��7@�j@�=q@���@�X@��@���@�ff@�=q@���@�Ĝ@�"�@�@�%@�(�@�?}@��y@�/@�u@�(�@�F@��@�+@���@�G�@�@�l�@��@◍@�-@�@��/@߾w@���@���@���@�z�@ۅ@ٺ^@�Ĝ@؋D@�bN@�1@�;d@֧�@��T@��@�r�@��@�C�@�?}@�  @�;d@�v�@���@�dZ@ʧ�@�=q@�J@�@�`B@��@���@ǝ�@ǅ@�C�@���@�?}@�  @�+@\@��^@��9@�  @�t�@���@���@�$�@�7L@���@�z�@�1@��w@�t�@�"�@��y@���@�M�@��T@�`B@�r�@��!@�@��7@���@��H@�=q@���@�`B@��@�r�@���@�-@�%@�j@�K�@�^5@���@��@�7L@���@��D@�9X@�b@���@��H@��R@���@�~�@�M�@�=q@���@�(�@��;@��@��-@�?}@��9@���@�+@���@�n�@���@��T@���@���@���@�b@�1@�  @��F@�33@���@��@���@�x�@�&�@��D@�1@�ƨ@�K�@��@�o@�
=@�@��@�ȴ@�M�@�@���@�x�@�7L@���@��D@�A�@�1@���@�S�@��R@�n�@���@�hs@�&�@���@��/@�j@��@�K�@��\@�@��h@�&�@��/@��@�9X@� �@��@�  @��@��m@��@�dZ@��H@�ff@�5?@�@��@���@�V@���@��9@�j@��@��@�;d@�o@���@�v�@�5?@��-@�p�@�O�@��@��u@�I�@���@���@�o@��+@�^5@�M�@�$�@��T@���@�/@�Ĝ@�A�@\)@~ff@}��@{t�@zn�@z-@y�@yhs@xr�@w�;@w��@v�R@u@u�h@u?}@t9X@so@r��@r~�@rn�@r=q@q7L@p�9@p��@p1'@o�@o�w@o�P@n��@n{@m��@m@m�@m�@l�/@lZ@lI�@l1@k�F@k�@k@j��@j�\@j^5@jM�@j�@i��@i��@iG�@iG�@i�@h��@hA�@h �@g�@g��@gl�@g+@f��@fV@f5?@f$�@f{@e�T@e�T@e�T@e@e��@e�h@e/@d�D@dZ@dI�@d(�@d1@c�
@c��@ct�@cC�@b�H@b-@a�@a�#@a�#@a�#@a��@a��@aG�@`��@`�9@`r�@_��@_;d@^�y@^v�@^ff@^V@]@]�@]/@\��@\��@\Z@[ƨ@["�@Z~�@Z�@Y�#@Y�^@Y%@X��@Xr�@XA�@W�@W|�@W
=@V�@Vv�@U��@U`B@UV@T�@TI�@T(�@T1@S��@S�@S33@S@R��@RM�@Q�@Q7L@PQ�@O�w@O+@N�R@Nff@N5?@N{@N@M��@M�-@M��@M�h@M�@Mp�@L��@L�j@L��@Lz�@L(�@K��@K�@KdZ@K"�@Ko@J�H@J�\@J-@I��@I�7@Ix�@Ihs@I&�@H��@H��@H��@H�@Hr�@Hr�@Hr�@HQ�@Hb@H  @G|�@F�@F�+@FV@F5?@F5?@F{@E�-@E�-@E��@Ep�@E�@E`B@E�@D��@D�@D��@D�@D�D@Dz�@DZ@D9X@D9X@C��@C�F@Ct�@C33@Co@Bn�@B�@A�@A��@A�^@A��@A�7@Ax�@AX@A%@@Ĝ@@��@@�u@@�@@r�@@bN@@Q�@@A�@@ �@@  @?�;@?�@?�P@?|�@?|�@?\)@?K�@?
=@>�y@>�@>�R@>V@>V@>{@=�T@=��@=�-@=��@=�h@=�@=�@=p�@=`B@=O�@=O�@=/@<��@<z�@<9X@<�@;�
@;�@;S�@;"�@;"�@;o@:�@:�H@:��@:�!@:��@:��@:�\@:~�@:=q@:�@:�@:�@:J@9��@9�#@9�#@9�#@9��@9�^@9��@9��@9��G�O�A�1'A�/A�33A�33A�33A�9XA�;dA�;dA�=qA�;dA�;dA�7LA�9XA�7LA�=qA�=qA�7LA�5?A�7LA�5?A�5?A�5?A�33A�33A�33A�33A�33A�1'A�5?A�33A�5?A�33A�5?A�7LA�7LA�7LA�9XA�9XA�9XA�9XA�9XA�=qA�M�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�O�A�Q�A�Q�A�Q�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�Q�A�O�A�Q�A�Q�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�S�A�Q�A�S�A�Q�A�S�A�VA�XA�VA�XA�VA�XA�XA�XA�XA�XA�XA�VA�ZA�XA�XA�ZA�XA�ZA�ZA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�ZA�ZA�XA�XA�VA�VA�VA�VA�VA�XA�S�A�S�A�S�A�S�A�S�A�S�A�VA�S�A�VA�S�A�S�A�S�A�S�A�S�A�VA�S�A�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�VA�ZA�XA�XA�VA�XA�VA�XA�XA�XA�VA�XA�VA�XA�VA�XA�XA�XA�XA�XA�XA�XA�XA�XA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�ZA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)A�ZA�\)A�\)A�\)A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�ZA�\)A�ZA�\)A�ZA�\)A�ZA�\)A�`BA�`BA�`BA�`BA�`BA�\)A�^5A�`BA�`BA�\)A�`BA�^5A�^5A�\)A�^5A�\)A�^5A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�dZA�dZA�dZA�dZA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�dZA�dZA�dZA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�hsA�ffA�hsA�ffA�hsA�hsA�hsA�ffA�ffA�hsA�ffA�hsA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�jA�jA�jA�jA�jA�jA�hsA�hsA�jA�hsA�jA�jA�jA�jA�jA�hsA�ffA�bNA�dZA�dZA�dZA�bNA�dZA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�dZA�ffA�dZA�hsA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�ffA�ffA�ffA�dZA�ffA�dZA�ffA�hsA�n�A�jA�ffA�dZA�ffA�jA�n�A�hsA�hsA�ffA�ffA�jA�jA�jA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�hsA�hsA�ffA�hsA�hsA�l�A�n�A�n�A�n�A�p�A�r�A�p�A�n�A�p�A�r�A�n�A�l�A�l�A�jA�jA�l�A�l�A�l�A�l�A�l�A�jA�l�A�n�A�l�A�l�A�jA�hsA�hsA�hsA�jA�hsA�jA�hsA�l�A�jA�hsA�ffA�jA�jA�jA�l�A�hsA�jA�jA�l�A�jA�jA�jA�jA�jA�l�A�jA�jA�jA�jA�jA�jA�jA�hsA�jA�jA�jA�jA�l�A�jA�l�A�l�A�l�A�l�A�l�A�l�A�l�A�n�A�l�A�l�A�l�A�l�A�l�A�l�A�n�A�l�A�dZA�ffA�`BA�ZA�\)A�^5A�\)A�`BA�\)A�\)A�\)A�^5A�\)A�\)A�^5A�^5A�^5A�^5A�^5A�`BA�bNA�`BA�^5A�dZA�dZA�hsA�bNA�ffA�^5A�dZA�^5A�bNA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�^5A�\)A�`BA�`BA�`BA�^5A�^5A�`BA�bNA�dZA�`BA�ZA�XA�XA�XA�XA�XA�ZA�XA�XA�XA�XA�ZA�XA�ZA�ZA�ZA�ZA�XA�XA�XA�ZA�\)A�^5A�^5A�`BA�`BA�bNA�`BA�`BA�^5A�`BA�^5A�^5A�`BA�^5A�`BA�^5A�`BA�`BA�^5A�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�bNA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�ffA�ffA�ffA�ffA�dZA�dZA�dZA�^5A�^5A�^5A�^5A�^5A�`BA�^5A�^5A�`BA�`BA�`BA�`BA�dZA�`BA�bNA�`BA�`BA�bNA�ffA�dZA�ffA�hsA�ffA�ffA�bNA�bNA�^5A�bNA�^5A�`BA�dZA�`BA�`BA�ffA�hsA�jA�jA�jA�l�A�jA�l�A�jA�l�A�jA�l�A�l�A�n�A�jA�l�A�p�A�l�A�l�A�p�A�n�A�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Be,Bd�Bd�Bd�Be,Bd�Bd�Bd�Bd�BcTBd�Bd�Bd�BdZBdZBdZBd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd&Bc�Bd�Bd&Bd�BdZBdZBd�Bd�Bd�Bd�Bd&BdZBd�Bd&Bc�Bd&Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bc�Bc�Bc�Bc�Bc�Bc�Bc�Bd&Bc�Bd&Bc�Bc�BdZBc�Bc�Bc�Bc�Bc�BcTBcTBc BcTBcTBc Bb�BbNBbNBbBbBbNBa�Ba|BaHBaHBa�Ba|Ba|Ba|BaHBa|BaHB`�B`�B`�B`�B`�Ba|Ba|BbBa�Ba�Bb�B`B_�B^�B]dB^�B]�B^�B^5B]�B^�B]�B]/BZ�BU2BR BC-B9�B7B4nB1�B-�B,�B+�B*�B)*B(�B'B$tB�BqBqB�B�B{BoB�B�B�B�BBB�B�B4B�B4B�BBBBuBBB�B�B:BhB(BJB~B
rB%B�BMBuB 4B��B�DB��B�|B�B�5B�QB��B��B� B�pB��BچB�EBӏB�jB�KB��B��B�XB�nB�B�B��B�bB�	B��B�MB��B�hB��B�~B��B��B��B�;B}�Bx�Bt�BqvBm�Bl�BlWBl"BkQBj�Bh�BbNB[WBYBW?BW�BT�BS�BR�BP�BNBL�BK�BG�B@�B?B=<B8�B5?B33B2aB1[B0!B/B,�B*0B(XB'B%�B!�B \BBeB�BYBSB�B�B
�B1B%B�BAB �B��B�	B��B�`B�TB�B�B�B��B�/B�B�mB��B�B��B� B�TB� B�NB�B�BخB�B՛B�}B�BB�<B�0B�#B�zB�3B��B��B��B��B��B��B��B��B��B�:B�VB��B��B��B��B�rB��B�AB�B�AB��B~�BzDBtTBs�Br�Bq�Bo�Bm�Bi�Be�Bc�BaHB]dBYBUgBOBBK�BH�BA�B=qB7B2�B-wB($B#B7B�B�BB:B B"B	lB�B�B  B�xB��B��B�B�GB��B��B�sB��B�?B�TB�6B��B�)BɆB��B��B�}B�*B��B�B��B��B�B��B�XB�B�bB��B��B��B��B��B|PBy	BuZBr|Bq�Bp�BpBo5Bm�Bl�BjBhsBa�BZ�BW�BV�BVmBQ�BPHBOBC-B>�B<6B9$B7�B5�B2�B.�B+6B'B$tB�BB�B_B�BhB�B"B�B�BMBB�B �B
�(B
��B
�	B
�2B
�+B
��B
�B
� B
�)B
�B
�B
��B
�dB
רB
��B
�vB
�jB
��B
ŢB
�-B
��B
�B
��B
�FB
�[B
��B
�}B
�B
�=B
�B
�*B
��B
�	B
��B
�1B
��B
�B
�MB
��B
�.B
��B
�PB
�7B
�MB
�B
�MB
~]B
{B
{JB
z�B
zxB
v+B
s�B
qB
n/B
l�B
i�B
e�B
b�B
`�B
]�B
[#B
W?B
V�B
UgB
T�B
R�B
QNB
L0B
HKB
GEB
EmB
AUB
?�B
?B
>B
<B
8RB
3hB
0UB
.}B
(�B
"4B
�B
=B
B
�B
$B
�B
B
�B
�B
PB
�B
xB

=B
	B
�B
�B
�B
  B	�VB	��B	�rB	�2B	��B	��B	�GB	�vB	�B	��B	�"B	�B	�
B	��B	��B	�HB	�dB	ںB	خB	�?B	�[B	уB	бB	ϫB	��B	��B	�B	˒B	�)B	�#B	�B	�KB	��B	ÖB	��B	�UB	��B	��B	��B	�dB	�*B	��B	��B	��B	�zB	�B	�tB	��B	�B	��B	�3B	��B	��B	�'B	��B	��B	�B	�6B	��B	�0B	��B	��B	��B	�zB	��B	��B	�nB	��B	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�!B	�!B	�!B	�B	��B	�IB	��B	��B	�CB	�kB	��B	��B	��B	�eB	��B	�kB	�kB	�kB	�eB	��B	�qB	��B	��B	��B	�=B	�qB	��B	��B	��B	�B	�xB	�xB	��B	�IB	�~B	�IB	�B	�B	��B	��B	�xB	�~B	�B	��B	�B	��B	�OB	��B	��B	��B	��B	��B	��B	�\B	�-B	�-B	�-B	��B	�-B	��B	�B	�4B	��B	��B	��B	��B	��B	��B	�B	�CB	��B	��B	��B	��B	�'B	��B	��B	��B	�B	�zB	��B	��B	�^B	��B	�*B	��B	��B	��B	�<B	�B	�wB	��B	B	�?B	ŢB	�B	�zB	ǮB	��B	��B	�zB	ɺB	�)B	�6B	��B	�<B	�jB	��B	��B	�dB	��B	��B	�6B	͟B	�B	�6B	ϫB	�}B	��B	��B	��B	уB	�[B	��B	��B	՛B	��B	ںB	��B	��B	یB	یB	�]B	یB	یB	�/B	ݘB	�dB	ݘB	�B	�B	�HB	�|B	�B	�B	�ZB	�B	��B	�B	��B	�B	�B	�B	�DB	��B	��B	�B	�B	�B	�"B	�"B	�B	�)B	��B	��B	�GB	�B	��B	��B	�+B	��B	�`B	��B	��B	��B	�rB	��B	��B	�(B	��B
 �B
B
�B
B
�B
�B
�B
fB
	7B

=B
�B
�B
bB
�B
�B
 B
 B
hB
@B
�B
�B
�B
_B
7B
qB
IB
�B
�B
 'B
 �B
!bB
"4B
"�B
$tB
&LB
(�B
+6B
,=B
-wB
-�B
0UB
1�B
2�B
3�B
4�B
6B
7B
7LB
8�B
:�B
<B
=qB
?B
@OB
@�B
@�B
A B
B�B
D3B
EB
H�B
J�B
K^B
M6B
P}B
R�B
U2B
W�B
YKB
ZB
ZQB
Z�B
[�B
[�B
\)B
\�B
\�B
]/B
^�B
_�B
`B
`�B
bB
c�B
dZB
e,B
e�B
f2B
f�B
h
B
i�B
kQB
l"B
l�B
l�B
m�B
ncB
o B
o�B
poB
p�B
poB
p�B
qAB
rB
rGB
t�B
v�B
x�B
yrB
y�B
zB
zxB
|B
|PB
|PB
}"B
}"B
}�B
~�B
cB
�B
�4B
��B
�;B
��B
�uB
��B
��B
��B
�B
�YB
�_B
��B
�rB
��B
�~B
��B
�B
��B
��B
��B
��B
� B
�B
��B
�{B
��B
�MB
��B
�SB
��B
��B
�_B
�1B
��B
�	B
�qB
��B
�xB
��B
��B
��B
�!B
��B
�4B
�hB
�@B
��B
�LB
�B
��B
��B
�XB
��B
��B
�*B
��B
��B
�0B
��B
�}B
�!B
��B
�-B
�9B
�tB
�zB
��B
��B
��B
�RB
��B
��B
�^B
��B
��B
�0B
�B
�6B
�jB
�jB
��B
�B
�qB
��B
�B
�wB
�}B
��B
� B
��B
Be,Be�Bd�Bd�BdZBa�Bd�Be�BdZBe`Bd�Bd�Bd�Bd�Bb�Be�Bd�Bd�Bd&Bf�Bd�Bd�Bd�Be�Bd�BdZBe,Be`Be`Be,Bd�Be,Bd�Bd�Bd�Bd�BdZBe,Bd�Be`Bd�Bd&B_Bd�Bd�Bd�Bd�Be`Bd�Be,Bd�Be,Bc�Be`Bd�Bd�Bd�Bd�Bd�Bd�Bd�Be�Bd&Bc�Bc�BdZBd�Bd�Bd&Bd�Bd�Bd�Bc�Bd�Bc�BdZBc�BdZBc�Bd�BdZBdZBdZBd&Bd&Bd�Bd�Bd�BdZBd�Bd�Bd&Bd�Bd�Bd�Bc�Bd�Bd&Be,BdZBd�Bd�Be`Bd�Bd�Bd�Be`Bd�Be,Bd�Bd�Bd�Be,Bd�Bd�Bd�Be`Bd�Bd�Be,Bd�Bd�Bd&Be,Be`Be�Bd�Be,Bd�Bd�Bd�Be`Bd�Bd�Be,Bd�Bd�Bd�Bd�BdZBe�Bd�Be`Bd�Bd�BdZBe�BdZBe,Bd�Be`Bd�Be,Bd�BdZBd�Be,Bd�Be,BdZBe`Bd�Bd�BdZBd�Bd�BdZBdZBd�Bd&Bd&BdZBdZBdZBd&Bc�Bd&BdZBd&Bc�Bc�BdZBc�BdZBc�BdZBd�Bd&Bd�Bd�Bd�Bc�Bd&Bd�Bc�Bc�Bc�Bd�Bd�Bd�BdZBd�Bc�Bd&BdZBd�Bd&Bd�Bc�Bd�Bc�BdZBc�Bd&BdZBd&Bd&Bd�Bd�BdZBd�BdZBd�Bd�Be,Bd�Bd�Bd�Bd�Bd�BdZBd�BdZBe,Bd�Bd�Bd�Be,Be,Bd�Bd�Bd�Bd�Bd�BdZBd�Bd�Bd�Bd�Bd�Bd�Bc�Bc�Bd&Bc�BdZBc�BdZBd�Bc�Bd�Bd�Bd�Bd�Be`Bd�BdZBd�Be`BdZBd�Bd�Bd&Bc�Bd&Bc�Bc�Bd&Bd&Bc�Bc�Bd�Bc�Bd�Bc�BdZBe,BdZBd�BdZBd�Bc�Bc�Bd�Bb�Be,Bd�Be,Bd�Be,Bd�Bd�Bd�Bd�Be,Bd�Bd�Bd�Bd�Bd�Be,Bd�Be,Bd�Bd�Bd�Be,Bd�Be,Bd�Bd�Bd�Bd�Be`BdZBe�Bd�Be,Bd�Bd�Bd�Bd�Bd�Be,Be,Bd�Bd�Bd�Be`Bd�Bd�Bd�Be,Bd�Bd�Bd�Bd�Bd�Bd�Be,Bd�Be,Bd�Be`Be,Be,Bd�Bd�Bd�Be,Bd�Bd�Bd�Bd�Bd�Be,Bd�Bd�Bd�Be`Bd�Bd�Bd�Bd�Bd�Bd�Be,Bd�Be,Be`Be�BdZBdZBc�Bd&Bb�Bd�Bc�Bc�Bc�Bc�Bc�Bc�Bd&Bd&BdZBc�Bc�Bc�BdZBd&BdZBc�BdZBd&Bc�Bd&Bc�Bc�Bc�Bc�Bc�Bd&Bc�Bc�BdZBd&Bc�Bd&Bc�Bc�Bd&Bd�Bc�Bc�Bc�Bc�Bd&Bc�Bc�Bc�Bd&Bc Bc�BcTBc�Be`Bc�Bc�Be,BbBc�Bc�Bc�BdZBb�Bd&Bc�Bd�Bd&Bd&Bc�Bc�BcTBc�Bc�Bc�Bd�Bc�Bc�Bc�Bd&Bb�BbNBdZBd�Bd&Bc�Be`Bc�BcTBcTBe�Bd&Bc�Bd&Bc Bc�BdZBc�Bd&Bc Bc�Bb�Bc�Bc�Bd�BcTBd&Bc�Bc�Bc�Bc�BcTBcTBb�BdZBb�Bd�Bb�BdZBc�Bc�Bd�Bc�Bc�BcTBdZBa�BcTBc�Bc�Bb�Bc�Bc Bc�BcTBc BcTBc Bb�BcTBc�Bc�Bc�Bb�Bc�Bc Bc�Bb�Bc�Bb�Bc�BcTBc�Bc�Bc�Bc�BcTBcTBb�Bc Bc�Ba�BdZBa�Bd�BaHB`�Be�B`vBbBbBb�Ba|Bb�Bc�BbBbNBbBb�BbBa�Bb�Bb�Bc B_pBa|Ba�Be,B`�Bb�BbNBb�Ba|BbNBa�Bb�BbBbNBa|BbNBbBbNBa|Bb�Bb�Bb�Ba�BbBbNB`�Bb�Bc B`�B`�BaHBa|Ba|BaBa|BaBa|Ba�B`�Ba|B`�BaHBa|BaBa|BaHBa|Ba|Bb�B`�Ba�BaBaHBa|Ba�BaHBa�BaHBa�Ba|Ba|Ba�Ba|Ba�BaBa�Ba�Ba�Ba|B`vBaBa|BaHBaHBa�BaHBa�BaHBa|BaHBa|Ba�BaHBaHBaHBa|B`�Ba�B`�B`�BaHBbBaHBa�BaB^�BbB`�B`�B`vB`vB`BBa|B_�BaHB`�BaHB`vBaBcTB`�BaHB`�B`�B_�BaHB`BaBa�B`BaHBaB`vB`vBa�B`�BaBa�B`BB`�B_�BbBa�BaHBa|Ba�BaHBa�BaHBa�BaHBa�Bb�Bb�B`�BbBb�B`BaBbNBbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                  B^5B^B^5B^�B^�B^5B^5B]�B]�B\)B^OB^5B^B]�B]�B]�B]�B^B^B^B^OB^5B^B^B^B^OB^5B^5B]~B]IB]�B]~B]�B]�B]�B^B]�B^B^B]~B]�B^B]~B]IB]dB^OB^OB^OB^OB^OB^OB^OB^OB^OB^jB^5B]IB]IB]IB]IB]IB]IB]IB]~B]B]~B]B]B]�B]IB]/B]B\�B\�B\�B\�B\xB\�B\�B\�B[�B[�B[�B[�B[qB[�B[#BZ�BZ�BZ�B[#BZ�BZ�BZ�BZ�BZ�BZ�BY�BZ7BZQBZ7BZBZ�BZ�B[qB[	B[=B\)BY�BY1BX_BV�BX+BV�BX+BW�BW
BXEBW?BW�BUMBQ BP�B?HB4TB1vB/B,WB'�B&�B%zB$@B"�B"�B"B �B�BB�B�B�B�B�B~B�B�B�B�B�B^BDB)B
�BJB�B�B(B�BB~B�BdB0BdB0B	�B?BEB�B B��B�B��B��B��B�ZB�B�qB�6B��B�zB��B�B�/BٴBּB��B�[B�(B�RB�MB�B�B��B��B��B��B��B��B��B��B�\B�B�0B�7B��B��B��B��B|jBy�BtBqBlqBg�Bf�Be�Be�Be,Be�Bd@B]�BVBS[BQ�BR�BO�BN<BM�BJ�BG�BF�BH�BCaB:�B;JB8�B4B/OB-B,WB+QB*KB)�B'mB$&B"�B!HB vB�B�B�BBTBNB B�B�BBuB �B��B��B��B��B�MB�vB��B��B��B��B��B�B��B��B��B߾B��B��B��B�B��B�]B�qB�1B��B��B��B�=B�BȀB�?BĜB�[B�wB��B�	B�fB�B��B��B�_B��B��B��B��B��B��B��B��B��B~(B|B{�B|B|BzBuZBnIBm]Bl�Bk�Bi�Bh�Bd�B_�B^jB\CBXyBS�BQBI�BFtBD�B=qB9	B2|B./B(�B$ZB;B,BBhBB0B�B�B{B�B B�B��B��B��B��B�=B�B��B�,B��B� B͹B�B�SBňB�SB��B��B�B��B�aB�}B�B�KB�2B�nB��B�4B�]B�9B�B��B��B{�BxBs�Bo�Bl=Bk�Bj�Bi�BiBg�Bf�Be,BeB^BU�BQ�BQhBQNBK�BK^BM�B>]B9$B6�B33B2�B0�B-)B*0B&fB"NB B�B�B2B�B\BxB
	B
XB	B
��B
�(B
��B
��B
��B
�XB
�%B
�3B
�'B
�UB
��B
��B
�yB
�*B
�B
�VB
��B
ٴB
��B
�PB
�XB
ɺB
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
��B
�B
�FB
�B
��B
�2B
��B
��B
�TB
� B
��B
��B
�=B
��B
�lB
��B
}qB
}qB
��B
yrB
u?B
t�B
u%B
vzB
p�B
n�B
k�B
h�B
h�B
d�B
aB
]�B
\]B
ZB
VSB
QB
Q B
O�B
OB
NpB
NpB
F�B
B�B
CGB
@�B
;�B
9�B
9>B
9�B
8�B
4�B
-�B
+�B
-)B
&�B
�B
_B
�B
{B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
MB
-B
  B	�]B	�B	�B	�fB	�`B	�|B	��B	��B	�B	�qB	�eB	��B	�8B	�B	�hB	�B	�4B	�/B	�yB	��B	�MB	�[B	�VB	��B	ʦB	�lB	ɺB	ɠB	�7B	ňB	�B	�3B	�{B	�9B	��B	��B	��B	��B	�JB	��B	�LB	��B	��B	�B	�B	�GB	��B	��B	��B	� B	�/B	��B	�]B	�B	�B	��B	�QB	��B	�
B	��B	��B	�FB	��B	��B	�-B	��B	��B	�'B	�OB	��B	��B	��B	�WB	�KB	�+B	��B	�KB	��B	��B	��B	��B	�eB	��B	��B	��B	�B	��B	��B	�B	�9B	�SB	�+B	��B	��B	��B	��B	�uB	��B	��B	�FB	�FB	�uB	�aB	�B	��B	�[B	�B	��B	��B	��B	�$B	��B	�SB	�?B	�$B	�
B	��B	�sB	�
B	��B	��B	��B	��B	�
B	�B	�B	��B	�EB	�B	��B	��B	��B	�B	�B	�kB	�B	�=B	��B	�WB	�#B	��B	��B	�IB	�/B	�/B	��B	��B	�\B	�B	��B	��B	��B	�B	��B	�DB	�yB	��B	�B	��B	�wB	��B	�B	�UB	��B	�GB	�nB	�nB	�nB	��B	�+B	��B	�2B	��B	�	B	��B	�"B	�OB	��B	�iB	��B	��B	�'B	��B	�uB	�MB	�B	��B	��B	�KB	ǮB	�EB	�+B	�B	��B	�_B	ǮB	�7B	ǔB	�+B	ɆB	ʦB	�xB	�B	��B	��B	��B	��B	��B	�.B	ӏB	ԯB	��B	�gB	�gB	�B	�mB	�MB	՛B	�
B	�sB	�?B	��B	�B	چB	��B	�WB	ܒB	ݘB	�jB	�;B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�@B	�B	�zB	��B	��B	�fB	�RB	�B	�B	�B	�B	��B	��B	�B	�UB	�B	�;B	��B	�vB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�}B
 �B
UB
'B
B
MB
%B
	lB

	B

=B

=B

�B

�B
^B
6B
pB
�B
.B
�B
,B
�B
�B
sB
�B
B
kB
#B
�B
�B
�B
 vB
# B
%,B
&B
'8B
'�B
*KB
+�B
,WB
-�B
.�B
0B
0�B
1AB
2�B
4�B
5�B
7fB
9	B
:B
:DB
:�B
;0B
<�B
=�B
>�B
B[B
D�B
E�B
G�B
J�B
L�B
O(B
Q�B
SB
S�B
S�B
T{B
U2B
U�B
U�B
V9B
VmB
W$B
X�B
YeB
Y�B
Z�B
\)B
]�B
^B
_B
_pB
_�B
`�B
a�B
c�B
eB
e�B
f2B
f�B
g�B
h$B
h�B
iyB
jB
j0B
i�B
jeB
kB
k�B
lWB
n�B
p�B
raB
sB
shB
s�B
tnB
u�B
u�B
vB
v�B
v�B
w�B
xlB
y	B
yrB
y�B
zxB
z�B
{B
|B
|jB
|�B
}�B
~�B
�B
� B
��B
�MB
�mB
�%B
�YB
��B
�+B
��B
��B
�fB
��B
��B
�PB
�"B
�VB
��B
��B
��B
��B
�hB
�B
��B
�FB
��B
��B
�MB
�B
�mB
��B
�_B
��B
��B
��B
�CB
�B
�VB
��B
��B
�bB
�bB
��B
�4B
�hB
��B
� B
�nB
��B
��B
�>B
��B
��B
�B
��B
�5B
�B
�UB
��B
�[B
��B
�aB
��B
��B
�nB
�nB
��B
��B
��B
��B
�B
�FB
��B
��B
�fB
��B
�B
�$B
�B
��B
��G�O�B^�B_!B^B^OB]�B[=B^B_!B]�B^�B^B^OB^B^B\B_VB^B^OB]~B`'B^OB^B^B^�B^B]�B^�B^�B^�B^�B^OB^�B^OB]�B^B^OB]�B^�B]�B^�B^B]~BX_B]�B^B^OB^B^�B^OB^�B]�B^�B]IB^�B^OB]�B^OB^OB^B^B]�B_VB]~B]IB]IB]�B^B^B]~B^OB]�B]�B]B^OB]B]�B]B]�B]B]�B]�B]�B]�B]~B]~B^B]�B^B]�B^OB^OB]~B^OB^OB^B]IB]�B]~B^�B]�B^OB^OB^�B^B^OB]�B^�B^B^�B^B]�B^B^�B^B^B^OB^�B^OB^OB^�B^OB^OB]~B^�B^�B^�B]�B^�B^B^B]�B^�B]�B^OB^�B^B^B^B^OB]�B^�B^B^�B^B^B]�B^�B]�B^�B^OB^�B]�B^�B]�B]�B]�B^�B]�B^�B]�B^�B]�B^OB]�B^OB^B]�B]�B]�B]~B]~B]�B]�B]�B]~B\�B]~B]�B]~B]B\�B]�B]B]�B]IB]�B^B]~B^OB]�B^B]IB]~B]�B]B\�B]B^B^OB^B]�B]�B]IB]~B]�B^B]~B^B]IB]�B]IB]�B]IB]~B]�B]~B]~B]�B^B]�B]�B]�B^OB^OB^�B]�B^OB]�B]�B^OB]�B^OB]�B^�B^OB^B^B^�B^�B]�B^OB^B^OB^B]�B^OB^OB^B]�B^B^B]IB]B]~B\�B]�B]IB]�B]�B\�B]�B]�B]�B^B^�B^B]�B^OB^�B]�B^OB]�B]~B]B]~B]IB]IB]~B]~B]IB\�B^B]B^B]B]�B^�B]�B^B]�B]�B]B]IB^OB\B^�B^B^�B^B^�B^B^OB^OB^OB^�B^OB^B^B^OB^B^�B^B^�B^OB^B^B^�B]�B^�B^B^OB]�B^B^�B]�B^�B^B^�B^B^B^B^OB^B^�B^�B^B^OB^OB^�B^B^OB^OB^�B]�B^B^B^OB^B^OB^�B^OB^�B^OB^�B^�B^�B^B^OB]�B^�B^B^OB]�B^B^B^�B^B^B^B^�B^OB^B^OB^B]�B]�B^�B^B^�B^�B_!B]�B]�B\�B]~B\B^B]IB]B]B]B]B]IB]~B]~B]�B]IB]IB]B]�B]~B]�B]B]�B]~B]B]~B]B]IB\�B]IB]B]~B\�B]IB]�B]~B\�B]~B]IB]B]~B]�B\�B\�B]B\�B]~B]IB]IB\�B]~B\xB\�B\�B]IB^�B]B\�B^�B[qB]IB]IB]IB]�B\CB]~B\�B^B]~B]~B\�B]IB\�B]B]IB]B^OB]IB]IB]B]~B[�B[�B]�B^OB]~B\�B^�B]IB\�B\�B^�B]~B]IB]~B\xB]IB]�B\�B]~B\xB]IB\CB\�B]IB]�B\�B]~B\�B]B]B]B\�B\�B\B]�B\B]�B[�B]�B]B\�B]�B]B\�B\�B]�B[	B\�B\�B]B[�B\�B\xB\�B\�B\xB\�B\xB\B\�B\�B\�B]B[�B]B\xB\�B\CB]B\CB\�B\�B\�B]B]B\�B\�B\�B\B\xB]B[=B]�B[	B]�BZ�BZ7B^�BY�B[qB[qB[�BZ�B[�B]B[qB[�B[qB[�B[qB[	B\CB[�B\xBX�BZ�B[	B^�BZB\CB[�B[�BZ�B[�B[=B[�B[qB[�BZ�B[�B[qB[�BZ�B[�B\B[�B[=B[qB[�BZB[�B\xBZ7BZ7BZ�BZ�BZ�BZkBZ�BZkBZ�B[=BZ7BZ�BZ7BZ�BZ�BZkBZ�BZ�BZ�BZ�B\CBZ7B[	BZkBZ�BZ�B[=BZ�B[=BZ�B[	BZ�BZ�B[	BZ�B[	BZkB[	B[=B[	BZ�BY�BZkBZ�BZ�BZ�B[	BZ�B[	BZ�BZ�BZ�BZ�B[	BZ�BZ�BZ�BZ�BZ7B[=BZ7BZBZ�B[qBZ�B[=BZkBW�B[qBZ7BZBY�BY�BY�BZ�BX�BZ�BZ7BZ�BY�BZkB\�BZ7BZ�BZ7BZ7BX�BZ�BYeBZkB[	BYeBZ�BZkBY�BY�B[=BZ7BZkB[=BY�BZ7BX�B[qB[	BZ�BZ�B[=BZ�B[	BZ�B[	BZ�B[	B[�B[�BZ7B[qB\BYeBZkB[�B[qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0064(+/-0.0026)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0064(+/-0.0026)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:3/1.5,y:1.5/0.5]; max_breaks=1;                                                                           PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:3/1.5,y:1.5/0.5]; max_breaks=1;                                                                           PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202211212141122022112121411220221121214112202211212141122022112121411220221121214112SI  SI  ARFMARFM                                                                                                                                                2019121100533320191211005333IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019122101004120191221010041QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019122101004120191221010041QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020012211305620200122113056IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2022112121411620221121214116IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022112121411620221121214116IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022112121411620221121214116IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                