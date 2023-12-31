CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-24T16:33:40Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210124163340  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               p   pAA  AOAO7314_008642_112                 7314_008642_112                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�Y'��w@�Y'��w11  @�Y'U�=@�Y'U�=@0���X%@0���X%�b����b���11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@B�\@�G�@�  @�G�@�G�A ��A��A   A+�A?\)A^�RA\)A�  A��A�Q�A�  A�  A�  A�Q�A��B  B  B�
B   B(  B0(�B8(�B@  BH(�BP  BW�
B`(�Bh  Bp  Bx(�B�(�B�  B��
B��B��B��B�  B��B�  B��B��
B��B��
B��B��B�(�B�{B��
B��
B��
B��
B��B�  B�  B��B�  B�{B�  B�  B�{B�{B�{C 
=C
=C��C  C
=C

=C
=C  C��C  C
=C
=C{C
=C  C  C 
=C"  C$
=C&{C(
=C*  C,
=C.
=C0
=C2
=C4  C6
=C8
=C:
=C<  C=��C@
=CB
=CD
=CF
=CH
=CJ  CL  CN
=CO��CR  CT
=CV  CX  CZ{C\  C^  C_��Ca��Cc��Cf  Cg��Ci��Ck��Cn  Cp
=Cr
=Ct
=Cv
=Cx
=Cz  C|  C~  C��C�C�
=C�  C���C���C���C�  C�C�
=C�  C���C���C���C���C���C���C���C�  C�  C�  C���C�C�
=C�  C���C�  C�
=C�C�  C�  C�C�C�C�  C���C�  C�  C�C�
=C�
=C�  C���C�  C�C�  C�  C�C���C���C���C�C�  C���C���C���C�  C�C�  C���C�  C�
=C�C�  C���C�  C���C���C�  C���C���C�C�C�  C�  C�  C�  C���C�C�  C���C�C�  C���C���C�  C�C�C�  C���C�  C���C���C���C���C�  C�C�
=C�C���C�  C�C�C�C�
=C�  C���C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�  C���D }qD  D��DD��D�qDz�D�qD��D  D� D  D}qD��D� D�D��D�qD	z�D	��D
}qD
�qD� D�D�D�qDz�D�qD� D�D� D  D�D�D� D  D� D�Dz�D��D� D  D}qD�qD� D�D��D�qDxRD�qD��D  D� D�qD}qD�qD}qD�qD}qD�qD}qD  D��D �D ��D!�D!}qD!��D"z�D#  D#��D$D$� D$�qD%��D&  D&z�D'  D'��D(  D(}qD)�D)��D*�D*��D+�D+� D,  D,� D,�qD-}qD-��D.� D/  D/� D0  D0� D1D1��D2  D2� D2�qD3��D4  D4}qD5�D5��D5��D6}qD7�D7�D8�D8��D9  D9� D:  D:� D;  D;}qD<  D<}qD<�qD=��D>  D>}qD?  D?��D@�D@� D@�qDA� DB�DB� DB�qDC� DD�DD� DE  DE� DE�qDF}qDG  DG� DH�DH��DIDI��DJ  DJ� DK  DK}qDL  DL��DM�DM��DN�DN� DO  DO� DP�DP��DQ  DQ}qDR  DR��DSDS� DS�qDT}qDU  DU��DV�DV��DW  DW� DX  DX� DY  DY��DZ�DZ� D[  D[� D\  D\� D]�D]��D^�D^��D_�D_��D`  D`}qDa�Da��Da�qDbz�Dc  Dc� Dd  Dd��De  De}qDf  Df��Df�qDg� Dh�Dh��Di  Di}qDj  Dj��DkDk��Dl�Dl��Dl�qDmz�Dm�qDn}qDo  Do��Dp  Dp� Dq�Dq� Dr  Dr��Ds  Ds}qDt  Dt� Du  Du}qDu�qDv}qDv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{}qD|�D|� D|��D}}qD}�qD~}qD  D� D�  D�AHD���D�� D���D�@ D�� D��HD�HD�@ D�� D���D���D�>�D�� D�� D�  D�@ D�� D�D�HD�>�D�~�D���D��qD�@ D��HD���D���D�>�D�~�D��qD���D�AHD�� D���D�HD�AHD��HD�� D�  D�=qD�}qD���D�  D�AHD�� D��qD���D�@ D��HD�� D���D�@ D�� D���D�  D�@ D�� D�D��D�@ D�� D�� D���D�AHD�� D�� D�  D�@ D�� D�� D��D�B�D��HD���D�  D�@ D�~�D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD�HD�@ D�� D�� D�HD�B�D��HD�� D�HD�B�D��HD��HD�  D�@ D�~�D�� D��D�B�D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D�� D��HD�  D�=qD�~�D�� D���D�>�D�� D�� D���D�AHD��HD�� D���D�>�D��HD�D���D�@ D�� D��qD���D�>�D�� D�D�HD�AHD�� D��HD�HD�>�D�}qD���D�  D�>�D�~�D�� D��qD�>�D�� D��qD���D�@ D�~�D���D�  D�>�D�}qD�� D�HD�>�D�}qD�� D�HD�@ D�� D�� D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�>�D�}qD��qD���D�@ D���D��HD�  D�@ D�� D��HD�  D�>�D��HD�� D�  D�@ D�~�D�� D��qD�=qD�}qD���D�  D�@ D�~�D���D��qD�>�D�� D��HD��D�@ D�}qD���D�  D�AHD���D�� D���D�@ D��HD�� D���D�@ D��HD�� D���D�@ D�� D���D�HD�>�D�� D��HD�HD�AHD�~�D���D�  D�@ D��HD�� D���D�>�D�� D�� D�  D�>�D�� D��HD�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHDHD�� D�  D�>�DÁHD�� D�  D�AHDĀ D�� D�  D�>�Dŀ D�D�HD�@ Dƀ Dƾ�D���D�AHDǂ�D�� D�  D�B�DȁHD�� D���D�>�D�~�D�� D�  D�@ Dʀ D�� D���D�>�D�~�D��HD�HD�>�D̀ D��HD�HD�@ D̀ D;�D���D�@ D�~�D�� D�  D�@ Dπ D�� D�  D�AHDЁHDо�D�HD�AHD�~�D��HD�  D�@ DҁHD�� D�  D�@ D�~�DӾ�D�  D�@ DԀ D�� D�  D�AHDՁHD�� D�  D�AHDր D�� D�HD�AHDׁHD�� D�  D�@ D؁HD�� D���D�@ Dـ Dپ�D���D�AHDځHD��HD�HD�@ Dۀ D�� D�  D�>�D܀ Dܾ�D���D�@ D�~�DݽqD���D�>�Dހ D��HD�  D�>�D߀ D߾�D��qD�>�D��HD��HD�  D�=qD�~�D��HD�HD�@ D� D�� D�  D�@ D� D㾸D�  D�AHD� D�� D�  D�@ D�HD�� D�  D�@ D�HD��HD�  D�AHD�HD�� D�HD�B�D�HD�� D���D�>�D� D�� D���D�>�D�~�D꾸D�  D�AHD� D�� D�HD�AHD� D��HD�HD�AHD�HD���D���D�AHDD��HD�HD�=qD�~�DﾸD���D�>�D�� D��HD�  D�>�D�~�D�D��qD�@ D�HD�� D�  D�@ D�~�D�D���D�>�D�}qD���D�HD�AHD��HD��HD�HD�B�D���D�� D��qD�=qD�~�D���D�  D�AHD��HD�� D���D�@ D��HD�� D��qD�AHD�p�D��\?\)?B�\?��?��
?�p�?�(�?��H@\)@�R@.{@@  @Q�@^�R@p��@�  @��@���@�@�p�@���@�{@�@��R@���@�{@�
=@�  @�@�{@�@��RA�\A
=A	��A{AG�A�AQ�A��A ��A%�A)��A.{A1�A6ffA;�A?\)AB�\AG�AK�AP��AUAY��A^�RAc�
Ai��An�RAr�\Aw�A|(�A�Q�A�=qA���A�\)A���A��
A�{A���A�33A��A�
=A���A��A�A��A���A��
A��RA���A��HA�p�A��A�=qA�z�A��RA���A��HA�p�A�  A�=qA��A�
=A��A�(�AθRA�G�A��
A�ffAأ�A��HA�p�A�  A�\A�p�A�A�\A���A�A�=qA�z�A�
=A���A�(�A�
=B ��B=qB\)B��B�B33Bz�B	�B33B��B{B\)B��B�B\)B��B{B�B��BffB  BG�B�RB   B!p�B"�RB$  B%p�B&�HB(z�B)�B+33B,��B.{B/\)B0��B2{B3�B5�B6�\B8  B9G�B:�\B;�
B=�B>ffB?�B@��BBffBC�
BE�BFffBG�BH��BJ{BK33BLz�BMBO
=BPz�BQ��BR�HBT  BUG�BV�\BW�BX��BZffB[�B]�B^=qB_�B`��Bb{Bc33BdQ�Be��Bg
=BhQ�Bi��Bj�HBl(�Bm��Bn�\Bo�
Bp��Br�\Bs�
Bu�BvffBw�Bx��Bz{B{\)B|z�B}�B\)B�Q�B�
=B��B�=qB��HB�p�B�=qB���B��B�{B��RB�\)B�  B��RB�\)B�  B���B�33B��B��\B�G�B��
B�z�B��B�B�Q�B��B�B�ffB�
=B���B�=qB��HB��B�=qB��HB���B�=qB���B�\)B�{B���B�p�B�(�B��HB�p�B�{B���B�33B��B���B�G�B��B���B�33B�B�ffB�
=B��B�ffB���B���B�{B���B�p�B�{B��RB�G�B��
B�z�B�33B��
B�Q�B���B��B�(�B���B�p�B�{B��\B��B��B�Q�B���B��B�{B��\B�33B��
B�z�B���B���B�(�B��HB��B�(�B���B�\)B�  B���B�p�B�{B���B�G�B�{B¸RB�\)B�  Bģ�B�\)B�{BƸRB�p�B�  Bȣ�B�\)B�{B���B�p�B�{B��HB͙�B�Q�B��HBϙ�B�ffB��B�B�ffB��B��
B�z�B��B�B�z�B�33B��B�z�B��B��Bڣ�B�G�B��Bܣ�B�p�B�(�B��HBߙ�B�=qB�
=B��
B�\B�33B��B�RB�B�(�B��HB�B�\B�33B��B��B�B�=qB���B��B�z�B�G�B�  B��B�B�=qB���B�B�Q�B��B��B���B�G�B�{B���B���B�Q�B�
=B��B��RB�\)B�{B��HB��C =qC ��C ��C\)C��C(�C�C�HCQ�C�RC{Cp�C�
C=qC��C��CffC��C(�C�C�HCQ�C�RC	
=C	p�C	�
C
=qC
��C
��CQ�CC�Cz�C�HCQ�C�C
=Cp�C�HCG�C��C
=Cz�C�
C33C��C
=CffC�RC(�C�\C�CG�C�RC�Cp�C�
CG�C��C��CffC�
C(�C�C��C\)C�RC�C�C��CQ�C�C�C�C�CG�CC�C�C�CQ�C�RC �C �\C �C!G�C!�RC"�C"�C"�C#Q�C#�C$
=C$p�C$��C%�C%ffC%C&
=C&G�C&��C&�
C'
=C'G�C'�\C'C'�C(�C(\)C(�\C(�RC(�C)(�C)Q�C)�\C)C)��C*�C*ffC*�\C*�RC*��C+33C+\)C+�C+��C,  C,(�C,ffC,��C,�
C-  C-=qC-z�C-��C-�
C.{C.G�C.p�C.�RC.�HC/
=C/Q�C/z�C/�C/�
C0�C0Q�C0z�C0�RC0�C1{C1Q�C1�C1�C1�C2(�C2Q�C2�\C2��C2�C3�C3\)C3�\C3�RC3��C433C4\)C4�\C4��C4��C5(�C5ffC5��C5�RC6  C633C6ffC6�\C6�
C7  C733C7p�C7��C7��C8{C8Q�C8z�C8�C8��C9(�C9\)C9��C9�
C:  C:33C:z�C:�RC:�HC;{C;\)C;�C;C<  C<(�C<\)C<��C<C=  C=33C=\)C=��C=�
C>  C>=qC>p�C>��C>�
C?
=C?=qC?z�C?��C?�HC@{C@G�C@z�C@�RC@�HCA{CA\)CA�CA�CA��CB(�CBQ�CB��CB�
CC  CC=qCCz�CC��CC�
CD�CDG�CDz�CDCD�CE�CE\)CE�\CE�RCF  CF33CF\)CF��CF�
CF��CG=qCGp�CG��CG�HCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                           ?��@�\@B�\@�G�@�  @�G�@�G�A ��A��A   A+�A?\)A^�RA\)A�  A��A�Q�A�  A�  A�  A�Q�A��B  B  B�
B   B(  B0(�B8(�B@  BH(�BP  BW�
B`(�Bh  Bp  Bx(�B�(�B�  B��
B��B��B��B�  B��B�  B��B��
B��B��
B��B��B�(�B�{B��
B��
B��
B��
B��B�  B�  B��B�  B�{B�  B�  B�{B�{B�{C 
=C
=C��C  C
=C

=C
=C  C��C  C
=C
=C{C
=C  C  C 
=C"  C$
=C&{C(
=C*  C,
=C.
=C0
=C2
=C4  C6
=C8
=C:
=C<  C=��C@
=CB
=CD
=CF
=CH
=CJ  CL  CN
=CO��CR  CT
=CV  CX  CZ{C\  C^  C_��Ca��Cc��Cf  Cg��Ci��Ck��Cn  Cp
=Cr
=Ct
=Cv
=Cx
=Cz  C|  C~  C��C�C�
=C�  C���C���C���C�  C�C�
=C�  C���C���C���C���C���C���C���C�  C�  C�  C���C�C�
=C�  C���C�  C�
=C�C�  C�  C�C�C�C�  C���C�  C�  C�C�
=C�
=C�  C���C�  C�C�  C�  C�C���C���C���C�C�  C���C���C���C�  C�C�  C���C�  C�
=C�C�  C���C�  C���C���C�  C���C���C�C�C�  C�  C�  C�  C���C�C�  C���C�C�  C���C���C�  C�C�C�  C���C�  C���C���C���C���C�  C�C�
=C�C���C�  C�C�C�C�
=C�  C���C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�  C���D }qD  D��DD��D�qDz�D�qD��D  D� D  D}qD��D� D�D��D�qD	z�D	��D
}qD
�qD� D�D�D�qDz�D�qD� D�D� D  D�D�D� D  D� D�Dz�D��D� D  D}qD�qD� D�D��D�qDxRD�qD��D  D� D�qD}qD�qD}qD�qD}qD�qD}qD  D��D �D ��D!�D!}qD!��D"z�D#  D#��D$D$� D$�qD%��D&  D&z�D'  D'��D(  D(}qD)�D)��D*�D*��D+�D+� D,  D,� D,�qD-}qD-��D.� D/  D/� D0  D0� D1D1��D2  D2� D2�qD3��D4  D4}qD5�D5��D5��D6}qD7�D7�D8�D8��D9  D9� D:  D:� D;  D;}qD<  D<}qD<�qD=��D>  D>}qD?  D?��D@�D@� D@�qDA� DB�DB� DB�qDC� DD�DD� DE  DE� DE�qDF}qDG  DG� DH�DH��DIDI��DJ  DJ� DK  DK}qDL  DL��DM�DM��DN�DN� DO  DO� DP�DP��DQ  DQ}qDR  DR��DSDS� DS�qDT}qDU  DU��DV�DV��DW  DW� DX  DX� DY  DY��DZ�DZ� D[  D[� D\  D\� D]�D]��D^�D^��D_�D_��D`  D`}qDa�Da��Da�qDbz�Dc  Dc� Dd  Dd��De  De}qDf  Df��Df�qDg� Dh�Dh��Di  Di}qDj  Dj��DkDk��Dl�Dl��Dl�qDmz�Dm�qDn}qDo  Do��Dp  Dp� Dq�Dq� Dr  Dr��Ds  Ds}qDt  Dt� Du  Du}qDu�qDv}qDv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{}qD|�D|� D|��D}}qD}�qD~}qD  D� D�  D�AHD���D�� D���D�@ D�� D��HD�HD�@ D�� D���D���D�>�D�� D�� D�  D�@ D�� D�D�HD�>�D�~�D���D��qD�@ D��HD���D���D�>�D�~�D��qD���D�AHD�� D���D�HD�AHD��HD�� D�  D�=qD�}qD���D�  D�AHD�� D��qD���D�@ D��HD�� D���D�@ D�� D���D�  D�@ D�� D�D��D�@ D�� D�� D���D�AHD�� D�� D�  D�@ D�� D�� D��D�B�D��HD���D�  D�@ D�~�D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD�HD�@ D�� D�� D�HD�B�D��HD�� D�HD�B�D��HD��HD�  D�@ D�~�D�� D��D�B�D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D�� D��HD�  D�=qD�~�D�� D���D�>�D�� D�� D���D�AHD��HD�� D���D�>�D��HD�D���D�@ D�� D��qD���D�>�D�� D�D�HD�AHD�� D��HD�HD�>�D�}qD���D�  D�>�D�~�D�� D��qD�>�D�� D��qD���D�@ D�~�D���D�  D�>�D�}qD�� D�HD�>�D�}qD�� D�HD�@ D�� D�� D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�>�D�}qD��qD���D�@ D���D��HD�  D�@ D�� D��HD�  D�>�D��HD�� D�  D�@ D�~�D�� D��qD�=qD�}qD���D�  D�@ D�~�D���D��qD�>�D�� D��HD��D�@ D�}qD���D�  D�AHD���D�� D���D�@ D��HD�� D���D�@ D��HD�� D���D�@ D�� D���D�HD�>�D�� D��HD�HD�AHD�~�D���D�  D�@ D��HD�� D���D�>�D�� D�� D�  D�>�D�� D��HD�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHDHD�� D�  D�>�DÁHD�� D�  D�AHDĀ D�� D�  D�>�Dŀ D�D�HD�@ Dƀ Dƾ�D���D�AHDǂ�D�� D�  D�B�DȁHD�� D���D�>�D�~�D�� D�  D�@ Dʀ D�� D���D�>�D�~�D��HD�HD�>�D̀ D��HD�HD�@ D̀ D;�D���D�@ D�~�D�� D�  D�@ Dπ D�� D�  D�AHDЁHDо�D�HD�AHD�~�D��HD�  D�@ DҁHD�� D�  D�@ D�~�DӾ�D�  D�@ DԀ D�� D�  D�AHDՁHD�� D�  D�AHDր D�� D�HD�AHDׁHD�� D�  D�@ D؁HD�� D���D�@ Dـ Dپ�D���D�AHDځHD��HD�HD�@ Dۀ D�� D�  D�>�D܀ Dܾ�D���D�@ D�~�DݽqD���D�>�Dހ D��HD�  D�>�D߀ D߾�D��qD�>�D��HD��HD�  D�=qD�~�D��HD�HD�@ D� D�� D�  D�@ D� D㾸D�  D�AHD� D�� D�  D�@ D�HD�� D�  D�@ D�HD��HD�  D�AHD�HD�� D�HD�B�D�HD�� D���D�>�D� D�� D���D�>�D�~�D꾸D�  D�AHD� D�� D�HD�AHD� D��HD�HD�AHD�HD���D���D�AHDD��HD�HD�=qD�~�DﾸD���D�>�D�� D��HD�  D�>�D�~�D�D��qD�@ D�HD�� D�  D�@ D�~�D�D���D�>�D�}qD���D�HD�AHD��HD��HD�HD�B�D���D�� D��qD�=qD�~�D���D�  D�AHD��HD�� D���D�@ D��HD�� D��qD�AHD�p�G�O�?\)?B�\?��?��
?�p�?�(�?��H@\)@�R@.{@@  @Q�@^�R@p��@�  @��@���@�@�p�@���@�{@�@��R@���@�{@�
=@�  @�@�{@�@��RA�\A
=A	��A{AG�A�AQ�A��A ��A%�A)��A.{A1�A6ffA;�A?\)AB�\AG�AK�AP��AUAY��A^�RAc�
Ai��An�RAr�\Aw�A|(�A�Q�A�=qA���A�\)A���A��
A�{A���A�33A��A�
=A���A��A�A��A���A��
A��RA���A��HA�p�A��A�=qA�z�A��RA���A��HA�p�A�  A�=qA��A�
=A��A�(�AθRA�G�A��
A�ffAأ�A��HA�p�A�  A�\A�p�A�A�\A���A�A�=qA�z�A�
=A���A�(�A�
=B ��B=qB\)B��B�B33Bz�B	�B33B��B{B\)B��B�B\)B��B{B�B��BffB  BG�B�RB   B!p�B"�RB$  B%p�B&�HB(z�B)�B+33B,��B.{B/\)B0��B2{B3�B5�B6�\B8  B9G�B:�\B;�
B=�B>ffB?�B@��BBffBC�
BE�BFffBG�BH��BJ{BK33BLz�BMBO
=BPz�BQ��BR�HBT  BUG�BV�\BW�BX��BZffB[�B]�B^=qB_�B`��Bb{Bc33BdQ�Be��Bg
=BhQ�Bi��Bj�HBl(�Bm��Bn�\Bo�
Bp��Br�\Bs�
Bu�BvffBw�Bx��Bz{B{\)B|z�B}�B\)B�Q�B�
=B��B�=qB��HB�p�B�=qB���B��B�{B��RB�\)B�  B��RB�\)B�  B���B�33B��B��\B�G�B��
B�z�B��B�B�Q�B��B�B�ffB�
=B���B�=qB��HB��B�=qB��HB���B�=qB���B�\)B�{B���B�p�B�(�B��HB�p�B�{B���B�33B��B���B�G�B��B���B�33B�B�ffB�
=B��B�ffB���B���B�{B���B�p�B�{B��RB�G�B��
B�z�B�33B��
B�Q�B���B��B�(�B���B�p�B�{B��\B��B��B�Q�B���B��B�{B��\B�33B��
B�z�B���B���B�(�B��HB��B�(�B���B�\)B�  B���B�p�B�{B���B�G�B�{B¸RB�\)B�  Bģ�B�\)B�{BƸRB�p�B�  Bȣ�B�\)B�{B���B�p�B�{B��HB͙�B�Q�B��HBϙ�B�ffB��B�B�ffB��B��
B�z�B��B�B�z�B�33B��B�z�B��B��Bڣ�B�G�B��Bܣ�B�p�B�(�B��HBߙ�B�=qB�
=B��
B�\B�33B��B�RB�B�(�B��HB�B�\B�33B��B��B�B�=qB���B��B�z�B�G�B�  B��B�B�=qB���B�B�Q�B��B��B���B�G�B�{B���B���B�Q�B�
=B��B��RB�\)B�{B��HB��C =qC ��C ��C\)C��C(�C�C�HCQ�C�RC{Cp�C�
C=qC��C��CffC��C(�C�C�HCQ�C�RC	
=C	p�C	�
C
=qC
��C
��CQ�CC�Cz�C�HCQ�C�C
=Cp�C�HCG�C��C
=Cz�C�
C33C��C
=CffC�RC(�C�\C�CG�C�RC�Cp�C�
CG�C��C��CffC�
C(�C�C��C\)C�RC�C�C��CQ�C�C�C�C�CG�CC�C�C�CQ�C�RC �C �\C �C!G�C!�RC"�C"�C"�C#Q�C#�C$
=C$p�C$��C%�C%ffC%C&
=C&G�C&��C&�
C'
=C'G�C'�\C'C'�C(�C(\)C(�\C(�RC(�C)(�C)Q�C)�\C)C)��C*�C*ffC*�\C*�RC*��C+33C+\)C+�C+��C,  C,(�C,ffC,��C,�
C-  C-=qC-z�C-��C-�
C.{C.G�C.p�C.�RC.�HC/
=C/Q�C/z�C/�C/�
C0�C0Q�C0z�C0�RC0�C1{C1Q�C1�C1�C1�C2(�C2Q�C2�\C2��C2�C3�C3\)C3�\C3�RC3��C433C4\)C4�\C4��C4��C5(�C5ffC5��C5�RC6  C633C6ffC6�\C6�
C7  C733C7p�C7��C7��C8{C8Q�C8z�C8�C8��C9(�C9\)C9��C9�
C:  C:33C:z�C:�RC:�HC;{C;\)C;�C;C<  C<(�C<\)C<��C<C=  C=33C=\)C=��C=�
C>  C>=qC>p�C>��C>�
C?
=C?=qC?z�C?��C?�HC@{C@G�C@z�C@�RC@�HCA{CA\)CA�CA�CA��CB(�CBQ�CB��CB�
CC  CC=qCCz�CC��CC�
CD�CDG�CDz�CDCD�CE�CE\)CE�\CE�RCF  CF33CF\)CF��CF�
CF��CG=qCGp�CG��CG�HCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                           @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�&G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A��
A���A���A���A���A���A���A��/A��A��/A��/A��/A��TA��HA��`A��TA��mA��mA��mA��yA��yA��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��TA���A�ƨAĴ9A�|�A�/A�(�A��TA¬A�ƨA��A�S�A��uA��PA�ȴA�1'A�bNA�A�\)A���A�  A��!A���A�%A� �A���A��A�I�A�r�A�1A�E�A��A�JA��!A��+A���A��RA��FA�A�A�I�A�hsA��A��A��mA���A�33A���A��A�33A��!A�9XA�K�A�p�A�n�A�G�A��PA��A��FA~�`AzQ�Ax5?Aw?}As�Am�wAil�Ad�!AahsA]�AY��AV��AR��ANZAKG�AI`BAG%AEƨAC
=A>A�A=+A<M�A9��A7ƨA6�RA6JA5
=A3G�A1�-A1+A1
=A0ĜA0�A.r�A.JA-�-A,n�A+
=A)oA'hsA%�A#��A#"�A"Q�A!��A!%A"��A#�A#;dA"�DA"n�A"ffA ~�AȴA~�AC�A ĜA%A�AZAp�A"�A�AI�A�A��A��A�DA�A�A|�AƨA  AI�AA�A  AJA�AS�Az�A�uA��AQ�AA
ffAI�A�A��A��A��A �A��A\)AK�A�A�uA�A�A1'A��AK�AXA��A�A ��A (�@���@��@��+@�@��w@�r�@���@�ff@��@�5?@���@���@��@���@�S�@�hs@�"�@�"�@�=q@�J@�n�@�+@�o@���@��+@�@�p�@��F@���@���@�ȴ@��@�@�$�@���@��
@�S�@��y@���@�Ĝ@�  @�S�@��@�
=@��@�!@�+@�E�@�@��/@�9@�w@�M�@�@��@��m@�-@�hs@��`@��@���@�\)@��H@�~�@�J@�`B@��@��`@ܓu@�ƨ@�V@�7L@�z�@ׅ@և+@�-@��`@�(�@��m@��@��m@�\)@���@�$�@�/@���@��/@�?}@��@���@�z�@�z�@���@��@�33@��y@���@�M�@�`B@�V@̣�@���@�\)@�V@ɡ�@�G�@�I�@���@��;@ǅ@�33@�K�@�33@�
=@�
=@Ɵ�@�@�x�@�G�@��@Ĭ@�bN@��@å�@�t�@�+@��y@¸R@�5?@���@�Z@� �@�dZ@�M�@��-@�V@�Ĝ@�b@��w@���@�C�@���@��@�n�@�E�@�{@���@�%@�Q�@�Q�@��
@��@���@�~�@��@��@��h@�&�@��@�bN@�1'@���@�;d@��H@��!@�=q@���@�hs@��@��@��@��@�bN@���@�\)@��@���@�M�@�@���@��@�%@�z�@��
@��@���@��@�+@���@�^5@�E�@�{@���@���@��h@�G�@��@��u@�1'@�ȴ@�n�@�ff@�M�@���@���@�x�@��/@�b@��@���@��@�v�@�=q@�@���@��-@�O�@�Ĝ@��@��@��w@�|�@�l�@�33@���@�~�@�V@���@��-@�x�@�`B@��@���@�bN@���@�t�@�C�@�o@���@�V@�M�@�=q@���@��7@�&�@��@�A�@�1@��
@��F@���@��P@�S�@���@�^5@�M�@�E�@�@���@�V@��@��9@�r�@�bN@�A�@�9X@��@���@���@���@�C�@�
=@��y@���@�n�@�M�@�-@��@�{@�{@��@��7@�p�@�G�@��@��j@�j@�b@��m@���@�l�@�"�@��y@�ȴ@���@�~�@�-@��^@��h@��@�`B@�Ĝ@�r�@�1@��F@���@�dZ@�@�ff@���@���@���@���@��7@�x�@�X@���@�r�@�b@�l�@�C�@�+@���@�v�@�^5@�=q@���@���@��^@���@��h@�p�@�X@��@�%@���@�r�@�r�@�r�@�j@�I�@�(�@��@���@��
@��w@��F@�|�@�\)@��@��!@�v�@��@�@�x�@��@��/@��@��D@�j@�@|�@
=@~�y@~�R@~V@}�-@}�@|j@{�m@{��@{@z=q@y�^@yhs@yG�@y�@x��@x�u@xA�@w�@w�@v��@vE�@v{@v@u?}@t(�@s�F@s"�@r^5@q��@q��@q�7@qG�@p��@p�@pr�@p  @o��@o;d@n5?@m�T@mO�@l��@l�j@l�@k�
@k��@k��@k�@k��@k�@j�@j-@i�^@iX@i%@h��@h�@h �@gl�@f�R@fE�@f$�@e��@dZ@c��@cdZ@cC�@c33@co@b�@b��@b��@bM�@bJ@`�`@`�@`  @_�w@_�P@_�@^�R@^��@^ff@^5?@]O�@]O�@]�@\j@[��@[33@["�@Z��@Z�@Y�^@Y�^@YX@Y7L@Y�@XĜ@XbN@W�w@W;d@W+@W
=@V��@V�y@V�+@U�T@U�-@U?}@T��@TI�@T�@S�m@SS�@R�\@R~�@RM�@Q��@Q��@Qhs@P��@Pb@O��@O��@O�;@O�w@O�P@N�y@N�+@M�-@M`B@MV@L�j@L�@L�@Lz�@Lz�@LZ@L1@K��@KdZ@K"�@Jn�@I�@IX@I7L@I7L@Hr�@G��@G+@F��@F�+@Fv�@FE�@E�@E`B@E?}@E/@D�@D�@D�D@C��@Cƨ@C�F@C�F@Ct�@B�@B~�@B-@A�@A�7@A7L@@��@@Ĝ@@�u@@�@@ �@?��@?;d@>��@>��@>5?@>@=p�@<��@<�D@<�@;�
@;ƨ@;�@;S�@:��@:~�@:M�@9��@9G�@9&�@8��@8��@8r�@8Q�@81'@8 �@7�@7��@7;d@6��@6��@6v�@6ff@65?@6$�@5�@5�-@5�@4��@4z�@4Z@4I�@3��@3S�@3"�@2��@2��@2��@2M�@2�@1��@1��@1G�@0�`@0�@0b@/�@/K�@.��@.v�@.V@-@-/@,��@,��@,�D@,(�@+ƨ@+t�@+dZ@+S�@+S�@+C�@+33@+o@*��@*��@*�!@*�!@*^5@*=q@*-@*J@)x�@)%@(�`@(�9@(�@(A�@( �@(b@'�w@'��@'K�@'+@'�@'
=@&��@&��@&{@%�-@%O�@%V@$�j@$Z@$�@#�
@#��@#dZ@#"�@"�H@!��@!�#@!��@!G�@ �`@ �9@ �u@ �u@ �u@ �u@ b@�;@|�@l�@+@��@�@�@�R@��@�+@v�@@@��@/@��@�/@�j@j@9X@�@�m@��@C�@o@��@~�@-@�#@�7@G�@Ĝ@��@�u@bN@A�@  @�;@��@�@��@\)@K�@�@��@��@v�@E�@@@�@?}@��@��@��@�D@9X@��@ƨ@��@�@C�@@�H@�!@~�@n�@n�@M�@-@��@�7@G�@&�@�@%@��@��@��@r�@ �@��@�@�P@l�@K�@;d@;d@�@ȴ@v�@E�@5?@{@@��@@��@p�@`B@?}@�@V@�/@�j@�j@z�@�@�m@�
@�F@�@t�@C�@o@@
�H@
�@
��@
^5@
M�@
J@	�^@	�^@	��@	hs@	&�@��@��@�9@�u@r�@bN@bN@bN@Q�@1'@b@  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A��#A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��;A��HA��;A��/A��#A��#A��A��#A��A��
A��
A��#A��/A��;A��;A��/A��/A��/A��#A��A��A��#A��/A��;A��;A��#A��;A��/A��/A��#A��/A��/A��;A��TA��TA��TA��TA��TA��TA��TA��TA��TA��HA��HA��HA��HA��TA��`A��`A��mA��mA��`A��`A��TA��`A��TA��TA��TA��`A��mA��yA��mA��`A��`A��`A��`A��`A��mA��yA��yA��yA��mA��`A��`A��mA��mA��mA��yA��yA��A��yA��mA��mA��mA��mA��yA��A��A��A��A��yA��mA��mA��mA��yA��yA��A��A��A��A��A��yA��yA��yA��yA��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��TA��HA��/A���A���A���A���A���A���A���A���A���A�ĜA�A�A�ĜAĴ9AĲ-AĲ-Aİ!Aİ!Aĩ�Aĝ�AċDAąA�v�A�`BA�M�A�1'A�-A�-A�/A�1'A�1'A�/A�/A�-A�/A�+A��A�oA�A��A��A��`A��;Aú^Aå�AÃA�=qA¥�A�1'A�%A��TA���A��7A��#A�~�A�A�A���A��!A���A�ĜA�p�A��A�l�A��A��TA��-A��DA�ZA��A��A���A��-A���A��hA��A�l�A�9XA��#A��wA���A�r�A��A�~�A�K�A��`A���A�S�A�"�A��A��A�(�A���A���A��+A�|�A�t�A�ffA�XA�O�A�;dA��A�
=A�oA���A��+A�I�A���A���A�n�A�-A���A��A�`BA�33A��A�p�A�\)A�I�A�9XA�(�A��A�~�A�I�A��mA���A��7A�v�A�^5A�=qA���A�ffA�G�A�/A�bA���A�  A�A�  A�%A�{A��A�"�A�1'A�33A�A�A�7LA���A��jA�7LA�`BA���A��#A��!A���A�r�A�ZA�=qA�5?A�33A�XA�t�A��7A�z�A�r�A�E�A�{A�
=A��A�A���A��A��/A��-A�1'A��A��A���A��A�x�A�`BA�C�A�9XA�"�A�A��HA�ƨA�+A�ĜA��A�~�A���A��A�x�A�XA�9XA��A��A���A�ƨA���A�A���A���A�A���A���A�7LA��TA���A��DA��A��A�v�A�5?A���A��DA��FA�ĜA���A���A�ĜA��uA���A�jA�G�A�JA��
A�n�A��-A�1A�ƨA��A�(�A���A��DA�?}A�?}A���A��TA�ȴA�ƨA��!A�\)A�(�A���A��+A�Q�A��A�7LA���A�7LA�A��A��;A�ĜA��A��A�^5A���A���A���A�v�A�ffA���A�;dA�A���A���A�G�A�/A��A�VA���A�ĜA���A�n�A�M�A�bA��#A��A��PA��PA�r�A�/A�oA��A��TA�ȴA���A�r�A�O�A�7LA�bA�A�|�A�O�A��A��-A���A���A��PA�r�A�I�A�+A��A���A��HA���A�ĜA��!A���A��DA�VA�"�A��A���A��A��TA���A�ȴA��^A��9A���A�z�A�Q�A�$�A��`A��PA�dZA���A�ƨA��\A�G�A�oA�
=A���A��A�ĜA��PA�?}A�ȴA�`BA��A���A���A�G�A��A��A���A��9A��\A�|�A�jA�^5A�O�A�K�A�A�A�5?A�33A�&�A��A�oA�JA�  A���A��mA��;A��;A���A�A��jA��-A���A��A�x�A�dZA��/A���A��A~�yA}��A};dA|M�A{�
A{x�A{hsA{+Az�HAz�9Azn�Az-Az  Ay�Ay�Ay33Ax�Ax�+Ax�AxZAx �Aw�Aw�wAw��Aw��Aw�AwhsAwhsAwXAw?}Aw;dAw"�Aw�Av��Av�`Av�AuAt-Ar��ArffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                           A���A���A���A���A���A��
A���A���A���A���A���A���A��/A��A��/A��/A��/A��TA��HA��`A��TA��mA��mA��mA��yA��yA��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��TA���A�ƨAĴ9A�|�A�/A�(�A��TA¬A�ƨA��A�S�A��uA��PA�ȴA�1'A�bNA�A�\)A���A�  A��!A���A�%A� �A���A��A�I�A�r�A�1A�E�A��A�JA��!A��+A���A��RA��FA�A�A�I�A�hsA��A��A��mA���A�33A���A��A�33A��!A�9XA�K�A�p�A�n�A�G�A��PA��A��FA~�`AzQ�Ax5?Aw?}As�Am�wAil�Ad�!AahsA]�AY��AV��AR��ANZAKG�AI`BAG%AEƨAC
=A>A�A=+A<M�A9��A7ƨA6�RA6JA5
=A3G�A1�-A1+A1
=A0ĜA0�A.r�A.JA-�-A,n�A+
=A)oA'hsA%�A#��A#"�A"Q�A!��A!%A"��A#�A#;dA"�DA"n�A"ffA ~�AȴA~�AC�A ĜA%A�AZAp�A"�A�AI�A�A��A��A�DA�A�A|�AƨA  AI�AA�A  AJA�AS�Az�A�uA��AQ�AA
ffAI�A�A��A��A��A �A��A\)AK�A�A�uA�A�A1'A��AK�AXA��A�A ��A (�@���@��@��+@�@��w@�r�@���@�ff@��@�5?@���@���@��@���@�S�@�hs@�"�@�"�@�=q@�J@�n�@�+@�o@���@��+@�@�p�@��F@���@���@�ȴ@��@�@�$�@���@��
@�S�@��y@���@�Ĝ@�  @�S�@��@�
=@��@�!@�+@�E�@�@��/@�9@�w@�M�@�@��@��m@�-@�hs@��`@��@���@�\)@��H@�~�@�J@�`B@��@��`@ܓu@�ƨ@�V@�7L@�z�@ׅ@և+@�-@��`@�(�@��m@��@��m@�\)@���@�$�@�/@���@��/@�?}@��@���@�z�@�z�@���@��@�33@��y@���@�M�@�`B@�V@̣�@���@�\)@�V@ɡ�@�G�@�I�@���@��;@ǅ@�33@�K�@�33@�
=@�
=@Ɵ�@�@�x�@�G�@��@Ĭ@�bN@��@å�@�t�@�+@��y@¸R@�5?@���@�Z@� �@�dZ@�M�@��-@�V@�Ĝ@�b@��w@���@�C�@���@��@�n�@�E�@�{@���@�%@�Q�@�Q�@��
@��@���@�~�@��@��@��h@�&�@��@�bN@�1'@���@�;d@��H@��!@�=q@���@�hs@��@��@��@��@�bN@���@�\)@��@���@�M�@�@���@��@�%@�z�@��
@��@���@��@�+@���@�^5@�E�@�{@���@���@��h@�G�@��@��u@�1'@�ȴ@�n�@�ff@�M�@���@���@�x�@��/@�b@��@���@��@�v�@�=q@�@���@��-@�O�@�Ĝ@��@��@��w@�|�@�l�@�33@���@�~�@�V@���@��-@�x�@�`B@��@���@�bN@���@�t�@�C�@�o@���@�V@�M�@�=q@���@��7@�&�@��@�A�@�1@��
@��F@���@��P@�S�@���@�^5@�M�@�E�@�@���@�V@��@��9@�r�@�bN@�A�@�9X@��@���@���@���@�C�@�
=@��y@���@�n�@�M�@�-@��@�{@�{@��@��7@�p�@�G�@��@��j@�j@�b@��m@���@�l�@�"�@��y@�ȴ@���@�~�@�-@��^@��h@��@�`B@�Ĝ@�r�@�1@��F@���@�dZ@�@�ff@���@���@���@���@��7@�x�@�X@���@�r�@�b@�l�@�C�@�+@���@�v�@�^5@�=q@���@���@��^@���@��h@�p�@�X@��@�%@���@�r�@�r�@�r�@�j@�I�@�(�@��@���@��
@��w@��F@�|�@�\)@��@��!@�v�@��@�@�x�@��@��/@��@��D@�j@�@|�@
=@~�y@~�R@~V@}�-@}�@|j@{�m@{��@{@z=q@y�^@yhs@yG�@y�@x��@x�u@xA�@w�@w�@v��@vE�@v{@v@u?}@t(�@s�F@s"�@r^5@q��@q��@q�7@qG�@p��@p�@pr�@p  @o��@o;d@n5?@m�T@mO�@l��@l�j@l�@k�
@k��@k��@k�@k��@k�@j�@j-@i�^@iX@i%@h��@h�@h �@gl�@f�R@fE�@f$�@e��@dZ@c��@cdZ@cC�@c33@co@b�@b��@b��@bM�@bJ@`�`@`�@`  @_�w@_�P@_�@^�R@^��@^ff@^5?@]O�@]O�@]�@\j@[��@[33@["�@Z��@Z�@Y�^@Y�^@YX@Y7L@Y�@XĜ@XbN@W�w@W;d@W+@W
=@V��@V�y@V�+@U�T@U�-@U?}@T��@TI�@T�@S�m@SS�@R�\@R~�@RM�@Q��@Q��@Qhs@P��@Pb@O��@O��@O�;@O�w@O�P@N�y@N�+@M�-@M`B@MV@L�j@L�@L�@Lz�@Lz�@LZ@L1@K��@KdZ@K"�@Jn�@I�@IX@I7L@I7L@Hr�@G��@G+@F��@F�+@Fv�@FE�@E�@E`B@E?}@E/@D�@D�@D�D@C��@Cƨ@C�F@C�F@Ct�@B�@B~�@B-@A�@A�7@A7L@@��@@Ĝ@@�u@@�@@ �@?��@?;d@>��@>��@>5?@>@=p�@<��@<�D@<�@;�
@;ƨ@;�@;S�@:��@:~�@:M�@9��@9G�@9&�@8��@8��@8r�@8Q�@81'@8 �@7�@7��@7;d@6��@6��@6v�@6ff@65?@6$�@5�@5�-@5�@4��@4z�@4Z@4I�@3��@3S�@3"�@2��@2��@2��@2M�@2�@1��@1��@1G�@0�`@0�@0b@/�@/K�@.��@.v�@.V@-@-/@,��@,��@,�D@,(�@+ƨ@+t�@+dZ@+S�@+S�@+C�@+33@+o@*��@*��@*�!@*�!@*^5@*=q@*-@*J@)x�@)%@(�`@(�9@(�@(A�@( �@(b@'�w@'��@'K�@'+@'�@'
=@&��@&��@&{@%�-@%O�@%V@$�j@$Z@$�@#�
@#��@#dZ@#"�@"�H@!��@!�#@!��@!G�@ �`@ �9@ �u@ �u@ �u@ �u@ b@�;@|�@l�@+@��@�@�@�R@��@�+@v�@@@��@/@��@�/@�j@j@9X@�@�m@��@C�@o@��@~�@-@�#@�7@G�@Ĝ@��@�u@bN@A�@  @�;@��@�@��@\)@K�@�@��@��@v�@E�@@@�@?}@��@��@��@�D@9X@��@ƨ@��@�@C�@@�H@�!@~�@n�@n�@M�@-@��@�7@G�@&�@�@%@��@��@��@r�@ �@��@�@�P@l�@K�@;d@;d@�@ȴ@v�@E�@5?@{@@��@@��@p�@`B@?}@�@V@�/@�j@�j@z�@�@�m@�
@�F@�@t�@C�@o@@
�H@
�@
��@
^5@
M�@
J@	�^@	�^@	��@	hs@	&�@��@��@�9@�u@r�@bN@bN@bN@Q�@1'@bG�O�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A��#A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��;A��HA��;A��/A��#A��#A��A��#A��A��
A��
A��#A��/A��;A��;A��/A��/A��/A��#A��A��A��#A��/A��;A��;A��#A��;A��/A��/A��#A��/A��/A��;A��TA��TA��TA��TA��TA��TA��TA��TA��TA��HA��HA��HA��HA��TA��`A��`A��mA��mA��`A��`A��TA��`A��TA��TA��TA��`A��mA��yA��mA��`A��`A��`A��`A��`A��mA��yA��yA��yA��mA��`A��`A��mA��mA��mA��yA��yA��A��yA��mA��mA��mA��mA��yA��A��A��A��A��yA��mA��mA��mA��yA��yA��A��A��A��A��A��yA��yA��yA��yA��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��TA��HA��/A���A���A���A���A���A���A���A���A���A�ĜA�A�A�ĜAĴ9AĲ-AĲ-Aİ!Aİ!Aĩ�Aĝ�AċDAąA�v�A�`BA�M�A�1'A�-A�-A�/A�1'A�1'A�/A�/A�-A�/A�+A��A�oA�A��A��A��`A��;Aú^Aå�AÃA�=qA¥�A�1'A�%A��TA���A��7A��#A�~�A�A�A���A��!A���A�ĜA�p�A��A�l�A��A��TA��-A��DA�ZA��A��A���A��-A���A��hA��A�l�A�9XA��#A��wA���A�r�A��A�~�A�K�A��`A���A�S�A�"�A��A��A�(�A���A���A��+A�|�A�t�A�ffA�XA�O�A�;dA��A�
=A�oA���A��+A�I�A���A���A�n�A�-A���A��A�`BA�33A��A�p�A�\)A�I�A�9XA�(�A��A�~�A�I�A��mA���A��7A�v�A�^5A�=qA���A�ffA�G�A�/A�bA���A�  A�A�  A�%A�{A��A�"�A�1'A�33A�A�A�7LA���A��jA�7LA�`BA���A��#A��!A���A�r�A�ZA�=qA�5?A�33A�XA�t�A��7A�z�A�r�A�E�A�{A�
=A��A�A���A��A��/A��-A�1'A��A��A���A��A�x�A�`BA�C�A�9XA�"�A�A��HA�ƨA�+A�ĜA��A�~�A���A��A�x�A�XA�9XA��A��A���A�ƨA���A�A���A���A�A���A���A�7LA��TA���A��DA��A��A�v�A�5?A���A��DA��FA�ĜA���A���A�ĜA��uA���A�jA�G�A�JA��
A�n�A��-A�1A�ƨA��A�(�A���A��DA�?}A�?}A���A��TA�ȴA�ƨA��!A�\)A�(�A���A��+A�Q�A��A�7LA���A�7LA�A��A��;A�ĜA��A��A�^5A���A���A���A�v�A�ffA���A�;dA�A���A���A�G�A�/A��A�VA���A�ĜA���A�n�A�M�A�bA��#A��A��PA��PA�r�A�/A�oA��A��TA�ȴA���A�r�A�O�A�7LA�bA�A�|�A�O�A��A��-A���A���A��PA�r�A�I�A�+A��A���A��HA���A�ĜA��!A���A��DA�VA�"�A��A���A��A��TA���A�ȴA��^A��9A���A�z�A�Q�A�$�A��`A��PA�dZA���A�ƨA��\A�G�A�oA�
=A���A��A�ĜA��PA�?}A�ȴA�`BA��A���A���A�G�A��A��A���A��9A��\A�|�A�jA�^5A�O�A�K�A�A�A�5?A�33A�&�A��A�oA�JA�  A���A��mA��;A��;A���A�A��jA��-A���A��A�x�A�dZA��/A���A��A~�yA}��A};dA|M�A{�
A{x�A{hsA{+Az�HAz�9Azn�Az-Az  Ay�Ay�Ay33Ax�Ax�+Ax�AxZAx �Aw�Aw�wAw��Aw��Aw�AwhsAwhsAwXAw?}Aw;dAw"�Aw�Av��Av�`Av�AuAt-Ar��ArffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                           ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
�-B
��B
�[B
��B
��B
�aB
�[B
��B
��B
�'B
ÖB
B
�-B
�aB
�[B
�aB
B
ÖB
�-B
�-B
ÖB
�-B
�aB
ÖB
�-B
�3B
��B
ĜB
ĜB
�3B
��B
�3B
�B
�mB
ŢB
�9B
��B
�B
�tB
�KB
��B
�jB
�vB
��B
�
B
�gB
�mB
�&B
��B
�VB
��B
��BB�B-�B%FB=�B`Bm�Be�B[�B_pBcTBg�B~]B�B��B�qB�NB�|B�B�gB��B��B�oB�oB�bB��B�B�]B��B��B�+B�\B|PBV9B+�B
��B
�}B
�dB
�LB
�YB
��B
y>B
n�B
R�B
G�B
<6B
!bB	��B	�sB	�]B	˒B	�qB	�B	qvB	dZB	C�B	7B	$�B	 �B	�B�B��B�5B�>B�8B��B��BɆBȴB�-B�B��B��B�B��B�B��B�OB��B��B��B��B��B��B��B�B�'B�?B�jB��B�}BÖB��B	�B	:B	'RB	1�B	K)B	IB	/�B	-�B	L�B	�B	�B	c�B	B�B	5B	6�B	$�B	*0B	B[B	G�B	A B	8�B	5�B	8�B	B�B	T�B	F?B	-�B	1�B	2�B	6FB	I�B	b�B	]/B	c B	o5B	l�B	jB	c�B	U�B	J#B	Q�B	WsB	W�B	R�B	H�B	M�B	S[B	ZB	V9B	OBB	\�B	f�B	_pB	v�B	��B	�AB	|�B	y�B	ncB	l�B	kQB	jB	m�B	f�B	iB	jB	f�B	f�B	o5B	kQB	o B	o�B	w�B	��B	��B	�B	��B	�B	�6B	��B	�B	� B	�-B	�EB	�zB	�tB	ƨB	��B	�B	��B	�dB	��B	�^B	��B	�LB	�9B	�nB	�3B	��B	��B	��B	�-B	�B	�9B	�B	��B	�9B	�LB	��B	�LB	��B	��B	�0B	��B	��B	��B	��B	��B	�*B	��B	�B	�B	�B	��B	�HB	��B	�B	��B	�wB	�B	�B	�B	�B	��B	��B	��B	��B	�dB	�jB	�wB	��B	�B	�<B	��B	�B	��B	�3B	��B	��B	�pB	�B	��B	�aB	��B	�?B	��B	��B	�dB	ݘB	ݘB	ܒB	��B	ݘB	��B	�B	�/B	��B	�WB	��B	��B	�;B	ߤB	�BB	�BB	�|B	�B	��B	�B	�|B	��B	��B	��B	�B	�B	��B	�B	�&B	��B	��B	��B	��B	�2B	�`B	�,B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�B	�oB	��B	�iB	�;B	�B	�cB	�B	�cB	�B	�B	�B	�B	�MB	�TB	�B	�TB	�B	�MB	��B	�%B	��B	��B	�+B	��B	��B	��B	��B	��B	�B	��B	��B	�(B	��B	��B	�.B	�]B	��B
  B
  B	��B
  B
B
�B
B
�B
GB
{B
GB
B
MB
�B
�B
�B
GB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
�B
�B
�B
�B
�B
�B
 B
 B
hB
�B
:B
oB
�B
@B
uB
@B
�B
FB
FB
�B
�B
�B
SB
�B
�B
�B
�B
+B
+B
YB
YB
YB
_B
1B
�B
eB
eB
7B
=B
�B
�B
�B
B
B
�B
�B
OB
�B
�B
!B
�B
VB
�B
�B
�B
 \B
 �B
 \B
 �B
!-B
!bB
!bB
!bB
!-B
!-B
!�B
!�B
!�B
!�B
"hB
"�B
#nB
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%B
%B
&B
&�B
&�B
&LB
&LB
'�B
'RB
(�B
(�B
(XB
(XB
)*B
*eB
*�B
*�B
*�B
*�B
*�B
*eB
*�B
+B
+�B
,B
-CB
,�B
-CB
.IB
.B
.B
.}B
/B
/�B
0!B
0�B
0�B
1�B
1�B
1�B
1�B
3�B
3�B
3�B
3�B
3�B
4nB
4�B
4nB
4�B
5tB
5tB
5?B
5tB
5?B
5�B
5?B
5�B
6zB
6�B
7�B
7�B
7�B
7�B
7�B
8B
8�B
8RB
8�B
8�B
9�B
:*B
:�B
:�B
<B
;dB
;�B
<�B
<�B
<jB
<�B
<�B
=<B
=<B
=�B
>BB
>wB
>wB
>BB
>BB
>�B
>�B
?�B
@�B
@�B
A�B
B'B
A�B
B'B
B'B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
DgB
DgB
EmB
EmB
EmB
EmB
EmB
E9B
E9B
FB
F�B
F�B
F�B
GB
F�B
F�B
GB
GEB
HKB
HB
HB
H�B
J�B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
MjB
L�B
M�B
MjB
NB
OBB
OB
OBB
O�B
PHB
R B
Q�B
R B
R�B
RTB
QNB
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
S&B
S�B
T�B
U�B
VB
VB
U�B
U�B
VmB
V�B
V�B
W?B
W?B
W�B
V�B
V�B
W?B
V�B
V�B
W?B
WsB
W?B
W?B
W
B
XEB
W�B
XB
XB
X�B
YB
YB
XyB
YKB
YB
Y�B
ZB
Z�B
[�B
\�B
^5B
^�B
_B
_B
^�B
_B
^�B
_;B
_;B
_;B
_pB
`vB
_�B
_�B
_pB
_�B
_�B
`B
`�B
aB
`�B
`�B
aB
aHB
a|B
b�B
bB
bNB
a�B
b�B
c B
cTB
cTB
c�B
dZB
d�B
e,B
e,B
e�B
e�B
f2B
ffB
gB
f�B
g�B
h>B
h>B
h�B
h�B
h�B
iDB
iyB
iyB
i�B
iyB
i�B
jB
jB
jKB
j�B
jB
kB
kB
kB
kB
kB
j�B
kB
kQB
k�B
k�B
j�B
kQB
k�B
l"B
lWB
l�B
m)B
m�B
n/B
n/B
ncB
ncB
o B
n�B
n�B
n/B
ncB
oiB
pB
pB
poB
poB
p;B
qB
qAB
rB
q�B
r|B
sB
sB
sMB
s�B
sMB
sMB
sMB
s�B
s�B
tB
tTB
tB
t�B
uZB
uZB
uZB
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
wfB
w2B
w2B
w�B
xlB
xlB
x�B
x�B
y	B
yrB
y>B
yrB
y>B
y>B
y�B
zDB
zxB
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
|B
|B
}"B
}�B
}�B
}�B
~(B
~(B
~]B
~]B
~]B
~]B
.B
~�B
~�B
~�B
.B
�B
�B
� B
�4B
�iB
��B
��B
�;B
�oB
��B
�AB
�AB
�B
��B
��B
�B
�AB
�AB
�uB
��B
�GB
�GB
�{B
�GB
��B
��B
�MB
��B
�MB
��B
��B
��B
�B
�B
�B
�SB
��B
��B
��B
�%B
�%B
�YB
��B
��B
�1B
�fB
��B
��B
��B
�B
��B
�B
�	B
�=B
�	B
�rB
�rB
��B
��B
��B
��B
�B
�DB
�xB
�xB
��B
�JB
��B
�~B
�~B
�JB
�~B
�~B
�~B
�PB
�B
��B
��B
��B
��B
�"B
�VB
�VB
�VB
�"B
��B
��B
��B
�(B
�\B
�\B
��B
��B
�bB
�.B
�bB
��B
�4B
�hB
��B
��B
��B
�B
�oB
�oB
�oB
��B
��B
��B
�uB
��B
��B
��B
�FB
�B
��B
�MB
��B
��B
�MB
��B
��B
��B
��B
��B
��B
�B
�B
�B
�SB
�B
��B
��B
�$B
��B
�aB
��B
��B
��B
��B
��B
�gB
��B
�aB
�gB
ÖB
��B
��B
�aB
��B
��B
�-B
��B
�'B
��B
��B
��B
�'B
�-B
�-B
��B
��B
�aB
ŢB
��B
��B
ÖB
��B
ÖB
��B
��B
�-B
B
�[B
�'B
�'B
B
��B
��B
ÖB
�3B
�3B
��B
��B
��B
�[B
��B
� B
B
�'B
�-B
��B
�aB
�gB
��B
�gB
�aB
��B
��B
�'B
�'B
�'B
�[B
��B
��B
ÖB
��B
ÖB
��B
B
�'B
�[B
��B
�'B
�aB
�-B
ÖB
�aB
��B
ÖB
��B
�[B
�'B
�[B
�'B
B
�[B
B
��B
��B
ÖB
�3B
��B
�aB
�[B
B
�[B
�'B
��B
��B
ÖB
�aB
��B
��B
��B
�-B
��B
B
�-B
��B
ÖB
��B
��B
ÖB
��B
B
B
B
�aB
��B
��B
��B
�aB
ÖB
��B
�[B
�[B
�-B
ÖB
�3B
��B
ÖB
�aB
B
�aB
��B
B
�aB
��B
�3B
��B
��B
ÖB
��B
�-B
��B
��B
�aB
��B
�gB
ĜB
�gB
�gB
��B
�aB
ÖB
�aB
ĜB
�B
�9B
�9B
�B
�3B
��B
��B
ÖB
�3B
��B
�B
�9B
�B
��B
�gB
��B
ÖB
��B
ĜB
ĜB
�B
�9B
�9B
�9B
�gB
�3B
��B
ÖB
��B
�3B
��B
ĜB
�mB
ŢB
��B
ĜB
�3B
��B
ŢB
ŢB
�9B
ŢB
�mB
��B
�mB
�9B
�B
�mB
�B
�B
ƨB
��B
ĜB
�gB
��B
��B
�?B
��B
ŢB
��B
�B
�tB
�tB
�?B
�mB
ĜB
�mB
�B
�B
�zB
�tB
�tB
�B
�mB
ƨB
�B
�EB
ǮB
�zB
�KB
ȀB
�#B
̘B
�B
�jB
�jB
�dB
�dB
�0B
�dB
�6B
��B
�<B
��B
��B
҉B
ϫB
�vB
��B
ϫB
�B
�[B
ӏB
��B
�aB
�gB
��B
�
B
�B
֡B
�B
�gB
�mB
��B
�B
՛B
��B
��B
�
B
՛B
��B
�gB
��B
ԕB
ԕB
یB
�2B
�EB
�B
�B
�ZB
�B
�B
��B
��B
�8B$�B
�B
�B)�B�B
�%BAB�B
��B
��B
�vB
�DB
�TB
�B
��B
�B
��B
�B
�TB
�B
�+B
�lB�B�BSB�B
	B{B{B�B�B#B�B �B&�B.}B;0B0�B.B(�B&�B&B&B%�B#B$�B#�B$B33B>�BR�By>BR�BY�BXEBe`Bo BffBj�BqABv�BrGBe�Bc�Bd&Bc Bh�BjBa�BiyBV�BW�BW
BW?BXBffBf�B^B\)Bd�Bc�Bc Bc�Bc�Bb�Be�Be�Bh�Be�Bm�Bk�Bw�B��B}�B��B�B|Bt�B~�B�+B�xB��B��B�qB�VB��B�@B��B��B�aB�B�0BʌB�}BںBٴB��B�;B�BfBB�B�B��B�dB�B��B��B��B֡BѷB�B��B�zB�3BΥB�mBʌB��B�+B�YB�SB��B��B��B��B�(B��B��B�hB��B��B��B��B��B��B�oB��B��B��B�qB��B��B�B��B�B�dB�dB�EB�B��B�|BٴB��B�cB�NB�gB�XB��B�B��B��B�B�CB��B��B��B��B��B��B�MB��B�MB�=B��B��Bp�BiBsBYBM�BZBOvBb�B>�B;dB9XB-wB,qBCaB&BkB�B�B�B_B�B
��B
�2B
�B
�B
��B
�B
��B
��B
�B
�2B
ӏB
��B
�EB
�HB
�#B
ŢB
ɆB
�B
��B
�B
�qB
�B
��B
�XB
�-B
��B
��B
�B
��B
�$B
�qB
��B
�$B
�hB
��B
�B
��B
��B
�B
��B
��B
��B
��B
�uB
��B
�(B
��B
�B
��B
��B
��B
��B
�~B
�B
��B
�fB
�4B
�uB
��B
|B
xlB
z�B
tTB
qAB
r�B
sMB
}"B
{�B
z�B
cB
sB
iDB
m�B
d�B
aHB
]�B
X�B
V�B
TaB
XyB
S[B
O�B
N�B
OB
J�B
K�B
L�B
H�B
H�B
GEB
J�B
EmB
D�B
A�B
F?B
@�B
>B
=qB
@B
<jB
;0B
:*B
49B
5�B
/�B
U2B
@B
(�B
kB
�B
B
�B
YB
�B	�B	�.B	��B	�	B	��B	�	B	�B	��B	��B	�/B	�GB	�B	�B	��B	�B	��B	�,B	��B	�vB	�NB	��B	�5B	�dB	�5B	�dB	��B	�?B	��B	�,B	�yB	�|B	�DB	�pB	� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021012416334020210124163340IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020315011120210203150111QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020315011120210203150111QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014420210427140144IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162921              CF      PSAL                            ?��G�O�D��\G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163434    CF                  PSAL            G�O�?\)G�O�CH{G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                