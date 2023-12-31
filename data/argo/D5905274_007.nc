CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-03-24T04:20:21Z creation; 2023-04-26T19:24:26Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180324042021  20230426192426  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7315_008643_007                 7315_008643_007                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�U��Q@�U��Q11  @�U�`A�7@�U�`A�7@0���L@0���L�d8���>W�d8���>W11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?��H@B�\@�G�@�G�@��R@�  AG�AG�A ��A,(�A@  A`��A���A�Q�A�  A���A��AϮA�  A�  B   B�
B�
B�
B�B'�B0  B8(�B@(�BH(�BO�
BW�
B`  Bh  Bp  Bw�B�B��
B�  B�{B��B��B��B�  B�{B�  B��
B��B�  B�{B�  B�  B�{B�  B��B�  B�{B�  B��B�  B�  B��B�  B��B�  B�(�B�{B�{C 
=C  C{C  C��C	��C  C
=C
=C
=C  C  C
=C{C  C��C 
=C"
=C#��C&  C(
=C)��C,  C.  C/��C1�C3��C6  C8
=C:
=C<
=C=��C?��CB{CD
=CF
=CH  CI�CK�CM�CO�CR  CS��CV  CX
=CZ  C[��C]��C`  Cb{Cd{Cf  Cg��Ci��Ck��Cm�Co�HCr  Ct{Cv
=Cw��Cz
=C|  C~
=C�
=C���C�  C�
=C�
=C�
=C�  C�  C�  C�  C�  C���C���C�C�
=C�\C�\C�C�  C���C���C�  C�C�  C�  C���C���C���C���C���C���C���C�  C���C�  C�C���C�  C���C���C���C���C���C�  C���C���C���C���C���C�  C�C�  C�  C�
=C�C�C�
=C�C�C���C���C���C�C�
=C�
=C�
=C���C�  C�C�  C���C���C���C���C�
=C�C�C�C���C�  C���C���C���C���C�  C�  C�C�C�  C���C�  C���C���C�  C�  C�C�C���C�  C�  C�  C�  C�C�C���C�  C�
=C�C�C�C���C���C�
=C���C��C���C�C���C�  C�
=C�C�
=C�  C�  C�C�  C���C�D D }qD�D� D�Dz�D�D}qD  D��D  D� D�qD}qD  D� D�D� D�qD	� D
  D
��D�D� D  D� D�D��D�D� D  D� D  D��D�D� DD��DD�D�D��D�D��D�qD}qD  D� D  D}qD  D�D  D}qD�qDz�D�qD}qD  D��D�qD� D�D��D �D � D!�D!��D"  D"� D#  D#� D#�qD$z�D$�qD%}qD&  D&� D'�D'�D(�D(� D)  D)}qD)�qD*��D+  D+��D,  D,� D-�D-� D-�qD.}qD.�qD/� D0�D0��D1  D1��D2  D2� D2�qD3��D3�qD4}qD4��D5� D6  D6� D7  D7� D7�qD8� D9  D9}qD:�D:��D:�qD;}qD<  D<�D=�D=��D>D>��D?  D?xRD@  D@� DA�DA� DBDB}qDC  DCz�DD  DD}qDE  DE��DF�DF��DG  DG��DHDH}qDI  DI� DJ  DJ��DJ�qDK�DL  DLz�DM  DM��DN  DN� DODO� DP�DP� DQ  DQ��DR  DR��DS  DS}qDS�qDT��DU�DU��DV�DV}qDV�qDW� DX  DX��DY  DY}qDZ�DZ�D[  D[}qD\  D\�D]D]� D]�qD^��D_  D_}qD`�D`� D`�qDa}qDb  Db�Dc  Dc� Dd  Dd� Dd�qDe� Df�Df}qDgDgz�Dg��Dh� Dh�qDi��Di�qDj� Dj�qDk�DlDl�DmDm�Dn�Dn� Do  Do}qDpDp}qDp�RDq� Dr  Dr� Ds  Dsz�Dt  Dtz�Dt�qDu}qDv  Dv��Dw  Dw� Dx�Dx��Dy�Dy}qDz�Dz� D{�D{��D|D|}qD}  D}}qD~  D~� D�Dz�D�  D�@ D���D��HD��qD�@ D�}qD���D�  D�=qD�}qD��HD��D�AHD��HD���D��)D�@ D���D�� D�HD�B�D�~�D�� D�HD�AHD�~�D���D���D�AHD���D�D�  D�>�D�}qD�� D��D�AHD�� D��)D���D�AHD���D���D�HD�AHD���D�� D���D�>�D�� D���D���D�@ D��HD�� D���D�=qD�� D�� D���D�@ D��HD���D���D�>�D�� D��HD�  D�AHD��HD�� D�HD�AHD�� D���D��qD�=qD�~�D�� D�  D�@ D��HD�� D���D�@ D��HD��HD���D�>�D�~�D��qD�HD�@ D�~�D�� D�  D�AHD��HD���D�  D�AHD�� D�D�HD�AHD��HD�� D���D�>�D��HD�D��D�AHD��HD��HD�HD�AHD�~�D��qD���D�AHD���D��HD�  D�AHD���D�� D�  D�AHD��HD�� D���D�>�D�~�D�� D�  D�@ D�� D���D���D�>�D�� D��HD�  D�=qD��HD�D��D�>�D�|)D���D��D�@ D�~�D��HD�HD�@ D�~�D��)D���D�B�D��HD���D�  D�>�D�� D��HD�  D�AHD�~�D�� D��D�AHD�� D���D���D�=qD�� D���D���D�@ D���D��HD�HD�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD�� D���D�>�D��HD�� D�  D�B�D���D�D�HD�=qD�}qD��qD��qD�>�D�~�D��HD��D�>�D�� D�� D��qD�=qD�}qD��qD�  D�@ D�� D�� D�  D�@ D�� D��HD��D�AHD�� D�� D�HD�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D���D�D�  D�@ D��HD�D��D�AHD�� D�� D�  D�AHD��HD�� D���D�@ D��HD���D�  D�@ D�� D��HD�  D�@ D D¾�D�  D�@ DÁHD��HD�  D�>�D�~�D�� D�HD�AHDŁHD��HD�  D�>�DƁHD�� D���D�B�DǁHDǾ�D�  D�@ D�}qDȾ�D�HD�B�Dɀ DɽqD�  D�AHDʁHDʽqD��qD�@ Dˀ D��HD��D�AHD̀ D̾�D�  D�AHD̀ D�� D�  D�AHD�~�Dξ�D�HD�B�Dπ DϾ�D�HD�AHDЁHD��HD�  D�@ DсHD�� D�  D�AHD҂�D�� D���D�@ Dӂ�D�� D���D�>�D�~�DԾ�D�  D�>�DՀ D��HD���D�>�Dր D־�D���D�AHD�~�D׽qD�  D�B�D؁HDؾ�D���D�@ Dـ D��HD�  D�>�D�~�Dھ�D�HD�@ D�~�D��HD���D�>�D܀ D�� D�HD�AHD݀ Dݾ�D�  D�>�D�~�D�� D��qD�>�D߁HD��HD�  D�>�D�}qDྸD��D�B�D�~�D�� D�  D�>�D� D�� D�  D�@ D�~�D�� D�  D�>�D�HD�qD��)D�>�D�HD徸D���D�B�D�HD�� D���D�<)D� D��HD���D�>�D�HD��HD��D�@ D�~�D龸D���D�=qD�~�D�� D�  D�@ D� D��HD��D�AHD�HD��HD�HD�@ D�HD�� D�  D�@ D� DD�HD�B�DD�D��D�C�D���D��HD�HD�B�D� D�� D�HD�AHD�HD�D�  D�AHD� D�D�  D�B�D�HD�� D���D�<)D�}qD���D���D�@ D�~�D���D��qD�@ D��HD���D���D�@ D�� D�� D�  D�AHD��HD��HD��D�>�D��HD���?��?k�?�z�?\?�ff@�\@
=@(��@B�\@W
=@fff@�  @���@�
=@�G�@�{@��H@�ff@��@޸R@�@�
=A�\A�A��AG�A�A��A"�\A(��A.{A4z�A;�AAG�AG
=AL(�AQ�AW�A]p�Ac33AhQ�Ap  Atz�Ay��A\)A��\A���A�
=A�=qA��A��A��\A��A�Q�A��\A�{A���A�(�A��RA�G�A��A�{A���A��\A�z�A�{A�\)A���A��A�p�A�\)A�G�A��
A�ffA�  A�=qA��
A�p�A�
=A�G�A�33A�p�A�
=A�G�A��
A�{A�  A��A��
A�A�  A��A�(�A�A�\)A���A�33A�p�A�\)A�G�A��A�B (�BG�BffB\)BQ�Bp�B�\B�B��B	p�B
=qB\)BQ�Bp�BffB�B��B{B\)BQ�BG�BffB�BQ�B�B=qB\)BQ�BG�B=qB�B z�B!B"�HB$  B$��B&{B'33B(  B(��B)�B+
=B,  B,��B-�B/
=B0Q�B1��B2�RB3�B4��B5B6�RB7�B8z�B9p�B:ffB;�B<z�B=p�B>�RB@(�BA�BB=qBC
=BC�BD��BE�BF�RBH(�BIG�BJ=qBK�BL  BL��BM�BN�HBP  BQ�BR=qBS33BTQ�BUG�BV{BV�HBW�BX��BY��BZffB[�B\��B]�B^�HB_�
B`��Ba�BbffBc\)BdQ�Be�Bf=qBg\)Bhz�BiG�Bj=qBk33Bl  Bl��Bmp�BnffBo33Bp(�Bqp�Br�\Bs�BtQ�Bup�Bu�Bv�RBw�
Bx��By��B{33B|(�B}�B~=qB
=B�B�ffB��HB��B�{B���B�33B�B�(�B���B�
=B���B�{B��RB�p�B��B�z�B�
=B�p�B��B�ffB���B�B�=qB��HB�\)B��B�=qB���B�p�B�(�B��RB�G�B�B�(�B���B�33B�B�Q�B�
=B��B�=qB���B�G�B��B�Q�B��RB�\)B�(�B��RB�\)B�B�Q�B��HB�p�B�=qB��HB��B��
B�z�B�
=B��B�ffB�
=B���B�  B��RB�33B�  B��RB�\)B�B�Q�B���B��B�=qB���B��B�{B�z�B�33B��B��\B�33B�B�=qB���B���B�=qB��HB�\)B��B�=qB���B�G�B�B�{B�=qB��\B���B�G�B���B��
B�  B�(�B�ffB��HB�33B��B��
B��
B�=qB�z�B���B�33B�p�B���B�B�=qB���B���B�33B�\)B��B�  B�ffB��HB�33B���B��B�  B�=qB���B��B�p�B���B��B�(�B���B�
=B�p�BÙ�B��B�(�Bď\B���B�p�B��
B�(�B�Q�BƏ\B��HB�G�B��
B�(�B�z�Bȣ�B��HB�p�B��
B�=qB�ffBʣ�B�33BˮB�{B�Q�B̏\B���B�G�B��B�Q�BθRB���B�33Bϙ�B�(�BЏ\B���B��B�p�B��B�ffB���B�
=B�\)BӮB�(�BԸRB�
=B�33Bՙ�B�=qB֣�B���B��B�B�(�B�ffBظRB�
=BٮB�{B�Q�Bڣ�B�33BۮB�{B�Q�Bܣ�B�G�B�B��B�Q�Bޣ�B�G�B�B�(�B�ffB�RB��B�B�{B�Q�B��B�
=B�B�{B�=qB�\B�33B噚B�B�=qB�RB��B癚B��
B�(�B��B��B�B�B�(�B�RB��B�\)B�B�{B�RB��B�G�B��B�Q�B�RB��HB�G�B��B�Q�B��\B��HB�B�  B�(�B��B��B�B�  B�=qB��HB�G�B��B��B�ffB���B�33B���B�=qB��RB���B�\)B��
B�ffB���B�
=B���B�(�B�Q�B��RB�\)B��B�  B��\B���B�33B���C {C 33C ffC �RC ��C  C=qC�\C��C�
C(�C\)Cz�C�C
=C33CQ�C�C�HC  CQ�C�\C�C  C=qCQ�C�\C�HC��C33C�C��C�
C(�CffC�CC{C(�Cp�C�RC�
C	
=C	\)C	�C	�C
{C
=qC
ffC
C
�C{Cp�C�C��C(�C\)C�C�HC�C=qC�\C��C�CG�C�C��C��C�C\)C�C�
C{CffC�\C�
C(�CQ�C��C�HC  CQ�C��CC{CQ�Cz�C��C��C33C�\C��C�CG�CffC�C  C�Cz�C��C�HC=qCz�C��C
=C33CffC��C��CQ�C��CC�CQ�C�C�C
=CffC�C��C(�CQ�C�C�
C{Cp�C��C�HC=qC\)CC��C�C�C�C 
=C =qC z�C �
C ��C!\)C!�C!��C"(�C"Q�C"�C"��C#(�C#p�C#��C$  C$(�C$�C$��C%  C%G�C%p�C%�
C&
=C&ffC&�\C&��C'33C'\)C'�RC'�C(33C(�\C(�RC){C)=qC)�\C)�C*
=C*p�C*��C*�C+(�C+p�C+��C+�C,Q�C,�C,�HC-
=C-ffC-��C-�HC.33C.\)C.�RC.�C/=qC/�C/�C0{C0=qC0��C0C1�C1\)C1��C1�C2�C2�C2�C3
=C3G�C3p�C3�
C3��C4\)C4z�C4�HC5
=C5G�C5��C5��C6=qC6\)C6C6�C7G�C7z�C7�
C8  C8\)C8�C8�HC9{C9ffC9�\C9�C:�C:Q�C:�C:�
C;�C;z�C;��C<  C<33C<�\C<�RC=�C=Q�C=�C=�HC>(�C>z�C>�C?
=C?33C?�\C?C@�C@=qC@��C@��CA33CAG�CA��CA�CB{CBp�CB��CB�CC�CCz�CC��CC�CD=qCDffCDCD��CEQ�CEp�CE�
CF{CF=qCF��CF��CG(�CGQ�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                         ?k�?��H@B�\@�G�@�G�@��R@�  AG�AG�A ��A,(�A@  A`��A���A�Q�A�  A���A��AϮA�  A�  B   B�
B�
B�
B�B'�B0  B8(�B@(�BH(�BO�
BW�
B`  Bh  Bp  Bw�B�B��
B�  B�{B��B��B��B�  B�{B�  B��
B��B�  B�{B�  B�  B�{B�  B��B�  B�{B�  B��B�  B�  B��B�  B��B�  B�(�B�{B�{C 
=C  C{C  C��C	��C  C
=C
=C
=C  C  C
=C{C  C��C 
=C"
=C#��C&  C(
=C)��C,  C.  C/��C1�C3��C6  C8
=C:
=C<
=C=��C?��CB{CD
=CF
=CH  CI�CK�CM�CO�CR  CS��CV  CX
=CZ  C[��C]��C`  Cb{Cd{Cf  Cg��Ci��Ck��Cm�Co�HCr  Ct{Cv
=Cw��Cz
=C|  C~
=C�
=C���C�  C�
=C�
=C�
=C�  C�  C�  C�  C�  C���C���C�C�
=C�\C�\C�C�  C���C���C�  C�C�  C�  C���C���C���C���C���C���C���C�  C���C�  C�C���C�  C���C���C���C���C���C�  C���C���C���C���C���C�  C�C�  C�  C�
=C�C�C�
=C�C�C���C���C���C�C�
=C�
=C�
=C���C�  C�C�  C���C���C���C���C�
=C�C�C�C���C�  C���C���C���C���C�  C�  C�C�C�  C���C�  C���C���C�  C�  C�C�C���C�  C�  C�  C�  C�C�C���C�  C�
=C�C�C�C���C���C�
=C���C��C���C�C���C�  C�
=C�C�
=C�  C�  C�C�  C���C�D D }qD�D� D�Dz�D�D}qD  D��D  D� D�qD}qD  D� D�D� D�qD	� D
  D
��D�D� D  D� D�D��D�D� D  D� D  D��D�D� DD��DD�D�D��D�D��D�qD}qD  D� D  D}qD  D�D  D}qD�qDz�D�qD}qD  D��D�qD� D�D��D �D � D!�D!��D"  D"� D#  D#� D#�qD$z�D$�qD%}qD&  D&� D'�D'�D(�D(� D)  D)}qD)�qD*��D+  D+��D,  D,� D-�D-� D-�qD.}qD.�qD/� D0�D0��D1  D1��D2  D2� D2�qD3��D3�qD4}qD4��D5� D6  D6� D7  D7� D7�qD8� D9  D9}qD:�D:��D:�qD;}qD<  D<�D=�D=��D>D>��D?  D?xRD@  D@� DA�DA� DBDB}qDC  DCz�DD  DD}qDE  DE��DF�DF��DG  DG��DHDH}qDI  DI� DJ  DJ��DJ�qDK�DL  DLz�DM  DM��DN  DN� DODO� DP�DP� DQ  DQ��DR  DR��DS  DS}qDS�qDT��DU�DU��DV�DV}qDV�qDW� DX  DX��DY  DY}qDZ�DZ�D[  D[}qD\  D\�D]D]� D]�qD^��D_  D_}qD`�D`� D`�qDa}qDb  Db�Dc  Dc� Dd  Dd� Dd�qDe� Df�Df}qDgDgz�Dg��Dh� Dh�qDi��Di�qDj� Dj�qDk�DlDl�DmDm�Dn�Dn� Do  Do}qDpDp}qDp�RDq� Dr  Dr� Ds  Dsz�Dt  Dtz�Dt�qDu}qDv  Dv��Dw  Dw� Dx�Dx��Dy�Dy}qDz�Dz� D{�D{��D|D|}qD}  D}}qD~  D~� D�Dz�D�  D�@ D���D��HD��qD�@ D�}qD���D�  D�=qD�}qD��HD��D�AHD��HD���D��)D�@ D���D�� D�HD�B�D�~�D�� D�HD�AHD�~�D���D���D�AHD���D�D�  D�>�D�}qD�� D��D�AHD�� D��)D���D�AHD���D���D�HD�AHD���D�� D���D�>�D�� D���D���D�@ D��HD�� D���D�=qD�� D�� D���D�@ D��HD���D���D�>�D�� D��HD�  D�AHD��HD�� D�HD�AHD�� D���D��qD�=qD�~�D�� D�  D�@ D��HD�� D���D�@ D��HD��HD���D�>�D�~�D��qD�HD�@ D�~�D�� D�  D�AHD��HD���D�  D�AHD�� D�D�HD�AHD��HD�� D���D�>�D��HD�D��D�AHD��HD��HD�HD�AHD�~�D��qD���D�AHD���D��HD�  D�AHD���D�� D�  D�AHD��HD�� D���D�>�D�~�D�� D�  D�@ D�� D���D���D�>�D�� D��HD�  D�=qD��HD�D��D�>�D�|)D���D��D�@ D�~�D��HD�HD�@ D�~�D��)D���D�B�D��HD���D�  D�>�D�� D��HD�  D�AHD�~�D�� D��D�AHD�� D���D���D�=qD�� D���D���D�@ D���D��HD�HD�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD�� D���D�>�D��HD�� D�  D�B�D���D�D�HD�=qD�}qD��qD��qD�>�D�~�D��HD��D�>�D�� D�� D��qD�=qD�}qD��qD�  D�@ D�� D�� D�  D�@ D�� D��HD��D�AHD�� D�� D�HD�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D���D�D�  D�@ D��HD�D��D�AHD�� D�� D�  D�AHD��HD�� D���D�@ D��HD���D�  D�@ D�� D��HD�  D�@ D D¾�D�  D�@ DÁHD��HD�  D�>�D�~�D�� D�HD�AHDŁHD��HD�  D�>�DƁHD�� D���D�B�DǁHDǾ�D�  D�@ D�}qDȾ�D�HD�B�Dɀ DɽqD�  D�AHDʁHDʽqD��qD�@ Dˀ D��HD��D�AHD̀ D̾�D�  D�AHD̀ D�� D�  D�AHD�~�Dξ�D�HD�B�Dπ DϾ�D�HD�AHDЁHD��HD�  D�@ DсHD�� D�  D�AHD҂�D�� D���D�@ Dӂ�D�� D���D�>�D�~�DԾ�D�  D�>�DՀ D��HD���D�>�Dր D־�D���D�AHD�~�D׽qD�  D�B�D؁HDؾ�D���D�@ Dـ D��HD�  D�>�D�~�Dھ�D�HD�@ D�~�D��HD���D�>�D܀ D�� D�HD�AHD݀ Dݾ�D�  D�>�D�~�D�� D��qD�>�D߁HD��HD�  D�>�D�}qDྸD��D�B�D�~�D�� D�  D�>�D� D�� D�  D�@ D�~�D�� D�  D�>�D�HD�qD��)D�>�D�HD徸D���D�B�D�HD�� D���D�<)D� D��HD���D�>�D�HD��HD��D�@ D�~�D龸D���D�=qD�~�D�� D�  D�@ D� D��HD��D�AHD�HD��HD�HD�@ D�HD�� D�  D�@ D� DD�HD�B�DD�D��D�C�D���D��HD�HD�B�D� D�� D�HD�AHD�HD�D�  D�AHD� D�D�  D�B�D�HD�� D���D�<)D�}qD���D���D�@ D�~�D���D��qD�@ D��HD���D���D�@ D�� D�� D�  D�AHD��HD��HD��D�>�D��HG�O�?��?k�?�z�?\?�ff@�\@
=@(��@B�\@W
=@fff@�  @���@�
=@�G�@�{@��H@�ff@��@޸R@�@�
=A�\A�A��AG�A�A��A"�\A(��A.{A4z�A;�AAG�AG
=AL(�AQ�AW�A]p�Ac33AhQ�Ap  Atz�Ay��A\)A��\A���A�
=A�=qA��A��A��\A��A�Q�A��\A�{A���A�(�A��RA�G�A��A�{A���A��\A�z�A�{A�\)A���A��A�p�A�\)A�G�A��
A�ffA�  A�=qA��
A�p�A�
=A�G�A�33A�p�A�
=A�G�A��
A�{A�  A��A��
A�A�  A��A�(�A�A�\)A���A�33A�p�A�\)A�G�A��A�B (�BG�BffB\)BQ�Bp�B�\B�B��B	p�B
=qB\)BQ�Bp�BffB�B��B{B\)BQ�BG�BffB�BQ�B�B=qB\)BQ�BG�B=qB�B z�B!B"�HB$  B$��B&{B'33B(  B(��B)�B+
=B,  B,��B-�B/
=B0Q�B1��B2�RB3�B4��B5B6�RB7�B8z�B9p�B:ffB;�B<z�B=p�B>�RB@(�BA�BB=qBC
=BC�BD��BE�BF�RBH(�BIG�BJ=qBK�BL  BL��BM�BN�HBP  BQ�BR=qBS33BTQ�BUG�BV{BV�HBW�BX��BY��BZffB[�B\��B]�B^�HB_�
B`��Ba�BbffBc\)BdQ�Be�Bf=qBg\)Bhz�BiG�Bj=qBk33Bl  Bl��Bmp�BnffBo33Bp(�Bqp�Br�\Bs�BtQ�Bup�Bu�Bv�RBw�
Bx��By��B{33B|(�B}�B~=qB
=B�B�ffB��HB��B�{B���B�33B�B�(�B���B�
=B���B�{B��RB�p�B��B�z�B�
=B�p�B��B�ffB���B�B�=qB��HB�\)B��B�=qB���B�p�B�(�B��RB�G�B�B�(�B���B�33B�B�Q�B�
=B��B�=qB���B�G�B��B�Q�B��RB�\)B�(�B��RB�\)B�B�Q�B��HB�p�B�=qB��HB��B��
B�z�B�
=B��B�ffB�
=B���B�  B��RB�33B�  B��RB�\)B�B�Q�B���B��B�=qB���B��B�{B�z�B�33B��B��\B�33B�B�=qB���B���B�=qB��HB�\)B��B�=qB���B�G�B�B�{B�=qB��\B���B�G�B���B��
B�  B�(�B�ffB��HB�33B��B��
B��
B�=qB�z�B���B�33B�p�B���B�B�=qB���B���B�33B�\)B��B�  B�ffB��HB�33B���B��B�  B�=qB���B��B�p�B���B��B�(�B���B�
=B�p�BÙ�B��B�(�Bď\B���B�p�B��
B�(�B�Q�BƏ\B��HB�G�B��
B�(�B�z�Bȣ�B��HB�p�B��
B�=qB�ffBʣ�B�33BˮB�{B�Q�B̏\B���B�G�B��B�Q�BθRB���B�33Bϙ�B�(�BЏ\B���B��B�p�B��B�ffB���B�
=B�\)BӮB�(�BԸRB�
=B�33Bՙ�B�=qB֣�B���B��B�B�(�B�ffBظRB�
=BٮB�{B�Q�Bڣ�B�33BۮB�{B�Q�Bܣ�B�G�B�B��B�Q�Bޣ�B�G�B�B�(�B�ffB�RB��B�B�{B�Q�B��B�
=B�B�{B�=qB�\B�33B噚B�B�=qB�RB��B癚B��
B�(�B��B��B�B�B�(�B�RB��B�\)B�B�{B�RB��B�G�B��B�Q�B�RB��HB�G�B��B�Q�B��\B��HB�B�  B�(�B��B��B�B�  B�=qB��HB�G�B��B��B�ffB���B�33B���B�=qB��RB���B�\)B��
B�ffB���B�
=B���B�(�B�Q�B��RB�\)B��B�  B��\B���B�33B���C {C 33C ffC �RC ��C  C=qC�\C��C�
C(�C\)Cz�C�C
=C33CQ�C�C�HC  CQ�C�\C�C  C=qCQ�C�\C�HC��C33C�C��C�
C(�CffC�CC{C(�Cp�C�RC�
C	
=C	\)C	�C	�C
{C
=qC
ffC
C
�C{Cp�C�C��C(�C\)C�C�HC�C=qC�\C��C�CG�C�C��C��C�C\)C�C�
C{CffC�\C�
C(�CQ�C��C�HC  CQ�C��CC{CQ�Cz�C��C��C33C�\C��C�CG�CffC�C  C�Cz�C��C�HC=qCz�C��C
=C33CffC��C��CQ�C��CC�CQ�C�C�C
=CffC�C��C(�CQ�C�C�
C{Cp�C��C�HC=qC\)CC��C�C�C�C 
=C =qC z�C �
C ��C!\)C!�C!��C"(�C"Q�C"�C"��C#(�C#p�C#��C$  C$(�C$�C$��C%  C%G�C%p�C%�
C&
=C&ffC&�\C&��C'33C'\)C'�RC'�C(33C(�\C(�RC){C)=qC)�\C)�C*
=C*p�C*��C*�C+(�C+p�C+��C+�C,Q�C,�C,�HC-
=C-ffC-��C-�HC.33C.\)C.�RC.�C/=qC/�C/�C0{C0=qC0��C0C1�C1\)C1��C1�C2�C2�C2�C3
=C3G�C3p�C3�
C3��C4\)C4z�C4�HC5
=C5G�C5��C5��C6=qC6\)C6C6�C7G�C7z�C7�
C8  C8\)C8�C8�HC9{C9ffC9�\C9�C:�C:Q�C:�C:�
C;�C;z�C;��C<  C<33C<�\C<�RC=�C=Q�C=�C=�HC>(�C>z�C>�C?
=C?33C?�\C?C@�C@=qC@��C@��CA33CAG�CA��CA�CB{CBp�CB��CB�CC�CCz�CC��CC�CD=qCDffCDCD��CEQ�CEp�CE�
CF{CF=qCF��CF��CG(�CGQ�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�l@�4@�"G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�XA�jA�bNA�`BA�ZA�M�A�S�A�VA�XA�VA�"�A��
A�33A�t�A�M�A���A̍PA�l�A�G�A���A�S�A��`A�v�A�jA�E�A�bA�~�AǇ+A�-A�A��#Aƴ9A�&�A���A��/A��mAǙ�Aư!A���A�=qA�ZA�A�A���A��9A��RA�z�A�$�A��^A�jA�n�A�(�A��A�+A��A���A���A��A�^5A���A��A�O�A�A���A�oA�oA�t�A�5?A��A�I�A�$�A�5?A��#A�%A��9A���A�ĜA�jA�=qA��+A�\)A���A�{A�I�A���A�{A���A�z�A��DA�$�A�VA�S�A�dZA�;dA��uA��RA��A�G�A}�As��Ar=qAp9XAk�;AdJAap�A_�PA]�wAYXAV�/AP�ANffAL�uAJVAGoAEVAB�AA7LA?�#A>��A>9XA=�PA<bA7�^A6��A6�A2��A1oA.��A-��A,��A+�A*ȴA)�hA'�wA&VA%�A$�A$��A$ȴA$M�A#�A"^5A"bA!�A!�A!;dA ^5A!;dA Q�A�;A?}A��AAp�A+A��A��A��A��AI�AoA��A�A;dA�7A�A��A�A�uA��AVA
�HA	33A�
A�7AA��A�/A�Al�Ar�A  Ap�A �yAXA �j@�~�@�$�A�AQ�A��A��AȴA33AA �9@��@��\@�M�@��
A�A33A��A�#AbA �A �A(�A(�A�AƨAM�A^5A5?AG�A�A�7At�A�PA�hA��A|�A33A ȴA A�@���@�ȴ@�@�hs@�&�@���@�ȴ@�J@���@�o@�V@��u@�P@�"�@�v�@�O�@��@��@���@��`@�j@�ƨ@�R@�5?@�{@���@�x�@�7L@�ȴ@�$�@��@�x�@�G�@�/@���@�u@��;@�l�@旍@�-@��@���@�7L@�A�@��
@�P@�;d@���@�%@��m@�\)@ޏ\@���@�@ݩ�@݉7@�`B@�`B@�?}@�l�@�^5@��@ى7@�&�@��/@�Ĝ@�Q�@�o@�^5@�5?@պ^@�Ĝ@��;@Ӆ@�o@ҟ�@��@�/@Гu@Ͼw@�o@�E�@ͺ^@�hs@��@��/@̴9@ˮ@�~�@�J@�hs@�V@���@��@���@���@ȼj@ȴ9@�z�@���@�K�@Ɵ�@�J@ŉ7@��@���@�Q�@�1@å�@�K�@��@°!@�n�@�$�@��7@��`@�1'@�ƨ@��@��@�v�@�=q@���@���@�x�@��`@��@�Q�@���@�\)@�33@��R@�V@�{@�O�@��@��@��@��@�K�@���@��!@�{@�x�@�/@��/@�j@��@���@��!@��T@���@�7L@��@� �@���@��m@�ƨ@��@��@�\)@�33@�~�@���@�7L@��9@���@��@�C�@��@�M�@���@�X@��`@��u@�r�@�bN@�9X@��m@��P@�\)@�o@���@�V@�&�@���@��u@�r�@���@�o@���@�5?@���@�&�@��@���@�Z@�(�@�1@��m@���@�ƨ@���@�S�@���@��\@�M�@�@��/@��@���@��@�9X@��@��P@�o@�ff@�J@���@��7@�hs@�X@��@�%@���@��D@� �@��@��@��
@���@��w@���@��P@�|�@�l�@�l�@�\)@�C�@��y@���@��\@�v�@�M�@��@�7L@���@��j@���@��@�I�@�9X@�b@�  @��m@��m@��;@��w@���@�|�@�l�@�\)@�C�@�@��H@�ȴ@��R@��\@�n�@�M�@�5?@��@���@��@���@�O�@��j@��m@�|�@�C�@��@���@�M�@��@�@��T@��T@��T@��#@��h@��@�A�@�  @�ƨ@��F@�t�@���@��y@�ȴ@�V@�@��h@�G�@�&�@��@�%@�Ĝ@�Q�@�(�@�1@���@�+@��!@�ff@��T@��h@��@�hs@�O�@�G�@��@�%@���@��`@��@� �@��w@�;d@���@�V@�M�@�-@�$�@��@��T@���@�p�@�G�@��@��/@��j@���@��D@��@�r�@�j@�A�@�;@�P@;d@;d@+@�@
=@~��@~ff@~@}��@}�@}�@|�@x�`@xb@w�w@w�@v��@u�@u/@t��@s�
@s@r~�@q�^@q&�@p��@p�9@o��@ol�@o
=@n��@n$�@m`B@l�/@l9X@k��@k@j��@j�\@jJ@iX@h��@h�9@h��@hbN@hb@h  @h  @g��@g�@g|�@g�@f�+@e�h@d�@c�m@b�@b��@bM�@aX@`��@`r�@`bN@`A�@_\)@_+@^��@^5?@^5?@^$�@^@]��@]��@]��@]p�@]`B@]?}@\�@\�@\Z@\1@[�
@[��@Z�@Z�\@Z�@YG�@X �@WK�@V��@V��@VE�@U�T@U��@U`B@U?}@T�/@T��@TZ@T(�@T�@S��@S�
@SC�@Q�7@Q7L@P��@P�9@P�u@P�u@P�u@PQ�@P1'@P1'@PA�@PA�@PA�@P1'@O��@O�P@Ol�@OK�@N�R@M��@M�-@M�@MO�@L�/@L�j@Lz�@LZ@L1@KS�@Ko@J�@J��@I��@IG�@H�@HbN@HQ�@HA�@HA�@HA�@H �@H  @G�@G\)@G�@F��@E�@E�h@EO�@D�j@D�@D��@Dz�@Dz�@Dj@C��@CdZ@CC�@B��@Bn�@Bn�@Bn�@Bn�@Bn�@BM�@A�^@Ahs@AX@A7L@@��@?�;@?K�@?�@?�@?
=@>��@>�@>ff@>{@=�T@=@=@=��@=��@=�@=O�@=/@<�@<��@<�j@<��@<z�@;��@;�@:��@9�@9��@9G�@9&�@9�@8��@8r�@8A�@8  @8  @7�@7��@7�@7�P@7+@6v�@5�@5�@5?}@4�/@4�D@4Z@3��@3�
@2��@2=q@2J@1�#@1��@1�@1%@0��@/�@/�P@/;d@.�@.�R@.�R@.�R@.v�@-��@-O�@-?}@-�@,�j@,�D@,j@,I�@,(�@+��@+��@+��@+��@+��@+�m@+�
@+�F@+�@+@*�!@*n�@*-@*-@)��@)�7@)X@(��@(bN@(bN@(bN@(A�@( �@'�;@'|�@'�@&v�@%�T@%`B@%�@$�j@#��@#"�@"��@"�!@"��@"��@"�\@"~�@"^5@"=q@!�@!hs@!7L@!7L@!7L@ ��@ ��@ ��@ bN@�@+@�y@ȴ@��@�+@ff@��@V@(�@�
@�F@��@��@��@�@dZ@�@��@~�@�@��@��@�7@hs@G�@7L@�@�@��@�u@�@bN@1'@  @�;@�w@�@�P@|�@\)@K�@;d@�@
=@��@�@�R@ff@$�@@V@�D@I�@�@��@�m@ƨ@S�@o@@�H@��@��@=q@x�@x�@hs@x�@hs@hs@G�@7L@%@��@��@Ĝ@�u@A�@ �@  @�;@�w@�P@l�@K�@
=@�R@v�@ff@5?@{@{@{@{@{@�@�@�@V@V@��@��@(�@ƨ@��@S�@33@o@o@
�H@
��@
��@
~�@
~�@
^5@
^5@
J@	��@	�@	�#@	�^@	hs@	X@	&�@	%@Ĝ@�@Q�@Q�@A�@b@�@�w@\)@+A�Q�A�Q�A�Q�A�O�A�S�A�Q�A�jA�l�A�jA�jA�hsA�bNA�`BA�^5A�^5A�bNA�^5A�S�A�K�A�I�A�M�A�O�A�XA�M�A�\)A�O�A�ZA�\)A�ZA�^5A�M�A�I�A�E�A�bA�A�%A���A��A��HA���AѰ!AѼjA�n�A�Q�A�O�A�I�A�5?AЏ\AЋDAЉ7AЉ7AЃA�E�A� �A��`AϬA�O�A��yA�t�A�\)A��A���A���A̺^A̴9A̮Ḁ�A̝�A̗�ȂhA̍PA̋DȦ+A�~�A�z�A�v�A�r�A�l�A�ffA�hsA�hsA�hsA�ffA�`BA�^5A�Q�A�O�A�C�A�9XA�+A�$�A� �A��A�  A��A��
A˧�A�`BA��Aʩ�A�l�A�O�A�A�A�33A�1'A�(�A�JA���A��A��yA��HA��A�ȴAɴ9Aɝ�A�v�A�ffA�p�A�p�A�n�A�l�A�l�A�n�A�l�A�n�A�jA�hsA�dZA�`BA�\)A�XA�S�A�I�A�?}A�5?A�1'A�(�A�"�A��A��A�
=A�A�  A�  A���A��AȾwAȇ+A�^5A�C�A�5?A�(�A�A�ƨAǩ�AǅA�hsA�VA�M�A�G�A�A�A�;dA�7LA�/A�(�A� �A��A�{A�VA�VA�
=A�A���A��A��A��yA��yA��A��;A��/A��
A���A�ĜA���AƸRAƶFAƲ-AƲ-AƶFAƲ-AƮAƧ�Aư!AƸRA��A�(�A�Q�A�z�AǑhAǮAǺ^Aǲ-AǺ^A��#A��yA��mA��mA��mA��`A��mA��TA��;A��#A��
A���A��/A��HA��;A��;A��A��A��A��yA��`A��HA��HA��;A��/A��
AǼjAǁA�K�A�A��#A��A��HA���A��
AƝ�A�;dA� �A�VA���A���A���Aź^Aš�Aŝ�AœuA�~�A�x�A�dZA�I�A��A��mA���AċDA�\)A�K�A�C�A�^5A�VA�"�A��yA��#A��A���A�Aá�A�|�A�jA�5?A� �A��HA�dZA�A���A�VA���A�ƨA�ȴA���A���A���A�ȴA�A��A��A��!A��!A��9A��-A��A��!A���A��wA��wA���A��jA��7A�dZA�\)A�K�A�E�A�C�A�7LA�/A�&�A�$�A�{A��;A��A�^5A���A�&�A���A���A��DA��A��+A��A�t�A�O�A�I�A�=qA�$�A���A��TA��jA���A�bNA�XA�Q�A�K�A�E�A�C�A�A�A�"�A���A��
A���A�ȴA���A�r�A�O�A�oA���A��A��yA��mA��;A���A���A�|�A�p�A�ffA�&�A��A��!A��PA�z�A�z�A�v�A�dZA�XA�Q�A�O�A�I�A�?}A�E�A�A�A�C�A�5?A�$�A�A���A��A��RA��A�E�A��A�1A�%A�A�A�  A���A��A��;A��
A���A��jA��DA�E�A�1A��HA��7A�VA�/A�$�A��A�{A�A��A��#A���A��9A���A���A��uA��DA�r�A�`BA�I�A�9XA� �A�{A���A��mA��/A���A��jA���A���A���A�~�A�XA�33A�1'A��A���A��yA��mA��`A���A���A���A��+A�t�A�I�A�1'A��A���A�M�A��/A�A��RA��!A��-A��A���A���A���A���A��hA��DA��A�t�A�Q�A�{A���A�`BA�M�A�1'A�bA��;A�ƨA���A��A�l�A�\)A�C�A�-A� �A�%A��yA��A��FA���A���A�dZA�M�A�33A��A���A��uA�n�A��yA�dZA�?}A�&�A��mA��9A��+A�bNA�/A�VA���A��/A���A���A���A��A��!A��A���A���A�r�A�33A�1A��mA���A���A�jA�9XA��A�VA�A��mA��TA��;A���A���A�ȴA���A���A���A���A��A�z�A�~�A�|�A�r�A�l�A�dZA�A�A�7LA�/A� �A��A��7A�C�A��^A�`BA��HA���A�`BA�A�A��A��A��A��!A�n�A�E�A��A��
A�l�A�Q�A�A�A�5?A�/A��A��/A���A���A�~�A���A�A�A� �A��A��A�oA�  A��mA�ƨA���A�p�A�=qA��A��-A��A�I�A�?}A�5?A��A�bA�VA�oA�bA��;A�dZA�hsA�hsA�33A��A��A��A��jA��^A��7A��A�z�A�~�A�jA�dZA�jA�jA�ffA�\)A�O�A�A�A�?}A�A�A�?}A�9XA�1'A�/A�9XA��A�JA�JA�1A�A�{A�dZA���A���A���A�hsA�S�A�;dA�(�A��A�oA�A��`A�ȴA���A��A�p�A�^5A�?}A�+A��A��A�VA�
=A���A��yA�ƨA�x�A� �A�ĜA��A�hsA��-A�  A��yA��RA���A��\A��DA��A�VA�S�A�=qA�bA�JA�1A�
=A�  A�
=A�A��A���A���A��uA�|�A��A���A�;dA���A���A��A�O�A��A�ZA��A��#A���A�n�A�^5A�VA�Q�A�5?A��A���A�^5A��A�ȴA�\)A���A� �A��HA��+A�/A���A���A��^A���A���A��+A�r�A�M�A�-A���A��yA���A���A�bNA�O�A�=qA�$�A�bA��A�x�A�/A�bA���A��A��yA���A��9A��!A���A��uA��A�x�A�`BA�O�A�9XA��A���A��A�r�A�$�A��wA�7LA�dZA���A�n�A�(�A��;A��A���A�ffA�G�A�G�A�G�A�?}A�/A��A�VA��A?}A~�A~{A}K�A{7LAu��AtJAsƨAs�-As�Ast�AsXAs"�Ar�HAr��Ar^5Ar=qAr{Aq��Aqt�Aq�Ap��Apv�ApffApM�Ap1Ao�Ao�An��An=qAm�Al��Akt�Ai�mAi;dAh(�Af�Ad��Ac��Acp�Ac+Ac%Ab�HAb�\Aa��Aa�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                         A�Q�A�XA�jA�bNA�`BA�ZA�M�A�S�A�VA�XA�VA�"�A��
A�33A�t�A�M�A���A̍PA�l�A�G�A���A�S�A��`A�v�A�jA�E�A�bA�~�AǇ+A�-A�A��#Aƴ9A�&�A���A��/A��mAǙ�Aư!A���A�=qA�ZA�A�A���A��9A��RA�z�A�$�A��^A�jA�n�A�(�A��A�+A��A���A���A��A�^5A���A��A�O�A�A���A�oA�oA�t�A�5?A��A�I�A�$�A�5?A��#A�%A��9A���A�ĜA�jA�=qA��+A�\)A���A�{A�I�A���A�{A���A�z�A��DA�$�A�VA�S�A�dZA�;dA��uA��RA��A�G�A}�As��Ar=qAp9XAk�;AdJAap�A_�PA]�wAYXAV�/AP�ANffAL�uAJVAGoAEVAB�AA7LA?�#A>��A>9XA=�PA<bA7�^A6��A6�A2��A1oA.��A-��A,��A+�A*ȴA)�hA'�wA&VA%�A$�A$��A$ȴA$M�A#�A"^5A"bA!�A!�A!;dA ^5A!;dA Q�A�;A?}A��AAp�A+A��A��A��A��AI�AoA��A�A;dA�7A�A��A�A�uA��AVA
�HA	33A�
A�7AA��A�/A�Al�Ar�A  Ap�A �yAXA �j@�~�@�$�A�AQ�A��A��AȴA33AA �9@��@��\@�M�@��
A�A33A��A�#AbA �A �A(�A(�A�AƨAM�A^5A5?AG�A�A�7At�A�PA�hA��A|�A33A ȴA A�@���@�ȴ@�@�hs@�&�@���@�ȴ@�J@���@�o@�V@��u@�P@�"�@�v�@�O�@��@��@���@��`@�j@�ƨ@�R@�5?@�{@���@�x�@�7L@�ȴ@�$�@��@�x�@�G�@�/@���@�u@��;@�l�@旍@�-@��@���@�7L@�A�@��
@�P@�;d@���@�%@��m@�\)@ޏ\@���@�@ݩ�@݉7@�`B@�`B@�?}@�l�@�^5@��@ى7@�&�@��/@�Ĝ@�Q�@�o@�^5@�5?@պ^@�Ĝ@��;@Ӆ@�o@ҟ�@��@�/@Гu@Ͼw@�o@�E�@ͺ^@�hs@��@��/@̴9@ˮ@�~�@�J@�hs@�V@���@��@���@���@ȼj@ȴ9@�z�@���@�K�@Ɵ�@�J@ŉ7@��@���@�Q�@�1@å�@�K�@��@°!@�n�@�$�@��7@��`@�1'@�ƨ@��@��@�v�@�=q@���@���@�x�@��`@��@�Q�@���@�\)@�33@��R@�V@�{@�O�@��@��@��@��@�K�@���@��!@�{@�x�@�/@��/@�j@��@���@��!@��T@���@�7L@��@� �@���@��m@�ƨ@��@��@�\)@�33@�~�@���@�7L@��9@���@��@�C�@��@�M�@���@�X@��`@��u@�r�@�bN@�9X@��m@��P@�\)@�o@���@�V@�&�@���@��u@�r�@���@�o@���@�5?@���@�&�@��@���@�Z@�(�@�1@��m@���@�ƨ@���@�S�@���@��\@�M�@�@��/@��@���@��@�9X@��@��P@�o@�ff@�J@���@��7@�hs@�X@��@�%@���@��D@� �@��@��@��
@���@��w@���@��P@�|�@�l�@�l�@�\)@�C�@��y@���@��\@�v�@�M�@��@�7L@���@��j@���@��@�I�@�9X@�b@�  @��m@��m@��;@��w@���@�|�@�l�@�\)@�C�@�@��H@�ȴ@��R@��\@�n�@�M�@�5?@��@���@��@���@�O�@��j@��m@�|�@�C�@��@���@�M�@��@�@��T@��T@��T@��#@��h@��@�A�@�  @�ƨ@��F@�t�@���@��y@�ȴ@�V@�@��h@�G�@�&�@��@�%@�Ĝ@�Q�@�(�@�1@���@�+@��!@�ff@��T@��h@��@�hs@�O�@�G�@��@�%@���@��`@��@� �@��w@�;d@���@�V@�M�@�-@�$�@��@��T@���@�p�@�G�@��@��/@��j@���@��D@��@�r�@�j@�A�@�;@�P@;d@;d@+@�@
=@~��@~ff@~@}��@}�@}�@|�@x�`@xb@w�w@w�@v��@u�@u/@t��@s�
@s@r~�@q�^@q&�@p��@p�9@o��@ol�@o
=@n��@n$�@m`B@l�/@l9X@k��@k@j��@j�\@jJ@iX@h��@h�9@h��@hbN@hb@h  @h  @g��@g�@g|�@g�@f�+@e�h@d�@c�m@b�@b��@bM�@aX@`��@`r�@`bN@`A�@_\)@_+@^��@^5?@^5?@^$�@^@]��@]��@]��@]p�@]`B@]?}@\�@\�@\Z@\1@[�
@[��@Z�@Z�\@Z�@YG�@X �@WK�@V��@V��@VE�@U�T@U��@U`B@U?}@T�/@T��@TZ@T(�@T�@S��@S�
@SC�@Q�7@Q7L@P��@P�9@P�u@P�u@P�u@PQ�@P1'@P1'@PA�@PA�@PA�@P1'@O��@O�P@Ol�@OK�@N�R@M��@M�-@M�@MO�@L�/@L�j@Lz�@LZ@L1@KS�@Ko@J�@J��@I��@IG�@H�@HbN@HQ�@HA�@HA�@HA�@H �@H  @G�@G\)@G�@F��@E�@E�h@EO�@D�j@D�@D��@Dz�@Dz�@Dj@C��@CdZ@CC�@B��@Bn�@Bn�@Bn�@Bn�@Bn�@BM�@A�^@Ahs@AX@A7L@@��@?�;@?K�@?�@?�@?
=@>��@>�@>ff@>{@=�T@=@=@=��@=��@=�@=O�@=/@<�@<��@<�j@<��@<z�@;��@;�@:��@9�@9��@9G�@9&�@9�@8��@8r�@8A�@8  @8  @7�@7��@7�@7�P@7+@6v�@5�@5�@5?}@4�/@4�D@4Z@3��@3�
@2��@2=q@2J@1�#@1��@1�@1%@0��@/�@/�P@/;d@.�@.�R@.�R@.�R@.v�@-��@-O�@-?}@-�@,�j@,�D@,j@,I�@,(�@+��@+��@+��@+��@+��@+�m@+�
@+�F@+�@+@*�!@*n�@*-@*-@)��@)�7@)X@(��@(bN@(bN@(bN@(A�@( �@'�;@'|�@'�@&v�@%�T@%`B@%�@$�j@#��@#"�@"��@"�!@"��@"��@"�\@"~�@"^5@"=q@!�@!hs@!7L@!7L@!7L@ ��@ ��@ ��@ bN@�@+@�y@ȴ@��@�+@ff@��@V@(�@�
@�F@��@��@��@�@dZ@�@��@~�@�@��@��@�7@hs@G�@7L@�@�@��@�u@�@bN@1'@  @�;@�w@�@�P@|�@\)@K�@;d@�@
=@��@�@�R@ff@$�@@V@�D@I�@�@��@�m@ƨ@S�@o@@�H@��@��@=q@x�@x�@hs@x�@hs@hs@G�@7L@%@��@��@Ĝ@�u@A�@ �@  @�;@�w@�P@l�@K�@
=@�R@v�@ff@5?@{@{@{@{@{@�@�@�@V@V@��@��@(�@ƨ@��@S�@33@o@o@
�H@
��@
��@
~�@
~�@
^5@
^5@
J@	��@	�@	�#@	�^@	hs@	X@	&�@	%@Ĝ@�@Q�@Q�@A�@b@�@�w@\)G�O�A�Q�A�Q�A�Q�A�O�A�S�A�Q�A�jA�l�A�jA�jA�hsA�bNA�`BA�^5A�^5A�bNA�^5A�S�A�K�A�I�A�M�A�O�A�XA�M�A�\)A�O�A�ZA�\)A�ZA�^5A�M�A�I�A�E�A�bA�A�%A���A��A��HA���AѰ!AѼjA�n�A�Q�A�O�A�I�A�5?AЏ\AЋDAЉ7AЉ7AЃA�E�A� �A��`AϬA�O�A��yA�t�A�\)A��A���A���A̺^A̴9A̮Ḁ�A̝�A̗�ȂhA̍PA̋DȦ+A�~�A�z�A�v�A�r�A�l�A�ffA�hsA�hsA�hsA�ffA�`BA�^5A�Q�A�O�A�C�A�9XA�+A�$�A� �A��A�  A��A��
A˧�A�`BA��Aʩ�A�l�A�O�A�A�A�33A�1'A�(�A�JA���A��A��yA��HA��A�ȴAɴ9Aɝ�A�v�A�ffA�p�A�p�A�n�A�l�A�l�A�n�A�l�A�n�A�jA�hsA�dZA�`BA�\)A�XA�S�A�I�A�?}A�5?A�1'A�(�A�"�A��A��A�
=A�A�  A�  A���A��AȾwAȇ+A�^5A�C�A�5?A�(�A�A�ƨAǩ�AǅA�hsA�VA�M�A�G�A�A�A�;dA�7LA�/A�(�A� �A��A�{A�VA�VA�
=A�A���A��A��A��yA��yA��A��;A��/A��
A���A�ĜA���AƸRAƶFAƲ-AƲ-AƶFAƲ-AƮAƧ�Aư!AƸRA��A�(�A�Q�A�z�AǑhAǮAǺ^Aǲ-AǺ^A��#A��yA��mA��mA��mA��`A��mA��TA��;A��#A��
A���A��/A��HA��;A��;A��A��A��A��yA��`A��HA��HA��;A��/A��
AǼjAǁA�K�A�A��#A��A��HA���A��
AƝ�A�;dA� �A�VA���A���A���Aź^Aš�Aŝ�AœuA�~�A�x�A�dZA�I�A��A��mA���AċDA�\)A�K�A�C�A�^5A�VA�"�A��yA��#A��A���A�Aá�A�|�A�jA�5?A� �A��HA�dZA�A���A�VA���A�ƨA�ȴA���A���A���A�ȴA�A��A��A��!A��!A��9A��-A��A��!A���A��wA��wA���A��jA��7A�dZA�\)A�K�A�E�A�C�A�7LA�/A�&�A�$�A�{A��;A��A�^5A���A�&�A���A���A��DA��A��+A��A�t�A�O�A�I�A�=qA�$�A���A��TA��jA���A�bNA�XA�Q�A�K�A�E�A�C�A�A�A�"�A���A��
A���A�ȴA���A�r�A�O�A�oA���A��A��yA��mA��;A���A���A�|�A�p�A�ffA�&�A��A��!A��PA�z�A�z�A�v�A�dZA�XA�Q�A�O�A�I�A�?}A�E�A�A�A�C�A�5?A�$�A�A���A��A��RA��A�E�A��A�1A�%A�A�A�  A���A��A��;A��
A���A��jA��DA�E�A�1A��HA��7A�VA�/A�$�A��A�{A�A��A��#A���A��9A���A���A��uA��DA�r�A�`BA�I�A�9XA� �A�{A���A��mA��/A���A��jA���A���A���A�~�A�XA�33A�1'A��A���A��yA��mA��`A���A���A���A��+A�t�A�I�A�1'A��A���A�M�A��/A�A��RA��!A��-A��A���A���A���A���A��hA��DA��A�t�A�Q�A�{A���A�`BA�M�A�1'A�bA��;A�ƨA���A��A�l�A�\)A�C�A�-A� �A�%A��yA��A��FA���A���A�dZA�M�A�33A��A���A��uA�n�A��yA�dZA�?}A�&�A��mA��9A��+A�bNA�/A�VA���A��/A���A���A���A��A��!A��A���A���A�r�A�33A�1A��mA���A���A�jA�9XA��A�VA�A��mA��TA��;A���A���A�ȴA���A���A���A���A��A�z�A�~�A�|�A�r�A�l�A�dZA�A�A�7LA�/A� �A��A��7A�C�A��^A�`BA��HA���A�`BA�A�A��A��A��A��!A�n�A�E�A��A��
A�l�A�Q�A�A�A�5?A�/A��A��/A���A���A�~�A���A�A�A� �A��A��A�oA�  A��mA�ƨA���A�p�A�=qA��A��-A��A�I�A�?}A�5?A��A�bA�VA�oA�bA��;A�dZA�hsA�hsA�33A��A��A��A��jA��^A��7A��A�z�A�~�A�jA�dZA�jA�jA�ffA�\)A�O�A�A�A�?}A�A�A�?}A�9XA�1'A�/A�9XA��A�JA�JA�1A�A�{A�dZA���A���A���A�hsA�S�A�;dA�(�A��A�oA�A��`A�ȴA���A��A�p�A�^5A�?}A�+A��A��A�VA�
=A���A��yA�ƨA�x�A� �A�ĜA��A�hsA��-A�  A��yA��RA���A��\A��DA��A�VA�S�A�=qA�bA�JA�1A�
=A�  A�
=A�A��A���A���A��uA�|�A��A���A�;dA���A���A��A�O�A��A�ZA��A��#A���A�n�A�^5A�VA�Q�A�5?A��A���A�^5A��A�ȴA�\)A���A� �A��HA��+A�/A���A���A��^A���A���A��+A�r�A�M�A�-A���A��yA���A���A�bNA�O�A�=qA�$�A�bA��A�x�A�/A�bA���A��A��yA���A��9A��!A���A��uA��A�x�A�`BA�O�A�9XA��A���A��A�r�A�$�A��wA�7LA�dZA���A�n�A�(�A��;A��A���A�ffA�G�A�G�A�G�A�?}A�/A��A�VA��A?}A~�A~{A}K�A{7LAu��AtJAsƨAs�-As�Ast�AsXAs"�Ar�HAr��Ar^5Ar=qAr{Aq��Aqt�Aq�Ap��Apv�ApffApM�Ap1Ao�Ao�An��An=qAm�Al��Akt�Ai�mAi;dAh(�Af�Ad��Ac��Acp�Ac+Ac%Ab�HAb�\Aa��Aa�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	_B	_B	�B	_B		B	_B	�B	_B	YB	+B	�B	xB	�B	 'B	7�B	N<B	GzB	E�B	D3B	B�B	:�B	6FB	=<B	@�B	A�B	AUB	D�B	J�B	LdB	OBB	S�B	X�B	�7B
	7B
 �B
P}B
�nB#nB8RB?BC-BL0BA�BM�BYB[�Bc BbNBx8Bp;Bf2BpoBqABrGB�GB��B��B�[B�EB�
B��B�B��B�8B�vB��B�HB�"B��B��B�&B��B��B��B�GB� By	Bj�Ba|B^jB:�B-�B�BSB
�B
�NB
�;B
�B
�B
�xB
�B
jB
^�B
P�B
B�B
8�B
#B
�B
�B	��B	��B	��B	kB	2-B	VB�xB�iB��BٴB�mB�<B�9B�B��B��B��B��B��B��B�tB��B��B�$B�'B��B�kB��B��B�$B�-B�FB�wB��B�wBƨBҽBרB��B��B�rB��B��B��B�B�B�]B�(B	@B		B	�B	)�B	$�B	!�B	�B	�B	�B	xB	!bB	%�B	 \B	�B	MB	B	\B	�B	(XB	�B	fB��B��B�B��B͟B�UB��B��B��BچBбB��B��B�BBɆBچB�BںB�B	AB	�B	IB	IB	 �B	-wB	2aB	'B	�B	 �B	!�B	/�B	H�B	MB	R B	��B	�"B	��B	��B	�bB	��B	�bB	��B	��B	�:B	��B	��B	�qB	�CB	��B	�4B	�B	��B	�zB	��B	��B	��B	�B	��B	��B	��B	��B	�_B	��B	�RB	�$B	��B	��B	�XB	��B	��B	��B	��B	��B	�B	�nB	��B	�RB	�$B	�XB	��B	��B	�XB	��B	�OB	�HB	�jB	��B	�B	��B	�B	�6B	�6B	��B	�B	�B	��B	��B	��B	��B	�aB	��B	ÖB	�gB	�3B	ƨB	�aB	��B	ŢB	�B	�3B	�gB	��B	ƨB	�KB	�B	�RB	�B	�EB	��B	��B	�KB	�B	��B	�XB	��B	�#B	�RB	ǮB	ȴB	�)B	�B	�pB	�B	ϫB	��B	�<B	�B	�vB	�pB	�B	ϫB	�vB	�BB	��B	�NB	�B	҉B	ӏB	�[B	ӏB	��B	ӏB	�,B	��B	��B	�
B	�9B	�QB	ٴB	��B	�QB	�]B	��B	�5B	�;B	ߤB	ߤB	�pB	�pB	�B	��B	�B	�B	��B	�vB	�HB	�B	�TB	�B	� B	� B	�B	�NB	��B	��B	�B	� B	� B	��B	��B	�TB	�ZB	��B	�&B	��B	�B	�`B	��B	�B	�B	��B	�`B	��B	�B	�B	�WB	��B	�B	�B	��B	��B	�"B	�B	�)B	��B	�WB	�WB	�B	��B	�B	�cB	�B	�B	��B	�B	�GB	�B	�B	�B	�B	��B	�B	��B	��B	��B	�`B	��B	�`B	�+B	��B	��B	�>B	�DB	�B	��B	�B	��B	��B	��B	�(B	�"B	��B	��B	�]B	��B	�cB	��B	��B	�.B	��B
;B
 �B
 4B
uB
�B
�B
uB
�B
�B
B
AB
�B
�B
SB
�B
�B
�B
�B
�B
�B
�B
�B
	B
	7B
�B
	7B
�B
	7B
	�B
	7B
	lB
	�B
	7B
	lB
	�B
B

=B

rB

rB

	B
�B
�B
�B
JB
�B
�B
PB
B
�B
�B
�B
�B
�B
VB
�B
�B
�B
VB
�B
�B
\B
�B
�B
�B
�B
�B
.B
 B
 B
�B
 B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
$B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
1B
_B
�B
�B
�B
1B
1B
1B
�B
�B
�B
�B
�B
B
B
kB
�B
kB
�B
�B
�B
B
B
B
CB
CB
B
�B
�B
IB
~B
�B
�B
�B
�B
�B
�B
�B
 'B
 �B
 �B
 �B
!bB
!�B
!�B
!�B
"4B
"4B
"4B
"4B
"�B
#B
#nB
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%FB
%FB
&LB
+�B
+kB
+�B
,=B
,�B
-wB
.B
.}B
/�B
0UB
1'B
1�B
2�B
2�B
2�B
4B
49B
4�B
5B
5�B
6zB
7B
7�B
8B
8RB
8RB
8RB
8�B
8�B
8RB
8RB
8RB
8�B
8RB
8RB
8B
8B
8B
8B
8B
8RB
8�B
8�B
9�B
:*B
:*B
:�B
;�B
<6B
<B
<B
;�B
=<B
<�B
=<B
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>B
>wB
>�B
>wB
>�B
>�B
?HB
?}B
?�B
@�B
A�B
A�B
B[B
B�B
C-B
C�B
C�B
C�B
D3B
D�B
D�B
EB
EB
EB
EmB
D�B
F?B
H�B
H�B
H�B
IRB
I�B
IRB
I�B
J#B
I�B
J#B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
LdB
L�B
M6B
MB
MB
L�B
L�B
M�B
M6B
MjB
M6B
N�B
OB
OvB
OBB
OBB
OBB
OvB
OB
OBB
OBB
OvB
O�B
O�B
P}B
QB
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S&B
S[B
S[B
S&B
S&B
S�B
T,B
T,B
S�B
T,B
T,B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VmB
VB
V�B
V9B
V9B
V�B
V9B
VmB
VmB
V�B
V�B
V�B
V�B
V�B
V�B
WsB
WsB
XB
X�B
X�B
YB
YKB
YB
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
Z�B
[#B
[WB
[�B
[�B
\)B
\]B
\]B
\�B
\�B
]�B
]�B
^B
^B
^jB
^�B
^�B
_B
_�B
_�B
`B
`BB
`BB
`BB
`B
`BB
a|B
aHB
aHB
aHB
a�B
a�B
a�B
bB
bB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
b�B
bNB
c B
cTB
cTB
c�B
cTB
c�B
d&B
c�B
e,B
e,B
d�B
d�B
d�B
e,B
e,B
e`B
e�B
ffB
g8B
g8B
g8B
gmB
h�B
iB
iyB
iyB
iyB
i�B
iyB
i�B
i�B
i�B
i�B
jB
jKB
jB
jKB
j�B
jB
j�B
j�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
m)B
ncB
ncB
ncB
ncB
n�B
ncB
ncB
n�B
o5B
o5B
oiB
p;B
p�B
p�B
poB
p�B
p�B
p�B
qB
p�B
p�B
qAB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
rB
rB
rGB
rGB
rGB
r|B
r|B
r�B
r|B
r�B
s�B
s�B
tB
tB
tB
tB
tTB
t�B
t�B
u%B
u%B
uZB
u%B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
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
xB
x8B
xlB
x8B
x�B
x�B
y	B
yrB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{JB
{�B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
~]B
~]B
~]B
~]B
~�B
~�B
~�B
cB
cB
�B
� B
�4B
� B
�4B
��B
��B
��B
�oB
��B	�B	�B	�B	�B	�B	�B	oB	�B	�B	�B	_B	fB	�B	fB	fB	�B	�B	B	�B		7B	+B	fB	YB	_B	�B		B	B	�B	�B	B	fB	�B	�B	�B	
�B	�B	B		lB	VB	B	DB	\B	#�B	4B	4B	�B	�B	;0B	OB	�B	CB	qB	'RB	"hB	/�B	-wB	:*B	@OB	MjB	~�B	M�B	L0B	L0B	I�B	J�B	IRB	HB	G�B	GzB	F�B	F?B	FB	F�B	GzB	GzB	G�B	F�B	FB	FtB	D�B	C�B	D3B	C�B	D�B	C�B	EB	D3B	F?B	D3B	C�B	A�B	A�B	B�B	B�B	@B	A�B	D3B	GB	C�B	F�B	=�B	8B	6�B	7�B	5tB	5tB	9�B	6zB	5�B	6zB	6B	5tB	6�B	7B	7�B	=<B	9�B	<jB	>�B	@�B	@�B	@�B	@�B	A�B	@�B	A B	@�B	A B	@�B	@�B	@�B	@�B	B'B	A�B	@B	@�B	@�B	@�B	@�B	AUB	D�B	A�B	AUB	@OB	?�B	@OB	I�B	G�B	F�B	EmB	C�B	DgB	J�B	IB	H�B	K^B	K�B	K)B	I�B	JXB	JXB	JXB	K�B	L�B	M�B	M�B	MjB	N<B	MB	M6B	M�B	N�B	PHB	Q�B	QNB	P�B	O�B	R�B	S�B	R�B	U�B	UgB	V�B	V�B	XEB	W
B	W?B	W?B	W?B	XB	Y�B	a|B	g�B	r�B	{B	�!B	��B	��B	�dB	҉B	�sB	�B	�B	�	B
IB
~B
�B
�B
�B
VB
IB
�B
B
qB
B
'�B
,�B
-B
,�B
7�B
J�B
W�B
iDB
m]B
o�B
tB
}�B
��B
�\B
��B
�9BPB�B�B
rB@BVB(�B;dB?}B5?B3�B3hB2-B2aB@OB>�B=B=B=�B9$B<�B<6B?BE�BK�BG�B=<B9$B7�BHBK)BT�BK)BGzBH�BK�BMjBR�BL�BK�BN�BEBD�BHB3hB4B/�BB[BO�BT�BVmBU�BVmBW�BY�BZ�B[�BX�BXBWsBZ�BYBZ�B[#B`�B`�BS�Bc�Bk�BbNBc�Ba�B_�BbNBd&Bc�Ba|B^�BaBa�Bp�B��B�iB��Bu�Bk�Bk�Bm)Bn�Bu�Bt�BqABn/BoiBo�BuZBl"BiBbNBe`B_;B^jB`B`vBaBaHBl�Bm�Bm�BiDBp�BzBwfBr�BsMBm�BkQBlWBl"BncBo Bv`BrGBm�Bk�Bt�B�rBt�Bn�Bm�Bl�Bm]Br�Bo Bp;Bn�BoiBrBpBqBm�BpBtBw2Bq�BsMB}�B�AB�YB�SB�B��B��B�B��B�B�{B�B��B��B��B�=B��B��B�$B�B�kB�	B��B�B��B�B��B��B��B�@B��B��B��B�tB��B�B��B�wB�OB�[B�?B�B�zB��B��B�B� B��B�gB͟B��BɺB˒B��B��B�}B�vB֡BרB�EB�mB��B�]B��B�#B��B��B�MB��B�B�B�B�QB�B�"B�B�B�B�QB�B��B�GB�B�VB�rB�+B��B�B�`B�fB�2B�B�B�vB�/B��B�WB�B�oB�B��B��B�B�vB�B��B�,B�NB��B�vB�B�B��B��B�#B��B�OB��B��B��B��B��B�B��B�XB�0B��B��B�'B�3B�B�B�/B�]B��B�TB��B�AB�B�B�B��B�8B�B�B�B��B��B��B��B�&B�2B�,B��B�B�QB�KB�"B�cB�]B�]B��B�rB�B�B�WB��B��B��B�WB�2B�B�B�B�^B��B�aB�mB��B�XB��B�IB��B�_B��B��B��B�~B�'B�mB�	B��B�B��B�B�B�B�~B��B�1B��B�	B��B�iB�Bz�B{�BcB{JBy�B}�B|�B��B�"BzxB|�B�xB�{Bz�Br|B{�BtTB��Bk�BjBo�Bi�BjBiyBiDBjKBp;Bf2BcTBdZBbNBa|BbNBaB]dBW�Bd�BWsB\)BW�BZQBl"BNBT�BA�BA B9�B9$B8�B3hB5B2aB0�B2�B/�B0!B,�B'�B($B&�B#:B �B�B�BxBCB	BeB�B�B�B$�B�B�B
�xB
�B
�B
��B
�DB
��B
�B
�B
�,B
�,B
�B
��B
�B
��B
��B
یB
�B
� B
��B
��B
�yB
�]B
�B
ںB
��B
�dB
��B
�0B
��B
ŢB
�OB
�B
��B
��B
�$B
��B
��B
�B
��B
�7B
��B
�xB
��B
�B
�kB
�OB
�JB
��B
��B
{B
{�B
s�B
qAB
o B
l"B
k�B
kQB
k�B
gB
h>B
_B
aHB
f�B
^jB
\]B
Z�B
[WB
W�B
Y�B
aHB
R B
N�B
H�B
GEB
G�B
IRB
C�B
C-B
GEB
?HB
B�B
A B
D3B
:�B
9XB
6FB
=�B
?}B
2aB
1�B
7�B
.�B
1[B
)�B
xB
IB
B
"B
B
�B
JB

�B

	B
�B
xB
�B

rB
�B
�B
�B
�B
:B
$@B
$B	ҽB	�FB	�9B	�tB	�B	��B	��B	��B	��B	�B	��B	�nB	�-B	�$B	��B	��B	� B	�B	�	B	�JB	��B	yrB	��B	|PB	poB	�7B	xlB	ZQB	X�B	E�B	hsB	C�B	3�B	 'B	"�B	�B	7B	�B	 'B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                         B	KB	B	�B	�B	�B		RB	EB	�B	�B	EB		B	
�B	BB	OB	'�B	DMB	O�B	HfB	F�B	G_B	J�B	=<B	8�B	=�B	A�B	B�B	D�B	J	B	L�B	MjB	P.B	TaB	V�B	��B
�B
!B
S@B
��B&�B;�BD3BGBR�BKBN�BYB]/Be�Bo�B�Bv`Bn/B{�Bw2Bx�B��B�_B�tB��B�JB�vB�-B��B��B�vBڠBðB׍B�B�yB�rB�BοB��B�$B��B�B{0BlWBf�Bj�B?�B2GB+�B%FB
� B
�fB
�B
�B
��B
��B
�?B
poB
e�B
UgB
IlB
K^B
,�B
�B
5�B	ɺB	��B	�B	��B	;�B	B	�B�qB�B��BˬBÖB�B��B�DB��B��B�B��B�xB�8B��B�dB�QB�&B��B��B��B��B��B�tB��B��BɆB�{BʦB�[B�EB�BB��B��B	 �B��B��B�nB�0B	 �B�6B	SB	�B	 'B	,B	&�B	#�B	�B	]B	�B	B	%,B	*KB	$tB	�B	�B	$B	�B	�B	/�B	!HB	HB	B	  B�*B��B�:B��B�[B�oB˒B��B�BƨB�AB�'B�gBȀB��B��B�	B��B��B	MB	IB	�B	�B	.�B	9�B	+�B	 �B	 �B	pB	,"B	G�B	KB	KDB	��B	��B	��B	��B	�}B	�oB	��B	�B	��B	�B	�RB	�,B	��B	��B	��B	�4B	�B	�,B	��B	�0B	��B	�XB	��B	�fB	�nB	�tB	��B	�B	�*B	�yB	�kB	�RB	��B	�B	��B	�B	��B	�=B	�cB	��B	��B	��B	��B	��B	�DB	��B	��B	��B	��B	�MB	��B	��B	��B	�qB	�B	��B	�B	��B	��B	��B	��B	�[B	��B	��B	āB	�3B	ĜB	�MB	�mB	�zB	ȚB	āB	�mB	ƨB	�mB	�gB	ĶB	�%B	��B	��B	�dB	�DB	��B	�B	ȚB	�lB	ȚB	�B	�)B	˒B	�)B	�)B	�B	�RB	�lB	��B	��B	��B	уB	��B	�}B	ϑB	�vB	�}B	�(B	��B	��B	��B	�NB	�B	�:B	�TB	�&B	��B	�uB	��B	��B	��B	�FB	�MB	�
B	�B	׍B	�WB	ںB	ںB	��B	�IB	ݘB	��B	��B	�\B	�'B	��B	�B	�-B	� B	�B	��B	�|B	�B	�4B	�:B	�&B	�&B	�nB	�&B	�:B	��B	�@B	�FB	�&B	�B	��B	�B	�TB	�&B	�,B	��B	��B	�B	�,B	��B	��B	�B	�B	�fB	�2B	�B	�B	�_B	��B	�qB	�WB	��B	��B	�B	�WB	��B	�]B	�B	�B	��B	��B	�5B	��B	�iB	�[B	�B	�B	�vB	�hB	�B	�nB	�B	�%B	�?B	��B	�B	�ZB	�FB	��B	�`B	��B	�2B	�$B	��B	��B	��B	��B	��B	�B	��B	��B	�qB	��B	��B	��B	�.B	��B	�B	��B	��B	�cB	��B
 �B
B
oB
UB
B
B
�B
�B
GB
GB
�B
GB
%B
?B
�B
?B
B
�B
�B
�B
�B
B
�B
	RB
	7B
�B
	RB
	B
	lB
	�B
	RB
	�B
	�B
	lB
	�B

XB
�B

rB

�B

�B

�B
6B
jB
B
�B
B
B
�B
jB
�B
�B
�B
B
�B
�B
B
�B
�B
�B
vB
�B
�B
�B
�B
B
HB
 B
bB
4B
4B
B
B
�B
gB
�B
�B
B
mB
sB
�B
$B
�B
$B
�B
�B
$B
�B
�B
KB
+B
�B
1B
eB
eB
�B
�B
KB
�B
�B
B
eB
1B
eB
�B
B
B
�B
�B
WB
=B
qB
xB
B
B
CB
)B
]B
xB
]B
]B
�B
�B
B
�B
pB
 \B
�B
�B
 B
�B
 'B
 �B
!-B
!B
!bB
!�B
"B
"4B
"4B
"NB
"NB
"NB
"�B
#:B
#TB
#�B
#�B
#�B
#�B
#�B
#�B
$@B
%FB
%B
%,B
%�B
&fB
)DB
,�B
+�B
,=B
,�B
-]B
.IB
.�B
/OB
0UB
0�B
1�B
2|B
33B
2�B
3�B
4nB
4�B
5B
5�B
6�B
6�B
7�B
8RB
8�B
8�B
8�B
8�B
9>B
8�B
8�B
8lB
8�B
8�B
8lB
8RB
8RB
8RB
8RB
8�B
8�B
9>B
9>B
9�B
:�B
:xB
:�B
;B
<PB
<jB
<B
<6B
<�B
=qB
=VB
=�B
=�B
=�B
=�B
>B
=�B
=�B
>B
=�B
>B
>]B
>]B
>�B
>�B
>�B
>�B
?}B
?�B
@ B
@�B
A�B
BuB
BAB
B�B
C-B
C�B
C�B
C�B
C�B
D�B
D�B
EB
E9B
EB
E9B
E�B
EmB
G�B
H�B
H�B
IB
IlB
I�B
IRB
I�B
J=B
I�B
J#B
I�B
I�B
I�B
JXB
J�B
J�B
J�B
K)B
L�B
L�B
L�B
L�B
M6B
MjB
M6B
M6B
MB
MjB
M�B
MjB
M�B
N<B
O(B
O�B
O�B
O\B
O\B
OBB
OvB
OBB
OvB
O�B
O�B
O�B
PbB
QB
Q�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
SB
R�B
SB
S�B
S&B
S[B
S[B
S&B
S[B
TB
T{B
TFB
T,B
T{B
U2B
VSB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
V9B
V�B
V9B
VSB
V�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W�B
X+B
X�B
X�B
YKB
Y1B
YeB
YKB
Y�B
Y�B
ZB
Y�B
ZB
ZB
Z7B
ZQB
Z�B
[=B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
]�B
^OB
^B
^5B
^OB
^�B
^�B
_B
_�B
`B
_�B
`vB
`\B
`BB
`BB
`\B
aB
a�B
abB
a|B
a�B
bB
a�B
a�B
b4B
bNB
bNB
bNB
bNB
bNB
bhB
bhB
bhB
b�B
b�B
cnB
c�B
c�B
c�B
c�B
c�B
dZB
dtB
e�B
e,B
d�B
d�B
e,B
ezB
e�B
e�B
f�B
gB
g�B
g�B
g�B
h>B
i�B
i_B
i�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
jeB
j�B
jKB
jB
jB
j�B
j�B
kB
k6B
lB
l=B
lB
lB
lB
l"B
l�B
mCB
nB
n�B
n}B
n}B
n}B
n�B
n}B
n�B
o B
oiB
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
qAB
q[B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r-B
raB
raB
raB
r�B
r�B
r�B
r�B
s�B
t9B
t9B
tTB
t9B
t9B
tTB
t�B
u%B
uB
u?B
u?B
utB
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wB
v�B
wB
w2B
w�B
w�B
w�B
w�B
xB
xlB
x�B
xlB
x�B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
{B
z�B
z�B
z�B
{B
{�B
|B
|�B
|�B
|�B
}B
|�B
|�B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~wB
~wB
~wB
~�B
~�B
B
.B
}B
�B
�B
�4B
�4B
�B
�iB
��B
��B
�;B
��G�O�B	�B	�B	�B	�B	�B	�B	oB	�B	�B	�B	_B	fB	�B	fB	fB	�B	�B	B	�B		7B	+B	fB	YB	_B	�B		B	B	�B	�B	B	fB	�B	�B	�B	
�B	�B	B		lB	VB	B	DB	\B	#�B	4B	4B	�B	�B	;0B	OB	�B	CB	qB	'RB	"hB	/�B	-wB	:*B	@OB	MjB	~�B	M�B	L0B	L0B	I�B	J�B	IRB	HB	G�B	GzB	F�B	F?B	FB	F�B	GzB	GzB	G�B	F�B	FB	FtB	D�B	C�B	D3B	C�B	D�B	C�B	EB	D3B	F?B	D3B	C�B	A�B	A�B	B�B	B�B	@B	A�B	D3B	GB	C�B	F�B	=�B	8B	6�B	7�B	5tB	5tB	9�B	6zB	5�B	6zB	6B	5tB	6�B	7B	7�B	=<B	9�B	<jB	>�B	@�B	@�B	@�B	@�B	A�B	@�B	A B	@�B	A B	@�B	@�B	@�B	@�B	B'B	A�B	@B	@�B	@�B	@�B	@�B	AUB	D�B	A�B	AUB	@OB	?�B	@OB	I�B	G�B	F�B	EmB	C�B	DgB	J�B	IB	H�B	K^B	K�B	K)B	I�B	JXB	JXB	JXB	K�B	L�B	M�B	M�B	MjB	N<B	MB	M6B	M�B	N�B	PHB	Q�B	QNB	P�B	O�B	R�B	S�B	R�B	U�B	UgB	V�B	V�B	XEB	W
B	W?B	W?B	W?B	XB	Y�B	a|B	g�B	r�B	{B	�!B	��B	��B	�dB	҉B	�sB	�B	�B	�	B
IB
~B
�B
�B
�B
VB
IB
�B
B
qB
B
'�B
,�B
-B
,�B
7�B
J�B
W�B
iDB
m]B
o�B
tB
}�B
��B
�\B
��B
�9BPB�B�B
rB@BVB(�B;dB?}B5?B3�B3hB2-B2aB@OB>�B=B=B=�B9$B<�B<6B?BE�BK�BG�B=<B9$B7�BHBK)BT�BK)BGzBH�BK�BMjBR�BL�BK�BN�BEBD�BHB3hB4B/�BB[BO�BT�BVmBU�BVmBW�BY�BZ�B[�BX�BXBWsBZ�BYBZ�B[#B`�B`�BS�Bc�Bk�BbNBc�Ba�B_�BbNBd&Bc�Ba|B^�BaBa�Bp�B��B�iB��Bu�Bk�Bk�Bm)Bn�Bu�Bt�BqABn/BoiBo�BuZBl"BiBbNBe`B_;B^jB`B`vBaBaHBl�Bm�Bm�BiDBp�BzBwfBr�BsMBm�BkQBlWBl"BncBo Bv`BrGBm�Bk�Bt�B�rBt�Bn�Bm�Bl�Bm]Br�Bo Bp;Bn�BoiBrBpBqBm�BpBtBw2Bq�BsMB}�B�AB�YB�SB�B��B��B�B��B�B�{B�B��B��B��B�=B��B��B�$B�B�kB�	B��B�B��B�B��B��B��B�@B��B��B��B�tB��B�B��B�wB�OB�[B�?B�B�zB��B��B�B� B��B�gB͟B��BɺB˒B��B��B�}B�vB֡BרB�EB�mB��B�]B��B�#B��B��B�MB��B�B�B�B�QB�B�"B�B�B�B�QB�B��B�GB�B�VB�rB�+B��B�B�`B�fB�2B�B�B�vB�/B��B�WB�B�oB�B��B��B�B�vB�B��B�,B�NB��B�vB�B�B��B��B�#B��B�OB��B��B��B��B��B�B��B�XB�0B��B��B�'B�3B�B�B�/B�]B��B�TB��B�AB�B�B�B��B�8B�B�B�B��B��B��B��B�&B�2B�,B��B�B�QB�KB�"B�cB�]B�]B��B�rB�B�B�WB��B��B��B�WB�2B�B�B�B�^B��B�aB�mB��B�XB��B�IB��B�_B��B��B��B�~B�'B�mB�	B��B�B��B�B�B�B�~B��B�1B��B�	B��B�iB�Bz�B{�BcB{JBy�B}�B|�B��B�"BzxB|�B�xB�{Bz�Br|B{�BtTB��Bk�BjBo�Bi�BjBiyBiDBjKBp;Bf2BcTBdZBbNBa|BbNBaB]dBW�Bd�BWsB\)BW�BZQBl"BNBT�BA�BA B9�B9$B8�B3hB5B2aB0�B2�B/�B0!B,�B'�B($B&�B#:B �B�B�BxBCB	BeB�B�B�B$�B�B�B
�xB
�B
�B
��B
�DB
��B
�B
�B
�,B
�,B
�B
��B
�B
��B
��B
یB
�B
� B
��B
��B
�yB
�]B
�B
ںB
��B
�dB
��B
�0B
��B
ŢB
�OB
�B
��B
��B
�$B
��B
��B
�B
��B
�7B
��B
�xB
��B
�B
�kB
�OB
�JB
��B
��B
{B
{�B
s�B
qAB
o B
l"B
k�B
kQB
k�B
gB
h>B
_B
aHB
f�B
^jB
\]B
Z�B
[WB
W�B
Y�B
aHB
R B
N�B
H�B
GEB
G�B
IRB
C�B
C-B
GEB
?HB
B�B
A B
D3B
:�B
9XB
6FB
=�B
?}B
2aB
1�B
7�B
.�B
1[B
)�B
xB
IB
B
"B
B
�B
JB

�B

	B
�B
xB
�B

rB
�B
�B
�B
�B
:B
$@B
$B	ҽB	�FB	�9B	�tB	�B	��B	��B	��B	��B	�B	��B	�nB	�-B	�$B	��B	��B	� B	�B	�	B	�JB	��B	yrB	��B	|PB	poB	�7B	xlB	ZQB	X�B	E�B	hsB	C�B	3�B	 'B	"�B	�B	7B	�B	 'B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<^?�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<3D�<#�
<#�
<#�
<#�
<ik8<#�
<#�
<#�
<N�z<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1��<#�
<+�;<#�
<=6O<J<<#�
<#�
<#�
<#�
<]m5<T��<*�<HƠ<#�
<#�
<#�
<#�
<#�
<#�
<[�<#�
<#�
<T��<�s<#�
<#�
<a�<U�u<A��<��I<#�
<#�
<#�
<#�
<#�
<�5�<4�~<K�.<�Y+<#�
<#�
<��8<��<3��<#�
<#�
<se<6��<���<#�
<#�
<#�
<C�#<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<n��<#�
<#�
<F�4<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2018032404202120180324042021IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018040307015420180403070154QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018040307015420180403070154QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107544220190521075442IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                