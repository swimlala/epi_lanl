CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-08-29T22:30:11Z creation; 2023-04-26T19:24:27Z DMQC;      
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
_FillValue        G�O�     �  d�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � aP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � i8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180829223011  20230426192427  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7315_008643_023                 7315_008643_023                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�}x<��+@�}x<��+11  @�}xg8~@�}xg8~@/c�PH@/c�PH�dKq����dKq���11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�\)@�\@=p�@}p�@�  @�  @�  A ��AG�A ��A,(�A?\)A_\)A�Q�A�  A��A�  A�Q�A�  A�\)A�B (�B  B�
B�B�B'�B/�B8(�B@Q�BH(�BP  BW�
B`  Bh  Bo�
Bx  B�
B��
B��
B�  B�  B��
B��
B��
B�  B�{B�(�B�{B�  B��B��B�=qB��B��B�  B��B��B�  B�  B��B��B�  B��B��
B��
B�  B�{B�(�C 
=C  C
=C{C
=C	�C
=C  C��C
=C��C�C��C
=C  C�C��C"
=C$
=C&  C'��C)��C+�C-�C/��C1��C3��C6
=C8{C:
=C<  C=��C?�CB  CD�CF{CH
=CJ
=CK��CN  CP  CR
=CT
=CV{CX
=CZ  C\
=C^  C`  Ca��Cc��Ce��Ch  Cj{Cl
=Cm��Co��Cr  Ct
=Cu��Cx  Cz
=C{�C}��C�
=C�  C���C���C�  C�C�
=C�  C���C���C�  C�\C�C�C�C�  C�C�  C���C���C�  C�\C�
=C�  C�  C���C�  C�C�  C���C�C�C�  C���C�  C���C���C�C�C�  C���C�  C���C���C�C�C�  C�C�  C���C�  C�  C���C�  C�C�C�
=C�C���C�  C�C���C��C���C���C�  C�
=C�  C���C�  C���C�  C�  C�  C���C�  C�  C���C�C�  C�  C�  C�  C�  C�  C���C�C�
=C�  C���C�  C�
=C�  C���C���C���C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C�C�  C���C���C�C�\C�C���C�  C�  C�  C�  C���C���C�  C�  C�  C�  D   D ��D  D� D  D��DD�D�D��D�D� D�D�D�qDxRD��Dz�D�qD	}qD	��D
� D�D��D�D��D��DxRD�qD}qD�qD� D  D}qD�D� D�qD� D�qD��D�D� D�qD}qD�D��D  D�D�D}qD�qD� D�D� D��D}qD�D� D��D}qD  D��D  Dz�D   D ��D!�D!}qD"  D"� D#�D#��D#�qD$��D%  D%}qD&�D&� D'�D'� D'�qD(� D(�qD)� D*�D*}qD*�qD+� D,  D,}qD-�D-� D-�RD.}qD/  D/� D0�D0��D0�qD1}qD1�qD2}qD3  D3� D3�qD4xRD5�D5� D5�qD6}qD7  D7��D8D8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�RD=� D>D>��D?  D?��D@  D@}qDA  DA� DB  DB}qDB�qDC� DD�DD�DE  DEz�DE��DF}qDF�qDG��DH�DH� DH�qDI� DJ�DJ� DJ��DKz�DL  DL}qDM  DM��DN�DN��DO  DO� DP�DP� DP��DQ}qDR�DR�DS  DS}qDS�qDT}qDT�qDUz�DU�qDV� DV�qDW}qDW�qDX� DY�DY}qDY�qDZ}qDZ��D[� D\�D\�D]�D]� D]��D^� D_�D_��D`�D`� Da�Da��Db�Db� Db�qDc}qDd  Dd� DeDe��De�qDf� DgDg� Dg�qDh� Di�Di}qDi�RDj}qDk�Dk� Dk�qDlz�Dm  Dm��Dn  Dn� Do�Do� Dp  Dp� Dq  Dq� Dr  Drz�Dr��Ds}qDt�Dt��Du  Du� Du�qDvz�Dw  Dw}qDx  Dx� Dy�Dy� DzDz� D{�D{�D|�D|� D}  D}� D}��D~� D�D� D�qD�@ D��HD���D���D�>�D�~�D���D�  D�B�D��HD�� D��)D�=qD�� D��HD���D�=qD�}qD��qD���D�=qD��HD��HD�HD�B�D���D�� D��)D�=qD�� D�D��D�C�D���D�� D�HD�AHD�� D��qD���D�@ D�� D��HD��D�AHD��HD���D��qD�=qD�}qD��qD���D�AHD���D�D�  D�=qD�}qD���D�HD�AHD�� D�D�  D�<)D�~�D�D��D�@ D�� D�� D��)D�>�D�� D���D���D�@ D���D��HD�HD�AHD�� D���D���D�@ D���D��HD���D�AHD�� D��HD��D�>�D�� D�� D�  D�>�D�� D��HD��qD�=qD�}qD��)D��qD�>�D�~�D��qD��qD�>�D��HD�D��D�>�D�~�D�� D�  D�AHD��HD���D�HD�>�D�� D���D��D�@ D�|)D�� D�  D�@ D��HD�D�HD�@ D�~�D��qD���D�@ D�~�D��)D�  D�AHD�~�D���D��qD�AHD�� D�� D�  D�>�D��HD�D�  D�>�D�� D�D�  D�AHD���D���D���D�AHD�� D���D�HD�AHD��HD�D���D�>�D��HD�D�  D�>�D���D��HD��qD�@ D���D��HD���D�>�D�~�D��HD��D�@ D��HD���D�HD�@ D�}qD���D�  D�@ D�~�D���D��qD�@ D�~�D���D�HD�AHD���D���D���D�AHD���D�D�HD�@ D�� D���D�  D�=qD�� D��HD���D�@ D��HD�� D���D�>�D�~�D���D�HD�@ D�� D���D��qD�@ D�� D�� D�HD�AHD��HD�D�  D�>�D�~�D���D��D�@ D�� D�D���D�@ D�~�D��HD�  D�@ D��HD�� D�HD�AHD�~�D��HD�  D�B�D�~�D�� D��D�>�D�� D���D�  D�=qD D�� D�  D�AHD�~�D��HD�HD�>�D�~�DĽqD�  D�AHDŀ D��HD���D�@ DƂ�D��HD��qD�@ D�}qD�D�HD�B�D�~�D�� D�  D�AHDɀ D�� D���D�AHD�~�D��HD���D�@ Dˀ D�� D�  D�@ D̀ D�� D���D�@ D�~�D;�D���D�>�D�~�Dξ�D���D�>�Dπ DϾ�D���D�AHDЀ Dо�D���D�AHDр D��HD�  D�>�DҁHD��HD�HD�>�DӀ D��HD�HD�@ DԀ DԾ�D��D�AHDՀ D��HD��D�=qD�~�D��HD�HD�>�D�}qD�� D���D�>�D�~�Dؾ�D���D�@ DفHDپ�D��qD�>�DځHD�� D��qD�@ DہHD�� D���D�>�D܀ D��HD�  D�AHD݁HD�� D�HD�AHDށHD�D�  D�AHD߂�D��HD�  D�@ D��HD�� D���D�@ D�HD�� D��D�AHD�~�D�� D�  D�>�D�|)D�� D��qD�>�D�~�D��HD�HD�@ D�~�D徸D��qD�=qD� D�� D�  D�@ D� D��HD�  D�@ D�HD��HD�HD�@ D� D�D�HD�AHDꂏD��HD�  D�>�D� D�� D�  D�@ D�HD�� D�  D�C�D�HD��HD��qD�AHD�}qD��HD���D�@ DD�� D�HD�@ D�� D��HD���D�B�D�~�D�D�HD�@ D� D�� D�HD�B�D�HD��HD��)D�=qD� D�� D��qD�@ D���D�� D�HD�B�D���D��qD��qD�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D�~�D���D���D�=qD�~�D���D��D�(�?#�
?8Q�?u?�z�?�p�?�G�?�@
=q@�@�R@&ff@333@B�\@O\)@aG�@p��@}p�@��@��@��@�Q�@�p�@��\@��@�\)@�z�@�(�@��@���@��@�Q�@޸R@��
@���@��@�Q�AG�Az�A�A
�HA�RAG�A33A
=A�A\)A#33A'
=A*=qA-p�A0  A2�\A5A:=qA>�RAA�AE�AG�AJ=qAN{AQG�AU�AY��A^{AaG�Ae�AhQ�Ai��Amp�AqG�Au�Ax��A~{A���A��\A�(�A�{A�  A�G�A�=qA�z�A�ffA�  A��A�z�A��RA�Q�A�=qA��
A�{A��A���A��\A�z�A�\)A�G�A��HA���A��RA���A�=qA��
A�A�
=A�G�A�33A��A�
=A���A�33A�p�AǮAə�A˅A�p�A�\)A�G�A��HA�z�A�ffA�Q�A�=qA�(�A�ffA�Q�A�\A�z�A�ffA�Q�A�=qA�(�A�{A�Q�A�=qA���A�
=A���A�33A��A�
=B ��B��B�\B�Bz�Bp�B=qB\)Bz�B	G�B
�\B�B��B�B
=B(�B�B{B33B(�BG�B{B
=B  B�B=qB33BQ�BG�B�\B�B ��B!�B#
=B$  B%�B&{B&�HB(  B(��B*{B+33B,Q�B-G�B.ffB/�B0��B1�B333B4Q�B5p�B6ffB7�B8z�B9��B:�RB;�
B<��B=p�B>�\B?�B@��BA�BC
=BC�
BD��BF{BG\)BHQ�BIp�BJffBK�BL��BMBO
=BP(�BQG�BRffBS�BT��BUBV�HBX  BY�BZ{B[\)B\z�B^{B_33B`Q�Bap�Bb�\Bc�Bd��BeBg
=Bg�
Bi�Bj=qBk
=Bl  Bm�Bn�\Bo\)Bp(�BqG�BrffBs\)Bt��Bu��Bv�RBw�Bx��Bz{B{�B|z�B}��B~�RB�
B�ffB���B��B�{B��\B�33B��B�=qB���B�33B��B�(�B���B�G�B��B�ffB�
=B��B�{B���B�33B�B�Q�B���B�\)B��
B�z�B���B���B�{B���B�33B�B�(�B��RB�G�B�B�ffB���B��B�{B���B�33B��B�{B���B��B��B�=qB��RB�G�B��
B��\B�
=B���B�{B��RB�33B�B�=qB���B�G�B��B�=qB���B�33B�B�ffB��HB�G�B��B�ffB���B�p�B�{B��\B�\)B��B��\B�
=B���B�(�B���B�G�B�B�=qB���B�p�B�  B���B�33B�B�ffB���B��B�{B��RB�G�B�{B���B�G�B��B�z�B�
=B��B�=qB��RB�G�B��
B�Q�B��RB�
=B�\)B���B��
B�(�B�z�B��\B��RB��HB��B�G�B��B�B��B�{B�Q�B�z�B��RB�
=B�G�B���B��
B�{B�Q�B\B¸RB�
=B�33B�p�BÙ�B��
B�  B�(�B�Q�Bď\B���B�
=B�33BŅBŮB��B�(�BƏ\B��HB��B�\)BǙ�B��
B�{B�(�B�Q�Bȏ\B���B��B�G�BɅB�B�  B�=qB�ffBʣ�B���B�\)B˙�B��B�{B�Q�B�ffḄ�B��HB��B�\)BͮB�  B�Q�BθRB��HB�G�BυBϙ�B��B�=qB�ffB���B�G�Bљ�B��B�(�B�z�B���B��B�G�BӅB�B�(�B�ffBԸRB�
=B�p�B��
B�{B�z�BָRB��HB��B�p�B׮B�  B�Q�BظRB��B�p�B�B�{B�Q�B�z�BڸRB�
=B�\)Bۙ�B��B�(�B�z�B���B��B�\)B�B�{B�ffBޣ�B��B�p�B��
B�(�B�ffB���B��B�\)B�p�B��
B�(�B�z�B���B��B�p�B�  B�Q�B��B�
=B�G�B�p�B�B�(�B�ffB���B�p�B�B�{B�z�B�RB���B�G�B陚B�  B�Q�B���B�G�B�B�{B�z�B�RB���B�\)B��B�{B��B�
=B�p�B�B��B�ffB�RB�
=B�B�{B�z�B���B�G�B�B�  B�z�B���B�33B��
B�Q�B���B�33B�p�B��B�=qB���B�\)B��B�=qB��\B��HB�p�B�B�ffB��HB�G�B��
B�  B�z�B��HB�p�C 
=C =qC z�C ��C �
C
=CG�C��C�HC�C\)C�C�RC��C(�Cz�C��C
=CG�CffC�C��CG�C�CC�C�CffCC��C=qCffC��C�
C�Cp�C�C��C	{C	Q�C	��C	�C
(�C
p�C
��C
�
C{C\)C�C��C33C\)C��C�C33Cz�C�RC�HC(�C\)C�RC  C=qCffC�C�CG�C�C��C�C33C�\C��C{C=qCz�C�HC�CffC�C��C�Cp�C�RC�
C�C\)C�RC��C(�CffC��C  CG�CffC�C��C=qCz�C�C�CG�C�C�C�C33C�CC�HC�Cz�CC��C(�C\)CC  C�C\)C�RC  C�C\)C�RC  C�C\)C�RC   C �C Q�C ��C ��C!
=C!G�C!��C!�HC"  C"=qC"�\C"�
C"��C#33C#�\C#��C#�C$(�C$�C$C$�C%(�C%�C%C%�C&�C&�C&C&�HC'�C'z�C'�RC'�
C({C(p�C(�RC(�C)�C)ffC)�RC)�HC*{C*p�C*�C*�
C+{C+ffC+��C+�HC,
=C,G�C,��C,��C-{C-ffC-��C-��C.
=C.ffC.��C.C/{C/ffC/�\C/��C0(�C0\)C0�\C0�HC133C1Q�C1��C1��C233C2\)C2C3  C333C3�C3�
C3��C4=qC4��C4C5
=C5Q�C5��C5��C6{C6ffC6�\C6�
C733C7p�C7��C8
=C8G�C8z�C8�HC933C9\)C9C:
=C:33C:�\C:�HC;
=C;Q�C;�RC<  C<(�C<z�C<�
C=
=C=ffC=�RC=�C>G�C>��C>��C?�C?�\C?C@�C@ffC@��CA
=CA=qCA�\CA��CB33CBz�CB�CC�CCp�CC�HCD{CDz�CD��CE  CEffCE��CF
=CF\)CF��CG  CG=qCG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                     ?�\)@�\@=p�@}p�@�  @�  @�  A ��AG�A ��A,(�A?\)A_\)A�Q�A�  A��A�  A�Q�A�  A�\)A�B (�B  B�
B�B�B'�B/�B8(�B@Q�BH(�BP  BW�
B`  Bh  Bo�
Bx  B�
B��
B��
B�  B�  B��
B��
B��
B�  B�{B�(�B�{B�  B��B��B�=qB��B��B�  B��B��B�  B�  B��B��B�  B��B��
B��
B�  B�{B�(�C 
=C  C
=C{C
=C	�C
=C  C��C
=C��C�C��C
=C  C�C��C"
=C$
=C&  C'��C)��C+�C-�C/��C1��C3��C6
=C8{C:
=C<  C=��C?�CB  CD�CF{CH
=CJ
=CK��CN  CP  CR
=CT
=CV{CX
=CZ  C\
=C^  C`  Ca��Cc��Ce��Ch  Cj{Cl
=Cm��Co��Cr  Ct
=Cu��Cx  Cz
=C{�C}��C�
=C�  C���C���C�  C�C�
=C�  C���C���C�  C�\C�C�C�C�  C�C�  C���C���C�  C�\C�
=C�  C�  C���C�  C�C�  C���C�C�C�  C���C�  C���C���C�C�C�  C���C�  C���C���C�C�C�  C�C�  C���C�  C�  C���C�  C�C�C�
=C�C���C�  C�C���C��C���C���C�  C�
=C�  C���C�  C���C�  C�  C�  C���C�  C�  C���C�C�  C�  C�  C�  C�  C�  C���C�C�
=C�  C���C�  C�
=C�  C���C���C���C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C�C�  C���C���C�C�\C�C���C�  C�  C�  C�  C���C���C�  C�  C�  C�  D   D ��D  D� D  D��DD�D�D��D�D� D�D�D�qDxRD��Dz�D�qD	}qD	��D
� D�D��D�D��D��DxRD�qD}qD�qD� D  D}qD�D� D�qD� D�qD��D�D� D�qD}qD�D��D  D�D�D}qD�qD� D�D� D��D}qD�D� D��D}qD  D��D  Dz�D   D ��D!�D!}qD"  D"� D#�D#��D#�qD$��D%  D%}qD&�D&� D'�D'� D'�qD(� D(�qD)� D*�D*}qD*�qD+� D,  D,}qD-�D-� D-�RD.}qD/  D/� D0�D0��D0�qD1}qD1�qD2}qD3  D3� D3�qD4xRD5�D5� D5�qD6}qD7  D7��D8D8}qD8�qD9}qD9�qD:}qD:�qD;}qD;�qD<}qD<�RD=� D>D>��D?  D?��D@  D@}qDA  DA� DB  DB}qDB�qDC� DD�DD�DE  DEz�DE��DF}qDF�qDG��DH�DH� DH�qDI� DJ�DJ� DJ��DKz�DL  DL}qDM  DM��DN�DN��DO  DO� DP�DP� DP��DQ}qDR�DR�DS  DS}qDS�qDT}qDT�qDUz�DU�qDV� DV�qDW}qDW�qDX� DY�DY}qDY�qDZ}qDZ��D[� D\�D\�D]�D]� D]��D^� D_�D_��D`�D`� Da�Da��Db�Db� Db�qDc}qDd  Dd� DeDe��De�qDf� DgDg� Dg�qDh� Di�Di}qDi�RDj}qDk�Dk� Dk�qDlz�Dm  Dm��Dn  Dn� Do�Do� Dp  Dp� Dq  Dq� Dr  Drz�Dr��Ds}qDt�Dt��Du  Du� Du�qDvz�Dw  Dw}qDx  Dx� Dy�Dy� DzDz� D{�D{�D|�D|� D}  D}� D}��D~� D�D� D�qD�@ D��HD���D���D�>�D�~�D���D�  D�B�D��HD�� D��)D�=qD�� D��HD���D�=qD�}qD��qD���D�=qD��HD��HD�HD�B�D���D�� D��)D�=qD�� D�D��D�C�D���D�� D�HD�AHD�� D��qD���D�@ D�� D��HD��D�AHD��HD���D��qD�=qD�}qD��qD���D�AHD���D�D�  D�=qD�}qD���D�HD�AHD�� D�D�  D�<)D�~�D�D��D�@ D�� D�� D��)D�>�D�� D���D���D�@ D���D��HD�HD�AHD�� D���D���D�@ D���D��HD���D�AHD�� D��HD��D�>�D�� D�� D�  D�>�D�� D��HD��qD�=qD�}qD��)D��qD�>�D�~�D��qD��qD�>�D��HD�D��D�>�D�~�D�� D�  D�AHD��HD���D�HD�>�D�� D���D��D�@ D�|)D�� D�  D�@ D��HD�D�HD�@ D�~�D��qD���D�@ D�~�D��)D�  D�AHD�~�D���D��qD�AHD�� D�� D�  D�>�D��HD�D�  D�>�D�� D�D�  D�AHD���D���D���D�AHD�� D���D�HD�AHD��HD�D���D�>�D��HD�D�  D�>�D���D��HD��qD�@ D���D��HD���D�>�D�~�D��HD��D�@ D��HD���D�HD�@ D�}qD���D�  D�@ D�~�D���D��qD�@ D�~�D���D�HD�AHD���D���D���D�AHD���D�D�HD�@ D�� D���D�  D�=qD�� D��HD���D�@ D��HD�� D���D�>�D�~�D���D�HD�@ D�� D���D��qD�@ D�� D�� D�HD�AHD��HD�D�  D�>�D�~�D���D��D�@ D�� D�D���D�@ D�~�D��HD�  D�@ D��HD�� D�HD�AHD�~�D��HD�  D�B�D�~�D�� D��D�>�D�� D���D�  D�=qD D�� D�  D�AHD�~�D��HD�HD�>�D�~�DĽqD�  D�AHDŀ D��HD���D�@ DƂ�D��HD��qD�@ D�}qD�D�HD�B�D�~�D�� D�  D�AHDɀ D�� D���D�AHD�~�D��HD���D�@ Dˀ D�� D�  D�@ D̀ D�� D���D�@ D�~�D;�D���D�>�D�~�Dξ�D���D�>�Dπ DϾ�D���D�AHDЀ Dо�D���D�AHDр D��HD�  D�>�DҁHD��HD�HD�>�DӀ D��HD�HD�@ DԀ DԾ�D��D�AHDՀ D��HD��D�=qD�~�D��HD�HD�>�D�}qD�� D���D�>�D�~�Dؾ�D���D�@ DفHDپ�D��qD�>�DځHD�� D��qD�@ DہHD�� D���D�>�D܀ D��HD�  D�AHD݁HD�� D�HD�AHDށHD�D�  D�AHD߂�D��HD�  D�@ D��HD�� D���D�@ D�HD�� D��D�AHD�~�D�� D�  D�>�D�|)D�� D��qD�>�D�~�D��HD�HD�@ D�~�D徸D��qD�=qD� D�� D�  D�@ D� D��HD�  D�@ D�HD��HD�HD�@ D� D�D�HD�AHDꂏD��HD�  D�>�D� D�� D�  D�@ D�HD�� D�  D�C�D�HD��HD��qD�AHD�}qD��HD���D�@ DD�� D�HD�@ D�� D��HD���D�B�D�~�D�D�HD�@ D� D�� D�HD�B�D�HD��HD��)D�=qD� D�� D��qD�@ D���D�� D�HD�B�D���D��qD��qD�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D�~�D���D���D�=qD�~�D���D��G�O�?#�
?8Q�?u?�z�?�p�?�G�?�@
=q@�@�R@&ff@333@B�\@O\)@aG�@p��@}p�@��@��@��@�Q�@�p�@��\@��@�\)@�z�@�(�@��@���@��@�Q�@޸R@��
@���@��@�Q�AG�Az�A�A
�HA�RAG�A33A
=A�A\)A#33A'
=A*=qA-p�A0  A2�\A5A:=qA>�RAA�AE�AG�AJ=qAN{AQG�AU�AY��A^{AaG�Ae�AhQ�Ai��Amp�AqG�Au�Ax��A~{A���A��\A�(�A�{A�  A�G�A�=qA�z�A�ffA�  A��A�z�A��RA�Q�A�=qA��
A�{A��A���A��\A�z�A�\)A�G�A��HA���A��RA���A�=qA��
A�A�
=A�G�A�33A��A�
=A���A�33A�p�AǮAə�A˅A�p�A�\)A�G�A��HA�z�A�ffA�Q�A�=qA�(�A�ffA�Q�A�\A�z�A�ffA�Q�A�=qA�(�A�{A�Q�A�=qA���A�
=A���A�33A��A�
=B ��B��B�\B�Bz�Bp�B=qB\)Bz�B	G�B
�\B�B��B�B
=B(�B�B{B33B(�BG�B{B
=B  B�B=qB33BQ�BG�B�\B�B ��B!�B#
=B$  B%�B&{B&�HB(  B(��B*{B+33B,Q�B-G�B.ffB/�B0��B1�B333B4Q�B5p�B6ffB7�B8z�B9��B:�RB;�
B<��B=p�B>�\B?�B@��BA�BC
=BC�
BD��BF{BG\)BHQ�BIp�BJffBK�BL��BMBO
=BP(�BQG�BRffBS�BT��BUBV�HBX  BY�BZ{B[\)B\z�B^{B_33B`Q�Bap�Bb�\Bc�Bd��BeBg
=Bg�
Bi�Bj=qBk
=Bl  Bm�Bn�\Bo\)Bp(�BqG�BrffBs\)Bt��Bu��Bv�RBw�Bx��Bz{B{�B|z�B}��B~�RB�
B�ffB���B��B�{B��\B�33B��B�=qB���B�33B��B�(�B���B�G�B��B�ffB�
=B��B�{B���B�33B�B�Q�B���B�\)B��
B�z�B���B���B�{B���B�33B�B�(�B��RB�G�B�B�ffB���B��B�{B���B�33B��B�{B���B��B��B�=qB��RB�G�B��
B��\B�
=B���B�{B��RB�33B�B�=qB���B�G�B��B�=qB���B�33B�B�ffB��HB�G�B��B�ffB���B�p�B�{B��\B�\)B��B��\B�
=B���B�(�B���B�G�B�B�=qB���B�p�B�  B���B�33B�B�ffB���B��B�{B��RB�G�B�{B���B�G�B��B�z�B�
=B��B�=qB��RB�G�B��
B�Q�B��RB�
=B�\)B���B��
B�(�B�z�B��\B��RB��HB��B�G�B��B�B��B�{B�Q�B�z�B��RB�
=B�G�B���B��
B�{B�Q�B\B¸RB�
=B�33B�p�BÙ�B��
B�  B�(�B�Q�Bď\B���B�
=B�33BŅBŮB��B�(�BƏ\B��HB��B�\)BǙ�B��
B�{B�(�B�Q�Bȏ\B���B��B�G�BɅB�B�  B�=qB�ffBʣ�B���B�\)B˙�B��B�{B�Q�B�ffḄ�B��HB��B�\)BͮB�  B�Q�BθRB��HB�G�BυBϙ�B��B�=qB�ffB���B�G�Bљ�B��B�(�B�z�B���B��B�G�BӅB�B�(�B�ffBԸRB�
=B�p�B��
B�{B�z�BָRB��HB��B�p�B׮B�  B�Q�BظRB��B�p�B�B�{B�Q�B�z�BڸRB�
=B�\)Bۙ�B��B�(�B�z�B���B��B�\)B�B�{B�ffBޣ�B��B�p�B��
B�(�B�ffB���B��B�\)B�p�B��
B�(�B�z�B���B��B�p�B�  B�Q�B��B�
=B�G�B�p�B�B�(�B�ffB���B�p�B�B�{B�z�B�RB���B�G�B陚B�  B�Q�B���B�G�B�B�{B�z�B�RB���B�\)B��B�{B��B�
=B�p�B�B��B�ffB�RB�
=B�B�{B�z�B���B�G�B�B�  B�z�B���B�33B��
B�Q�B���B�33B�p�B��B�=qB���B�\)B��B�=qB��\B��HB�p�B�B�ffB��HB�G�B��
B�  B�z�B��HB�p�C 
=C =qC z�C ��C �
C
=CG�C��C�HC�C\)C�C�RC��C(�Cz�C��C
=CG�CffC�C��CG�C�CC�C�CffCC��C=qCffC��C�
C�Cp�C�C��C	{C	Q�C	��C	�C
(�C
p�C
��C
�
C{C\)C�C��C33C\)C��C�C33Cz�C�RC�HC(�C\)C�RC  C=qCffC�C�CG�C�C��C�C33C�\C��C{C=qCz�C�HC�CffC�C��C�Cp�C�RC�
C�C\)C�RC��C(�CffC��C  CG�CffC�C��C=qCz�C�C�CG�C�C�C�C33C�CC�HC�Cz�CC��C(�C\)CC  C�C\)C�RC  C�C\)C�RC  C�C\)C�RC   C �C Q�C ��C ��C!
=C!G�C!��C!�HC"  C"=qC"�\C"�
C"��C#33C#�\C#��C#�C$(�C$�C$C$�C%(�C%�C%C%�C&�C&�C&C&�HC'�C'z�C'�RC'�
C({C(p�C(�RC(�C)�C)ffC)�RC)�HC*{C*p�C*�C*�
C+{C+ffC+��C+�HC,
=C,G�C,��C,��C-{C-ffC-��C-��C.
=C.ffC.��C.C/{C/ffC/�\C/��C0(�C0\)C0�\C0�HC133C1Q�C1��C1��C233C2\)C2C3  C333C3�C3�
C3��C4=qC4��C4C5
=C5Q�C5��C5��C6{C6ffC6�\C6�
C733C7p�C7��C8
=C8G�C8z�C8�HC933C9\)C9C:
=C:33C:�\C:�HC;
=C;Q�C;�RC<  C<(�C<z�C<�
C=
=C=ffC=�RC=�C>G�C>��C>��C?�C?�\C?C@�C@ffC@��CA
=CA=qCA�\CA��CB33CBz�CB�CC�CCp�CC�HCD{CDz�CD��CE  CEffCE��CF
=CF\)CF��CG  CG=qCG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                     @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�$@ɇG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�^A�FA�!A�FA�!A�A�?}A��A���A�&�A�dZA�  A�p�AݾwA�/A܅A���A�`BA�$�A�{A��AڶFAڇ+A�Q�A�C�A��A���A��A�C�A�33A�bA�bA���A�(�A؉7A�`BA�-A��A���A���A�t�A��A�33A��A�|�A��A���A���A���A�ƨA�A���A���A��
A�1A�M�A���A���A�A�~�A��`A���A��uA��A�`BA��/A���A�n�A�ZA�7LA��A� �A��#A�dZA�"�A�/A���A��A��HA�l�A��
A�A�&�A���A�9XA�K�A�A�A�A�7LA�\)A�VA�ȴA���A��mA��/A�x�A�=qA��A��PA�%A
=A|^5Av�As�hAr�jAq��Ap�Ao�Ak
=Aj�AjjAh��Af�/Ad�!Ac��Ac�Ab�+AbJA_�A\�AX�`AUƨAS��AQ"�AOhsAK�^AGAB��AA�^A@v�A?A>ȴA=33A:r�A7��A6��A3x�A0VA.A�A-��A-p�A-"�A,��A,��A*E�A(��A&�!A&  A#��A#%A"Q�A!��A r�A;dAr�A��A�/A��A|�A��A�
AO�A��AM�AA��A&�A�`A?}A�A��Ar�Av�Ar�A~�A��A^5A�TA��AĜA9XA1A��A"�A
ffA	��A�A	%AJA��AbA�FA��A�A�7A V@��D@�/@�1@�v�@�7L@���@���@�p�@�`B@�dZ@��@�$�@�7L@��D@�S�@��@���@��@��^@�(�@�"�@ꗍ@�ff@�{@���@�F@�dZ@�^5@�@�7L@�u@�33@��@��;@��y@ݑh@�&�@��`@� �@�"�@�
=@�o@�n�@���@�|�@�M�@Ձ@���@��H@�@�x�@�`B@���@��;@�o@Χ�@���@�V@�x�@��@��@��@��@�/@�X@�X@̛�@��;@˥�@�t�@�;d@�+@�ȴ@��@�x�@ɑh@�?}@��@���@�E�@��T@���@�@ř�@�/@��@�Z@��;@�t�@���@�5?@���@�G�@�V@��@���@���@�A�@�ƨ@��y@�ff@��#@��@�&�@�7L@�7L@�7L@�?}@��`@���@�r�@�1'@��m@��@��@�33@�o@��H@��\@�V@�@��h@���@��/@�z�@��m@�"�@���@�M�@���@�G�@��j@�I�@� �@�b@�t�@�33@���@�@�K�@���@��!@��+@�ff@�$�@�x�@��j@�z�@�(�@��@�5?@��@���@���@�?}@�%@���@��@��@���@�t�@�"�@���@�^5@��@��#@��h@�G�@��@��@���@� �@�I�@�j@�1'@��w@���@���@�^5@�@��T@��T@���@�X@�V@���@�/@�/@��@���@���@�A�@�  @��w@���@�K�@�33@�"�@��@��\@�=q@���@�7L@��/@�r�@�9X@�1'@�1'@�(�@��@��P@�S�@�ȴ@�M�@�5?@�J@�@��h@�G�@�&�@���@��@���@��u@�I�@�1'@��;@��@�\)@�@��!@�~�@�{@��^@�hs@���@��@�I�@�  @��P@�33@��@���@�5?@�J@�@�?}@��/@�Ĝ@��@�9X@���@��m@��w@�l�@�+@�o@���@�n�@�5?@��^@���@��@���@���@�r�@�1'@�  @��w@��P@�l�@�;d@��@��!@��+@�5?@�-@��-@�G�@�V@���@��@��@���@��w@�|�@�"�@��H@��+@��T@�x�@�`B@�O�@�&�@��j@�z�@�bN@�A�@���@�|�@�;d@�o@���@��\@�=q@���@�p�@�V@�Ĝ@��u@�z�@�j@�9X@���@��w@��F@���@�|�@�S�@�+@�o@��@���@�v�@�v�@�5?@�@��#@���@�p�@�7L@���@���@���@��@�I�@�1'@�b@�w@~��@}�@}`B@|�@|z�@|I�@{�m@{ƨ@{��@{t�@{dZ@{o@z�\@y�^@y��@y7L@x�9@xQ�@x �@x  @w�w@wl�@v��@vff@u�h@up�@uO�@t��@t9X@sƨ@sdZ@r�!@r~�@r=q@q�@q�^@qhs@p�`@p�9@pr�@o��@o�@ol�@ol�@o�P@o\)@n�y@nff@m�-@l�j@lI�@k�
@k��@ko@j��@j=q@i��@iX@h��@g��@g�P@g�@fȴ@fff@f$�@e��@e�@d��@d�@dj@c��@c��@cdZ@cC�@c33@co@b��@bM�@a�@a��@a7L@`��@`A�@`  @_�@_K�@^�+@^{@]�h@]O�@]V@\��@\z�@\I�@\9X@\(�@[�m@[��@[t�@[S�@["�@Z��@ZM�@Y��@X��@Xr�@XA�@W��@W�P@W;d@V��@VV@V$�@V@U@U��@U�@U�@U/@T�/@Tz�@T(�@S��@S�F@S@R=q@Q��@QG�@Q%@P��@PA�@O�w@O\)@N�R@NE�@M�-@M/@L�@L�@K�m@K�@KC�@K@J�@J~�@JJ@I��@IG�@HĜ@HbN@G�@G��@GK�@G�@F�y@F�R@Fv�@FV@FE�@F{@E�-@E�@D��@D�/@D��@Dz�@C�
@C33@B��@B-@BJ@A�^@AX@A7L@@��@@��@@r�@@A�@?�@?��@?K�@?�@?
=@>�R@>v�@>E�@>5?@=��@=�@=�@<�/@<�@<�@<j@<1@;@:�@9��@9X@9G�@9&�@8Ĝ@8A�@8  @7�@7K�@6�y@6�y@6ȴ@6��@65?@6@5��@5O�@4��@4�D@41@3t�@2�@2~�@1�@1x�@1&�@0�`@0Ĝ@0��@01'@/��@/;d@.�@.��@.E�@.@-�@-��@-��@-?}@-V@,��@,�@,��@,�D@,Z@,�@+��@+�F@+t�@+33@*�@*��@*~�@*^5@*-@*-@)�@)��@)X@)�@(��@(��@(��@(r�@(A�@(1'@(  @'��@'+@&�R@&��@&�+@&$�@%�-@%�@%`B@%/@$��@$�D@$Z@#�m@#��@#33@"��@"=q@!�@!�#@!��@!hs@!&�@!�@ Ĝ@  �@�w@|�@�y@��@�+@v�@$�@�h@`B@O�@V@�@��@��@Z@(�@��@�m@�
@�F@33@"�@o@o@�@�H@�H@�H@��@�\@n�@^5@=q@��@�7@hs@X@X@7L@&�@�@�@�`@��@�9@�u@�u@�u@r�@Q�@ �@��@|�@
=@�R@�+@ff@ff@E�@{@�@�T@��@@��@�@O�@/@�@V@��@z�@j@I�@1@�
@ƨ@ƨ@ƨ@��@�@t�@S�@33@"�@��@n�@=q@�@��@��@�7@X@�@��@��@Ĝ@r�@b@�w@�P@\)@�@�@�R@��@v�@V@E�@$�@$�@{@@�@`B@?}@V@�j@�D@z�@Z@9X@�@�F@��@t�@dZ@"�@@
�!@
��@
~�@
n�@
-@
�@	�#@	��@	hs@	7L@	7L@	7L@	&�@	�@�`@Ĝ@��@�u@�@Q�@ �@  @��@�w@��@|�@l�@\)@+@��@��@�@ȴ@��@v�@ff@5?@5?@{@�@��@@�-@��@��@�h@p�@O�@/@��@�/@�j@�jA��;A��#A���A�ȴA�ĜA�jA�wA�wA�wA�A���A�wA�A�A�^A�RA�RA�FA�^A�RA�^A�-A�RA�FA�RA�A�!A�A�A�A�A�!A�RA�ȴA�RA�!A�9A�A��A��A��AᕁA�^5A�VA�K�A�G�A�33A�/A�$�A��A�oA�1A��A��HA�ȴA�wA�RA�9A��A���A��\A�~�A�l�A�A�A�&�A�{A���Aߺ^Aߧ�A߉7A߃A�z�A�n�A�bNA�\)A�K�A�9XA�7LA�5?A�/A��A�bA�A���A��
A�ȴA޶FAޮAޥ�AލPA�p�A�Q�A�M�A�A�A�&�A��A���A��TA���Aݩ�Aݛ�Aݙ�A݋DA�v�A�K�A�E�A�C�A�;dA�&�A� �A��A�VA��TA��A�ƨAܡ�A�ffA�A�A�1'A�"�A�JA���A��TA���A�ƨA۾wA۲-A۟�AۍPA�l�A�^5A�ZA�XA�XA�Q�A�C�A�9XA�/A�+A�$�A� �A��A��A�oA�VA�VA�bA�{A��A��A��A�{A�1A�  A���A���A��`A��A���A�ƨA�ĜAھwAں^AڶFAڰ!AڬAڥ�Aڝ�Aڙ�Aڏ\AڅAځA�z�A�jA�^5A�ZA�VA�S�A�O�A�K�A�K�A�I�A�I�A�K�A�I�A�E�A�?}A�=qA�;dA�7LA�1'A�(�A�$�A��A�JA���A���A��A��A��A���A���AټjAٺ^AٸRAٸRA�A���A�  A��A��A��A�"�A�1'A�?}A�O�A�VA�bNA�l�A�p�A�l�A�A�A��A�JA�
=A�
=A�
=A�1A�JA�{A�oA�oA�oA�bA�oA�oA�{A�oA�bA�VA�JA�
=A�%A�%A�  A�  A���A��
A١�A�p�A�G�A� �A�JA��HAة�A؟�A؛�A؏\A؃A�~�A�z�A�x�A�r�A�jA�hsA�ffA�\)A�XA�M�A�K�A�G�A�G�A�A�A�1'A�"�A��A���A�  A��TA���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�A׶FA׶FA��;A��`A��;A�ƨA״9Aן�Aׇ+A�x�A�v�A�n�A�=qA�+A�
=A�  A���A���A���A���AּjA֥�A֟�A�p�A�A�A�&�A��A��;Aթ�A�x�A�C�A�`BA�~�A���Aч+A�t�A�/A�{A��#Aϧ�A�v�A�ZA�-A��yAδ9A�hsA�/A�bA��A���A͝�A�O�A��A��A̰!ÁA�O�A��A���A��A��HA��A˼jA˓uA�v�A�bNA�9XA�1Aʴ9A�E�A�JA���A��mA��/A���A�ȴAɴ9Aɴ9AɮAɩ�Aɧ�Aɛ�AɋDA�v�A�jA�\)A�G�A�9XA�33A�(�A�{A�A��yA�ĜAȩ�A�~�A�hsA�XA�I�A�=qA�-A��A�
=A���A��
A���A�ƨA���AǴ9AǮAǴ9AǗ�AǇ+AǅA�z�A�t�A�ffA�`BA�\)A�?}A�7LA��A�%A��A��
A�ȴAƸRAƥ�AƗ�AƏ\AƅA�v�A�ffA�Q�A�I�A�A�A�9XA�1'A�&�A��A���A���Aź^AŸRAŬAś�AœuAōPAŅA�ffA�-A�+A�"�A� �A�  A�l�A�oAüjAÏ\A�v�A�O�A��A��`A���A¡�A+A�v�A�jA�^5A�Q�A�G�A�A�A�=qA�7LA�(�A� �A��A��A�oA��A�{A�VA�
=A���A��A��A��A��A��`A��`A��`A��TA��TA��HA��#A��
A���A���A���A���A���A�ȴA�ƨA�ĜA��jA��-A���A�hsA�$�A���A��A��#A���A��9A�~�A�7LA�JA�ĜA�~�A�`BA�G�A��A��A���A��;A�XA�=qA�$�A� �A��A���A��
A���A�ZA��A��DA��/A���A���A��DA�|�A�p�A�(�A�=qA��A��7A�n�A�VA��A��yA���A�|�A�A���A�v�A�hsA�jA�hsA�bNA�S�A�&�A��A���A��A�v�A�r�A�"�A��FA�XA�hsA�z�A��7A���A���A��DA�~�A�Q�A�33A�&�A�VA��A��yA��A��A���A���A��yA��A���A�v�A�^5A��A��/A���A�VA�S�A�/A�
=A�  A��`A��-A��A�O�A�;dA��A���A��A���A��PA�~�A�r�A�C�A�1'A��A�  A���A�O�A�=qA� �A�oA�1A�A���A���A��HA�A��!A���A��A�~�A�|�A�jA�XA���A�p�A��
A�A�A���A�?}A�$�A�{A�1A��A��TA���A��!A�VA��A��#A��uA�n�A�E�A��A�ƨA��A�VA�O�A�?}A�-A���A��A��TA��
A�ĜA��uA�bNA�=qA��A���A�z�A��A��
A�ȴA���A��9A���A���A���A���A���A���A���A��PA�dZA�E�A�33A�"�A�
=A��A���A�ȴA��jA���A���A��hA��7A��+A�~�A�l�A�XA�;dA�oA��mA���A�z�A��A��+A�&�A��`A��A�K�A�1'A��A�
=A���A��
A���A�n�A�G�A��A��hA��A�ĜA��uA�7LA��A��RA��uA�XA�1A���A�p�A�O�A�;dA�+A�1A��HA���A�+A�ƨA��RA���A���A��7A�|�A�ffA�ZA�C�A�5?A�JA��yA���A���A�ffA�7LA��/A��-A�(�A��A���A��A�ffA�O�A�5?A��A��mA���A���A�r�A�&�A� �A��A�oA�JA�JA�1A��A��FA�?}A�VA��TA��;A��
A�ƨA���A�n�A� �A��A��A���A��A��+A�M�A���A���A���A�n�A�9XA�JA���A��A��`A��#A�ȴA��!A���A���A��7A�ffA�;dA�(�A��A�p�A��A��A��7A�VA��A��jA�1'A��#A��A���A��mA�p�A�bA���A��A�|�A�=qA��`A�VA��yA�|�A�A��jA���A�&�A���A��A�p�A�/A���A�ƨA��PA�t�A�I�A�9XA�/A��A��A�
=A��A�TA��A�-A�PAK�A&�A~�A~��A~1A}t�A|�A|��A|-A{x�Az�!AzA�Ay��Aw�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                     A���A���A���A�^A�FA�!A�FA�!A�A�?}A��A���A�&�A�dZA�  A�p�AݾwA�/A܅A���A�`BA�$�A�{A��AڶFAڇ+A�Q�A�C�A��A���A��A�C�A�33A�bA�bA���A�(�A؉7A�`BA�-A��A���A���A�t�A��A�33A��A�|�A��A���A���A���A�ƨA�A���A���A��
A�1A�M�A���A���A�A�~�A��`A���A��uA��A�`BA��/A���A�n�A�ZA�7LA��A� �A��#A�dZA�"�A�/A���A��A��HA�l�A��
A�A�&�A���A�9XA�K�A�A�A�A�7LA�\)A�VA�ȴA���A��mA��/A�x�A�=qA��A��PA�%A
=A|^5Av�As�hAr�jAq��Ap�Ao�Ak
=Aj�AjjAh��Af�/Ad�!Ac��Ac�Ab�+AbJA_�A\�AX�`AUƨAS��AQ"�AOhsAK�^AGAB��AA�^A@v�A?A>ȴA=33A:r�A7��A6��A3x�A0VA.A�A-��A-p�A-"�A,��A,��A*E�A(��A&�!A&  A#��A#%A"Q�A!��A r�A;dAr�A��A�/A��A|�A��A�
AO�A��AM�AA��A&�A�`A?}A�A��Ar�Av�Ar�A~�A��A^5A�TA��AĜA9XA1A��A"�A
ffA	��A�A	%AJA��AbA�FA��A�A�7A V@��D@�/@�1@�v�@�7L@���@���@�p�@�`B@�dZ@��@�$�@�7L@��D@�S�@��@���@��@��^@�(�@�"�@ꗍ@�ff@�{@���@�F@�dZ@�^5@�@�7L@�u@�33@��@��;@��y@ݑh@�&�@��`@� �@�"�@�
=@�o@�n�@���@�|�@�M�@Ձ@���@��H@�@�x�@�`B@���@��;@�o@Χ�@���@�V@�x�@��@��@��@��@�/@�X@�X@̛�@��;@˥�@�t�@�;d@�+@�ȴ@��@�x�@ɑh@�?}@��@���@�E�@��T@���@�@ř�@�/@��@�Z@��;@�t�@���@�5?@���@�G�@�V@��@���@���@�A�@�ƨ@��y@�ff@��#@��@�&�@�7L@�7L@�7L@�?}@��`@���@�r�@�1'@��m@��@��@�33@�o@��H@��\@�V@�@��h@���@��/@�z�@��m@�"�@���@�M�@���@�G�@��j@�I�@� �@�b@�t�@�33@���@�@�K�@���@��!@��+@�ff@�$�@�x�@��j@�z�@�(�@��@�5?@��@���@���@�?}@�%@���@��@��@���@�t�@�"�@���@�^5@��@��#@��h@�G�@��@��@���@� �@�I�@�j@�1'@��w@���@���@�^5@�@��T@��T@���@�X@�V@���@�/@�/@��@���@���@�A�@�  @��w@���@�K�@�33@�"�@��@��\@�=q@���@�7L@��/@�r�@�9X@�1'@�1'@�(�@��@��P@�S�@�ȴ@�M�@�5?@�J@�@��h@�G�@�&�@���@��@���@��u@�I�@�1'@��;@��@�\)@�@��!@�~�@�{@��^@�hs@���@��@�I�@�  @��P@�33@��@���@�5?@�J@�@�?}@��/@�Ĝ@��@�9X@���@��m@��w@�l�@�+@�o@���@�n�@�5?@��^@���@��@���@���@�r�@�1'@�  @��w@��P@�l�@�;d@��@��!@��+@�5?@�-@��-@�G�@�V@���@��@��@���@��w@�|�@�"�@��H@��+@��T@�x�@�`B@�O�@�&�@��j@�z�@�bN@�A�@���@�|�@�;d@�o@���@��\@�=q@���@�p�@�V@�Ĝ@��u@�z�@�j@�9X@���@��w@��F@���@�|�@�S�@�+@�o@��@���@�v�@�v�@�5?@�@��#@���@�p�@�7L@���@���@���@��@�I�@�1'@�b@�w@~��@}�@}`B@|�@|z�@|I�@{�m@{ƨ@{��@{t�@{dZ@{o@z�\@y�^@y��@y7L@x�9@xQ�@x �@x  @w�w@wl�@v��@vff@u�h@up�@uO�@t��@t9X@sƨ@sdZ@r�!@r~�@r=q@q�@q�^@qhs@p�`@p�9@pr�@o��@o�@ol�@ol�@o�P@o\)@n�y@nff@m�-@l�j@lI�@k�
@k��@ko@j��@j=q@i��@iX@h��@g��@g�P@g�@fȴ@fff@f$�@e��@e�@d��@d�@dj@c��@c��@cdZ@cC�@c33@co@b��@bM�@a�@a��@a7L@`��@`A�@`  @_�@_K�@^�+@^{@]�h@]O�@]V@\��@\z�@\I�@\9X@\(�@[�m@[��@[t�@[S�@["�@Z��@ZM�@Y��@X��@Xr�@XA�@W��@W�P@W;d@V��@VV@V$�@V@U@U��@U�@U�@U/@T�/@Tz�@T(�@S��@S�F@S@R=q@Q��@QG�@Q%@P��@PA�@O�w@O\)@N�R@NE�@M�-@M/@L�@L�@K�m@K�@KC�@K@J�@J~�@JJ@I��@IG�@HĜ@HbN@G�@G��@GK�@G�@F�y@F�R@Fv�@FV@FE�@F{@E�-@E�@D��@D�/@D��@Dz�@C�
@C33@B��@B-@BJ@A�^@AX@A7L@@��@@��@@r�@@A�@?�@?��@?K�@?�@?
=@>�R@>v�@>E�@>5?@=��@=�@=�@<�/@<�@<�@<j@<1@;@:�@9��@9X@9G�@9&�@8Ĝ@8A�@8  @7�@7K�@6�y@6�y@6ȴ@6��@65?@6@5��@5O�@4��@4�D@41@3t�@2�@2~�@1�@1x�@1&�@0�`@0Ĝ@0��@01'@/��@/;d@.�@.��@.E�@.@-�@-��@-��@-?}@-V@,��@,�@,��@,�D@,Z@,�@+��@+�F@+t�@+33@*�@*��@*~�@*^5@*-@*-@)�@)��@)X@)�@(��@(��@(��@(r�@(A�@(1'@(  @'��@'+@&�R@&��@&�+@&$�@%�-@%�@%`B@%/@$��@$�D@$Z@#�m@#��@#33@"��@"=q@!�@!�#@!��@!hs@!&�@!�@ Ĝ@  �@�w@|�@�y@��@�+@v�@$�@�h@`B@O�@V@�@��@��@Z@(�@��@�m@�
@�F@33@"�@o@o@�@�H@�H@�H@��@�\@n�@^5@=q@��@�7@hs@X@X@7L@&�@�@�@�`@��@�9@�u@�u@�u@r�@Q�@ �@��@|�@
=@�R@�+@ff@ff@E�@{@�@�T@��@@��@�@O�@/@�@V@��@z�@j@I�@1@�
@ƨ@ƨ@ƨ@��@�@t�@S�@33@"�@��@n�@=q@�@��@��@�7@X@�@��@��@Ĝ@r�@b@�w@�P@\)@�@�@�R@��@v�@V@E�@$�@$�@{@@�@`B@?}@V@�j@�D@z�@Z@9X@�@�F@��@t�@dZ@"�@@
�!@
��@
~�@
n�@
-@
�@	�#@	��@	hs@	7L@	7L@	7L@	&�@	�@�`@Ĝ@��@�u@�@Q�@ �@  @��@�w@��@|�@l�@\)@+@��@��@�@ȴ@��@v�@ff@5?@5?@{@�@��@@�-@��@��@�h@p�@O�@/@��@�/@�jG�O�A��;A��#A���A�ȴA�ĜA�jA�wA�wA�wA�A���A�wA�A�A�^A�RA�RA�FA�^A�RA�^A�-A�RA�FA�RA�A�!A�A�A�A�A�!A�RA�ȴA�RA�!A�9A�A��A��A��AᕁA�^5A�VA�K�A�G�A�33A�/A�$�A��A�oA�1A��A��HA�ȴA�wA�RA�9A��A���A��\A�~�A�l�A�A�A�&�A�{A���Aߺ^Aߧ�A߉7A߃A�z�A�n�A�bNA�\)A�K�A�9XA�7LA�5?A�/A��A�bA�A���A��
A�ȴA޶FAޮAޥ�AލPA�p�A�Q�A�M�A�A�A�&�A��A���A��TA���Aݩ�Aݛ�Aݙ�A݋DA�v�A�K�A�E�A�C�A�;dA�&�A� �A��A�VA��TA��A�ƨAܡ�A�ffA�A�A�1'A�"�A�JA���A��TA���A�ƨA۾wA۲-A۟�AۍPA�l�A�^5A�ZA�XA�XA�Q�A�C�A�9XA�/A�+A�$�A� �A��A��A�oA�VA�VA�bA�{A��A��A��A�{A�1A�  A���A���A��`A��A���A�ƨA�ĜAھwAں^AڶFAڰ!AڬAڥ�Aڝ�Aڙ�Aڏ\AڅAځA�z�A�jA�^5A�ZA�VA�S�A�O�A�K�A�K�A�I�A�I�A�K�A�I�A�E�A�?}A�=qA�;dA�7LA�1'A�(�A�$�A��A�JA���A���A��A��A��A���A���AټjAٺ^AٸRAٸRA�A���A�  A��A��A��A�"�A�1'A�?}A�O�A�VA�bNA�l�A�p�A�l�A�A�A��A�JA�
=A�
=A�
=A�1A�JA�{A�oA�oA�oA�bA�oA�oA�{A�oA�bA�VA�JA�
=A�%A�%A�  A�  A���A��
A١�A�p�A�G�A� �A�JA��HAة�A؟�A؛�A؏\A؃A�~�A�z�A�x�A�r�A�jA�hsA�ffA�\)A�XA�M�A�K�A�G�A�G�A�A�A�1'A�"�A��A���A�  A��TA���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�A׶FA׶FA��;A��`A��;A�ƨA״9Aן�Aׇ+A�x�A�v�A�n�A�=qA�+A�
=A�  A���A���A���A���AּjA֥�A֟�A�p�A�A�A�&�A��A��;Aթ�A�x�A�C�A�`BA�~�A���Aч+A�t�A�/A�{A��#Aϧ�A�v�A�ZA�-A��yAδ9A�hsA�/A�bA��A���A͝�A�O�A��A��A̰!ÁA�O�A��A���A��A��HA��A˼jA˓uA�v�A�bNA�9XA�1Aʴ9A�E�A�JA���A��mA��/A���A�ȴAɴ9Aɴ9AɮAɩ�Aɧ�Aɛ�AɋDA�v�A�jA�\)A�G�A�9XA�33A�(�A�{A�A��yA�ĜAȩ�A�~�A�hsA�XA�I�A�=qA�-A��A�
=A���A��
A���A�ƨA���AǴ9AǮAǴ9AǗ�AǇ+AǅA�z�A�t�A�ffA�`BA�\)A�?}A�7LA��A�%A��A��
A�ȴAƸRAƥ�AƗ�AƏ\AƅA�v�A�ffA�Q�A�I�A�A�A�9XA�1'A�&�A��A���A���Aź^AŸRAŬAś�AœuAōPAŅA�ffA�-A�+A�"�A� �A�  A�l�A�oAüjAÏ\A�v�A�O�A��A��`A���A¡�A+A�v�A�jA�^5A�Q�A�G�A�A�A�=qA�7LA�(�A� �A��A��A�oA��A�{A�VA�
=A���A��A��A��A��A��`A��`A��`A��TA��TA��HA��#A��
A���A���A���A���A���A�ȴA�ƨA�ĜA��jA��-A���A�hsA�$�A���A��A��#A���A��9A�~�A�7LA�JA�ĜA�~�A�`BA�G�A��A��A���A��;A�XA�=qA�$�A� �A��A���A��
A���A�ZA��A��DA��/A���A���A��DA�|�A�p�A�(�A�=qA��A��7A�n�A�VA��A��yA���A�|�A�A���A�v�A�hsA�jA�hsA�bNA�S�A�&�A��A���A��A�v�A�r�A�"�A��FA�XA�hsA�z�A��7A���A���A��DA�~�A�Q�A�33A�&�A�VA��A��yA��A��A���A���A��yA��A���A�v�A�^5A��A��/A���A�VA�S�A�/A�
=A�  A��`A��-A��A�O�A�;dA��A���A��A���A��PA�~�A�r�A�C�A�1'A��A�  A���A�O�A�=qA� �A�oA�1A�A���A���A��HA�A��!A���A��A�~�A�|�A�jA�XA���A�p�A��
A�A�A���A�?}A�$�A�{A�1A��A��TA���A��!A�VA��A��#A��uA�n�A�E�A��A�ƨA��A�VA�O�A�?}A�-A���A��A��TA��
A�ĜA��uA�bNA�=qA��A���A�z�A��A��
A�ȴA���A��9A���A���A���A���A���A���A���A��PA�dZA�E�A�33A�"�A�
=A��A���A�ȴA��jA���A���A��hA��7A��+A�~�A�l�A�XA�;dA�oA��mA���A�z�A��A��+A�&�A��`A��A�K�A�1'A��A�
=A���A��
A���A�n�A�G�A��A��hA��A�ĜA��uA�7LA��A��RA��uA�XA�1A���A�p�A�O�A�;dA�+A�1A��HA���A�+A�ƨA��RA���A���A��7A�|�A�ffA�ZA�C�A�5?A�JA��yA���A���A�ffA�7LA��/A��-A�(�A��A���A��A�ffA�O�A�5?A��A��mA���A���A�r�A�&�A� �A��A�oA�JA�JA�1A��A��FA�?}A�VA��TA��;A��
A�ƨA���A�n�A� �A��A��A���A��A��+A�M�A���A���A���A�n�A�9XA�JA���A��A��`A��#A�ȴA��!A���A���A��7A�ffA�;dA�(�A��A�p�A��A��A��7A�VA��A��jA�1'A��#A��A���A��mA�p�A�bA���A��A�|�A�=qA��`A�VA��yA�|�A�A��jA���A�&�A���A��A�p�A�/A���A�ƨA��PA�t�A�I�A�9XA�/A��A��A�
=A��A�TA��A�-A�PAK�A&�A~�A~��A~1A}t�A|�A|��A|-A{x�Az�!AzA�Ay��Aw�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�:B	�hB	� B	�hB	�4B	� B	� B	��B	� B	��B	�VB	�B	��B	�iB	}�B	z�B	~�B	�{B	�fB	��B	�1B	�_B	�xB	��B	�B	�YB	�{B	��B	�uB	� B	�B	��B	�B	��B	�NB	�BB	͟B	��B	��B	��B	��B	��B	�"B
MB
B
&�B
&�B
!�B
HB
~�B
��B
�}B
��B
ɺB
�B
�|B
�&B
�%B
�B�B�B�BB"BB
��BCB}�B�[B�B��B��B��B��B��B�B��B�eB��B��B��B�Bx�Bd�BA�B7�B�B iB
�|B
��B
��B
� B
�XB
��B
��B
�oB
|�B
ZQB
)�B	�B	�
B	��B	�B	�CB	��B	��B	�DB	��B	}�B	t�B	p�B	m�B	��B	�'B	��B	�qB	�(B	�MB	~�B	u�B	q�B	tTB	h�B	[�B	GEB	9�B	/�B	�B	�B	�B�2B�B�TB��B҉B͟BĜB�B��B��B��B��B�=B��B��B�XB��B��B�4B��B�B��B�qB��B��B��B��B�4B��B��B��B��B�~B�B��B~�B|�Bz�By�Bw�B{BwfB|�B��B��B�rB��B��B�!B�9B��B�XB�jB��B��B�RB��B��B��B��B��B��B��B�?B��B�B��B�IB��B�nB�7B�B��B��B�$B��B��B�HB� BרB�sB�B�WB�|B�yB��B�B��B	 4B	uB	�B	�B	�B	SB	�B	�B	�B	+B	
rB	
	B	
	B	�B	YB	1B	PB	�B	xB	�B	CB	!B	$B	+�B	-�B	-�B	6FB	=B	=qB	>�B	>BB	;�B	:�B	;0B	;�B	<6B	=<B	D3B	EmB	I�B	N�B	OvB	OvB	O�B	PHB	R�B	Y�B	a�B	d&B	gB	i�B	jKB	k�B	o�B	o�B	q�B	tB	uZB	u�B	y	B	y�B	{B	{B	z�B	{�B	}VB	cB	�uB	�B	��B	�7B	��B	�VB	� B	�:B	��B	�uB	�MB	��B	�7B	�!B	�-B	�:B	�B	�zB	��B	�*B	��B	��B	�IB	�OB	�UB	�'B	��B	�hB	��B	�tB	��B	�RB	��B	��B	�$B	�XB	��B	�qB	��B	��B	��B	�3B	�-B	�[B	ÖB	�gB	ŢB	ƨB	�KB	�B	�0B	�<B	�B	�B	��B	�)B	�5B	�5B	ޞB	ݘB	�]B	یB	�jB	�HB	�|B	�vB	�B	�|B	�B	��B	�B	�`B	�
B	�>B	�DB	��B	��B	�"B	�WB	�B	��B	�/B	�B	� B	�iB	��B	�B	�B	�B	�B	�;B	��B	�B	��B	��B	�B	�B	�B	�+B	��B	�	B	��B	��B	�DB	�JB	��B	��B	��B	��B
oB
;B
B
�B
AB
�B
�B
GB
B
�B
�B
B
B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
�B
_B
�B
fB
fB
�B
	7B
	B
	7B

	B

	B

=B

�B

�B
B
�B
PB
�B
�B
"B
�B
B
�B
�B
"B
�B
�B
�B
�B
�B
�B
.B
�B
�B
hB
4B
4B
�B
B
B
�B
�B
oB
B
�B
@B
uB
uB
�B
�B
�B
�B
B
�B
MB
�B
�B
YB
�B
YB
$B
_B
_B
�B
�B
�B
�B
B
�B
�B
xB
CB
CB
xB
�B
B
�B
OB
�B
�B
�B
OB
�B
�B
�B
 'B
 �B
 �B
!�B
"4B
"hB
"�B
"�B
#B
#nB
#�B
#�B
$@B
$�B
$�B
%B
%FB
$�B
$�B
%zB
&LB
&�B
&�B
'B
'B
'B
'B
'�B
(�B
(�B
)_B
)�B
)�B
)�B
)�B
)*B
)*B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+B
+kB
,B
-B
-CB
.�B
/B
/�B
/�B
/�B
/B
.�B
.�B
.�B
/B
/B
/�B
/�B
/�B
0�B
1'B
0�B
1�B
2aB
33B
49B
49B
4�B
5B
5�B
5�B
5?B
5�B
6B
6FB
6zB
6FB
7B
7�B
8B
7�B
8�B
8�B
9$B
9�B
:*B
:*B
9�B
9�B
:�B
:�B
;dB
;0B
;�B
;dB
;�B
<B
<6B
<�B
<jB
=<B
=�B
>B
>BB
>BB
>�B
>�B
?}B
?�B
@B
@OB
@OB
@�B
@�B
@�B
@OB
@�B
@�B
@�B
@�B
@�B
A B
A�B
B[B
C-B
B�B
C-B
C�B
C-B
C�B
C�B
D3B
C�B
C�B
C�B
D3B
C�B
C�B
DgB
DgB
EB
EB
EB
EB
E�B
F?B
F?B
GB
F�B
GzB
G�B
G�B
GzB
G�B
G�B
HB
H�B
I�B
I�B
J�B
J�B
K)B
K)B
K)B
K�B
K�B
L0B
L�B
L�B
MjB
M�B
N<B
N<B
NpB
N�B
N�B
OB
OB
OB
OBB
OvB
O�B
PHB
PHB
O�B
P�B
P�B
Q�B
RTB
R B
R B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
U2B
T�B
UgB
U2B
U�B
U�B
VB
V�B
V�B
V�B
WsB
V�B
W?B
W
B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
ZQB
[#B
[�B
[�B
\]B
[�B
[�B
\]B
\�B
\�B
]/B
\�B
]/B
]�B
]�B
^5B
^�B
_B
_pB
_�B
`BB
`BB
`vB
`B
`�B
aB
aB
a�B
a�B
a�B
bNB
bB
b�B
bB
b�B
b�B
c B
c�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
d&B
d�B
dZB
e,B
d�B
d�B
e`B
d�B
e�B
e�B
e�B
f2B
e�B
ffB
ffB
f�B
f2B
f�B
f�B
g�B
g�B
h
B
gmB
h�B
h�B
h�B
hsB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jB
kB
kB
kB
kB
k�B
lWB
lWB
l"B
m)B
m�B
m]B
m�B
m�B
n/B
ncB
o5B
o5B
o�B
pB
pB
o�B
oiB
o�B
poB
p�B
p;B
p�B
p�B
qB
q�B
qB
q�B
q�B
qvB
q�B
q�B
qAB
q�B
rB
q�B
q�B
rGB
r|B
r|B
r|B
r|B
r|B
sB
s�B
sB
r�B
r�B
sB
sB
s�B
tB
s�B
sMB
s�B
tTB
tTB
tB
t�B
uZB
uZB
uZB
uZB
u�B
u%B
u�B
uZB
u%B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v+B
v`B
wfB
v�B
v�B
wfB
v�B
w�B
w2B
xB
w�B
w�B
w�B
xB
x8B
x�B
xlB
x�B
x�B
y>B
y�B
zB
zB
zB
{B
{B
{�B
{�B
{�B
{B
{JB
{�B
{�B
{�B
|�B
|PB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
}VB
}�B
~�B
~(B
~�B
~]B
.B
~�B
�B
cB
� B
�B
� B
�4B
�iB
��B
�;B
��B
��B
�B
��B
�;B
�B
��B
�uB
�uB
��B
�uB
�B
��B
�{B
�GB
�GB
�{B
��B
��B
�MB
�MB
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�SB
�B
��B
��B
�SB
��B
��B
�YB
��B
��B
��B
�_B
�+B	�JB	�uB	� B	�hB	��B	��B	��B	�hB	��B	��B	�bB	��B	�bB	��B	�uB	��B	�B	�B	��B	��B	�oB	��B	��B	��B	�VB	�hB	�.B	�4B	�:B	� B	��B	��B	��B	�VB	��B	�.B	�bB	�4B	��B	��B	��B	��B	��B	�"B	�bB	�(B	�VB	��B	�VB	��B	��B	�B	�~B	��B	��B	��B	��B	�B	��B	�1B	�B	��B	�fB	��B	�bB	��B	�lB	�SB	�B	�B	��B	�oB	�AB	�oB	�oB	�{B	� B	~]B	|�B	}�B	{�B	{B	}�B	�4B	~�B	}�B	}�B	|B	{JB	.B	}"B	{B	xB	yrB	y	B	yrB	~(B	|�B	~�B	��B	�B	~�B	�4B	��B	��B	�;B	�B	��B	��B	�B	�AB	�B	�+B	��B	�_B	��B	�"B	�7B	�lB	��B	��B	��B	��B	�lB	��B	�1B	�1B	�lB	�	B	�=B	�fB	��B	��B	�YB	�fB	��B	��B	��B	��B	��B	�1B	�1B	�fB	��B	�B	�fB	��B	�fB	��B	��B	�"B	� B	��B	��B	��B	�@B	�B	�YB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�_B	��B	�MB	��B	�B	�B	�@B	�B	�uB	�uB	�FB	��B	�MB	�B	�B	��B	�YB	�B	��B	�FB	�B	��B	�oB	��B	��B	�\B	��B	�B	�bB	��B	�\B	��B	�\B	�B	��B	�kB	��B	�0B	��B	�jB	�B	�<B	�<B	уB	�aB	�B	�&B	�fB	�sB	�B	�jB	��B	ݘB	ݘB	��B	�B	��B	��B	�&B	�B	�B	� B	� B	�NB	�NB	�B	�B	�B	�B	�B	�vB	�B	�/B	�5B	�)B	�yB	�gB	� B	��B	�B	ȴB	��B	��B	�aB	�B	��B	��B	�gB	�mB	�B	ʌB	�#B	��B	�0B	��B	�vB	ΥB	�BB	�BB	��B	�NB	ԕB	�,B	�aB	��B	�#B	�gB	��B	�aB	�,B	�,B	�2B	�2B	�sB	ںB	�jB	�B	�yB	��B	�B	�B	�iB	��B
B
hB
�B
7B
IB
�B
{B
�B
B
B
:B
�B
�B
hB
\B
�B
�B
�B
�B
7B
%FB
"�B
0�B
+6B
B�B
0�B
6�B
)�B
V�B
�B
$B
	B
B
~B
�B
xB
"hB
#B
"hB
+6B
.B
1�B
:�B
F�B
I�B
N�B
T�B
[�B
l�B
qAB
xB
�{B
�_B
�VB
�:B
��B
��B
�B
��B
��B
�IB
��B
�!B
��B
�B
�zB
�jB
��B
�6B
��B
��B
��B
��B
�wB
�jB
�qB
�B
��B
�qB
�OB
��B
�'B
�'B
�3B
�B
��B
ĜB
��B
�EB
�0B
ΥB
��B
֡B
�,B
�
B
�EB
خB
��B
��B
ݘB
��B
�dB
ݘB
ޞB
�pB
�B
ߤB
ܒB
��B
ޞB
�dB
�5B
��B
�5B
ޞB
�dB
�|B
��B
��B
��B
�|B
� B
�B
�B
�B
��B
�B
�HB
�NB
�NB
�B
�B
�B
��B
�HB
�B
�|B
�2B
�B
��B
�&B
�B
�`B
�B
�B
��B
�B
�B
�,B
�B
�B
�QB
��B
�+B
�B
��B
�`B 4B
��BB�B_BfB	�B
=B
	B
=B�B
	B
�B
rB~BBB�BBPB�B�B�BbB�B�B�B�B.B�B�B�B(B�B\B(B�B\B�B\B�B�B�BPB�B�B4B�B \B�BIBBB�B$tB"hB�B&�B�B7BkB�BIB$�B6�B�BYB�B�BBxBVBbB�B�B�B{B�BBMB�B �BhB�B�B
��B
�B
�2B
�JB
��B
��BoB
�rB
�B
�B
��B
�B
��B
�MB
�ZB
�B
��B �B:B�B:B#nB$B.�B,�B<�BHKB]�Bs�B}"B�GB��B�4B�hB��B��B��B�7B��B��B�kB�UB�KB�#B�;B��B�,B�dB�B�,B҉B�B��B��B�tB�dB��BÖB�UB�B�RB�B�?B�UB�-B�IB��B�IB�B�6B�-B��B��B��B�eB�eB��B�B�6B��B��B�B��B��B�LB�B�HB�wB�BB��B�KB��BȴB��B��B�=B�B��B�B��B�kB��B��B�_B��B�B�RB�B��B�B��B�B�$B�kB�eB��B��B�B�{B�_B��B�@B�B��B�YB�B�"B�DB�fB��B�lB��B��B�SB��B��B�SB��B�B��B�MB�YB��B��B��BcB��B}�B{�B|�B{JBx�Bz�Bx�Bx8Bt�By>BoiBqABm�Bs�B^B[�BS[BX�B@�B?�B?HB@�B?�B?�BA�B?B?B=�BQ�BC-B2�B0UB,�B(�B�BB1B�B�B	�B	7B�BB�B
��B
��B�B
�B
�+B
�JB
��B
��B
�B
�|B
�B
��B
��B
��B
�yB
�B
�fB
�8B
�|B
�B
�dB
��B
�KB
�B
�pB
��B
�pB
�jB
��B
�HB
�B
��B
��B
�'B
�}B
�B
�6B
�B
�dB
�*B
�B
ɺB
��B
�B
��B
�}B
��B
�wB
�3B
��B
�kB
��B
��B
��B
�B
��B
�B
��B
��B
�oB
��B
��B
��B
��B
�iB
�;B
��B
��B
� B
~]B
zB
zB
~�B
|�B
v�B
�B
}�B
�hB
oiB
`B
U2B
X�B
_�B
[�B
J�B
zxB
7�B
6�B
$B
@B
�B
B
oB	�fB	��B	��B	�cB	�QB	�TB	ޞB	�B	�,B	�6B	�B	��B	�gB	�B	��B	ÖB	�dB	�B	��B	�B	�XB	��B	�FB	��B	��B	��B	��B	��B	�=B	��B	�XB	�kB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	ѷG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                     B	��B	��B	�NB	��B	�hB	�B	��B	��B	��B	��B	�NB	��B	��B	��B	�B	~�B	��B	�EB	�0B	�B	�lB	��B	�dB	��B	�?B	�yB	��B	�mB	��B	��B	��B	��B	��B	�B	� B	�ZB	�B	�B	�6B	ԕB	�gB	�VB	�B
EB
_B
7�B
8�B
+B
O�B
��B
�dB
��B
��B
��B
�:B
�B
�B
�wB�B�B�B(�B-�BpB�B;B!-B� B��B�!B�EB�RB�-B��B�NB�+B�]B��B�!B��B��B�SB��Bp!BMjBEB�B�B
��B
�;B
յB
��B
��B
�(B
�CB
��B
�}B
u�B
>�B
�B	�B	�MB	��B	��B	��B	�*B	�pB	�XB	� B	zxB	|�B	n}B	��B	�2B	�WB	�hB	��B	�EB	��B	xB	y>B	�iB	s�B	f�B	OB	BB	6+B	,�B	+�B	&B��B�B�BּB�_B��B�PB��B��BŢB��B��B�B��B��B�KB��B��B��B�B��B�nB��B��B�#B�IB��B��B��B�2B�HB�~B�B�B��B��B}�B|�B|BcB�Bx�B~(B��B��B��B��B��B�'B��B�DB�PB�BB�dB�*B�JB�0B��B��B�GB�^B�JB�B��B��B�^B��B��B��B�yB��B��B�B�B�mB��B�B��B�B�1B�1BچB�dB�B�_B��B�tB��B	�B	{B	aB	{B	9B	+B	SB	�B	�B	KB	�B	�B	�B	)B	KB	
�B	<B	B	�B	dB	�B	;B	%zB	.�B	0oB	/�B	7�B	>wB	@�B	@iB	?HB	<B	;�B	=<B	=VB	=B	=VB	EB	F�B	J=B	N�B	OvB	OvB	O�B	PB	S&B	[#B	b�B	d�B	gmB	jKB	jB	l�B	q'B	p�B	q�B	t�B	u�B	xlB	{�B	z�B	{JB	{0B	{JB	|�B	}�B	��B	�aB	��B	�?B	�=B	�~B	�BB	�hB	��B	��B	��B	�B	��B	��B	�'B	�4B	��B	��B	�`B	��B	�*B	��B	��B	��B	��B	��B	��B	�3B	��B	�nB	��B	�LB	��B	�rB	�DB	�B	�xB	�DB	�BB	��B	�MB	��B	��B	āB	�-B	ĜB	�9B	��B	��B	�RB	ȚB	̘B	�<B	ϫB	��B	�B	�xB	ބB	��B	��B	��B	��B	�]B	�\B	��B	��B	��B	�-B	�4B	��B	�ZB	�FB	�B	�B	�B	��B	��B	�qB	�B	��B	�)B	�}B	�}B	�B	��B	�;B	��B	��B	�3B	�B	�ZB	��B	�[B	�GB	�B	��B	�9B	��B	�%B	�zB	�B	�	B	��B	�dB	��B	�B	�jB	�<B	�.B
 OB
�B
oB
�B
uB
�B
�B
�B
�B
�B
B
�B
B
'B
[B
-B
[B
�B
�B
�B
9B
SB
YB
?B
?B
�B
�B
�B
�B
	B
�B
	lB
	�B
	lB
	�B

�B

rB
B
�B
�B
�B
VB
B
PB
�B
�B
<B
�B
�B
VB
�B
�B
�B
�B
HB
4B
NB
bB
bB
hB
�B
�B
�B
oB
�B
�B
B
B
uB
�B
@B
�B
�B
�B
B
�B
FB
�B
gB
MB
�B
�B
�B
+B
�B
�B
�B
1B
�B
1B
B
QB
QB
�B
�B
�B
�B
xB
�B
IB
5B
OB
5B
;B
�B
;B
�B
�B
B
VB
 \B
 �B
!�B
!�B
!�B
"hB
"�B
#B
#TB
#nB
#�B
#�B
$&B
$�B
%,B
%B
%`B
%�B
%,B
$�B
%�B
&�B
'B
'RB
'�B
'�B
'�B
'�B
(
B
(�B
)_B
)�B
)�B
*0B
*B
*�B
)�B
)�B
)*B
(�B
(�B
)*B
(�B
(�B
)B
)B
)yB
*0B
*0B
)�B
*B
+6B
+B
+B
+B
+QB
+�B
,�B
-�B
-wB
.�B
/�B
0UB
0UB
0!B
/�B
.�B
/ B
/5B
/OB
/�B
0;B
/�B
0;B
1'B
1[B
1'B
1�B
2GB
3hB
4�B
4�B
5tB
5�B
6+B
6B
5tB
6+B
6�B
6�B
6�B
6�B
7�B
8RB
8lB
8RB
8�B
9$B
9rB
:B
:�B
:DB
:B
:xB
;dB
;B
;�B
;JB
;�B
;�B
;�B
<jB
<�B
<�B
=B
=�B
>BB
>]B
>�B
>�B
?cB
?cB
@ B
@4B
@iB
@�B
@�B
@�B
@�B
@�B
@�B
AB
AB
@�B
A B
A;B
A�B
BAB
C-B
C�B
C-B
C�B
C�B
C{B
DB
DMB
DgB
C�B
DMB
DB
DMB
DB
DMB
D�B
D�B
ESB
E9B
ESB
E�B
F�B
F�B
F�B
G_B
GEB
G�B
HfB
HB
HB
HfB
H�B
H�B
H�B
I�B
J=B
J�B
J�B
K^B
KDB
K�B
K�B
LB
L�B
MB
M6B
M�B
M�B
N�B
NpB
N�B
N�B
N�B
O(B
O(B
OBB
O�B
O�B
P.B
PbB
PbB
PHB
QhB
QNB
RTB
R�B
RTB
RoB
S[B
R�B
S�B
S�B
S�B
S�B
TB
T{B
T�B
T�B
UMB
T�B
U�B
UgB
U�B
VB
VSB
W
B
W$B
W
B
WsB
V�B
W�B
XB
YB
ZB
ZB
Y�B
ZB
ZQB
[qB
Z�B
[qB
[�B
\]B
\]B
[�B
[�B
\�B
\�B
\�B
]~B
]IB
]�B
^B
^jB
^�B
_B
_�B
_�B
_�B
`�B
`vB
`�B
`�B
a|B
a|B
a|B
a�B
bB
b4B
bhB
b4B
b�B
b�B
c B
c:B
c:B
c�B
c:B
c�B
c�B
d&B
c�B
d@B
d@B
dtB
d�B
d�B
eFB
e,B
eB
e�B
eFB
e�B
fLB
e�B
f2B
f2B
f�B
f�B
f�B
ffB
g8B
g8B
h
B
g�B
h$B
g�B
iB
iB
h�B
h�B
iDB
h�B
h�B
jB
j0B
jB
jKB
kB
kkB
k6B
kQB
kkB
l"B
lqB
l�B
l�B
m�B
nB
m�B
nIB
nB
nIB
n�B
o�B
oiB
o�B
pUB
p!B
o�B
o�B
p!B
p�B
qB
pUB
p�B
qB
q�B
q�B
q'B
q�B
q�B
q�B
q�B
q�B
q[B
rB
r-B
q�B
rB
r�B
r�B
r�B
r�B
r|B
r�B
s3B
s�B
sB
r�B
r�B
s3B
s3B
s�B
tB
s�B
shB
s�B
t�B
t�B
t�B
uB
u�B
utB
uZB
utB
u�B
u?B
u�B
utB
u?B
u?B
vB
v+B
u�B
vB
u�B
u�B
u�B
vB
u�B
vB
v+B
vFB
v�B
v�B
vFB
vzB
w�B
v�B
v�B
w�B
w2B
w�B
wfB
xB
w�B
xB
xB
x8B
x�B
x�B
xlB
x�B
y$B
y�B
y�B
zDB
zDB
z^B
{JB
{�B
|B
|B
{�B
{�B
{dB
{�B
{�B
|6B
|�B
|jB
|�B
|�B
|�B
}"B
}qB
}�B
}�B
}�B
~(B
~�B
~BB
~�B
~�B
cB
B
�B
}B
�B
�B
�B
��B
��B
� B
�oB
��B
��B
� B
��B
�oB
�'B
��B
��B
��B
�B
��B
�-B
�B
��B
�aB
�aB
��B
��B
��B
��B
�MB
��B
��B
��B
�B
��B
�B
�B
�B
��B
��B
�mB
�9B
��B
��B
�mB
��B
��B
�tB
��B
�B
�B
�_G�O�B	�JB	�uB	� B	�hB	��B	��B	��B	�hB	��B	��B	�bB	��B	�bB	��B	�uB	��B	�B	�B	��B	��B	�oB	��B	��B	��B	�VB	�hB	�.B	�4B	�:B	� B	��B	��B	��B	�VB	��B	�.B	�bB	�4B	��B	��B	��B	��B	��B	�"B	�bB	�(B	�VB	��B	�VB	��B	��B	�B	�~B	��B	��B	��B	��B	�B	��B	�1B	�B	��B	�fB	��B	�bB	��B	�lB	�SB	�B	�B	��B	�oB	�AB	�oB	�oB	�{B	� B	~]B	|�B	}�B	{�B	{B	}�B	�4B	~�B	}�B	}�B	|B	{JB	.B	}"B	{B	xB	yrB	y	B	yrB	~(B	|�B	~�B	��B	�B	~�B	�4B	��B	��B	�;B	�B	��B	��B	�B	�AB	�B	�+B	��B	�_B	��B	�"B	�7B	�lB	��B	��B	��B	��B	�lB	��B	�1B	�1B	�lB	�	B	�=B	�fB	��B	��B	�YB	�fB	��B	��B	��B	��B	��B	�1B	�1B	�fB	��B	�B	�fB	��B	�fB	��B	��B	�"B	� B	��B	��B	��B	�@B	�B	�YB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�_B	��B	�MB	��B	�B	�B	�@B	�B	�uB	�uB	�FB	��B	�MB	�B	�B	��B	�YB	�B	��B	�FB	�B	��B	�oB	��B	��B	�\B	��B	�B	�bB	��B	�\B	��B	�\B	�B	��B	�kB	��B	�0B	��B	�jB	�B	�<B	�<B	уB	�aB	�B	�&B	�fB	�sB	�B	�jB	��B	ݘB	ݘB	��B	�B	��B	��B	�&B	�B	�B	� B	� B	�NB	�NB	�B	�B	�B	�B	�B	�vB	�B	�/B	�5B	�)B	�yB	�gB	� B	��B	�B	ȴB	��B	��B	�aB	�B	��B	��B	�gB	�mB	�B	ʌB	�#B	��B	�0B	��B	�vB	ΥB	�BB	�BB	��B	�NB	ԕB	�,B	�aB	��B	�#B	�gB	��B	�aB	�,B	�,B	�2B	�2B	�sB	ںB	�jB	�B	�yB	��B	�B	�B	�iB	��B
B
hB
�B
7B
IB
�B
{B
�B
B
B
:B
�B
�B
hB
\B
�B
�B
�B
�B
7B
%FB
"�B
0�B
+6B
B�B
0�B
6�B
)�B
V�B
�B
$B
	B
B
~B
�B
xB
"hB
#B
"hB
+6B
.B
1�B
:�B
F�B
I�B
N�B
T�B
[�B
l�B
qAB
xB
�{B
�_B
�VB
�:B
��B
��B
�B
��B
��B
�IB
��B
�!B
��B
�B
�zB
�jB
��B
�6B
��B
��B
��B
��B
�wB
�jB
�qB
�B
��B
�qB
�OB
��B
�'B
�'B
�3B
�B
��B
ĜB
��B
�EB
�0B
ΥB
��B
֡B
�,B
�
B
�EB
خB
��B
��B
ݘB
��B
�dB
ݘB
ޞB
�pB
�B
ߤB
ܒB
��B
ޞB
�dB
�5B
��B
�5B
ޞB
�dB
�|B
��B
��B
��B
�|B
� B
�B
�B
�B
��B
�B
�HB
�NB
�NB
�B
�B
�B
��B
�HB
�B
�|B
�2B
�B
��B
�&B
�B
�`B
�B
�B
��B
�B
�B
�,B
�B
�B
�QB
��B
�+B
�B
��B
�`B 4B
��BB�B_BfB	�B
=B
	B
=B�B
	B
�B
rB~BBB�BBPB�B�B�BbB�B�B�B�B.B�B�B�B(B�B\B(B�B\B�B\B�B�B�BPB�B�B4B�B \B�BIBBB�B$tB"hB�B&�B�B7BkB�BIB$�B6�B�BYB�B�BBxBVBbB�B�B�B{B�BBMB�B �BhB�B�B
��B
�B
�2B
�JB
��B
��BoB
�rB
�B
�B
��B
�B
��B
�MB
�ZB
�B
��B �B:B�B:B#nB$B.�B,�B<�BHKB]�Bs�B}"B�GB��B�4B�hB��B��B��B�7B��B��B�kB�UB�KB�#B�;B��B�,B�dB�B�,B҉B�B��B��B�tB�dB��BÖB�UB�B�RB�B�?B�UB�-B�IB��B�IB�B�6B�-B��B��B��B�eB�eB��B�B�6B��B��B�B��B��B�LB�B�HB�wB�BB��B�KB��BȴB��B��B�=B�B��B�B��B�kB��B��B�_B��B�B�RB�B��B�B��B�B�$B�kB�eB��B��B�B�{B�_B��B�@B�B��B�YB�B�"B�DB�fB��B�lB��B��B�SB��B��B�SB��B�B��B�MB�YB��B��B��BcB��B}�B{�B|�B{JBx�Bz�Bx�Bx8Bt�By>BoiBqABm�Bs�B^B[�BS[BX�B@�B?�B?HB@�B?�B?�BA�B?B?B=�BQ�BC-B2�B0UB,�B(�B�BB1B�B�B	�B	7B�BB�B
��B
��B�B
�B
�+B
�JB
��B
��B
�B
�|B
�B
��B
��B
��B
�yB
�B
�fB
�8B
�|B
�B
�dB
��B
�KB
�B
�pB
��B
�pB
�jB
��B
�HB
�B
��B
��B
�'B
�}B
�B
�6B
�B
�dB
�*B
�B
ɺB
��B
�B
��B
�}B
��B
�wB
�3B
��B
�kB
��B
��B
��B
�B
��B
�B
��B
��B
�oB
��B
��B
��B
��B
�iB
�;B
��B
��B
� B
~]B
zB
zB
~�B
|�B
v�B
�B
}�B
�hB
oiB
`B
U2B
X�B
_�B
[�B
J�B
zxB
7�B
6�B
$B
@B
�B
B
oB	�fB	��B	��B	�cB	�QB	�TB	ޞB	�B	�,B	�6B	�B	��B	�gB	�B	��B	ÖB	�dB	�B	��B	�B	�XB	��B	�FB	��B	��B	��B	��B	��B	�=B	��B	�XB	�kB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	ѷG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��@<�3<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<8��<(�?<#�
<#�
<#�
<o�e<��><�E�<k9<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5�7<W'�<%��<#�
<#�
<#�
<#�
<#�
<#�
<2� <Ih�<J�$<a��<.i'<#�
<#�
<8��<#�
<#�
<#�
<adF<#�
<#�
<��u<�<'<�U�<�V�<2+�<#�
<#�
<*�i<�{<8(m<#�
<#�
<#�
<#�
<S�&<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Pq�<A4�<;*M<#�
<#�
<#�
<_�<_�<�J]<#�
<#�
<#�
<#�
<#�
<'�O<#�
<#�
<<�<>.v<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2018082922301120180829223011IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723143520180917231435QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723143520180917231435QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107544620190521075446IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                