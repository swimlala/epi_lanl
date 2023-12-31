CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-23T18:26:48Z creation; 2021-10-15T19:29:27Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  c<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ɛ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � W�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` }�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210223182648  20211015173717  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               i   iAA  AOAO7824_008764_105                 7824_008764_105                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�`��$5i@�`��$5i11  @�`�!�R�@�`�!�R�@6US&��@6US&���d�7KƧ��d�7KƧ�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q?�@=p�@�  @�G�@�G�@�  A ��A��A   A+�A@  A`  A�Q�A���A�  A��A��A�  A߮A�  B (�B(�B  B  B�
B(  B0  B7�
B@  BH(�BP  BW�
B_�
Bg�
Bo�
Bx  B�{B��B��B��B��B�  B�  B�  B�{B�(�B�{B�  B�  B�  B��B�  B�  B�{B�  B��B�  B�  B�{B�  B�  B�  B�{B�{B�  B��B�{B�  C   C  C  C  C  C	��C  C
=C
=C  C��C  C  C  C  C  C 
=C"  C$  C&  C(  C)��C+��C.  C0  C2  C3��C5��C8
=C:
=C<  C>  C@  CB  CC��CE��CG��CI��CK�CN  CP  CR
=CT  CU��CW��CY��C\  C^  C`  Ca��Cc�Ce��Ch  Ci��Ck��Cn  Cp  Cr
=Ct  Cu��Cx  Cy��C{��C}��C��C�  C�C�C�  C�  C���C�  C�C�C�C�C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C�  C�C�  C�  C�C�C�  C�  C�  C�  C���C�C�C�  C�C�  C�  C���C���C�  C�  C�  C�  C�  C�  C���C�C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C���C�  C�C�  C�  C���C���C�  C�  C�  C�  C�C�
=C�C�  C���C�  C�  C�  C�C���C�  C�C�  C���C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�C�
=C�  C�  C���C���C�  C�  C���C���C�  C�  D �D ��D �qD}qD�qD}qD�qD� D  D� D  D� D�D� D  D� D  D��D	�D	� D	�qD
}qD
�qD� D  D� D  D� D  D� D  D� D�qD� D�D��D�D��D�D��D�D� D��D}qD�qD}qD  D��D  D� D  D� D  D}qD�qD� D  D� D  D}qD�qD}qD�qD}qD�qD }qD!  D!� D!�qD"}qD"�qD#}qD#�qD$}qD%  D%��D&D&��D'  D'��D(D(� D(�qD)� D*  D*��D+�D+� D+�qD,}qD-  D-}qD-�qD.��D/�D/� D/�qD0}qD1  D1��D2  D2� D3  D3� D4  D4� D5�D5� D5��D6}qD7  D7� D8  D8� D8�qD9}qD9�qD:� D;�D;� D;�qD<��D=  D=}qD>�D>��D?  D?� D?�qD@� DA  DA}qDB  DB��DC�DC��DC�qDD}qDE  DE� DFDF�DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DK�qDLz�DL�qDM� DN�DN}qDN��DO}qDP  DP}qDP�qDQ� DR  DR� DS  DS}qDS�qDT}qDT�qDU}qDV�DV��DV�qDW� DX  DX}qDY  DY��DZ  DZ� DZ�qD[��D\  D\� D\�qD]}qD^�D^��D_D_� D_�qD`}qDa  Da��Db  Db}qDb�qDc� Dd�Dd�De�De� De�qDf}qDg  Dg}qDg�qDh� Dh�qDi}qDj�Dj��Dk�Dk��Dk�qDl� Dm  Dm� Dn�Dn}qDn�qDo� Dp  Dp��Dq  Dq}qDr  Dr��Dr�qDs� Dt�Dt}qDu  Du��Du�qDvz�Dw  Dw��Dw�qDxz�Dx�qDy� Dy�qDz}qD{  D{}qD|  D|��D}  D}}qD~  D~��D  D}qD��D�>�D�� D���D�  D�AHD�� D���D��qD�>�D�� D���D��qD�@ D��HD�� D���D�>�D�� D��HD�HD�B�D���D��HD�  D�AHD�� D���D���D�@ D�~�D�� D�HD�>�D�~�D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�@ D�� D���D���D�>�D�� D��HD�HD�@ D�� D���D�  D�AHD��HD�� D�  D�>�D�� D�� D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD�D�HD�@ D�� D���D���D�>�D�~�D�� D�  D�AHD�� D���D�  D�@ D�� D��HD���D�@ D�� D���D�  D�AHD�� D���D�  D�@ D�~�D���D���D�>�D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�>�D�� D��HD�  D�>�D�~�D��HD���D�=qD�~�D��HD�HD�AHD��HD��HD��D�@ D�~�D�� D�  D�>�D��HD��HD�HD�AHD�� D���D�  D�AHD�~�D�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D���D�� D�  D�B�D���D��HD�HD�@ D�� D���D���D�>�D�~�D��qD���D�>�D�~�D���D��qD�@ D���D�D�HD�AHD�� D���D�HD�B�D��HD��HD��D�AHD��HD�� D��qD�>�D�~�D�� D�  D�AHD�� D��HD�HD�AHD�� D��qD��qD�>�D�~�D�� D�  D�@ D��HD�D�  D�@ D�� D���D���D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�}qD�� D�HD�@ D�~�D���D�  D�>�D�~�D���D�  D�AHD�� D���D���D�>�D�}qD��qD���D�@ D���D��HD�HD�AHD��HD��HD�  D�>�D�}qD�� D�HD�@ D�~�D½qD���D�@ D�~�Dþ�D���D�@ DāHDľ�D���D�=qD�}qDž�D�HD�@ D�~�D��HD��D�@ Dǀ DǾ�D�HD�@ DȀ D�� D�  D�B�DɁHD��HD�  D�@ D�~�Dʾ�D���D�@ D˂�D��HD�  D�>�D̀ D��HD�HD�@ D̀ D�� D�  D�>�D΀ D��HD�HD�B�D�~�DϽqD��qD�>�DЀ D�� D�HD�B�Dт�D��HD�HD�@ DҀ D�� D���D�@ DӁHD�� D���D�>�DԀ D��HD�HD�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��HD�  D�>�D؀ D�� D�  D�>�D�~�Dپ�D���D�>�D�~�D�� D�  D�AHDۀ D�� D�HD�@ D�~�Dܾ�D�  D�AHD݀ Dݾ�D�HD�@ D�~�D�� D���D�>�D߀ D��HD�HD�@ D�� D�� D�  D�@ D� D��HD�HD�@ D� D⾸D���D�@ D�HD�� D�  D�@ D�~�D�� D�  D�AHD� D��HD�HD�AHD� D�� D�HD�B�D�HD�� D�  D�@ D� D��HD�HD�AHD�~�D�� D�HD�AHD�HD��HD�  D�@ D�~�D뾸D�  D�@ D�~�D�� D�HD�AHD� D���D�  D�AHD�HD�D�HD�AHD� D��HD�HD�@ D�~�D�D�  D�@ D� D�� D�  D�@ D�HD�� D��?B�\?L��?k�?�\)?���?�Q�?\?�ff?�@��@��@&ff@333@E�@W
=@c�
@s33@�  @��@�\)@�@��H@��\@�=q@�\)@�@��H@\@�=q@�\)@�z�@ٙ�@�  @�@�@��@�
=@��RA�A�A�A(�A�RAG�Az�A
=A=qAp�A   A#�
A&ffA(��A.{A0  A3�
A7�A:=qA=p�AAG�ADz�AHQ�AK�AO\)AS33AVffAZ�HA^{Ab�\AeAi��Amp�Ap��AuAx��A}p�A���A�=qA�z�A�{A�  A�=qA��
A�A��A�G�A��A�p�A�
=A���A��\A���A��RA���A��\A�z�A�{A�  A��A��A�p�A�  A��A��
A�A��A���A�33A�p�A��A�G�A�33A��A�\)A�G�A��HA��AθRAУ�A��HA���AָRA�Q�Aڏ\A�(�A�ffA�  A��A�(�A�{A�Q�A��A�(�A�ffA�Q�A�\A���A��RA���A��HA��A�\)B ��BB�RB�
B��B{B
=B(�B	�B
=qB\)B(�Bp�BffB�Bz�BB�HB�
B��B{B33B(�BG�B=qB�B��B��B�RB�B ��B!�B#
=B$(�B%�B&=qB'\)B(z�B)��B*�RB+�
B,��B.{B/
=B0(�B1p�B2ffB3\)B4��B5B6�HB8(�B9�B:=qB;�B<z�B=B>�HB@  B@��BBffBC\)BD��BE��BF�RBH  BH��BJ{BK33BLQ�BM��BN�\BO�BP��BQ�BS33BTQ�BUG�BV=qBW�BX��BY�B[
=B\(�B]G�B^=qB_\)B`��BaBb�HBd  BeG�BfffBg�Bh��Bi�Bj�HBl(�Bmp�BnffBo�Bp��Bq�Bs
=Bt(�Bup�Bv�\Bw�Bx��Bz=qB{\)B|z�B}��B~�RB�
B��\B�
=B���B�=qB��RB�G�B�B�ffB��HB�p�B�{B���B�33B�B�Q�B��HB�p�B�{B���B��B��
B�ffB���B��B�(�B���B�\)B�  B��\B�33B��B�Q�B��HB��B�{B��RB�G�B�  B��\B�
=B��B�Q�B���B��B�(�B��RB�G�B��B�z�B�
=B���B�(�B���B�\)B��B�z�B��B�B�=qB��HB�\)B�  B��\B�33B�B�Q�B��HB�p�B�{B���B�33B�B�ffB�
=B���B�(�B��RB�\)B��B��\B�33B�B�Q�B��HB��B�{B��RB�\)B�  B���B�33B�B�ffB�
=B���B�=qB��HB�p�B�  B���B�\)B�  B���B�33B��
B�z�B�
=B��B�Q�B��HB���B�=qB��HBÅB�{BĸRB�\)B�  BƏ\B�G�B��B�z�B�33B�B�ffB�
=B˙�B�=qB��HBͅB�{BθRB�\)B�  BУ�B�G�B��B�z�B�33BӮB�Q�B��HBՅB�(�B���BׅB�{B؏\B�G�B��B�z�B��BۮB�ffB��HB݅B�(�B޸RB�p�B�  B��\B�33B��
B�ffB�
=B�B�=qB��HB�B�(�B���B�p�B�  B��B�G�B�  B�z�B�33B�B�Q�B�
=B홚B�Q�B��HB�B�{B���B�\)B�{B��B�G�B��B�z�B�
=B�B�=qB���B�\)B��B�z�B��HB�G�B�B�(�B�z�B��HB�33B��B��B�Q�B���B�
=B�\)B��B�  B�ffB���B��B�p�B�C {C G�C z�C ��C ��C ��C�CQ�Cz�C��C��C��C(�CQ�C�C�C�
C  C33C\)Cz�C�C��C  C33CQ�C�C�C�
C  C(�CQ�Cz�C�C�
C  C33C\)C�\C�RC�HC
=C=qCffC��CC��C�CQ�Cz�C�C�
C	  C	33C	\)C	�C	�C	�HC

=C
=qC
ffC
��C
C
��C�CG�Cz�C��CC��C(�CQ�C�C�C�HC
=C=qCffC��CC�C�CQ�Cz�C��C�
C  C33C\)C�C�RC�C{CG�Cp�C��C��C��C�CQ�Cz�C�C�
C��C(�C\)C�C��C�
C
=C(�C\)C�\C�C�HC
=C=qCffC��CC��C(�CQ�C�C�RC�C�CG�Cz�C�C�HC
=CG�Cz�C��C�
C
=C=qCp�C��C��C  C33Cp�C��C��C  C33CffC��C��C��C(�C\)C�\CC��C(�CQ�C�\CC�C(�CQ�C�\CC��C33C\)C��C��C  C33CffC�\C��C 
=C =qC z�C �C �HC!{C!Q�C!�C!�RC!�C"(�C"ffC"��C"��C#
=C#33C#z�C#��C#�HC$�C$Q�C$�\C$�RC$��C%33C%ffC%��C%�
C&{C&G�C&�C&�RC&�C'�C'\)C'�\C'��C(  C(33C(p�C(��C(�HC){C)Q�C)�C)C)��C*33C*p�C*��C*�HC+{C+Q�C+�C+�RC+��C,33C,ffC,��C,�HC-{C-Q�C-�C-C-�C.33C.\)C.��C.��C/  C/=qC/z�C/�C/�HC0{C0Q�C0�\C0C0�C1�C1\)C1�\C1��C1��C233C2p�C2��C2�HC3{C3=qC3z�C3�RC3�C4�C4\)C4�\C4��C5  C533C5p�C5��C5�HC6�C6G�C6�C6�RC6��C733C7ffC7��C7�HC8{C8G�C8�C8�RC8��C933C9ffC9��C9�HC:�C:Q�C:�\C:�RC;  C;33C;p�C;�C;�HC<�C<\)C<�\C<C=
=C=G�C=z�C=�RC=�C>(�C>ffC>��C>�
C?{C?Q�C?�\C?C@
=C@=qC@z�C@�RC@�CA33CAp�CA��CA�CB�CBQ�CB��CB��CC{CC=qCC�CC�RCC��CD33CDp�CD��CD�HCE{CEQ�CE��CE��CF  CFG�CFz�CF�RCF�CG(�CGffCG��CG�
CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                ?�=q?�@=p�@�  @�G�@�G�@�  A ��A��A   A+�A@  A`  A�Q�A���A�  A��A��A�  A߮A�  B (�B(�B  B  B�
B(  B0  B7�
B@  BH(�BP  BW�
B_�
Bg�
Bo�
Bx  B�{B��B��B��B��B�  B�  B�  B�{B�(�B�{B�  B�  B�  B��B�  B�  B�{B�  B��B�  B�  B�{B�  B�  B�  B�{B�{B�  B��B�{B�  C   C  C  C  C  C	��C  C
=C
=C  C��C  C  C  C  C  C 
=C"  C$  C&  C(  C)��C+��C.  C0  C2  C3��C5��C8
=C:
=C<  C>  C@  CB  CC��CE��CG��CI��CK�CN  CP  CR
=CT  CU��CW��CY��C\  C^  C`  Ca��Cc�Ce��Ch  Ci��Ck��Cn  Cp  Cr
=Ct  Cu��Cx  Cy��C{��C}��C��C�  C�C�C�  C�  C���C�  C�C�C�C�C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C�  C�C�  C�  C�C�C�  C�  C�  C�  C���C�C�C�  C�C�  C�  C���C���C�  C�  C�  C�  C�  C�  C���C�C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C���C�  C�C�  C�  C���C���C�  C�  C�  C�  C�C�
=C�C�  C���C�  C�  C�  C�C���C�  C�C�  C���C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�  C�  C�  C�  C�  C�C�
=C�  C�  C���C���C�  C�  C���C���C�  C�  D �D ��D �qD}qD�qD}qD�qD� D  D� D  D� D�D� D  D� D  D��D	�D	� D	�qD
}qD
�qD� D  D� D  D� D  D� D  D� D�qD� D�D��D�D��D�D��D�D� D��D}qD�qD}qD  D��D  D� D  D� D  D}qD�qD� D  D� D  D}qD�qD}qD�qD}qD�qD }qD!  D!� D!�qD"}qD"�qD#}qD#�qD$}qD%  D%��D&D&��D'  D'��D(D(� D(�qD)� D*  D*��D+�D+� D+�qD,}qD-  D-}qD-�qD.��D/�D/� D/�qD0}qD1  D1��D2  D2� D3  D3� D4  D4� D5�D5� D5��D6}qD7  D7� D8  D8� D8�qD9}qD9�qD:� D;�D;� D;�qD<��D=  D=}qD>�D>��D?  D?� D?�qD@� DA  DA}qDB  DB��DC�DC��DC�qDD}qDE  DE� DFDF�DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DK�qDLz�DL�qDM� DN�DN}qDN��DO}qDP  DP}qDP�qDQ� DR  DR� DS  DS}qDS�qDT}qDT�qDU}qDV�DV��DV�qDW� DX  DX}qDY  DY��DZ  DZ� DZ�qD[��D\  D\� D\�qD]}qD^�D^��D_D_� D_�qD`}qDa  Da��Db  Db}qDb�qDc� Dd�Dd�De�De� De�qDf}qDg  Dg}qDg�qDh� Dh�qDi}qDj�Dj��Dk�Dk��Dk�qDl� Dm  Dm� Dn�Dn}qDn�qDo� Dp  Dp��Dq  Dq}qDr  Dr��Dr�qDs� Dt�Dt}qDu  Du��Du�qDvz�Dw  Dw��Dw�qDxz�Dx�qDy� Dy�qDz}qD{  D{}qD|  D|��D}  D}}qD~  D~��D  D}qD��D�>�D�� D���D�  D�AHD�� D���D��qD�>�D�� D���D��qD�@ D��HD�� D���D�>�D�� D��HD�HD�B�D���D��HD�  D�AHD�� D���D���D�@ D�~�D�� D�HD�>�D�~�D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�@ D�� D���D���D�>�D�� D��HD�HD�@ D�� D���D�  D�AHD��HD�� D�  D�>�D�� D�� D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD�D�HD�@ D�� D���D���D�>�D�~�D�� D�  D�AHD�� D���D�  D�@ D�� D��HD���D�@ D�� D���D�  D�AHD�� D���D�  D�@ D�~�D���D���D�>�D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�>�D�� D��HD�  D�>�D�~�D��HD���D�=qD�~�D��HD�HD�AHD��HD��HD��D�@ D�~�D�� D�  D�>�D��HD��HD�HD�AHD�� D���D�  D�AHD�~�D�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D���D�� D�  D�B�D���D��HD�HD�@ D�� D���D���D�>�D�~�D��qD���D�>�D�~�D���D��qD�@ D���D�D�HD�AHD�� D���D�HD�B�D��HD��HD��D�AHD��HD�� D��qD�>�D�~�D�� D�  D�AHD�� D��HD�HD�AHD�� D��qD��qD�>�D�~�D�� D�  D�@ D��HD�D�  D�@ D�� D���D���D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�}qD�� D�HD�@ D�~�D���D�  D�>�D�~�D���D�  D�AHD�� D���D���D�>�D�}qD��qD���D�@ D���D��HD�HD�AHD��HD��HD�  D�>�D�}qD�� D�HD�@ D�~�D½qD���D�@ D�~�Dþ�D���D�@ DāHDľ�D���D�=qD�}qDž�D�HD�@ D�~�D��HD��D�@ Dǀ DǾ�D�HD�@ DȀ D�� D�  D�B�DɁHD��HD�  D�@ D�~�Dʾ�D���D�@ D˂�D��HD�  D�>�D̀ D��HD�HD�@ D̀ D�� D�  D�>�D΀ D��HD�HD�B�D�~�DϽqD��qD�>�DЀ D�� D�HD�B�Dт�D��HD�HD�@ DҀ D�� D���D�@ DӁHD�� D���D�>�DԀ D��HD�HD�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��HD�  D�>�D؀ D�� D�  D�>�D�~�Dپ�D���D�>�D�~�D�� D�  D�AHDۀ D�� D�HD�@ D�~�Dܾ�D�  D�AHD݀ Dݾ�D�HD�@ D�~�D�� D���D�>�D߀ D��HD�HD�@ D�� D�� D�  D�@ D� D��HD�HD�@ D� D⾸D���D�@ D�HD�� D�  D�@ D�~�D�� D�  D�AHD� D��HD�HD�AHD� D�� D�HD�B�D�HD�� D�  D�@ D� D��HD�HD�AHD�~�D�� D�HD�AHD�HD��HD�  D�@ D�~�D뾸D�  D�@ D�~�D�� D�HD�AHD� D���D�  D�AHD�HD�D�HD�AHD� D��HD�HD�@ D�~�D�D�  D�@ D� D�� D�  D�@ D�HD�� G�O�?B�\?L��?k�?�\)?���?�Q�?\?�ff?�@��@��@&ff@333@E�@W
=@c�
@s33@�  @��@�\)@�@��H@��\@�=q@�\)@�@��H@\@�=q@�\)@�z�@ٙ�@�  @�@�@��@�
=@��RA�A�A�A(�A�RAG�Az�A
=A=qAp�A   A#�
A&ffA(��A.{A0  A3�
A7�A:=qA=p�AAG�ADz�AHQ�AK�AO\)AS33AVffAZ�HA^{Ab�\AeAi��Amp�Ap��AuAx��A}p�A���A�=qA�z�A�{A�  A�=qA��
A�A��A�G�A��A�p�A�
=A���A��\A���A��RA���A��\A�z�A�{A�  A��A��A�p�A�  A��A��
A�A��A���A�33A�p�A��A�G�A�33A��A�\)A�G�A��HA��AθRAУ�A��HA���AָRA�Q�Aڏ\A�(�A�ffA�  A��A�(�A�{A�Q�A��A�(�A�ffA�Q�A�\A���A��RA���A��HA��A�\)B ��BB�RB�
B��B{B
=B(�B	�B
=qB\)B(�Bp�BffB�Bz�BB�HB�
B��B{B33B(�BG�B=qB�B��B��B�RB�B ��B!�B#
=B$(�B%�B&=qB'\)B(z�B)��B*�RB+�
B,��B.{B/
=B0(�B1p�B2ffB3\)B4��B5B6�HB8(�B9�B:=qB;�B<z�B=B>�HB@  B@��BBffBC\)BD��BE��BF�RBH  BH��BJ{BK33BLQ�BM��BN�\BO�BP��BQ�BS33BTQ�BUG�BV=qBW�BX��BY�B[
=B\(�B]G�B^=qB_\)B`��BaBb�HBd  BeG�BfffBg�Bh��Bi�Bj�HBl(�Bmp�BnffBo�Bp��Bq�Bs
=Bt(�Bup�Bv�\Bw�Bx��Bz=qB{\)B|z�B}��B~�RB�
B��\B�
=B���B�=qB��RB�G�B�B�ffB��HB�p�B�{B���B�33B�B�Q�B��HB�p�B�{B���B��B��
B�ffB���B��B�(�B���B�\)B�  B��\B�33B��B�Q�B��HB��B�{B��RB�G�B�  B��\B�
=B��B�Q�B���B��B�(�B��RB�G�B��B�z�B�
=B���B�(�B���B�\)B��B�z�B��B�B�=qB��HB�\)B�  B��\B�33B�B�Q�B��HB�p�B�{B���B�33B�B�ffB�
=B���B�(�B��RB�\)B��B��\B�33B�B�Q�B��HB��B�{B��RB�\)B�  B���B�33B�B�ffB�
=B���B�=qB��HB�p�B�  B���B�\)B�  B���B�33B��
B�z�B�
=B��B�Q�B��HB���B�=qB��HBÅB�{BĸRB�\)B�  BƏ\B�G�B��B�z�B�33B�B�ffB�
=B˙�B�=qB��HBͅB�{BθRB�\)B�  BУ�B�G�B��B�z�B�33BӮB�Q�B��HBՅB�(�B���BׅB�{B؏\B�G�B��B�z�B��BۮB�ffB��HB݅B�(�B޸RB�p�B�  B��\B�33B��
B�ffB�
=B�B�=qB��HB�B�(�B���B�p�B�  B��B�G�B�  B�z�B�33B�B�Q�B�
=B홚B�Q�B��HB�B�{B���B�\)B�{B��B�G�B��B�z�B�
=B�B�=qB���B�\)B��B�z�B��HB�G�B�B�(�B�z�B��HB�33B��B��B�Q�B���B�
=B�\)B��B�  B�ffB���B��B�p�B�C {C G�C z�C ��C ��C ��C�CQ�Cz�C��C��C��C(�CQ�C�C�C�
C  C33C\)Cz�C�C��C  C33CQ�C�C�C�
C  C(�CQ�Cz�C�C�
C  C33C\)C�\C�RC�HC
=C=qCffC��CC��C�CQ�Cz�C�C�
C	  C	33C	\)C	�C	�C	�HC

=C
=qC
ffC
��C
C
��C�CG�Cz�C��CC��C(�CQ�C�C�C�HC
=C=qCffC��CC�C�CQ�Cz�C��C�
C  C33C\)C�C�RC�C{CG�Cp�C��C��C��C�CQ�Cz�C�C�
C��C(�C\)C�C��C�
C
=C(�C\)C�\C�C�HC
=C=qCffC��CC��C(�CQ�C�C�RC�C�CG�Cz�C�C�HC
=CG�Cz�C��C�
C
=C=qCp�C��C��C  C33Cp�C��C��C  C33CffC��C��C��C(�C\)C�\CC��C(�CQ�C�\CC�C(�CQ�C�\CC��C33C\)C��C��C  C33CffC�\C��C 
=C =qC z�C �C �HC!{C!Q�C!�C!�RC!�C"(�C"ffC"��C"��C#
=C#33C#z�C#��C#�HC$�C$Q�C$�\C$�RC$��C%33C%ffC%��C%�
C&{C&G�C&�C&�RC&�C'�C'\)C'�\C'��C(  C(33C(p�C(��C(�HC){C)Q�C)�C)C)��C*33C*p�C*��C*�HC+{C+Q�C+�C+�RC+��C,33C,ffC,��C,�HC-{C-Q�C-�C-C-�C.33C.\)C.��C.��C/  C/=qC/z�C/�C/�HC0{C0Q�C0�\C0C0�C1�C1\)C1�\C1��C1��C233C2p�C2��C2�HC3{C3=qC3z�C3�RC3�C4�C4\)C4�\C4��C5  C533C5p�C5��C5�HC6�C6G�C6�C6�RC6��C733C7ffC7��C7�HC8{C8G�C8�C8�RC8��C933C9ffC9��C9�HC:�C:Q�C:�\C:�RC;  C;33C;p�C;�C;�HC<�C<\)C<�\C<C=
=C=G�C=z�C=�RC=�C>(�C>ffC>��C>�
C?{C?Q�C?�\C?C@
=C@=qC@z�C@�RC@�CA33CAp�CA��CA�CB�CBQ�CB��CB��CC{CC=qCC�CC�RCC��CD33CDp�CD��CD�HCE{CEQ�CE��CE��CF  CFG�CFz�CF�RCF�CG(�CGffCG��CG�
CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AÕ�AÓuAá�Aß�AÝ�AÝ�Aß�Aá�Aá�Aã�Aã�Aá�Aã�A�x�A�1'A��;A�v�A���A�n�A�{A�JA��A�A��#A���A��FA�K�A�bA���A�A���A��-A���A�jA�?}A�  A���A�ZA�  A��PA��A��uA�dZA�=qA���A���A�K�A���A���A�;dA�A���A�A��#A���A���A��;A��
A��DA�r�A�ffA��A�  A�"�A��#A��-A��A���A��
A���A�p�A��`A���A��A�r�A���A��A��A��7A�ffA�|�A�x�A�XA��/A�;dA��A�bA�\)A��A��`A�VA��hA���A��A��uA�`BA��/A���A���A��jA�A���A�5?A�G�A��RA���A|��A{�
Az��Ay�-Ax��Ax��Aw�;Av9XAtĜAr�!Ap�!Ao`BAk��Aj��Ai��Ah�Af��Ae�7Ad-Ac��Ac"�Ab�Aa?}A_��A^�A^~�A^JA]�TA]��A]�wA\��A[�#AW�AT��ARbAPn�ANI�ALȴAJ��AJ1AI;dAH$�AE��AD�`AD$�AC&�AB9XAA+A@1A>�!A=��A=K�A=oA<�jA<ffA;�TA;K�A9�A9
=A8ffA7�wA7C�A5�
A4�uA3�A3hsA3VA2��A0�jA.�\A-�#A,M�A+XA*��A)��A)&�A't�A%�A%x�A$E�A"�A!�;A �jA�DA�A%A�RA��AhsA��A��A%AG�A�A`BA$�A?}A��A+AbNAhsA�RAJA33AE�A�TA\)A
�\A	��A	p�A�HA�A(�A�^A��A�hAVA�+A��A��A�7A;dA ��@��m@�=q@���@�1'@��R@��7@�A�@���@�J@��/@�R@�^5@���@���@�O�@�bN@�"�@��@�r�@�@�o@�J@��@�@�-@�j@�A�@��;@�ff@ݲ-@�p�@ܣ�@�1'@ڸR@ّh@���@�b@֏\@�j@�-@�X@�V@���@�r�@�l�@ΰ!@͡�@̣�@�ƨ@�33@ʏ\@��#@�/@�1@�|�@��H@�^5@�J@�X@���@��@��H@�J@�X@��j@�b@�dZ@��@�@���@��#@���@�7L@�%@���@��@���@�=q@�hs@��D@�l�@��@�-@��h@��@��j@���@�(�@���@�K�@�"�@�ȴ@��@��`@�z�@�b@��
@��@��H@�@�x�@�G�@�&�@��@��/@��u@�z�@� �@��P@��y@��@��@�O�@�7L@��@�%@���@��@��@��@�t�@�;d@���@��y@��\@�-@�@���@���@��T@���@��^@���@�ƨ@�"�@��!@��\@�n�@�ff@�M�@�M�@�E�@���@��7@��@��D@� �@��F@��P@�K�@�ff@��@���@���@�p�@�G�@�%@��@�Q�@���@�E�@��/@��D@�dZ@�-@��#@���@���@���@��#@��#@���@��7@��7@�X@��@�Ĝ@�r�@�1'@���@�S�@��@�ff@���@�@���@��7@�hs@�G�@�&�@��j@�9X@�  @���@���@���@�C�@��@��@��\@�V@���@���@��h@��7@�x�@�hs@�O�@��/@��u@�A�@� �@�ƨ@�|�@�\)@��@�
=@��R@�V@���@��@��@��T@�@��^@���@�x�@�X@�&�@�V@��@�z�@�I�@�9X@�(�@�1@��;@��F@���@��P@�"�@���@��y@�ȴ@��\@�5?@��@��^@���@���@�hs@��@���@���@��9@���@�Q�@�9X@� �@��;@��P@��P@��P@���@��P@�l�@�\)@�\)@�o@���@�~�@��@��T@�@��-@���@�p�@�/@���@���@�z�@�j@�Q�@�A�@�b@��@;d@
=@
=@~��@~�@~��@~V@}�@}@}/@|�j@|�@{��@{��@{"�@z�H@z�!@zn�@zM�@z=q@zJ@y��@y��@y��@yx�@yX@x�u@x1'@x  @w��@v��@v�@vv�@v$�@v@u��@u��@u��@u�@up�@u?}@u�@t�/@tj@t�@s��@r�H@r��@r~�@r-@rJ@r~�@r=q@q��@q&�@p�9@pbN@o�@o��@oK�@n�y@nv�@nV@m�@mp�@mV@l�/@l�@l�@lz�@lZ@l�@k�F@k��@l�@kC�@j�!@j�H@j��@i�@h�u@g��@gl�@gK�@f��@f��@f{@e�T@e�-@e?}@e�@d�j@d1@c�F@ct�@cS�@cC�@c@b��@bJ@a�#@a�^@a7L@`Ĝ@`r�@` �@`  @_�;@_�@_�P@_+@^�y@^�@^�R@^��@^��@^�R@^��@^ff@]O�@\�/@\�@\I�@\9X@[��@[S�@Z�@Z��@Z��@Z~�@Z=q@Y��@X��@XbN@X  @W��@W�@W|�@W\)@V��@V��@Vv�@VE�@U�@U?}@UV@T�/@T�D@T9X@T�@Sƨ@S��@S��@SC�@S@R�H@R��@R^5@QG�@Q%@Q%@P��@P�`@P��@P�@PQ�@P �@O�;@O�w@O|�@O�@N��@Nff@N@M@M�@MO�@MV@Lj@Lj@Lj@LZ@Lj@K�
@J^5@J�@JJ@Ix�@I&�@HĜ@H��@H��@Hr�@H  @G�w@GK�@F�@F��@Fff@E�T@E@E`B@D�/@Dz�@D(�@C�F@Ct�@CdZ@CC�@C@B��@B��@B�@A�^@A�7@AG�@@�@@Q�@@1'@@ �@@  @?�@?+@>��@>�R@>�+@>@=@=@=@=��@=�h@=�@=�@<��@<�@<��@<�@<�D@<z�@<9X@;��@;��@;o@:��@:=q@:�@:-@:M�@:-@:�@:J@9��@9x�@9X@9&�@9�@9%@8�`@8A�@7�@7��@7l�@6��@6�R@6ff@6V@5@5�-@5��@5�@5`B@4�j@4z�@4I�@3ƨ@3t�@3S�@3@2n�@2^5@2�@1��@1x�@17L@0�`@0��@0r�@0 �@/�w@/�P@/\)@/�@/�@/
=@/
=@.��@-@-��@-�@-p�@-`B@-/@,�@,z�@,1@+dZ@*�!@*~�@*M�@)x�@(��@(1'@(1'@(b@'�@'�;@'�w@'��@'|�@'l�@'+@&�y@&��@&{@%�h@%�@$�j@$j@$I�@$1@#�
@#ƨ@#�F@#��@#��@#�@#�@#"�@"�H@"��@"��@"J@!�^@ ��@ bN@ b@�w@�@��@�P@ȴ@V@��@��@p�@�@��@9X@1@��@�m@ƨ@C�@�!@n�@M�@M�@-@-@�@��@��@�^@hs@X@G�@7L@&�@�@%@Ĝ@r�@  @��@�P@��@�R@��@v�@v�@ff@ff@ff@V@5?@$�@{@��@�@Z@�@��@�
@�F@��@�@dZ@S�@"�@@�@��@��@~�@J@�7@7L@��@Ĝ@Ĝ@��@��@r�@r�@1'@�;@�w@�@|�@l�@
=@ȴ@��@V@5?@@�-@�h@`B@O�@?}@�@�/@��@��@��@z�@z�@z�@(�@dZ@S�@33@
��@
�\@
=q@
�@	��@	�@	�#@	��@	�^AÍPAÏ\AÛ�A×�AÓuAÑhAÏ\AÑhAÏ\AÑhAÝ�Aß�Aá�Aá�Aß�AÝ�Aß�Aá�Aá�Aá�AÝ�AÛ�AÝ�AÝ�AÛ�AÙ�AÙ�AÝ�Aß�Aß�AÝ�AÝ�Aá�Aá�Aá�Aß�Aß�Aã�Aã�Aá�Aß�Aß�Aá�Aã�Aã�Aß�Aá�Aå�Aå�Aå�Aã�Aá�Aã�Aå�Aç�Aã�Aã�Aá�Aã�Aá�Aã�AÝ�AÛ�Aã�Aç�Aå�Aã�Aß�Aá�Aã�Aå�AÛ�AÓuAÑhAËDA�ffA�`BA�bNA�dZA�hsA�bNA�`BA�K�A�-A�$�A��A�JA�
=A�A��A��A��A���A���A���A�ĜA®AhA+A�~�AAA�r�A�dZA�%A�  A���A��A��#A��wA��-A��A���A��\A��DA��A�l�A�ffA�\)A�VA�K�A�A�A�5?A�(�A�"�A��A�oA�A��#A���A���A�ZA�-A�bA��#A���A�Q�A�5?A�-A�&�A�"�A��A�JA�VA�
=A�%A�A�%A�A�  A���A���A��A��TA��HA��#A���A��
A���A���A���A���A���A���A���A���A�ĜA�ƨA���A��wA��wA��9A���A���A���A��7A�Q�A�A�A�9XA�1'A�+A�"�A��A� �A��A�{A�1A���A��`A���A���A���A�A�A�ĜA�ƨA�ĜA�ĜA���A���A���A�A�ĜA�ĜA�ĜA�A���A��jA��^A��jA��jA��jA��RA��9A��9A��A���A���A���A���A���A���A��hA��7A��+A��A�v�A�ffA�^5A�ZA�ZA�ZA�XA�O�A�A�A�33A�+A�"�A��A�oA�  A���A���A���A��yA���A��RA��A���A���A��DA�t�A�n�A�l�A�ffA�`BA�ZA�K�A�;dA�5?A�(�A�{A�VA�  A��`A���A��PA�dZA�1'A�JA��!A���A���A��!A��7A��A�  A��A��HA�ƨA���A��9A��uA��7A��A�v�A�r�A�t�A�p�A�jA�ffA�\)A�S�A�Q�A�O�A�E�A�G�A�;dA�5?A�+A�bA�VA�A���A���A��A��A��A��TA��;A��#A���A�ƨA��!A���A��uA��DA��DA��hA�M�A�ZA�1'A�$�A�{A��yA���A���A���A��^A��FA��FA��A��A�v�A�\)A�XA�I�A�=qA�33A�-A�&�A�"�A�oA��A���A��A��A��`A��/A�ĜA��^A���A�|�A�&�A�oA�JA�1A�A�A���A��;A��HA��/A��;A��#A���A��
A���A���A�ĜA���A��7A�^5A��A�{A�
=A���A��A��A��`A��HA��HA��;A��/A��;A��;A��#A��;A��#A���A���A���A���A��wA��RA���A��\A�jA�E�A��yA���A�|�A�ffA�VA�G�A�9XA��A�
=A��yA�?}A��A�XA�A���A��uA��A�v�A�`BA�9XA���A���A�ffA���A�C�A���A��\A�7LA�"�A�{A�JA�A���A��yA��;A��#A��A���A���A���A��FA��A��FA��9A���A���A���A��\A��+A�x�A�jA�5?A�(�A���A��-A�t�A�ffA�bNA�XA�Q�A�G�A�=qA�9XA�&�A�  A��;A�A���A��uA�p�A�ZA�&�A�bA�1A���A��A���A�ĜA���A���A��DA��+A��A��A��A�|�A�~�A�z�A�x�A�p�A�hsA�`BA�\)A�Q�A�G�A�7LA�&�A�$�A� �A��A�ĜA��wA��A��A���A���A���A���A���A���A���A�|�A�dZA��A���A���A�?}A��`A���A��jA�x�A�G�A��A�A��A���A���A�ĜA��^A��-A���A���A���A���A��DA��+A�n�A�VA�9XA�"�A�A��yA��A���A��FA���A���A��hA�z�A�bNA�VA�O�A�A�A�/A� �A���A���A���A�|�A�p�A�bNA�ZA�S�A�C�A�?}A�+A�$�A�JA���A��A��
A���A���A��jA��!A���A���A���A��uA��\A��A�v�A�v�A�t�A�t�A�t�A�hsA�`BA�Q�A�C�A�oA���A�p�A�"�A�p�A�O�A�A�A��#A���A�v�A���A�~�A�dZA�O�A�9XA���A���A��DA�=qA�  A��^A�~�A�n�A�ZA�O�A�K�A�7LA�bA���A��9A���A��+A�^5A�A�A�7LA��A�{A�oA�%A�A�A�%A�  A�  A���A���A��A��A�jA�ffA�K�A�G�A�A�A�9XA�9XA�-A�(�A�"�A��A��A�bA�
=A�A��`A��/A��A���A��A��uA�z�A�\)A�+A���A��TA���A�A��jA��9A��!A���A�|�A�`BA�K�A�=qA�"�A�bA�%A�A���A��A��A��A��A��A��A��A��A��yA��yA��mA��mA��`A��TA��HA��/A��#A�ƨA��-A���A�bNA�&�A��A���A��RA���A���A���A���A���A��uA��PA��A�n�A�7LA��A�ƨA���A��uA��+A�r�A�ffA�VA�O�A�I�A�I�A�C�A�;dA�
=A�A��A�S�A�/A��A�1A��yA�ĜA���A�p�A�G�A��A���A��yA��TA���A��+A�t�A�XA��A��;A�A���A�v�A�C�A��A��A���A���A�t�A�I�A��A��A���A�VA�$�A���A���A�G�A�$�A�VA�  A��yA�ƨA��9A���A��PA�z�A�hsA�XA�33A�oA���A��TA���A���A�XA��A��;A���A���A�l�A�/A��mA��\A�=qA��A��wA�p�A�+A�bA��A���A��jA��\A�O�A�1'A��mA�|�A�I�A��
A���A���A���A��hA�~�A�t�A�`BA�M�A�/A��A�A���A��A��`A���A�A��!A���A���A��uA��A�jA�^5A�E�A�(�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                AÕ�AÓuAá�Aß�AÝ�AÝ�Aß�Aá�Aá�Aã�Aã�Aá�Aã�A�x�A�1'A��;A�v�A���A�n�A�{A�JA��A�A��#A���A��FA�K�A�bA���A�A���A��-A���A�jA�?}A�  A���A�ZA�  A��PA��A��uA�dZA�=qA���A���A�K�A���A���A�;dA�A���A�A��#A���A���A��;A��
A��DA�r�A�ffA��A�  A�"�A��#A��-A��A���A��
A���A�p�A��`A���A��A�r�A���A��A��A��7A�ffA�|�A�x�A�XA��/A�;dA��A�bA�\)A��A��`A�VA��hA���A��A��uA�`BA��/A���A���A��jA�A���A�5?A�G�A��RA���A|��A{�
Az��Ay�-Ax��Ax��Aw�;Av9XAtĜAr�!Ap�!Ao`BAk��Aj��Ai��Ah�Af��Ae�7Ad-Ac��Ac"�Ab�Aa?}A_��A^�A^~�A^JA]�TA]��A]�wA\��A[�#AW�AT��ARbAPn�ANI�ALȴAJ��AJ1AI;dAH$�AE��AD�`AD$�AC&�AB9XAA+A@1A>�!A=��A=K�A=oA<�jA<ffA;�TA;K�A9�A9
=A8ffA7�wA7C�A5�
A4�uA3�A3hsA3VA2��A0�jA.�\A-�#A,M�A+XA*��A)��A)&�A't�A%�A%x�A$E�A"�A!�;A �jA�DA�A%A�RA��AhsA��A��A%AG�A�A`BA$�A?}A��A+AbNAhsA�RAJA33AE�A�TA\)A
�\A	��A	p�A�HA�A(�A�^A��A�hAVA�+A��A��A�7A;dA ��@��m@�=q@���@�1'@��R@��7@�A�@���@�J@��/@�R@�^5@���@���@�O�@�bN@�"�@��@�r�@�@�o@�J@��@�@�-@�j@�A�@��;@�ff@ݲ-@�p�@ܣ�@�1'@ڸR@ّh@���@�b@֏\@�j@�-@�X@�V@���@�r�@�l�@ΰ!@͡�@̣�@�ƨ@�33@ʏ\@��#@�/@�1@�|�@��H@�^5@�J@�X@���@��@��H@�J@�X@��j@�b@�dZ@��@�@���@��#@���@�7L@�%@���@��@���@�=q@�hs@��D@�l�@��@�-@��h@��@��j@���@�(�@���@�K�@�"�@�ȴ@��@��`@�z�@�b@��
@��@��H@�@�x�@�G�@�&�@��@��/@��u@�z�@� �@��P@��y@��@��@�O�@�7L@��@�%@���@��@��@��@�t�@�;d@���@��y@��\@�-@�@���@���@��T@���@��^@���@�ƨ@�"�@��!@��\@�n�@�ff@�M�@�M�@�E�@���@��7@��@��D@� �@��F@��P@�K�@�ff@��@���@���@�p�@�G�@�%@��@�Q�@���@�E�@��/@��D@�dZ@�-@��#@���@���@���@��#@��#@���@��7@��7@�X@��@�Ĝ@�r�@�1'@���@�S�@��@�ff@���@�@���@��7@�hs@�G�@�&�@��j@�9X@�  @���@���@���@�C�@��@��@��\@�V@���@���@��h@��7@�x�@�hs@�O�@��/@��u@�A�@� �@�ƨ@�|�@�\)@��@�
=@��R@�V@���@��@��@��T@�@��^@���@�x�@�X@�&�@�V@��@�z�@�I�@�9X@�(�@�1@��;@��F@���@��P@�"�@���@��y@�ȴ@��\@�5?@��@��^@���@���@�hs@��@���@���@��9@���@�Q�@�9X@� �@��;@��P@��P@��P@���@��P@�l�@�\)@�\)@�o@���@�~�@��@��T@�@��-@���@�p�@�/@���@���@�z�@�j@�Q�@�A�@�b@��@;d@
=@
=@~��@~�@~��@~V@}�@}@}/@|�j@|�@{��@{��@{"�@z�H@z�!@zn�@zM�@z=q@zJ@y��@y��@y��@yx�@yX@x�u@x1'@x  @w��@v��@v�@vv�@v$�@v@u��@u��@u��@u�@up�@u?}@u�@t�/@tj@t�@s��@r�H@r��@r~�@r-@rJ@r~�@r=q@q��@q&�@p�9@pbN@o�@o��@oK�@n�y@nv�@nV@m�@mp�@mV@l�/@l�@l�@lz�@lZ@l�@k�F@k��@l�@kC�@j�!@j�H@j��@i�@h�u@g��@gl�@gK�@f��@f��@f{@e�T@e�-@e?}@e�@d�j@d1@c�F@ct�@cS�@cC�@c@b��@bJ@a�#@a�^@a7L@`Ĝ@`r�@` �@`  @_�;@_�@_�P@_+@^�y@^�@^�R@^��@^��@^�R@^��@^ff@]O�@\�/@\�@\I�@\9X@[��@[S�@Z�@Z��@Z��@Z~�@Z=q@Y��@X��@XbN@X  @W��@W�@W|�@W\)@V��@V��@Vv�@VE�@U�@U?}@UV@T�/@T�D@T9X@T�@Sƨ@S��@S��@SC�@S@R�H@R��@R^5@QG�@Q%@Q%@P��@P�`@P��@P�@PQ�@P �@O�;@O�w@O|�@O�@N��@Nff@N@M@M�@MO�@MV@Lj@Lj@Lj@LZ@Lj@K�
@J^5@J�@JJ@Ix�@I&�@HĜ@H��@H��@Hr�@H  @G�w@GK�@F�@F��@Fff@E�T@E@E`B@D�/@Dz�@D(�@C�F@Ct�@CdZ@CC�@C@B��@B��@B�@A�^@A�7@AG�@@�@@Q�@@1'@@ �@@  @?�@?+@>��@>�R@>�+@>@=@=@=@=��@=�h@=�@=�@<��@<�@<��@<�@<�D@<z�@<9X@;��@;��@;o@:��@:=q@:�@:-@:M�@:-@:�@:J@9��@9x�@9X@9&�@9�@9%@8�`@8A�@7�@7��@7l�@6��@6�R@6ff@6V@5@5�-@5��@5�@5`B@4�j@4z�@4I�@3ƨ@3t�@3S�@3@2n�@2^5@2�@1��@1x�@17L@0�`@0��@0r�@0 �@/�w@/�P@/\)@/�@/�@/
=@/
=@.��@-@-��@-�@-p�@-`B@-/@,�@,z�@,1@+dZ@*�!@*~�@*M�@)x�@(��@(1'@(1'@(b@'�@'�;@'�w@'��@'|�@'l�@'+@&�y@&��@&{@%�h@%�@$�j@$j@$I�@$1@#�
@#ƨ@#�F@#��@#��@#�@#�@#"�@"�H@"��@"��@"J@!�^@ ��@ bN@ b@�w@�@��@�P@ȴ@V@��@��@p�@�@��@9X@1@��@�m@ƨ@C�@�!@n�@M�@M�@-@-@�@��@��@�^@hs@X@G�@7L@&�@�@%@Ĝ@r�@  @��@�P@��@�R@��@v�@v�@ff@ff@ff@V@5?@$�@{@��@�@Z@�@��@�
@�F@��@�@dZ@S�@"�@@�@��@��@~�@J@�7@7L@��@Ĝ@Ĝ@��@��@r�@r�@1'@�;@�w@�@|�@l�@
=@ȴ@��@V@5?@@�-@�h@`B@O�@?}@�@�/@��@��@��@z�@z�@z�@(�@dZ@S�@33@
��@
�\@
=q@
�@	��@	�@	�#@	��G�O�AÍPAÏ\AÛ�A×�AÓuAÑhAÏ\AÑhAÏ\AÑhAÝ�Aß�Aá�Aá�Aß�AÝ�Aß�Aá�Aá�Aá�AÝ�AÛ�AÝ�AÝ�AÛ�AÙ�AÙ�AÝ�Aß�Aß�AÝ�AÝ�Aá�Aá�Aá�Aß�Aß�Aã�Aã�Aá�Aß�Aß�Aá�Aã�Aã�Aß�Aá�Aå�Aå�Aå�Aã�Aá�Aã�Aå�Aç�Aã�Aã�Aá�Aã�Aá�Aã�AÝ�AÛ�Aã�Aç�Aå�Aã�Aß�Aá�Aã�Aå�AÛ�AÓuAÑhAËDA�ffA�`BA�bNA�dZA�hsA�bNA�`BA�K�A�-A�$�A��A�JA�
=A�A��A��A��A���A���A���A�ĜA®AhA+A�~�AAA�r�A�dZA�%A�  A���A��A��#A��wA��-A��A���A��\A��DA��A�l�A�ffA�\)A�VA�K�A�A�A�5?A�(�A�"�A��A�oA�A��#A���A���A�ZA�-A�bA��#A���A�Q�A�5?A�-A�&�A�"�A��A�JA�VA�
=A�%A�A�%A�A�  A���A���A��A��TA��HA��#A���A��
A���A���A���A���A���A���A���A���A�ĜA�ƨA���A��wA��wA��9A���A���A���A��7A�Q�A�A�A�9XA�1'A�+A�"�A��A� �A��A�{A�1A���A��`A���A���A���A�A�A�ĜA�ƨA�ĜA�ĜA���A���A���A�A�ĜA�ĜA�ĜA�A���A��jA��^A��jA��jA��jA��RA��9A��9A��A���A���A���A���A���A���A��hA��7A��+A��A�v�A�ffA�^5A�ZA�ZA�ZA�XA�O�A�A�A�33A�+A�"�A��A�oA�  A���A���A���A��yA���A��RA��A���A���A��DA�t�A�n�A�l�A�ffA�`BA�ZA�K�A�;dA�5?A�(�A�{A�VA�  A��`A���A��PA�dZA�1'A�JA��!A���A���A��!A��7A��A�  A��A��HA�ƨA���A��9A��uA��7A��A�v�A�r�A�t�A�p�A�jA�ffA�\)A�S�A�Q�A�O�A�E�A�G�A�;dA�5?A�+A�bA�VA�A���A���A��A��A��A��TA��;A��#A���A�ƨA��!A���A��uA��DA��DA��hA�M�A�ZA�1'A�$�A�{A��yA���A���A���A��^A��FA��FA��A��A�v�A�\)A�XA�I�A�=qA�33A�-A�&�A�"�A�oA��A���A��A��A��`A��/A�ĜA��^A���A�|�A�&�A�oA�JA�1A�A�A���A��;A��HA��/A��;A��#A���A��
A���A���A�ĜA���A��7A�^5A��A�{A�
=A���A��A��A��`A��HA��HA��;A��/A��;A��;A��#A��;A��#A���A���A���A���A��wA��RA���A��\A�jA�E�A��yA���A�|�A�ffA�VA�G�A�9XA��A�
=A��yA�?}A��A�XA�A���A��uA��A�v�A�`BA�9XA���A���A�ffA���A�C�A���A��\A�7LA�"�A�{A�JA�A���A��yA��;A��#A��A���A���A���A��FA��A��FA��9A���A���A���A��\A��+A�x�A�jA�5?A�(�A���A��-A�t�A�ffA�bNA�XA�Q�A�G�A�=qA�9XA�&�A�  A��;A�A���A��uA�p�A�ZA�&�A�bA�1A���A��A���A�ĜA���A���A��DA��+A��A��A��A�|�A�~�A�z�A�x�A�p�A�hsA�`BA�\)A�Q�A�G�A�7LA�&�A�$�A� �A��A�ĜA��wA��A��A���A���A���A���A���A���A���A�|�A�dZA��A���A���A�?}A��`A���A��jA�x�A�G�A��A�A��A���A���A�ĜA��^A��-A���A���A���A���A��DA��+A�n�A�VA�9XA�"�A�A��yA��A���A��FA���A���A��hA�z�A�bNA�VA�O�A�A�A�/A� �A���A���A���A�|�A�p�A�bNA�ZA�S�A�C�A�?}A�+A�$�A�JA���A��A��
A���A���A��jA��!A���A���A���A��uA��\A��A�v�A�v�A�t�A�t�A�t�A�hsA�`BA�Q�A�C�A�oA���A�p�A�"�A�p�A�O�A�A�A��#A���A�v�A���A�~�A�dZA�O�A�9XA���A���A��DA�=qA�  A��^A�~�A�n�A�ZA�O�A�K�A�7LA�bA���A��9A���A��+A�^5A�A�A�7LA��A�{A�oA�%A�A�A�%A�  A�  A���A���A��A��A�jA�ffA�K�A�G�A�A�A�9XA�9XA�-A�(�A�"�A��A��A�bA�
=A�A��`A��/A��A���A��A��uA�z�A�\)A�+A���A��TA���A�A��jA��9A��!A���A�|�A�`BA�K�A�=qA�"�A�bA�%A�A���A��A��A��A��A��A��A��A��A��yA��yA��mA��mA��`A��TA��HA��/A��#A�ƨA��-A���A�bNA�&�A��A���A��RA���A���A���A���A���A��uA��PA��A�n�A�7LA��A�ƨA���A��uA��+A�r�A�ffA�VA�O�A�I�A�I�A�C�A�;dA�
=A�A��A�S�A�/A��A�1A��yA�ĜA���A�p�A�G�A��A���A��yA��TA���A��+A�t�A�XA��A��;A�A���A�v�A�C�A��A��A���A���A�t�A�I�A��A��A���A�VA�$�A���A���A�G�A�$�A�VA�  A��yA�ƨA��9A���A��PA�z�A�hsA�XA�33A�oA���A��TA���A���A�XA��A��;A���A���A�l�A�/A��mA��\A�=qA��A��wA�p�A�+A�bA��A���A��jA��\A�O�A�1'A��mA�|�A�I�A��
A���A���A���A��hA�~�A�t�A�`BA�M�A�/A��A�A���A��A��`A���A�A��!A���A���A��uA��A�jA�^5A�E�A�(�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bl�BlWBl�Bl�Bl�Bl�BlWBlWBl"Bk�Bk�Bk�BkQBu�B}VB��B�PB��B��B�tB��B�HB��B��B�?B��B�0B��B��BƨB�BŢBŢB�B��BŢB�B��B�qB�BB��B��B��B�tB��B��BٴB�^B�aB��B��B�[B��B�B�}B��B��B��B�tB��B�9B�kB�B�FB�oB��B��B�~B��B}�Bx�Bt�Bp;B]dBU2BM6BE�B=qB5�B+�B�B�|B�B��B�B�EB�HB�3B�*B��B��B�@B��B��Bt�BXEB=<B'�B BB�B�B�UB��B��B�'B��BxlBqBk�Be`BbNB\�BS&BI�B<�B0�B(�B{B�BSB
�xB
�B
�B
� B
ޞB
��B
�2B
�NB
��B
��B
�B
��B
��B
�B
��B
��B
��B
�{B
��B
qvB
i�B
_�B
WsB
M�B
GzB
C-B
@�B
5tB
0�B
,�B
'�B
#�B
~B
�B
�B
�B
B
	�B
�B
�B
AB
 iB	��B	��B	�B	�B	�]B	�B	��B	�B	�pB	�)B	�KB	�EB	̘B	�B	ȴB	��B	��B	�B	�$B	��B	�[B	��B	��B	��B	��B	�@B	��B	��B	��B	��B	�+B	�B	��B	��B	�B	��B	� B	�PB	��B	�=B	�B	�1B	��B	��B	��B	�GB	�B	~�B	~(B	~�B	~]B	|�B	{JB	{B	zB	y>B	yrB	}VB	x�B	x�B	w2B	w�B	w�B	x�B	v�B	z�B	{B	{�B	x�B	y	B	y�B	{B	zDB	zxB	~(B	�B	cB	��B	��B	��B	��B	�7B	�=B	�B	�DB	�B	�DB	�JB	�B	��B	��B	�(B	��B	��B	�bB	�(B	��B	��B	��B	�VB	��B	��B	�PB	��B	�"B	�\B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�7B	�CB	�B	��B	�*B	��B	��B	�wB	��B	�[B	�B	�$B	��B	�$B	��B	��B	��B	��B	ȀB	�^B	ϫB	��B	�aB	�pB	�&B	��B	�B	�B	��B	�"B	��B	��B	��B	��B	��B	��B	��B
oB
�B
�B
�B
+B
fB
�B
hB
�B
�B
YB
xB
!-B
!bB
!�B
#�B
$@B
&�B
($B
(�B
+B
.IB
1�B
6FB
8B
8�B
9$B
:*B
:�B
>�B
B'B
B[B
C�B
E9B
I�B
K�B
K�B
OB
RTB
UgB
V�B
XEB
YKB
ZB
Z�B
_pB
b�B
e�B
ffB
f�B
ffB
ffB
gB
f�B
f�B
iyB
kB
o�B
tTB
x8B
zB
{JB
cB
�{B
��B
��B
�YB
��B
��B
�	B
��B
�B
��B
�~B
��B
�_B
�xB
�\B
��B
�bB
��B
��B
��B
�oB
��B
�_B
�B
�=B
�qB
�IB
�B
�CB
�CB
��B
�B
��B
��B
��B
��B
��B
�:B
�:B
�nB
�FB
��B
��B
�XB
��B
��B
��B
�0B
�kB
�}B
�OB
��B
�nB
��B
��B
��B
��B
��B
��B
�*B
��B
�^B
�6B
�B
�}B
��B
��B
�'B
��B
�B
�B
ȴB
�B
ɺB
�^B
̘B
̘B
�B
��B
�HB
�}B
� B
� B
ҽB
��B
�aB
՛B
֡B
�
B
�sB
�WB
�)B
��B
ݘB
�;B
�B
�B
��B
��B
�`B
�B
�sB
�sB
�B
�yB
�yB
�QB
�B
�B
��B
��B
��B
��B
��B
��B
� B
�iB
�5B
�AB
�GB
�B
��B
��B
�fB
�fB
�fB
�lB
�	B
��B
�B
��B
��B
��B
��B  B;B�B�B�BBB�B�BB�B�B1B	�B	lB	�B
	B
�B�BB�B�B(B\B�B�B�B.BhBbB�B�BuBuB�B�BFBMB�BBB�B�B�B�B$B�B�BkB�B�B�BIB�B!�B �B \B 'B�B 'B!�B"hB#�B$@B$tB%FB%�B&�B&�B&�B&LB&LB&B&�B'RB($B,�B.�B,�B/OB/B.}B,=B+�B+�B+�B,=B,�B-�B-�B.�B/OB/�B0!B0�B/�B/�B/�B/OB/�B/�B/�B/OB/�B1[B2�B33B4B4�B4�B5B5?B5�B6�B6�B6FB6�B7LB6�B6�B8B;0B:�B;0B;dB:�B<jB;�B<B;�B<B<6B<B<�B>B=�B=�B>wB>wB>wB>�B?�B@OB?�B@OBA�BA BA�BA�BA�BAUBA�BA�BB[BB�BB[BB�BB[BA�BC�BE9BDgBC�BC�BC-BD3BEBEBD�BE9BE9BEBF?BGBF�BHKBIRBJ�BK�BLdBM�BMjBNBM�BM6BN�BN<BNBM�BOBBN�BP}BP�BR BR BR�BS&BR�BT,BS�BS�BT�BTaBUgBV�BV�BV�BX�BXBW�BXyBX�BXEBXBY�BYKBY�BY�B[#BZ�BZ�BZ�B[#B[WB[�B\�B\�B\�B^5B^�B^jB]�B^B^jB_;B`�B`�B`�B`�BaBaBa|Bc BcTBc�Bc�Bc�Bc�Bc�Bc�Bd�Bd�Be,Bd�Be�Bd�Bd�Bd�BdZBdZBdZBe�BffBe�Bf2Be�Be�Be�Bd�Be`Bd�Bd�Bd�Bd�Bd�Bd�Bd&Be`BffBffBffBf2BffBg8BgBg�BgmBh�BhsBh�Bi�Bi�BiyBi�Bi�BiyBjBi�Bj�Bk�Bj�BkBk�Bk�Bk�BkQBjBlWBl�Bm]Bm]BlWBk�Bk�Bj�Bj�BkBl"Bk�Bl"Bl�Bm�Bn�BpBp�BqBqABqvBqvBq�Bq�Bq�Bq�BrGBrBr|BrBrGBr�Br�BsMBs�BsMBs�BtBtTBuZBu%Bu�Bu�Bv+Bv�Bv�Bw�Bw�Bw�Bw2Bw�BxlBy	By>ByrBx�By	By	BzBzxBzBzBy�By�ByrBzDBzxBzBz�Bz�B{JB{B{JB{JB{JB{JB{�B|B|�B|�B|�B~(B}�B~]B~]B~]B~]B~�B}�B~�B~�B~(B}�B~�B�;B�B�oB�oB�;B�B��B�B�AB�B�AB�AB�AB�uB��B��B�MB��B��B��B��B��B��B�B��B�SB��B��B��B��B��B�YB�_B�+B��B�fB��B��B��B�lB��B�7B�B�B�lB��B�7B��B��B��B�lB�rB�=B��B��B��B��B�xB�xB�xB��B��B��B�Bk�Bl"Bl�BjKBn/Bm�BncBk�Bk�Bk�Bl"Bm�Bl�BlWBm�Bm�Bl�Bl�Bk�Bk�Bm)Bm�Bl�Bl"Bl�Bn�Bn/Bl�BkBk�Bm�Bm)Bk�BkQBk�BlWBm)Bl"BkQBl�Bm)Bm)BjBj�Bl�Bm]Bk�Bk�BkBkQBk�BlWBlWBkQBj�Bm�Bk�Bm)Bi�BkBj�Bk�BkBk�BkQBjBkBl�Bl"Bk�BkQBp;BsMBsMBt�B~(Bv�BxlBv�BwfBx8Bx�B|B��B|�B�B�B~�B��B��B��B��B��B��B�B��B�B��B�xB��B�1B�1B�DB��B��B��B��B�FB��B�+B��B��B�B��B��B��B�:B��B�hB��B��B��B��B�:B��B��B�:B�RB�B�bB��B��B�3B�OB��B��B��B��B��B��B�<B�HB��B�OB� B�'B�[B��B��B��B��B��B��B��BĜBŢB�B�?B��BƨBƨB�9BƨB��B�9B�tB�zB�B��B�tB�mBƨBɺBȴB��B˒B�HB�B�B�XB�RB��BʌB�BȴB��B�XB��B�dB��B�EB�BɆB�EB�B�B�tB��BǮBǮB�zB�B�mB��B��BƨB�?BȀB��B�?B�tB�B�tB�gB��B��B��B��BĜBĜB��BĜB�9B�tB��B�?B�EB�KB�B�BƨB��B�9B�tBȴBȀB�B�B��B�tB�B�mBÖB�3B�B�#B�3B�3B��B��B�B��B��B�OB�UB� B�OB�OB��B�<B��B�B��B��B�<B�B�BB�B��B��B�[B�,B�aB�aB��B��B�3B��B��B�XB��B�6B�6B��B�}B�'B�'B��B��B�3B�'B�[B�9B�gB��B��B��BǮBȀB��B�6B�B��B��B��B�dB�B˒B��B�pB��B��B��B��BϫB�TB�TB��B��B�yB�]B�BбB�pB�B�dB�-B�gB��B�aB�[B�6B��B��B�)B��B�<B��B�tB��B��B�9B�^B�'B��B�?B��B��B�'B�IB�B��B��B�B��B��B��B�CB�CB�B�[B��B�qB�eB��B�B�XB��B��B�B�CB�B�B�3B�-B�3B�B��B��B��B�3B�-B��B�3B��B��B��B��B�[B��B�[B�'B��B�9B��B��B��B�B��B��B��B��B��B�zB��B��B�3B��B�aB�B�B��B��B��B��B��B��B��B�7B��B��B��B��B��B�xB��B��B��B�B�{B�B��B��B�{B�B�4B�bB��B�4B��B� B��B��B�B�B��B�:B��B�~B�hB�B��B�B�1B��B�B�+B�fB�_B��B��B��B��B�_B��B��B�MB��B��B�uB��B�B~(B~�B~�B�oB|B|B{�B|�BzDBy�BzxBx�By�Bx�ByrBy	BzDBwfBx�Bv�Bw2Bx�Bx8Bw2Bu%Bt�B|�By�Bq�Bq�BqABq�BpBp;Bm�BncBm)BkQBm)Bh
BncB}�B.B|PBg�BaHB`�Bd�B_�B_�B]/B[�BZ�BYKBW?BXEBV�BW
BW
BT,BT�BU�BR�BW
BU�BRTBTaBS�BQ�BOBS�BQBO�BMBK^BO�BLdBJ�BGzBI�BI�BF�BL�BJ�BGEBG�BB[BB�BA�B@�BA�B?�B@�B?B>wB>�B9�B=B9�B:^B<jB8�B8�B7�B7�B;dB6zB4�B7B2�B4B1�B1�B1�B0UB0�B.IB2�B.�B3hB,�B2aBVBVB-B�B�BA�B�B�B�(B��B	�B�B�`B�B�lB�PB�B�B��B��B�B�B�B�QB�B��B��B��B��BܒB��B�KB�B�mB�B��B֡B��B��B�B��B�EB�WB�B�B�B�BچB��B�?B�QBٴB��B�B��B֡B�sB�KB�#B�sB��BخBںB��B��B�9B�gB��B�jB�^BʌB�RB�EB�9B�#B�zB��B�-B�aB�UB�BB��B��B��B�^B�XB�*B��B�*B��B��B��B��B��B�RB�B��B��B�FB�B�tB�B�9B�?B�*B��B��B��B��B��B��B�@B�B�B�nB��B��B��B�XB�nB��B��B��B��B�B�oB�oB�\B��B��B��B�~B�uB��B�rB�GB��B|�B|PB{JBzDBv�Bo�Bq�Bl�Bk�B`vBbB^�Bh�BRTB[#BMjBPBGBEBH�BA�B@B=<B<6B8�B6�B5tB3�B2-B8RB,qB&LB%�B4�BCB�B�B�B�B�B�B�BVB�B
�BDB~B1B�B�B{BB�B�B�xB�GB��B�%B��B�MB�/B�DB��B��B�pB�B�aB�2B��B��BѷB�BB�3B�,B��B��BӏB��B�9B�B�aB�[B��B��B��B��B�kB��B��B��B�kB��B��B��B��B�B��B��B�nB��B��B��B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202110151929172021101519291720211015192917202110151929172021101519291720211015192917SI  SI  ARFMARFM                                                                                                                                                2021022318264820210223182648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030518010320210305180103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030518010320210305180103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074520211014130745IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                