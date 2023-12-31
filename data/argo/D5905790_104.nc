CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-13T18:28:14Z creation; 2021-10-15T19:29:26Z DMQC;      
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
_FillValue        G�O�     �  c\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ά   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � `P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ~�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210213182814  20211015173716  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               h   hAA  AOAO7824_008764_104                 7824_008764_104                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�^-�eU�@�^-�eU�11  @�^.
�L0@�^.
�L0@5��9C@5��9C�d�\|ؘ��d�\|ؘ�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?�@:�H@}p�@�  @�  @�G�AG�A  A�RA*�HA>�RA`  A�  A�  A�  A�  A�Q�A�  A�Q�A���B (�B  B  B(�B   B((�B0(�B8(�B@(�BH  BO�
BX  B`  Bh  Bp  Bx(�B�  B�  B�  B�  B�  B�  B�  B�{B��B��B�  B�{B�  B�{B�  B��B�  B�{B�{B�  B�{B�{B�  B�  B�  B�  B��B�  B�  B�  B�  B��B��C��C��C��C��C	��C��C  C
=C
=C  C  C��C��C
=C{C 
=C"
=C$
=C&
=C(
=C*{C,
=C.
=C0
=C2
=C4
=C6  C7��C9��C<  C>{C@
=CB  CC��CE��CH  CJ
=CL  CN  CP  CR  CT  CV  CW��CY��C[��C]��C`  Ca��Cc��Cf
=Ch  Cj  Cl
=Cn  Cp  Cr
=Ct  Cv  Cx  Cy��C{�C~  C�  C���C���C�  C���C�  C�C�C�C�C�C�C�  C�  C�  C���C�  C�C�  C�  C�  C�  C�  C���C�  C�C�  C���C���C�  C�  C���C�  C�C�C���C���C���C�C�C�C�
=C�
=C�  C���C�  C�C�  C���C�  C���C���C�C�C�  C�
=C�C���C���C�C�C�  C���C���C�  C�C���C���C���C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�  C���C���C�  C�  C���C���C�  C���C���C�  C�  C���C�  C�  C���C�  C�
=C�  C���C���C�  C�  C�  C�  C���C�  C�  C���C���C�C�
=C�
=C�C�  C�  C���C�  C�C�
=C�
=C�
=C�C�  C�  D   D }qD �qD}qD�qD}qD  D��D  D}qD  D� D  D� D�D� D  D� D	  D	}qD	�qD
��D�D� D  D� D�qD}qD�qD� D�D��D  D� D  D� D  D� D�qD}qD�qD}qD�D��D�qD� D  D}qD�qD}qD�qD}qD  D� D�qDz�D�qD� D  D� D�D��DD� D   D � D!  D!� D"  D"��D#  D#}qD$  D$� D$�qD%z�D%�qD&� D&�qD'� D(�D(��D)  D)� D*  D*}qD+  D+� D+�qD,� D,��D-}qD.  D.}qD/  D/� D/�qD0� D1�D1� D1�qD2� D3�D3� D3�qD4� D5  D5� D6  D6� D7�D7� D8  D8� D9  D9� D:  D:� D:�qD;� D<�D<� D=  D=��D>  D>� D?  D?� D@  D@� DA�DA}qDB  DB}qDC  DC� DC�qDD}qDE�DE��DF  DF� DG�DG�DH  DH}qDI  DI��DJ  DJ}qDK  DK�DK�qDL}qDM�DM��DN  DN}qDO  DO� DP  DP� DQ�DQ� DR  DR� DR�qDSz�DT  DT� DT�qDU}qDU�qDV� DW  DW� DW�qDX}qDX�qDY}qDZ  DZ� D[  D[}qD\  D\� D]�D]� D]�qD^}qD_  D_� D`�D`�Da�Da� Db�Db� Db�qDc� Dd  Ddz�Dd�qDe}qDe�qDf��Dg�Dg}qDh  Dh� Di  Di� Di�qDj}qDj�qDk� Dk��Dl� Dm�Dm� Dm�qDnz�Dn��Do� DpDp�Dp�qDq}qDr�Dr� Ds  Ds� Dt  Dt}qDu  Du}qDu�qDv}qDv�qDw}qDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D{�qD|� D|�qD}}qD~  D~��D�D�D��D�@ D�~�D���D���D�@ D��HD�� D���D�AHD���D��HD�  D�AHD�� D���D���D�>�D��HD�� D���D�AHD��HD�� D�  D�AHD�� D�� D�HD�AHD��HD�� D�HD�AHD�� D���D���D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D��HD��HD�  D�@ D�~�D���D�  D�>�D�~�D��HD�HD�>�D�� D�� D�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�AHD�� D���D���D�@ D�� D�� D�  D�=qD�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D���D���D�@ D��HD�� D���D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�� D�� D�HD�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�~�D��qD���D�@ D�� D���D���D�@ D�� D��HD�  D�=qD�~�D�� D�HD�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D��HD��HD�  D�@ D�� D��HD�  D�@ D�� D��HD�  D�=qD�~�D�� D�  D�>�D�� D��HD�  D�>�D�~�D�� D�HD�@ D�~�D��HD�  D�@ D�� D���D�HD�AHD��HD�� D�  D�AHD��HD�� D�  D�>�D�� D�� D���D�>�D�� D���D���D�@ D�� D�� D�  D�>�D��HD��HD���D�>�D�~�D�� D�  D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D��HD���D�  D�B�D���D��HD�  D�AHD���D��HD�  D�AHD���D��HD�  D�>�D�}qD�� D��D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�~�D���D���D�>�D�~�D��HD�HD�AHD�� D��HD��D�B�D��HD���D���D�>�D D�� D�  D�AHDÀ DýqD���D�@ DāHD�D�  D�@ D�~�Dž�D�  D�B�DƁHD��HD�HD�AHD�~�DǾ�D�HD�@ D�~�D�� D�HD�AHDɀ D�� D�  D�@ D�~�DʽqD���D�@ D�~�D˽qD���D�AHD̀ D̾�D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�Dπ DϾ�D���D�AHDЀ Dо�D���D�AHDр D�� D�HD�AHD҂�D�D�  D�@ DӀ D�� D���D�@ DԀ DԾ�D���D�>�DՀ D��HD�HD�AHDցHD��HD�HD�AHD׀ D�� D���D�>�D�~�Dؾ�D���D�@ Dـ D��HD�  D�=qD�~�D�� D�HD�@ Dۀ D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�D�� D�  D�AHDހ D޾�D�HD�@ D�~�D�� D�  D�@ D�� DྸD�HD�AHD� D�� D�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�AHD� D�� D�  D�@ D�~�D�qD��qD�=qD� D��HD�  D�>�D� D�� D�HD�AHD� D�� D�  D�>�D� D��HD�  D�>�D� D�� D�  D�>�D�~�D���D�  D�@ D�HD�� D�HD�AHD�~�DﾸD���D�>�D�~�D�� D�  D�>�D� D�� D���D�@ D�HD�� D���D�@ D�D�>���?B�\?�\)?\?�ff@
=q@#�
@:�H@Tz�@n{@��\@�{@�Q�@��@���@�p�@Ǯ@�33@޸R@�=q@�z�A   A�A
=qA\)AA�HA   A%�A*�HA0  A5A:�HA@��AE�AJ=qAP��AUAZ=qA_\)Ae�Aj�HAp  Au�Az=qA\)A��\A�p�A�  A��HA�A���A�33A�ffA���A�(�A�
=A��\A�p�A���A��A�ffA�G�A�z�A�\)A��\A�p�A���A�33A�ffA�G�A�(�AϮAҏ\A�p�A�Q�A�33A�ffA�G�A�A�RA�G�A�z�A�A�\A���A�  A��\A�p�B (�Bp�B33Bz�BB�B��B
=qB�B�B�\B�
Bp�B
=Bz�BB\)B��B=qB�
B�B�RB   B!p�B"�HB$(�B%��B'
=B(Q�B)p�B*�RB+�B,��B.{B.�HB0  B0��B1�B2�HB4  B4��B5�B6�HB7�
B8��B9�B;
=B<  B<��B=�B>�HB@  B@��BB{BC
=BD  BD��BF{BG33BH  BI�BJ{BK
=BL(�BM�BN{BO33BP(�BQ�BR=qBS33BT(�BU�BV=qBW
=BX(�BYG�BZ=qB[
=B\(�B]G�B^=qB_33B`Q�BaG�Bb=qBc\)BdQ�Bep�BfffBg\)Bhz�Bi��Bj�\Bk�Bl��Bmp�Bn�\Bo�Bpz�Bq��Br�\Bs�Bt��BuG�BvffBw�Bxz�Byp�BzffB{\)B|Q�B}G�B~=qB33B�(�B���B�
=B���B�{B��\B�
=B��B�{B�z�B�
=B��B��B�z�B���B�\)B��
B�ffB���B�p�B��
B�ffB��HB�p�B�  B�z�B�
=B���B�{B���B�
=B���B�(�B���B�33B��B�=qB���B�\)B�B�ffB��HB�\)B�  B�z�B��B��B�{B���B�33B�B�=qB��HB�\)B��B�z�B�
=B��B�(�B���B�G�B��
B�Q�B��HB�p�B�  B��\B��B���B�=qB���B�33B��
B�Q�B��HB�p�B��B��\B��B��B�(�B��RB�33B�B�ffB��HB��B��B�z�B���B�p�B�  B�z�B��B��B�{B��\B�
=B���B�{B��\B��B���B�  B�z�B��HB��B��B�ffB���B�G�B��
B�=qB��RB�33B��B�(�B���B��B���B�(�B���B��B���B�{B���B��B��B�(�B¸RB�33BîB�(�Bģ�B�33B�B�=qBƸRB�33BǮB�(�BȸRB��BɮB�{Bʣ�B��B˙�B�(�Ḅ�B��BͮB�(�BΣ�B�33BϮB�(�BиRB�33BѮB�=qB���B�G�B�B�=qB���B�\)B��B�z�B���BׅB�  B؏\B��Bٙ�B�=qB���B�G�B��
B�Q�B���B݅B�{Bޣ�B�33B߮B�Q�B���B�\)B��B�z�B��B㙚B�{B��B�G�B��
B�ffB���B�B�{B��B�33B��
B�ffB�
=B뙚B�=qB���B�\)B��B�\B��B�B�Q�B��HB�B�{B���B�p�B�{B���B�G�B��B��\B�33B�B�ffB��B�B�ffB���B��B�Q�B��HB��B�(�B���B�p�C   C \)C ��C  CG�C�C�CG�C��C�C=qC�\C�HC(�C�\C��C33Cz�C�
C�Cp�CC{CffCC{CffC�RC	
=C	Q�C	�C	��C
Q�C
��C
�C=qC�\C�HC=qC�C�
C(�C�C�
C�Cz�C��C(�CffC�RC
=C\)C��C��CG�C�\C�C=qC�C��C�CffC�RC  CQ�C��C�C=qC�\C�HC(�Cz�CC{CffC�RC{CffC�C  CQ�C�C��CG�C��C��C=qC�\C�HC33C�C�
C(�C�C�
C(�C�C�
C(�Cz�C��C (�C p�C C!{C!ffC!�C"  C"\)C"�C"��C#Q�C#��C#��C$G�C$��C$�C%G�C%��C%�C&G�C&��C&��C'G�C'��C'��C(G�C(��C)  C)Q�C)��C*  C*\)C*�C+  C+\)C+�RC,
=C,ffC,�RC-
=C-p�C-��C.(�C.p�C.��C/(�C/�C/�
C033C0�\C0�C1G�C1�\C1��C2Q�C2��C3  C3Q�C3�RC4  C4ffC4C5{C5z�C5��C633C6�C6�HC7G�C7��C7��C8\)C8�C9{C9ffC9��C:�C:z�C:�
C;33C;�\C;��C<G�C<��C=  C=\)C=�RC>{C>ffC>��C?33C?�\C?�C@Q�C@�CA
=CAp�CA��CB33CB�\CB��CCG�CC�RCD{CDp�CD�
CE33CE��CE�CF\)CF�CG�CGp�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                     ?u?�@:�H@}p�@�  @�  @�G�AG�A  A�RA*�HA>�RA`  A�  A�  A�  A�  A�Q�A�  A�Q�A���B (�B  B  B(�B   B((�B0(�B8(�B@(�BH  BO�
BX  B`  Bh  Bp  Bx(�B�  B�  B�  B�  B�  B�  B�  B�{B��B��B�  B�{B�  B�{B�  B��B�  B�{B�{B�  B�{B�{B�  B�  B�  B�  B��B�  B�  B�  B�  B��B��C��C��C��C��C	��C��C  C
=C
=C  C  C��C��C
=C{C 
=C"
=C$
=C&
=C(
=C*{C,
=C.
=C0
=C2
=C4
=C6  C7��C9��C<  C>{C@
=CB  CC��CE��CH  CJ
=CL  CN  CP  CR  CT  CV  CW��CY��C[��C]��C`  Ca��Cc��Cf
=Ch  Cj  Cl
=Cn  Cp  Cr
=Ct  Cv  Cx  Cy��C{�C~  C�  C���C���C�  C���C�  C�C�C�C�C�C�C�  C�  C�  C���C�  C�C�  C�  C�  C�  C�  C���C�  C�C�  C���C���C�  C�  C���C�  C�C�C���C���C���C�C�C�C�
=C�
=C�  C���C�  C�C�  C���C�  C���C���C�C�C�  C�
=C�C���C���C�C�C�  C���C���C�  C�C���C���C���C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�  C���C���C�  C�  C���C���C�  C���C���C�  C�  C���C�  C�  C���C�  C�
=C�  C���C���C�  C�  C�  C�  C���C�  C�  C���C���C�C�
=C�
=C�C�  C�  C���C�  C�C�
=C�
=C�
=C�C�  C�  D   D }qD �qD}qD�qD}qD  D��D  D}qD  D� D  D� D�D� D  D� D	  D	}qD	�qD
��D�D� D  D� D�qD}qD�qD� D�D��D  D� D  D� D  D� D�qD}qD�qD}qD�D��D�qD� D  D}qD�qD}qD�qD}qD  D� D�qDz�D�qD� D  D� D�D��DD� D   D � D!  D!� D"  D"��D#  D#}qD$  D$� D$�qD%z�D%�qD&� D&�qD'� D(�D(��D)  D)� D*  D*}qD+  D+� D+�qD,� D,��D-}qD.  D.}qD/  D/� D/�qD0� D1�D1� D1�qD2� D3�D3� D3�qD4� D5  D5� D6  D6� D7�D7� D8  D8� D9  D9� D:  D:� D:�qD;� D<�D<� D=  D=��D>  D>� D?  D?� D@  D@� DA�DA}qDB  DB}qDC  DC� DC�qDD}qDE�DE��DF  DF� DG�DG�DH  DH}qDI  DI��DJ  DJ}qDK  DK�DK�qDL}qDM�DM��DN  DN}qDO  DO� DP  DP� DQ�DQ� DR  DR� DR�qDSz�DT  DT� DT�qDU}qDU�qDV� DW  DW� DW�qDX}qDX�qDY}qDZ  DZ� D[  D[}qD\  D\� D]�D]� D]�qD^}qD_  D_� D`�D`�Da�Da� Db�Db� Db�qDc� Dd  Ddz�Dd�qDe}qDe�qDf��Dg�Dg}qDh  Dh� Di  Di� Di�qDj}qDj�qDk� Dk��Dl� Dm�Dm� Dm�qDnz�Dn��Do� DpDp�Dp�qDq}qDr�Dr� Ds  Ds� Dt  Dt}qDu  Du}qDu�qDv}qDv�qDw}qDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D{�qD|� D|�qD}}qD~  D~��D�D�D��D�@ D�~�D���D���D�@ D��HD�� D���D�AHD���D��HD�  D�AHD�� D���D���D�>�D��HD�� D���D�AHD��HD�� D�  D�AHD�� D�� D�HD�AHD��HD�� D�HD�AHD�� D���D���D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D��HD��HD�  D�@ D�~�D���D�  D�>�D�~�D��HD�HD�>�D�� D�� D�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�AHD�� D���D���D�@ D�� D�� D�  D�=qD�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D���D���D�@ D��HD�� D���D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�� D�� D�HD�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�~�D��qD���D�@ D�� D���D���D�@ D�� D��HD�  D�=qD�~�D�� D�HD�@ D�~�D��HD�HD�@ D�~�D���D�  D�@ D��HD��HD�  D�@ D�� D��HD�  D�@ D�� D��HD�  D�=qD�~�D�� D�  D�>�D�� D��HD�  D�>�D�~�D�� D�HD�@ D�~�D��HD�  D�@ D�� D���D�HD�AHD��HD�� D�  D�AHD��HD�� D�  D�>�D�� D�� D���D�>�D�� D���D���D�@ D�� D�� D�  D�>�D��HD��HD���D�>�D�~�D�� D�  D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D��HD���D�  D�B�D���D��HD�  D�AHD���D��HD�  D�AHD���D��HD�  D�>�D�}qD�� D��D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�~�D���D���D�>�D�~�D��HD�HD�AHD�� D��HD��D�B�D��HD���D���D�>�D D�� D�  D�AHDÀ DýqD���D�@ DāHD�D�  D�@ D�~�Dž�D�  D�B�DƁHD��HD�HD�AHD�~�DǾ�D�HD�@ D�~�D�� D�HD�AHDɀ D�� D�  D�@ D�~�DʽqD���D�@ D�~�D˽qD���D�AHD̀ D̾�D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�Dπ DϾ�D���D�AHDЀ Dо�D���D�AHDр D�� D�HD�AHD҂�D�D�  D�@ DӀ D�� D���D�@ DԀ DԾ�D���D�>�DՀ D��HD�HD�AHDցHD��HD�HD�AHD׀ D�� D���D�>�D�~�Dؾ�D���D�@ Dـ D��HD�  D�=qD�~�D�� D�HD�@ Dۀ D۾�D���D�>�D�~�Dܾ�D���D�>�D�~�D�� D�  D�AHDހ D޾�D�HD�@ D�~�D�� D�  D�@ D�� DྸD�HD�AHD� D�� D�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�AHD� D�� D�  D�@ D�~�D�qD��qD�=qD� D��HD�  D�>�D� D�� D�HD�AHD� D�� D�  D�>�D� D��HD�  D�>�D� D�� D�  D�>�D�~�D���D�  D�@ D�HD�� D�HD�AHD�~�DﾸD���D�>�D�~�D�� D�  D�>�D� D�� D���D�@ D�HD�� D���D�@ D�G�O�>���?B�\?�\)?\?�ff@
=q@#�
@:�H@Tz�@n{@��\@�{@�Q�@��@���@�p�@Ǯ@�33@޸R@�=q@�z�A   A�A
=qA\)AA�HA   A%�A*�HA0  A5A:�HA@��AE�AJ=qAP��AUAZ=qA_\)Ae�Aj�HAp  Au�Az=qA\)A��\A�p�A�  A��HA�A���A�33A�ffA���A�(�A�
=A��\A�p�A���A��A�ffA�G�A�z�A�\)A��\A�p�A���A�33A�ffA�G�A�(�AϮAҏ\A�p�A�Q�A�33A�ffA�G�A�A�RA�G�A�z�A�A�\A���A�  A��\A�p�B (�Bp�B33Bz�BB�B��B
=qB�B�B�\B�
Bp�B
=Bz�BB\)B��B=qB�
B�B�RB   B!p�B"�HB$(�B%��B'
=B(Q�B)p�B*�RB+�B,��B.{B.�HB0  B0��B1�B2�HB4  B4��B5�B6�HB7�
B8��B9�B;
=B<  B<��B=�B>�HB@  B@��BB{BC
=BD  BD��BF{BG33BH  BI�BJ{BK
=BL(�BM�BN{BO33BP(�BQ�BR=qBS33BT(�BU�BV=qBW
=BX(�BYG�BZ=qB[
=B\(�B]G�B^=qB_33B`Q�BaG�Bb=qBc\)BdQ�Bep�BfffBg\)Bhz�Bi��Bj�\Bk�Bl��Bmp�Bn�\Bo�Bpz�Bq��Br�\Bs�Bt��BuG�BvffBw�Bxz�Byp�BzffB{\)B|Q�B}G�B~=qB33B�(�B���B�
=B���B�{B��\B�
=B��B�{B�z�B�
=B��B��B�z�B���B�\)B��
B�ffB���B�p�B��
B�ffB��HB�p�B�  B�z�B�
=B���B�{B���B�
=B���B�(�B���B�33B��B�=qB���B�\)B�B�ffB��HB�\)B�  B�z�B��B��B�{B���B�33B�B�=qB��HB�\)B��B�z�B�
=B��B�(�B���B�G�B��
B�Q�B��HB�p�B�  B��\B��B���B�=qB���B�33B��
B�Q�B��HB�p�B��B��\B��B��B�(�B��RB�33B�B�ffB��HB��B��B�z�B���B�p�B�  B�z�B��B��B�{B��\B�
=B���B�{B��\B��B���B�  B�z�B��HB��B��B�ffB���B�G�B��
B�=qB��RB�33B��B�(�B���B��B���B�(�B���B��B���B�{B���B��B��B�(�B¸RB�33BîB�(�Bģ�B�33B�B�=qBƸRB�33BǮB�(�BȸRB��BɮB�{Bʣ�B��B˙�B�(�Ḅ�B��BͮB�(�BΣ�B�33BϮB�(�BиRB�33BѮB�=qB���B�G�B�B�=qB���B�\)B��B�z�B���BׅB�  B؏\B��Bٙ�B�=qB���B�G�B��
B�Q�B���B݅B�{Bޣ�B�33B߮B�Q�B���B�\)B��B�z�B��B㙚B�{B��B�G�B��
B�ffB���B�B�{B��B�33B��
B�ffB�
=B뙚B�=qB���B�\)B��B�\B��B�B�Q�B��HB�B�{B���B�p�B�{B���B�G�B��B��\B�33B�B�ffB��B�B�ffB���B��B�Q�B��HB��B�(�B���B�p�C   C \)C ��C  CG�C�C�CG�C��C�C=qC�\C�HC(�C�\C��C33Cz�C�
C�Cp�CC{CffCC{CffC�RC	
=C	Q�C	�C	��C
Q�C
��C
�C=qC�\C�HC=qC�C�
C(�C�C�
C�Cz�C��C(�CffC�RC
=C\)C��C��CG�C�\C�C=qC�C��C�CffC�RC  CQ�C��C�C=qC�\C�HC(�Cz�CC{CffC�RC{CffC�C  CQ�C�C��CG�C��C��C=qC�\C�HC33C�C�
C(�C�C�
C(�C�C�
C(�Cz�C��C (�C p�C C!{C!ffC!�C"  C"\)C"�C"��C#Q�C#��C#��C$G�C$��C$�C%G�C%��C%�C&G�C&��C&��C'G�C'��C'��C(G�C(��C)  C)Q�C)��C*  C*\)C*�C+  C+\)C+�RC,
=C,ffC,�RC-
=C-p�C-��C.(�C.p�C.��C/(�C/�C/�
C033C0�\C0�C1G�C1�\C1��C2Q�C2��C3  C3Q�C3�RC4  C4ffC4C5{C5z�C5��C633C6�C6�HC7G�C7��C7��C8\)C8�C9{C9ffC9��C:�C:z�C:�
C;33C;�\C;��C<G�C<��C=  C=\)C=�RC>{C>ffC>��C?33C?�\C?�C@Q�C@�CA
=CAp�CA��CB33CB�\CB��CCG�CC�RCD{CDp�CD�
CE33CE��CE�CF\)CF�CG�CGp�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                     @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A�$�A�&�A�&�A�&�A�(�A�(�A�+A�+A�+A�-A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�33A�/A�/A�9XA�9XA�33A�33A�1'A�(�A�(�A�+A�(�A�"�A�(�A� �A�%A���AĴ9A�VA��A�z�A�|�A�`BA�A�O�A��/A�n�A�bNA�"�A���A��HA�r�A�VA���A��-A�n�A��;A��A�  A��TA�$�A���A��+A�K�A��FA� �A���A��+A��FA�5?A�VA��RA�S�A��A�C�A��7A��HA���A���A���A�^5A��
A�t�A��
A�%A��A�|�A���A�p�A�VA�/A���A�ZA���A��RA� �A��jA�bNA�ȴA���A�33A�A�A�A���A�I�A��mA�33A�XA��TA��A��A}+Au�;Ar�uAp$�Ao%AmK�Ak+Ah=qAf�yAe�Ab��AadZA_A]S�A\�`A\bNAZĜAX1'ATZAP��AP-AL^5AH��AHI�AG�PAFffADVAA�PAA`BA@��A@z�A?�wA?+A>�A=�A=�
A=�A<bNA;t�A8�/A61A5K�A3�;A2��A0A�A//A/%A/A.^5A,~�A*bNA(��A'�A&ȴA&�DA&5?A%��A$��A#�mA"��A!�-A!33A!�A �jA�AbNA�A7LA�A/A�TA
=Az�A�TAVAffA��AȴA�A`BA�+A�AK�A9XA�mAoA^5A��AVA
Q�A	��A�A��AQ�A��A�A9XAS�A��A�+AA�A  �@�~�@�E�@��@�@���@��@�(�@���@�K�@�G�@�I�@�R@�@�M�@�b@�-@���@�P@��@���@��D@�A�@ߝ�@�n�@�1'@�7L@ش9@ج@؛�@؋D@�  @�t�@�o@��@�7L@�bN@��@ӕ�@�^5@ѩ�@У�@�(�@Ϯ@��@Χ�@�E�@ͺ^@�/@̣�@�33@ʇ+@��@ɑh@��/@��@���@���@��/@ȓu@Ǖ�@�C�@��@���@š�@þw@�E�@���@���@�O�@���@��m@�C�@��y@��R@�n�@��7@�t�@�M�@���@�@��-@�`B@�hs@��u@�b@���@�K�@�@��y@���@��@�X@�Ĝ@�Q�@� �@�ƨ@���@�S�@�o@�ȴ@�^5@��#@�x�@��@���@�j@���@�K�@�@��+@���@�`B@��9@��@�A�@���@��@�33@�o@�o@��@�E�@���@���@��@�O�@�&�@��@��@�bN@��@���@��F@���@��@�S�@��y@�p�@�%@���@��D@�z�@�A�@�(�@��;@�dZ@��y@�ȴ@���@��\@�5?@���@�X@�?}@��@�%@��@���@��D@�bN@��m@�ȴ@��\@��\@��\@��+@�n�@�ff@�=q@��T@���@���@�%@�Q�@� �@��F@�\)@��@��R@��R@�^5@��-@�Ĝ@�Z@�A�@�1'@��@�S�@��-@��D@���@��w@���@��@�l�@�\)@�K�@��@���@���@��R@��!@��+@���@�%@��@��@��@�G�@��@���@��^@��T@��@���@�G�@��;@�bN@�1'@�ƨ@��P@�K�@�v�@�M�@���@���@�I�@�bN@�r�@�I�@�1'@�  @�  @�ƨ@��@�\)@�V@�5?@�$�@�p�@�?}@��@�G�@��@��T@���@�x�@���@�|�@��@�l�@�33@�
=@�
=@�o@���@���@�-@���@�`B@�V@���@�Q�@��@��F@�\)@�33@���@�=q@�-@�-@�{@���@��T@��T@��#@���@��h@�G�@�7L@�V@��/@�bN@�Q�@�Z@�Q�@�9X@�(�@�(�@��@�b@�1@�  @�  @~��@~5?@}�@|�@|z�@|I�@|1@|1@{��@{�m@{�
@{ƨ@{��@{��@{t�@{33@{"�@{@z�@y�^@yG�@x��@x�u@xb@w��@v��@v�@v��@u@uO�@tz�@s��@s33@r�@q��@q��@q��@q��@q�7@qhs@q�@q%@p�`@p�9@p��@pQ�@o�@n�y@n��@n@m/@lz�@k��@kC�@j��@jn�@j^5@jM�@jM�@jM�@i��@ihs@hr�@g\)@g�@g
=@f�y@f�R@fV@e��@e�T@e�@d�@c�m@c@b=q@a��@a&�@a�@`�@`  @_�P@^�y@^�+@^{@^@^{@^@]�T@]�T@]�T@]@]�-@]�h@]/@\�@\I�@[�m@[�@Zn�@Y��@Y�@Yhs@X�9@Xr�@Xb@W��@Wl�@Vȴ@Vv�@U@U�@T�/@T��@T�j@T�j@T��@T�D@TI�@S�m@SS�@So@R��@R~�@R=q@RJ@Q��@Q�@Qhs@P�`@P�@PbN@Pb@O��@N��@Nv�@Nv�@Nff@NE�@N@M�@L��@L��@L�@L�D@Lj@L9X@L9X@L�@K�F@K�@I�#@I��@I��@I�@H�9@HbN@H �@G�@G�@Gl�@G+@F�@F�R@F��@Fv�@E@DI�@C�F@C��@C��@C��@C��@C�F@C��@C��@Ct�@C33@B=q@A�#@A��@A��@A��@A��@A��@A�7@A�7@Ax�@AX@@��@@Q�@?�;@?l�@?
=@>�+@>$�@=��@=�h@=p�@=/@<�j@<(�@;ƨ@;S�@;o@:�H@:��@:=q@9G�@8�9@8��@8�@8bN@81'@81'@7�@7�;@7�@7l�@7;d@7
=@6�R@6�+@6{@5�h@5`B@5/@5V@4�/@4j@3�
@3�@3dZ@3dZ@3S�@333@3"�@3o@2�H@2~�@2-@1��@1��@1�7@1x�@1G�@0��@0Ĝ@0r�@0bN@0bN@0bN@0r�@0bN@0r�@01'@/��@/�@/��@/K�@/
=@.ȴ@.��@.��@.ff@.E�@.$�@-�T@-/@,��@,�@,1@+�
@+�@+dZ@+S�@+"�@*�H@*�!@*��@*n�@*J@)��@)x�@)X@)7L@)�@(��@(�`@(��@(��@(A�@(  @'K�@'+@'�@&��@&�R@&ff@&5?@%�@%p�@%/@$�@$�@$�@$�j@$��@$�@#��@#"�@#o@"��@"��@"M�@"-@!�@!�^@!�7@!7L@ �`@ bN@   @�@|�@\)@�y@ff@��@`B@�@��@�F@��@"�@o@�@�\@^5@M�@-@J@J@��@�^@��@%@�`@�u@ �@�@��@|�@+@�@
=@
=@��@�y@�@�R@��@�+@5?@�@�-@`B@?}@�@��@�@��@�D@9X@1@�
@��@t�@C�@�\@^5@��@��@��@X@%@��@Ĝ@Ĝ@�9@��@Q�@A�@  @�@��@l�@;d@�@��@ȴ@�+@v�@ff@E�@{@@`B@O�@/@�@`B@p�@?}@�@��@�j@(�@�@�@�@��@ƨ@dZ@"�@
�@
�!@
�\@
~�@
n�@
n�@
^5@
M�@
J@	��@	�7@	G�@	&�@	�@	�@	%@Ĝ@�u@r�@b@�@�@�P@|�@\)@;d@+@�@
=@�y@�+@{@@�-A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A� �A��A� �A��A��A��A��A��A��A�oA��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A� �A� �A��A��A� �A��A��A��A��A��A��A� �A�"�A�"�A�$�A�(�A�$�A�$�A�(�A�(�A�&�A�&�A�(�A�(�A�&�A�$�A�(�A�&�A�$�A�&�A�(�A�&�A�&�A�+A�+A�(�A�&�A�(�A�(�A�&�A�(�A�+A�+A�(�A�(�A�+A�-A�+A�(�A�(�A�+A�-A�+A�(�A�-A�-A�(�A�(�A�-A�/A�+A�(�A�(�A�+A�/A�/A�-A�-A�1'A�1'A�/A�-A�/A�1'A�/A�/A�33A�1'A�/A�/A�1'A�33A�1'A�/A�/A�1'A�33A�33A�33A�/A�1'A�33A�33A�1'A�-A�-A�-A�1'A�33A�/A�-A�-A�1'A�33A�/A�/A�1'A�33A�33A�/A�/A�1'A�33A�1'A�1'A�/A�1'A�/A�/A�/A�/A�33A�33A�1'A�/A�1'A�33A�5?A�1'A�-A�/A�1'A�1'A�1'A�33A�1'A�1'A�33A�5?A�7LA�5?A�1'A�-A�/A�/A�/A�-A�/A�/A�/A�/A�-A�-A�1'A�+A�+A�1'A�1'A�5?A�7LA�9XA�;dA�;dA�7LA�7LA�;dA�;dA�7LA�7LA�9XA�;dA�;dA�9XA�7LA�33A�7LA�1'A�1'A�7LA�7LA�1'A�/A�/A�33A�7LA�33A�33A�5?A�/A�/A�/A�/A�1'A�33A�7LA�1'A�/A�-A�(�A�-A�+A�(�A�&�A�&�A�(�A�+A�+A�+A�(�A�&�A�(�A�+A�+A�(�A�&�A�&�A�+A�-A�+A�(�A�(�A�-A�-A�(�A�&�A��A��A� �A� �A� �A�"�A�$�A�&�A�(�A�+A�+A�&�A�&�A�&�A�(�A�+A�+A�+A�(�A�$�A��A��A��A�{A�bA�%A�A�%A�A�A�A�A�A�A�  A�  A���A���A��A��A��A��yAļjAĺ^Aĕ�Aĉ7AċDAčPAăA�dZA�`BA�\)A�S�A�E�AÍPA��HA�G�A��!A�S�A���A��jA�p�A�^5A�C�A���A��/A��^A���A��PA�z�A�C�A��
A�ȴA��-A���A��DA�v�A�ZA�O�A�E�A�"�A���A��/A�z�A�K�A���A��A�ƨA��A�;dA�
=A���A���A�|�A�ffA�O�A�{A��A���A��PA�7LA���A���A�x�A�E�A�oA���A��A��`A��FA�=qA�oA���A���A�z�A�I�A�{A��mA���A��hA�n�A�`BA�O�A�E�A�C�A�?}A�7LA�7LA�1'A�(�A��A�JA���A���A��A�ĜA���A��A�$�A�
=A�A���A��mA��
A���A���A��RA��9A���A�|�A�ffA�VA�33A��!A�|�A�n�A�Q�A�5?A�(�A�"�A��A�VA���A�p�A��;A���A��RA��!A��-A��FA��A��A���A���A��hA�|�A�G�A�$�A���A��
A��HA��`A��TA���A��
A��RA���A��PA�t�A�ZA�E�A�E�A�"�A�bA���A��
A�A��A�v�A�oA��\A�~�A�l�A�^5A�-A�"�A��TA��!A��+A�C�A�"�A��HA��uA��7A�?}A��mA��mA���A��!A���A�`BA��A��A��A�ffA�O�A�-A�VA���A��mA���A�A��9A���A��A�Q�A�E�A��^A��A��uA�
=A��^A��A���A��A��A�Q�A�9XA�(�A��9A�$�A�Q�A�hsA�A�A��A��`A���A��jA���A�v�A�XA�?}A�7LA�5?A�5?A�/A�/A�(�A��A��A�bA�bA��A��A�ƨA��RA��A��A���A���A���A���A���A��uA�bA�A��
A�A�r�A�
=A��wA���A��A�bNA�5?A�&�A��A�bA�  A���A���A�M�A�5?A�JA���A��yA��TA��/A��#A�ƨA���A��FA���A��7A�p�A�C�A�"�A���A��A��`A��
A�ȴA��-A���A���A��PA��A��A�r�A�hsA�^5A�;dA�$�A���A��A��yA���A��uA�&�A���A�jA�XA�-A��A���A��#A���A���A���A��FA�|�A�E�A��HA�~�A�I�A��A�ĜA���A�C�A�1A��;A��uA���A�ffA�1A���A���A���A�|�A�|�A�|�A�z�A�t�A�r�A�l�A�l�A�jA�hsA�`BA�XA�C�A�E�A�;dA�7LA�-A�/A��A���A��mA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                     A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A�$�A�&�A�&�A�&�A�(�A�(�A�+A�+A�+A�-A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�33A�/A�/A�9XA�9XA�33A�33A�1'A�(�A�(�A�+A�(�A�"�A�(�A� �A�%A���AĴ9A�VA��A�z�A�|�A�`BA�A�O�A��/A�n�A�bNA�"�A���A��HA�r�A�VA���A��-A�n�A��;A��A�  A��TA�$�A���A��+A�K�A��FA� �A���A��+A��FA�5?A�VA��RA�S�A��A�C�A��7A��HA���A���A���A�^5A��
A�t�A��
A�%A��A�|�A���A�p�A�VA�/A���A�ZA���A��RA� �A��jA�bNA�ȴA���A�33A�A�A�A���A�I�A��mA�33A�XA��TA��A��A}+Au�;Ar�uAp$�Ao%AmK�Ak+Ah=qAf�yAe�Ab��AadZA_A]S�A\�`A\bNAZĜAX1'ATZAP��AP-AL^5AH��AHI�AG�PAFffADVAA�PAA`BA@��A@z�A?�wA?+A>�A=�A=�
A=�A<bNA;t�A8�/A61A5K�A3�;A2��A0A�A//A/%A/A.^5A,~�A*bNA(��A'�A&ȴA&�DA&5?A%��A$��A#�mA"��A!�-A!33A!�A �jA�AbNA�A7LA�A/A�TA
=Az�A�TAVAffA��AȴA�A`BA�+A�AK�A9XA�mAoA^5A��AVA
Q�A	��A�A��AQ�A��A�A9XAS�A��A�+AA�A  �@�~�@�E�@��@�@���@��@�(�@���@�K�@�G�@�I�@�R@�@�M�@�b@�-@���@�P@��@���@��D@�A�@ߝ�@�n�@�1'@�7L@ش9@ج@؛�@؋D@�  @�t�@�o@��@�7L@�bN@��@ӕ�@�^5@ѩ�@У�@�(�@Ϯ@��@Χ�@�E�@ͺ^@�/@̣�@�33@ʇ+@��@ɑh@��/@��@���@���@��/@ȓu@Ǖ�@�C�@��@���@š�@þw@�E�@���@���@�O�@���@��m@�C�@��y@��R@�n�@��7@�t�@�M�@���@�@��-@�`B@�hs@��u@�b@���@�K�@�@��y@���@��@�X@�Ĝ@�Q�@� �@�ƨ@���@�S�@�o@�ȴ@�^5@��#@�x�@��@���@�j@���@�K�@�@��+@���@�`B@��9@��@�A�@���@��@�33@�o@�o@��@�E�@���@���@��@�O�@�&�@��@��@�bN@��@���@��F@���@��@�S�@��y@�p�@�%@���@��D@�z�@�A�@�(�@��;@�dZ@��y@�ȴ@���@��\@�5?@���@�X@�?}@��@�%@��@���@��D@�bN@��m@�ȴ@��\@��\@��\@��+@�n�@�ff@�=q@��T@���@���@�%@�Q�@� �@��F@�\)@��@��R@��R@�^5@��-@�Ĝ@�Z@�A�@�1'@��@�S�@��-@��D@���@��w@���@��@�l�@�\)@�K�@��@���@���@��R@��!@��+@���@�%@��@��@��@�G�@��@���@��^@��T@��@���@�G�@��;@�bN@�1'@�ƨ@��P@�K�@�v�@�M�@���@���@�I�@�bN@�r�@�I�@�1'@�  @�  @�ƨ@��@�\)@�V@�5?@�$�@�p�@�?}@��@�G�@��@��T@���@�x�@���@�|�@��@�l�@�33@�
=@�
=@�o@���@���@�-@���@�`B@�V@���@�Q�@��@��F@�\)@�33@���@�=q@�-@�-@�{@���@��T@��T@��#@���@��h@�G�@�7L@�V@��/@�bN@�Q�@�Z@�Q�@�9X@�(�@�(�@��@�b@�1@�  @�  @~��@~5?@}�@|�@|z�@|I�@|1@|1@{��@{�m@{�
@{ƨ@{��@{��@{t�@{33@{"�@{@z�@y�^@yG�@x��@x�u@xb@w��@v��@v�@v��@u@uO�@tz�@s��@s33@r�@q��@q��@q��@q��@q�7@qhs@q�@q%@p�`@p�9@p��@pQ�@o�@n�y@n��@n@m/@lz�@k��@kC�@j��@jn�@j^5@jM�@jM�@jM�@i��@ihs@hr�@g\)@g�@g
=@f�y@f�R@fV@e��@e�T@e�@d�@c�m@c@b=q@a��@a&�@a�@`�@`  @_�P@^�y@^�+@^{@^@^{@^@]�T@]�T@]�T@]@]�-@]�h@]/@\�@\I�@[�m@[�@Zn�@Y��@Y�@Yhs@X�9@Xr�@Xb@W��@Wl�@Vȴ@Vv�@U@U�@T�/@T��@T�j@T�j@T��@T�D@TI�@S�m@SS�@So@R��@R~�@R=q@RJ@Q��@Q�@Qhs@P�`@P�@PbN@Pb@O��@N��@Nv�@Nv�@Nff@NE�@N@M�@L��@L��@L�@L�D@Lj@L9X@L9X@L�@K�F@K�@I�#@I��@I��@I�@H�9@HbN@H �@G�@G�@Gl�@G+@F�@F�R@F��@Fv�@E@DI�@C�F@C��@C��@C��@C��@C�F@C��@C��@Ct�@C33@B=q@A�#@A��@A��@A��@A��@A��@A�7@A�7@Ax�@AX@@��@@Q�@?�;@?l�@?
=@>�+@>$�@=��@=�h@=p�@=/@<�j@<(�@;ƨ@;S�@;o@:�H@:��@:=q@9G�@8�9@8��@8�@8bN@81'@81'@7�@7�;@7�@7l�@7;d@7
=@6�R@6�+@6{@5�h@5`B@5/@5V@4�/@4j@3�
@3�@3dZ@3dZ@3S�@333@3"�@3o@2�H@2~�@2-@1��@1��@1�7@1x�@1G�@0��@0Ĝ@0r�@0bN@0bN@0bN@0r�@0bN@0r�@01'@/��@/�@/��@/K�@/
=@.ȴ@.��@.��@.ff@.E�@.$�@-�T@-/@,��@,�@,1@+�
@+�@+dZ@+S�@+"�@*�H@*�!@*��@*n�@*J@)��@)x�@)X@)7L@)�@(��@(�`@(��@(��@(A�@(  @'K�@'+@'�@&��@&�R@&ff@&5?@%�@%p�@%/@$�@$�@$�@$�j@$��@$�@#��@#"�@#o@"��@"��@"M�@"-@!�@!�^@!�7@!7L@ �`@ bN@   @�@|�@\)@�y@ff@��@`B@�@��@�F@��@"�@o@�@�\@^5@M�@-@J@J@��@�^@��@%@�`@�u@ �@�@��@|�@+@�@
=@
=@��@�y@�@�R@��@�+@5?@�@�-@`B@?}@�@��@�@��@�D@9X@1@�
@��@t�@C�@�\@^5@��@��@��@X@%@��@Ĝ@Ĝ@�9@��@Q�@A�@  @�@��@l�@;d@�@��@ȴ@�+@v�@ff@E�@{@@`B@O�@/@�@`B@p�@?}@�@��@�j@(�@�@�@�@��@ƨ@dZ@"�@
�@
�!@
�\@
~�@
n�@
n�@
^5@
M�@
J@	��@	�7@	G�@	&�@	�@	�@	%@Ĝ@�u@r�@b@�@�@�P@|�@\)@;d@+@�@
=@�y@�+@{@G�O�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�"�A� �A��A� �A��A��A��A��A��A��A�oA��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A� �A� �A��A��A� �A��A��A��A��A��A��A� �A�"�A�"�A�$�A�(�A�$�A�$�A�(�A�(�A�&�A�&�A�(�A�(�A�&�A�$�A�(�A�&�A�$�A�&�A�(�A�&�A�&�A�+A�+A�(�A�&�A�(�A�(�A�&�A�(�A�+A�+A�(�A�(�A�+A�-A�+A�(�A�(�A�+A�-A�+A�(�A�-A�-A�(�A�(�A�-A�/A�+A�(�A�(�A�+A�/A�/A�-A�-A�1'A�1'A�/A�-A�/A�1'A�/A�/A�33A�1'A�/A�/A�1'A�33A�1'A�/A�/A�1'A�33A�33A�33A�/A�1'A�33A�33A�1'A�-A�-A�-A�1'A�33A�/A�-A�-A�1'A�33A�/A�/A�1'A�33A�33A�/A�/A�1'A�33A�1'A�1'A�/A�1'A�/A�/A�/A�/A�33A�33A�1'A�/A�1'A�33A�5?A�1'A�-A�/A�1'A�1'A�1'A�33A�1'A�1'A�33A�5?A�7LA�5?A�1'A�-A�/A�/A�/A�-A�/A�/A�/A�/A�-A�-A�1'A�+A�+A�1'A�1'A�5?A�7LA�9XA�;dA�;dA�7LA�7LA�;dA�;dA�7LA�7LA�9XA�;dA�;dA�9XA�7LA�33A�7LA�1'A�1'A�7LA�7LA�1'A�/A�/A�33A�7LA�33A�33A�5?A�/A�/A�/A�/A�1'A�33A�7LA�1'A�/A�-A�(�A�-A�+A�(�A�&�A�&�A�(�A�+A�+A�+A�(�A�&�A�(�A�+A�+A�(�A�&�A�&�A�+A�-A�+A�(�A�(�A�-A�-A�(�A�&�A��A��A� �A� �A� �A�"�A�$�A�&�A�(�A�+A�+A�&�A�&�A�&�A�(�A�+A�+A�+A�(�A�$�A��A��A��A�{A�bA�%A�A�%A�A�A�A�A�A�A�  A�  A���A���A��A��A��A��yAļjAĺ^Aĕ�Aĉ7AċDAčPAăA�dZA�`BA�\)A�S�A�E�AÍPA��HA�G�A��!A�S�A���A��jA�p�A�^5A�C�A���A��/A��^A���A��PA�z�A�C�A��
A�ȴA��-A���A��DA�v�A�ZA�O�A�E�A�"�A���A��/A�z�A�K�A���A��A�ƨA��A�;dA�
=A���A���A�|�A�ffA�O�A�{A��A���A��PA�7LA���A���A�x�A�E�A�oA���A��A��`A��FA�=qA�oA���A���A�z�A�I�A�{A��mA���A��hA�n�A�`BA�O�A�E�A�C�A�?}A�7LA�7LA�1'A�(�A��A�JA���A���A��A�ĜA���A��A�$�A�
=A�A���A��mA��
A���A���A��RA��9A���A�|�A�ffA�VA�33A��!A�|�A�n�A�Q�A�5?A�(�A�"�A��A�VA���A�p�A��;A���A��RA��!A��-A��FA��A��A���A���A��hA�|�A�G�A�$�A���A��
A��HA��`A��TA���A��
A��RA���A��PA�t�A�ZA�E�A�E�A�"�A�bA���A��
A�A��A�v�A�oA��\A�~�A�l�A�^5A�-A�"�A��TA��!A��+A�C�A�"�A��HA��uA��7A�?}A��mA��mA���A��!A���A�`BA��A��A��A�ffA�O�A�-A�VA���A��mA���A�A��9A���A��A�Q�A�E�A��^A��A��uA�
=A��^A��A���A��A��A�Q�A�9XA�(�A��9A�$�A�Q�A�hsA�A�A��A��`A���A��jA���A�v�A�XA�?}A�7LA�5?A�5?A�/A�/A�(�A��A��A�bA�bA��A��A�ƨA��RA��A��A���A���A���A���A���A��uA�bA�A��
A�A�r�A�
=A��wA���A��A�bNA�5?A�&�A��A�bA�  A���A���A�M�A�5?A�JA���A��yA��TA��/A��#A�ƨA���A��FA���A��7A�p�A�C�A�"�A���A��A��`A��
A�ȴA��-A���A���A��PA��A��A�r�A�hsA�^5A�;dA�$�A���A��A��yA���A��uA�&�A���A�jA�XA�-A��A���A��#A���A���A���A��FA�|�A�E�A��HA�~�A�I�A��A�ĜA���A�C�A�1A��;A��uA���A�ffA�1A���A���A���A�|�A�|�A�|�A�z�A�t�A�r�A�l�A�l�A�jA�hsA�`BA�XA�C�A�E�A�;dA�7LA�-A�/A��A���A��mA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bx8Bw�Bw�BxBxBwfBw2Bx8BxBxBxBw�Bw�Bw�Bx8Bw�Bw�BxBw�Bw�BxBxBxBxBxBw�BxBw�BxBw�Bw�BxBw�Bw�Bw�BxBw�BxBxBx8BxBw�Bx8BxBw�BxBw�BwfBv�Bu�BtTBrBr|Bv�BxBpBjKBf�BhsBi�BpBqBoiBrGBo�BncBr�Bz�Bu�By	Bt�Bt�Bo5Bq�BxlBpoBh�B^5BUgBUgB=qB9�B"�BB_B�B�B��B��B��B�B�jB��B�UB�BB��B��B�=B��BcBn�BM6BDgBC�B@�B<B7LB/B,B&�BIB+BBfB��B�B�yB�TB�)B�sB�6B��B�'B��B�\B� BW�B=B+kB \B�B
rB
��B
�B
��B
�6B
��B
�}B
�OB
��B
�B
�qB
�PB
~�B
bB
YKB
QNB
:�B
4nB
1[B
+�B
*�B
�B
�B
�B
{B
@B
�B
"B
	�B
�B
�B
oB	�JB	�JB	��B	�B	�fB	��B	��B	��B	��B	ӏB	�2B	ΥB	�B	��B	��B	�B	��B	�B	�3B	��B	��B	�IB	�=B	��B	��B	��B	��B	��B	�4B	��B	��B	�\B	��B	�7B	��B	�YB	��B	�MB	��B	��B	�B	��B	�:B	�(B	��B	�rB	��B	��B	��B	�B	�oB	~�B	}�B	}�B	.B	z�B	{�B	zDB	z�B	y�B	yrB	xB	w�B	wfB	wfB	u%B	uZB	uZB	u�B	v�B	uZB	t�B	wfB	t�B	w2B	x8B	xB	tTB	uZB	�B	{JB	z�B	{�B	�B	�iB	�iB	�B	�4B	��B	�xB	�bB	�:B	��B	��B	��B	�zB	��B	�eB	��B	��B	�UB	�[B	�-B	��B	�B	��B	��B	��B	��B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	�'B	��B	�tB	��B	ɺB	��B	�#B	�RB	��B	�)B	��B	�dB	ΥB	уB	�sB	��B	�B	�B	��B	�ZB	��B	�`B	�B	�/B	�B	�ZB	��B	�lB	��B	��B
 �B
B
�B
�B
�B
fB
B
�B
~B
�B
 B
�B
�B
�B
�B
�B
YB
�B
qB
CB
�B
B
~B
�B
 �B
!�B
#:B
&B
($B
)�B
/�B
1[B
4nB
6�B
:*B
:^B
:*B
:�B
>�B
@B
@�B
A�B
B�B
C-B
DgB
F�B
H�B
J�B
LdB
PB
Q�B
R B
S[B
VB
WsB
V9B
WsB
X�B
YKB
\]B
]dB
`vB
e`B
e�B
e�B
e�B
f�B
j�B
m)B
n/B
tB
wfB
w�B
w2B
x8B
x8B
y>B
yrB
zB
zxB
~(B
�B
�4B
��B
��B
��B
��B
��B
��B
�PB
� B
�hB
�FB
�MB
�SB
��B
�MB
�$B
��B
��B
�B
�B
�B
��B
��B
��B
�_B
�1B
�1B
��B
��B
��B
�qB
��B
��B
�!B
��B
��B
��B
��B
�nB
�B
��B
��B
�IB
��B
�B
�B
��B
��B
�wB
�B
�UB
�qB
�3B
��B
��B
�?B
��B
ĜB
�EB
�KB
��B
�XB
��B
͟B
��B
�vB
��B
��B
�9B
�B
�gB
�[B
ӏB
��B
��B
ԕB
�mB
֡B
��B
�B
��B
�fB
�
B
�,B
�mB
��B
��B
�B
�B
�oB
��B
��B
�B
��B
�B
�%B
�`B
��B
��B
�B
��B
�B
��B
��B �BoBAB�B�B�B�BB�BYB�B�B	B	�BB�B�B(B�B�BbB�B�B�B~B(B�B�B�BVB�B(B(B�B�B(B�B�B�B4B�B B�BB�B�B�B�B�B�B�B7B�B�BxBOB�B�B \B#:B#B#B#B#B#:B#�B#�B$B$�B$tB%B&B'B'B(�B)_B+�B-�B-�B/OB/OB.�B/�B/�B.�B0UB0�B2aB5tB5B5?B5tB5tB5�B5�B7B8B8RB6�B5�B4�B5?B5�B6B7LB6�B7�B8RB8�B9�B:*B;0B;�B;�B;�B;�B<jB=B=B=<B=<B?B>�B?}B>�B?B@OBA�BB[BB�BCaBCaBC�BD�BD�BE�BFtBF?BFtBF?BFtBF�BF?BF?BGzBGzBGzBG�BHKBHKBH�BH�BHKBI�BJXBJXBJ#BJXBJ�BK�BK^BK�BK�BL0BL�BMBL�BLdBL0BL�BMBL�BK�BK�BL0BK�BK�BI�BI�BI�BI�BI�BI�BI�BI�BJXBJXBJ�BJ�BJ�BJ�BL�BNpBM6BM�BM�BMjBM�BMjBM�BMjBM�BM�BN�BNBN<BN�BN<BNpBN<BN�BN<BN<BNpBN�BO�BO�BPHBO�BPBO�BP}BQ�BQNBQ�BQ�BR�BR�BS�BS�BS�BS�BT�BVBU�BUgBU�BVmBVmBV�BV�BW
BW
BW?BW?BWsBW�BW�BX�BX�BX�BY�BYBY�B[�B[#BZ�BZ�BZ�BZ�BZ�BZ�BZ�BZ�B[#BZ�B[WB[�B[�B[#B[WB\]B\)B\)B\�B\)B\]B[�B\�B\)B\�B]dB]/B]/B^B]�B^5B^5B^B^5B^jB^jB^�B_�B_�B_pB`�B`�BaBaHBaBaBa�Ba|Ba�Ba|BbNBb�Bc Bb�Bb�Bc�Bc�Bc�Bc�Bc�Bd&Bd�Be`Be`Be�Be�Bf2Bf�Bf�Bg8Bg�Bh
Bh
Bg�BgmBh
Bg�Bh�BiyBi�Bi�BjBjBj�BkQBl"Bl�Bl�Bm]Bm�Bn�Bo BoiBo�BoiBp;Bq�Br�BsBs�Bu�Bu%Bu%Bu�Bt�Bt�Bu�BwfBw�BxlBxlBx8Bx8Bw�BxBx8Bw�BxBx�Bx�Bx8Bx�BzBzxB{�B|B|B|B{�B|B|�B|B}VB}VB}�B~(B~(B~(B~�B~�B~�B~�BcB�B�B� B�4B�4B��B�B��B�uB�AB�B�B�{B��B��B��B�{B��B��B��B��B��B��B�SB�SB�SB��B��B�YB��B�+B��B��B��B��B�1B��B�lB�=B�	B�B��B�B��B�B�B�B�B�JB�B��B��B�"B��B�VB�"B�"B�"B�"B�"B�(B��B��B��B�\B�(B�\B��B��B��B��B��B��B��B��B��B��B� B��B��B��B�4B�hB��B�:Bw2Bx8ByrBw2Bv�Bx�Bv�Bv�BxlBy	BwfBxBx�BxBu�Bx8BxlBu�Bv�Bx�Bx�Bv�BxBy�Bv�Bv�Bx�By>BxBv�Bv�Bx8Bx8Bw2BxBx�Bv�Bv�BxByrBw2Bw2Bx�Bv�Bw2BxlBw�Bv�Bw�BzxBw2BxBy>Bv�BuZBu�Bx�BxBv�BxlBxlBv�Bw2BxBx�BwfBw2Bx�Bx�Bv�Bw�Bx�BxBv�BxBx8Bw2BwfBx8Bx�BwfBw�Bx�Bx�Bw2BwfBx�Bx�Bw�Bw�Bw�Bx�ByrBw�BwfBxBx�BwfBwfBx�Bx�Bw�Bv�BxlBy	By	Bx�Bw2BwfBx�BxBv�Bw2BxBx�Bw�Bv�Bx�BxlBw2Bw�Bx�Bx8BwfBv�Bw�By	Bx�BxBv�Bv�Bw�By	Bx8Bw�Bw2BxBy	Bx8BwfBwfBv�BxlBx�BxlBwfBv�Bx�Bx�BwfBv�Bw�Bx�Bx�Bw�Bv�Bw�BxBx�Bx8BwfBw2BxBx�BwfBv�BxBx�Bw�BxBv�Bx�Bw�Bw�Bw�BxBw�Bv�BxlBy>Bx8Bw�BwfBw�Bw�By>Bw�Bv�BwfBx�Bw�BxBv�Bv�BzDBx8Bv�Bx�Bx8Bv�Bw�ByrBy	Bw�Bv`BxBy	Bx�Bw2Bw�Bx�By	Bx�Bw2BwfBx�Bx�By�Bv+Bx8Bx�Bw�Bv�By�By	By�BxBv�Bx�BxlBw�Bv�Bv�Bx�Bx�Bv�Bv�Bv�Bx�Bx�By	Bw2Bv�Bw�BxBy	By	BxlBw2Bv�BwfBx�Bx�Bx8Bv�Bw�BxBy	ByrBw�Bv�Bw2BxlBx�Bw2Bw�Bw2BxlB{Bw�BwfBv�Bw2BxBxBx�BxBv`Bv`BwfBxBw�Bw�Bv�Bv+Bv�Bw�Bv�BxBv+Bs�Bv�BwfBv`Bv�Bv`Bv�BtBt�Bt�Bv�Bt�Bu�Br�BsMBt�Bv�Br�BqABs�Br|BtBs�BqBo Bo5Bl�Bt�Bk�Bj�BjKBhsB�JBy>Bx�B��Bo�BpBn�BlWBm�Bu%BzBr�BrBt�BrBtBv�B�lB�uBo�Bo�Bm]Bo5BpoBkBiyBg�BiBi�Bu�BjKBi�BiyBa�Bp�Bj�Bd�BlWBh�Bb�BbBa�BiyBa�Bg�Bh
Bf�Bk�Bj�Bf�BjBh�Bf2Be,Bc Bj�BcBkQBkBrGBj�Bl�Bv+Bp�Bu%Bo�Bt�Bo�BqABp;BncBo BpBm]Bn�BpoBo�Bo�Bo5Bm�BncBuZBr�Br|Bz�Bp;BoiBoiBo�Bp;Bn�Bm�BoiBn�Bl�BqBo BkQBo5B��Bm]Bm�BqvBr�Br�BqABsMBqABs�B��BcBw2Bw2Bv`Bu%Bs�Bt�Bu%Bx�BrGBw�By	B�GBx8B��Bu�Bn�Br�Bt�Bp�Br�BrGBsBs�BtBu%BuZBqBo�Bl�BncBl�Bk�BiyBrGBz�Bo�Bm)BkQBk�ByrB�MBsMBrBsBtTBl�Bx�BiyBk�Bp;BgmBe�Bl�BhsBjBffBuZBf�BaB^5B\�B[�B^jBW�BYKBW�BU�BS�BT�BQ�BXBIB`B]�BW�BOvBNpBH�BDgB;dB:�B;�B5?B2-BD�BA�BPHB*eB&�B'�B'RB 'B 'B \B$@B�B7BkB1B�B�B_B1BBMB�B{BIBhB�BMB�BVB\B�B
rBB�B�B(�B	BB�B��B��B�DB�B�fB�fB�NB��B��B��B�BߤB�/B�B�2B�B� B�NB�BB�vB�B��B�6B˒B�)B��B˒B��B��BɺB�gB�9BÖB�mB��B�OB��B��B�}B�B��B�qB��BB��B�<B�nB�?B��B�XB��B�zB�$B�tB��B��B�nB��B��B��B�$B�SB�=B��B�_B�uB��B��B��B}�ByrBuZBr|BxB�BiBa|Be�BlWBS&BK)BD3BC�BCaBGBC-BC�BCaBC�BC�BC�BD�BD�B?HBA�B@�B@B=�B@�B@B>�B<�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202110151929172021101519291720211015192917202110151929172021101519291720211015192917SI  SI  ARFMARFM                                                                                                                                                2021021318281420210213182814IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022320005720210223200057QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022320005720210223200057QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074520211014130745IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                