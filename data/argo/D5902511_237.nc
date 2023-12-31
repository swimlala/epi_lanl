CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-11-16T04:02:15Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     @  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     @  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     @  �(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  �x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ >�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ^   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ e�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221116040215  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_237                 6810_008521_237                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��G!�.I@��G!�.I11  @��GJ#9�@��GJ#9�@2_!-w1�@2_!-w1��d�"}��d�"}�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @}p�@��R@�G�@�G�@��RA\)A   A+�A?\)A`��A���A���A�Q�A�\)A�\)A�\)A�  A�Q�A��B  B  B�
B   B((�B0(�B7�
B@  BG�
BO�
BX(�B`Q�Bh(�Bo�
Bw�
B�  B��B��B�  B�  B�{B�(�B�(�B�  B�  B�  B�  B�  B��
B�  B�  B��
B��B�{B�{B�=qB�(�B��B��
B��B�{B�  B��B��
B��
B��B�  B��C  C
=C  C��C	��C  C
=C{C  C��C
=C  C��C��C��C   C"  C$  C%��C'��C*  C+��C.  C0  C2  C3��C5�C7��C:
=C<
=C>  C@  CB  CD
=CF  CG��CJ
=CL
=CM��CP  CR  CT  CV  CX  CZ  C\
=C^  C`
=Ca��Cd  Cf  Ch  Cj
=Ck��Cn  Cp  Cr
=Ct  Cu��Cx  Cz
=C{��C~  C�C���C�C�
=C�C���C�  C�C�  C�
=C���C���C���C���C�  C�  C���C�  C�  C���C���C�  C�  C�C���C���C�  C�  C�  C�  C���C���C�  C�  C���C���C�C�
=C�C�  C���C�  C�C�C�C�  C�  C���C���C�  C�C�
=C�  C�  C�C�C�
=C�
=C���C���C�  C���C�  C�C�C�C�  C�  C�  C�  C�C�C�  C�C�  C�  C�  C�C�  C���C�C�C���C�  C�  C�  C�  C�  C�  C���C���C�C�C���C���C�  C�  C�C�  C���C���C�  C�  C�  C�C���C��C���C�C�
=C�C�  C�C�C���C���C�  C�C�C�  C���C���C���C�  C�C�C�  C�D �D ��D  D� D�qD� D�D�D�D}qD��D� D  D� D  D}qD  D� D�qD	� D
  D
��D  D}qD  D��D�D� D�qD� D  D� D�D}qD��D}qD  D� D  D��D�qDz�D�qD}qD��Dz�D�qD}qD�D�D�qDxRD  D��D  D� D  D}qD�qD}qD��D}qD��D}qD�qD }qD �qD!}qD"  D"�D#�D#� D$  D$z�D$�qD%� D&�D&}qD&�qD'� D'�qD(}qD(�qD)� D*D*� D*�qD+}qD,�D,��D-�D-��D.�D.� D/  D/��D0�D0�D1  D1��D2�D2��D3  D3� D4�D4��D5�D5}qD5�qD6��D7�D7� D8  D8� D9  D9� D:  D:� D;  D;}qD<  D<� D=  D=��D=�qD>� D?  D?}qD?�qD@� DA�DA}qDB  DB��DC  DCz�DC�qDD��DE  DEz�DE�qDF��DG  DG��DH  DH� DIDI� DJ�DJ��DK  DK� DL  DL��DM�DM��DN  DN� DN�qDO� DO�qDP}qDQ  DQ��DR  DR��DSDS}qDS��DT}qDU�DU��DU��DV� DW�DW� DX�DX�DY  DY}qDY�qDZ}qD[  D[}qD\  D\�D]D]��D^  D^� D_  D_� D`  D`� Da�Da� Da��Db��DcDc}qDc��Dd}qDe  De}qDf  Df� Dg�Dg��Dh�Dh� Di�Di��Dj  Dj� Dk�Dk� Dl  Dl}qDl�qDm}qDn  Dn��Do�Do� Dp  Dpz�Dp�qDq��Dr�Dr��Ds  Ds��DtDt� Dt�qDu� Dv�Dv� Dv��Dwz�Dw��Dxz�Dx�qDy� Dz  Dz� D{�D{��D|  D|� D}  D}� D}�qD~��DD� D�  D�@ D�� D��qD�  D�@ D�� D��HD�  D�>�D�~�D��HD�HD�@ D�~�D�� D�  D�@ D�~�D�� D�HD�@ D�}qD��qD���D�AHD�� D���D���D�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�� D���D��qD�@ D��HD�� D���D�@ D�~�D���D�  D�>�D�� D�� D���D�AHD�� D���D�  D�AHD�~�D�� D�HD�@ D��HD���D�  D�AHD�� D��HD�  D�@ D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D�  D�AHD�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D��HD�  D�AHD��HD�� D���D�@ D��HD�� D�  D�AHD�� D�� D�  D�@ D��HD���D���D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD�~�D�� D�  D�>�D�� D�� D���D�>�D��HD�� D���D�@ D��HD�D�  D�>�D�� D�� D�HD�AHD�� D�� D�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�� D���D�  D�AHD���D��HD�  D�>�D�� D�� D�HD�AHD�� D��qD��qD�>�D�� D���D�  D�AHD�� D���D���D�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD���D��HD�  D�@ D�~�D�� D�  D�>�D�� D�� D�  D�=qD�� D��HD��D�AHD�~�D��qD���D�>�D�~�D��HD��D�>�D�� D�D�  D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D�~�D���D�HD�>�D�}qD��qD��qD�@ D���D��HD�  D�@ D�� D�� D���D�=qD�~�D���D��qD�>�D��HD��HD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�AHD��HD��HD�  D�AHD�� D��qD���D�@ D�~�D���D�  D�>�D�~�D�� D���D�@ DÁHD�� D���D�>�DĀ D��HD���D�=qD�~�D�D��D�>�Dƀ D��HD�HD�>�Dǀ D��HD���D�>�DȁHD�� D�HD�B�DɁHD�� D���D�@ DʁHD�� D���D�>�D�~�D˽qD��qD�=qD̀ D��HD�  D�AHD́HD�� D�  D�<)D�}qDξ�D���D�@ Dς�D��HD�  D�@ DЀ D�� D�HD�@ D�~�DѽqD���D�AHDҁHD��HD�  D�AHDӁHDӾ�D���D�=qD�|)DԽqD���D�>�DՀ D��HD�HD�AHDցHD��HD�HD�AHDׁHD�� D�  D�AHD؀ Dؾ�D���D�>�D�}qDٽqD���D�AHDځHD�� D��D�B�DہHD۾�D���D�AHD܁HD��HD�HD�@ D�~�Dݾ�D�  D�@ DށHD�D�HD�@ D߁HD��HD���D�>�D�}qD�qD�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�=qD�}qD㾸D�  D�B�D�HD��HD�HD�@ D�~�D�� D�  D�>�D�}qD�� D�  D�=qD� D��HD�HD�B�D� D辸D�HD�AHD�~�D�� D�  D�@ D�~�D�� D�  D�>�D� D�� D�  D�@ D�HD�D�HD�>�D� D�� D��qD�>�D�HD��HD�HD�>�D� D��HD���D�=qD�~�D�� D�HD�AHD� D��HD�  D�=qD� D�� D���D�AHD� D�D�HD�B�D�HD���D�  D�AHD��HD���D��qD�>�D�� D��qD���D�>�D�~�D�� D�  D�AHD�u�>�?��?u?�z�?�p�?��@�\@��@(�@(��@=p�@L��@W
=@p��@z�H@��@���@�Q�@��@��@�
=@\@���@�
=@�  @�@�
=@��RAffA�A  A
=A�HA!�A'
=A,��A3�
A8Q�A?\)AE�AJ=qAQ�AW
=A\��Adz�Ah��Ap  Au�Az�HA�Q�A��\A�{A��A�33A���A�  A��A�(�A�
=A���A�(�A�{A�  A�33A�z�A��A�G�A�(�A�{A���A��\A�p�A�  A��A��A��RA��A��
A�\)A�G�A��A�
=Aҏ\A���A�Q�Aڏ\A�A���A�33A�
=A���A���A�ffA�=qA��
A�\)A���A���A�
=B ��B{B�B��B=qB
=B��B	��B\)B(�B�B�HBz�Bp�B�HB  BG�B�HB�BG�B{B�B��B=qB\)B ��B!�B"�HB$z�B%p�B&�HB((�B)G�B*�HB+�
B-p�B.ffB/�B1G�B2=qB3�B4��B5�B7�B8��B9�B;�B<z�B>=qB?\)B@��BB=qBC
=BD��BEBG33BH��BI��BK\)BLz�BM��BO\)BP(�BQ��BS33BT(�BU��BW
=BX  BYBZ�RB\  B]��B^�RB_�
Bap�BbffBc�
Be�BfffBg�
Bh��Bj�\Bk�Bl��Bn=qBo\)Bp��Bq�Bs\)Bt��BuBw\)BxQ�By��B{33B|  B}p�B~�HB�B��RB��B��
B��\B�
=B��
B�z�B���B�B�=qB���B�B�(�B���B�\)B�(�B���B�G�B�{B��\B�G�B��
B�ffB�33B��B�ffB��HB�p�B�=qB���B�p�B�(�B��\B�\)B�  B��\B�\)B�B�z�B�33B���B�ffB���B�p�B�=qB���B�p�B��B�ffB�33B�B�(�B���B���B�{B��RB�G�B�{B�z�B�33B��B�ffB��B�B�=qB�
=B��B�(�B��HB�\)B�  B���B�G�B��
B���B��B�B��\B��B��B�z�B���B���B�z�B���B��B�ffB��HB���B�ffB��HB��B�ffB��HB�p�B�=qB��HB�\)B�(�B���B�G�B�  B���B��B�{B¸RB��B�  Bģ�B��B�  Bƣ�B��B�B�z�B���BɅB�{Bʏ\B��B�B�{B̏\B�33B͙�B��B�z�B���B��B�B�  B�Q�B���B���B�\)B��
B�  B�ffB���B�
=B�p�B��B�{B�z�B��HB�
=Bՙ�B��
B�(�B֣�B���B�
=Bי�B��
B�{B؏\B��HB��Bٙ�B�{B�(�Bڣ�B���B�33B�B�{B�=qBܸRB��B�G�B��
B�{B�ffB��HB�33B�\)B��
B�Q�B�z�B���B�\)B�B��B�ffB�\B��HB�\)B�B��B�ffB�RB���B�p�B��
B�  B�ffB��HB�
=B�p�B��
B�  B�z�B��HB�33B�\)B��
B�Q�B�z�B���B�\)B�B��B�Q�B��HB��B�p�B��B�Q�B�\B��HB�p�BB�  B�z�B�RB�
=B�B�  B�=qB�\B��B�B�B�=qB���B��HB�G�B��
B�{B�ffB���B�33B��B�  B�ffB��RB�33B�p�B��
B�ffB��\B���B��B�B�{B��RB��HB�33B�B�  B�Q�B��HB��B�p�C   C {C \)C �\C ��C �C{C=qC�C��C��C{C=qC\)C�CC  C33CG�C�\C�RC�
C�CG�C\)C��C�
C��C�CffC�C�C��C{CG�C�C�C��C{C=qC\)C��C�
C�C33CffC�CC	
=C	�C	\)C	��C	�RC
  C
33C
Q�C
��C
C
�C(�CffCz�CC��C{CG�C�C��C��C
=CG�CffC��C�
C��C33Cp�C��C��C��C33Cz�C�C��C  C33Cp�C�C�
C  C=qCz�C�C��C
=CG�Cz�C��C�
C{C33Cz�C�RC�HC
=CQ�C�C�C�
C�C\)C�\C�RC�C�C\)C��C��C�C{CffC�\CC  C=qCffC��CC
=CQ�Cp�C��C�
C�CG�Cp�C�RC  C�CG�C�C��C��C�CQ�C��C�
C{C=qCffC��C�HC�C=qCz�C�RC��C{CQ�C��CC�HC (�C p�C ��C C!  C!=qC!ffC!�\C!��C"{C"Q�C"�C"�C"�C#(�C#z�C#�C#�
C$33C$ffC$�\C$��C%{C%G�C%p�C%�RC&  C&33C&\)C&��C&�HC'33C'ffC'�\C'��C(�C(\)C(�C(C){C)\)C)�\C)C)��C*G�C*�\C*�
C+
=C+=qC+p�C+�C+��C,G�C,z�C,�RC,�HC-�C-ffC-�C-��C.33C.ffC.��C.�
C/
=C/Q�C/��C/�
C0{C0Q�C0�\C0��C1  C1Q�C1��C1�C2�C2Q�C2��C2�C333C3p�C3��C3�HC4(�C4z�C4C5
=C5Q�C5�\C5��C6
=C6G�C6��C6�HC733C7z�C7�RC7��C833C8p�C8�RC9  C9Q�C9��C9�
C:�C:\)C:��C:�
C;{C;Q�C;��C;�HC<(�C<z�C<C<��C=33C=p�C=�RC>
=C>\)C>��C>��C?
=C?G�C?��C?�HC@(�C@p�C@�C@�HCA�CA\)CA�CA��CB=qCB�CB�CB�CC33CC�\CC�
CD{CD\)CD�\CD��CE
=CEQ�CE��CE�CF33CFp�CF�CF�HCG�CG\)CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                       ?�  @   @@  @}p�@��R@�G�@�G�@��RA\)A   A+�A?\)A`��A���A���A�Q�A�\)A�\)A�\)A�  A�Q�A��B  B  B�
B   B((�B0(�B7�
B@  BG�
BO�
BX(�B`Q�Bh(�Bo�
Bw�
B�  B��B��B�  B�  B�{B�(�B�(�B�  B�  B�  B�  B�  B��
B�  B�  B��
B��B�{B�{B�=qB�(�B��B��
B��B�{B�  B��B��
B��
B��B�  B��C  C
=C  C��C	��C  C
=C{C  C��C
=C  C��C��C��C   C"  C$  C%��C'��C*  C+��C.  C0  C2  C3��C5�C7��C:
=C<
=C>  C@  CB  CD
=CF  CG��CJ
=CL
=CM��CP  CR  CT  CV  CX  CZ  C\
=C^  C`
=Ca��Cd  Cf  Ch  Cj
=Ck��Cn  Cp  Cr
=Ct  Cu��Cx  Cz
=C{��C~  C�C���C�C�
=C�C���C�  C�C�  C�
=C���C���C���C���C�  C�  C���C�  C�  C���C���C�  C�  C�C���C���C�  C�  C�  C�  C���C���C�  C�  C���C���C�C�
=C�C�  C���C�  C�C�C�C�  C�  C���C���C�  C�C�
=C�  C�  C�C�C�
=C�
=C���C���C�  C���C�  C�C�C�C�  C�  C�  C�  C�C�C�  C�C�  C�  C�  C�C�  C���C�C�C���C�  C�  C�  C�  C�  C�  C���C���C�C�C���C���C�  C�  C�C�  C���C���C�  C�  C�  C�C���C��C���C�C�
=C�C�  C�C�C���C���C�  C�C�C�  C���C���C���C�  C�C�C�  C�D �D ��D  D� D�qD� D�D�D�D}qD��D� D  D� D  D}qD  D� D�qD	� D
  D
��D  D}qD  D��D�D� D�qD� D  D� D�D}qD��D}qD  D� D  D��D�qDz�D�qD}qD��Dz�D�qD}qD�D�D�qDxRD  D��D  D� D  D}qD�qD}qD��D}qD��D}qD�qD }qD �qD!}qD"  D"�D#�D#� D$  D$z�D$�qD%� D&�D&}qD&�qD'� D'�qD(}qD(�qD)� D*D*� D*�qD+}qD,�D,��D-�D-��D.�D.� D/  D/��D0�D0�D1  D1��D2�D2��D3  D3� D4�D4��D5�D5}qD5�qD6��D7�D7� D8  D8� D9  D9� D:  D:� D;  D;}qD<  D<� D=  D=��D=�qD>� D?  D?}qD?�qD@� DA�DA}qDB  DB��DC  DCz�DC�qDD��DE  DEz�DE�qDF��DG  DG��DH  DH� DIDI� DJ�DJ��DK  DK� DL  DL��DM�DM��DN  DN� DN�qDO� DO�qDP}qDQ  DQ��DR  DR��DSDS}qDS��DT}qDU�DU��DU��DV� DW�DW� DX�DX�DY  DY}qDY�qDZ}qD[  D[}qD\  D\�D]D]��D^  D^� D_  D_� D`  D`� Da�Da� Da��Db��DcDc}qDc��Dd}qDe  De}qDf  Df� Dg�Dg��Dh�Dh� Di�Di��Dj  Dj� Dk�Dk� Dl  Dl}qDl�qDm}qDn  Dn��Do�Do� Dp  Dpz�Dp�qDq��Dr�Dr��Ds  Ds��DtDt� Dt�qDu� Dv�Dv� Dv��Dwz�Dw��Dxz�Dx�qDy� Dz  Dz� D{�D{��D|  D|� D}  D}� D}�qD~��DD� D�  D�@ D�� D��qD�  D�@ D�� D��HD�  D�>�D�~�D��HD�HD�@ D�~�D�� D�  D�@ D�~�D�� D�HD�@ D�}qD��qD���D�AHD�� D���D���D�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�� D���D��qD�@ D��HD�� D���D�@ D�~�D���D�  D�>�D�� D�� D���D�AHD�� D���D�  D�AHD�~�D�� D�HD�@ D��HD���D�  D�AHD�� D��HD�  D�@ D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D�  D�AHD�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D��HD�  D�AHD��HD�� D���D�@ D��HD�� D�  D�AHD�� D�� D�  D�@ D��HD���D���D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD�~�D�� D�  D�>�D�� D�� D���D�>�D��HD�� D���D�@ D��HD�D�  D�>�D�� D�� D�HD�AHD�� D�� D�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�� D���D�  D�AHD���D��HD�  D�>�D�� D�� D�HD�AHD�� D��qD��qD�>�D�� D���D�  D�AHD�� D���D���D�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD���D��HD�  D�@ D�~�D�� D�  D�>�D�� D�� D�  D�=qD�� D��HD��D�AHD�~�D��qD���D�>�D�~�D��HD��D�>�D�� D�D�  D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D�~�D���D�HD�>�D�}qD��qD��qD�@ D���D��HD�  D�@ D�� D�� D���D�=qD�~�D���D��qD�>�D��HD��HD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�AHD��HD��HD�  D�AHD�� D��qD���D�@ D�~�D���D�  D�>�D�~�D�� D���D�@ DÁHD�� D���D�>�DĀ D��HD���D�=qD�~�D�D��D�>�Dƀ D��HD�HD�>�Dǀ D��HD���D�>�DȁHD�� D�HD�B�DɁHD�� D���D�@ DʁHD�� D���D�>�D�~�D˽qD��qD�=qD̀ D��HD�  D�AHD́HD�� D�  D�<)D�}qDξ�D���D�@ Dς�D��HD�  D�@ DЀ D�� D�HD�@ D�~�DѽqD���D�AHDҁHD��HD�  D�AHDӁHDӾ�D���D�=qD�|)DԽqD���D�>�DՀ D��HD�HD�AHDցHD��HD�HD�AHDׁHD�� D�  D�AHD؀ Dؾ�D���D�>�D�}qDٽqD���D�AHDځHD�� D��D�B�DہHD۾�D���D�AHD܁HD��HD�HD�@ D�~�Dݾ�D�  D�@ DށHD�D�HD�@ D߁HD��HD���D�>�D�}qD�qD�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�=qD�}qD㾸D�  D�B�D�HD��HD�HD�@ D�~�D�� D�  D�>�D�}qD�� D�  D�=qD� D��HD�HD�B�D� D辸D�HD�AHD�~�D�� D�  D�@ D�~�D�� D�  D�>�D� D�� D�  D�@ D�HD�D�HD�>�D� D�� D��qD�>�D�HD��HD�HD�>�D� D��HD���D�=qD�~�D�� D�HD�AHD� D��HD�  D�=qD� D�� D���D�AHD� D�D�HD�B�D�HD���D�  D�AHD��HD���D��qD�>�D�� D��qD���D�>�D�~�D�� D�  D�AHG�O�>�?��?u?�z�?�p�?��@�\@��@(�@(��@=p�@L��@W
=@p��@z�H@��@���@�Q�@��@��@�
=@\@���@�
=@�  @�@�
=@��RAffA�A  A
=A�HA!�A'
=A,��A3�
A8Q�A?\)AE�AJ=qAQ�AW
=A\��Adz�Ah��Ap  Au�Az�HA�Q�A��\A�{A��A�33A���A�  A��A�(�A�
=A���A�(�A�{A�  A�33A�z�A��A�G�A�(�A�{A���A��\A�p�A�  A��A��A��RA��A��
A�\)A�G�A��A�
=Aҏ\A���A�Q�Aڏ\A�A���A�33A�
=A���A���A�ffA�=qA��
A�\)A���A���A�
=B ��B{B�B��B=qB
=B��B	��B\)B(�B�B�HBz�Bp�B�HB  BG�B�HB�BG�B{B�B��B=qB\)B ��B!�B"�HB$z�B%p�B&�HB((�B)G�B*�HB+�
B-p�B.ffB/�B1G�B2=qB3�B4��B5�B7�B8��B9�B;�B<z�B>=qB?\)B@��BB=qBC
=BD��BEBG33BH��BI��BK\)BLz�BM��BO\)BP(�BQ��BS33BT(�BU��BW
=BX  BYBZ�RB\  B]��B^�RB_�
Bap�BbffBc�
Be�BfffBg�
Bh��Bj�\Bk�Bl��Bn=qBo\)Bp��Bq�Bs\)Bt��BuBw\)BxQ�By��B{33B|  B}p�B~�HB�B��RB��B��
B��\B�
=B��
B�z�B���B�B�=qB���B�B�(�B���B�\)B�(�B���B�G�B�{B��\B�G�B��
B�ffB�33B��B�ffB��HB�p�B�=qB���B�p�B�(�B��\B�\)B�  B��\B�\)B�B�z�B�33B���B�ffB���B�p�B�=qB���B�p�B��B�ffB�33B�B�(�B���B���B�{B��RB�G�B�{B�z�B�33B��B�ffB��B�B�=qB�
=B��B�(�B��HB�\)B�  B���B�G�B��
B���B��B�B��\B��B��B�z�B���B���B�z�B���B��B�ffB��HB���B�ffB��HB��B�ffB��HB�p�B�=qB��HB�\)B�(�B���B�G�B�  B���B��B�{B¸RB��B�  Bģ�B��B�  Bƣ�B��B�B�z�B���BɅB�{Bʏ\B��B�B�{B̏\B�33B͙�B��B�z�B���B��B�B�  B�Q�B���B���B�\)B��
B�  B�ffB���B�
=B�p�B��B�{B�z�B��HB�
=Bՙ�B��
B�(�B֣�B���B�
=Bי�B��
B�{B؏\B��HB��Bٙ�B�{B�(�Bڣ�B���B�33B�B�{B�=qBܸRB��B�G�B��
B�{B�ffB��HB�33B�\)B��
B�Q�B�z�B���B�\)B�B��B�ffB�\B��HB�\)B�B��B�ffB�RB���B�p�B��
B�  B�ffB��HB�
=B�p�B��
B�  B�z�B��HB�33B�\)B��
B�Q�B�z�B���B�\)B�B��B�Q�B��HB��B�p�B��B�Q�B�\B��HB�p�BB�  B�z�B�RB�
=B�B�  B�=qB�\B��B�B�B�=qB���B��HB�G�B��
B�{B�ffB���B�33B��B�  B�ffB��RB�33B�p�B��
B�ffB��\B���B��B�B�{B��RB��HB�33B�B�  B�Q�B��HB��B�p�C   C {C \)C �\C ��C �C{C=qC�C��C��C{C=qC\)C�CC  C33CG�C�\C�RC�
C�CG�C\)C��C�
C��C�CffC�C�C��C{CG�C�C�C��C{C=qC\)C��C�
C�C33CffC�CC	
=C	�C	\)C	��C	�RC
  C
33C
Q�C
��C
C
�C(�CffCz�CC��C{CG�C�C��C��C
=CG�CffC��C�
C��C33Cp�C��C��C��C33Cz�C�C��C  C33Cp�C�C�
C  C=qCz�C�C��C
=CG�Cz�C��C�
C{C33Cz�C�RC�HC
=CQ�C�C�C�
C�C\)C�\C�RC�C�C\)C��C��C�C{CffC�\CC  C=qCffC��CC
=CQ�Cp�C��C�
C�CG�Cp�C�RC  C�CG�C�C��C��C�CQ�C��C�
C{C=qCffC��C�HC�C=qCz�C�RC��C{CQ�C��CC�HC (�C p�C ��C C!  C!=qC!ffC!�\C!��C"{C"Q�C"�C"�C"�C#(�C#z�C#�C#�
C$33C$ffC$�\C$��C%{C%G�C%p�C%�RC&  C&33C&\)C&��C&�HC'33C'ffC'�\C'��C(�C(\)C(�C(C){C)\)C)�\C)C)��C*G�C*�\C*�
C+
=C+=qC+p�C+�C+��C,G�C,z�C,�RC,�HC-�C-ffC-�C-��C.33C.ffC.��C.�
C/
=C/Q�C/��C/�
C0{C0Q�C0�\C0��C1  C1Q�C1��C1�C2�C2Q�C2��C2�C333C3p�C3��C3�HC4(�C4z�C4C5
=C5Q�C5�\C5��C6
=C6G�C6��C6�HC733C7z�C7�RC7��C833C8p�C8�RC9  C9Q�C9��C9�
C:�C:\)C:��C:�
C;{C;Q�C;��C;�HC<(�C<z�C<C<��C=33C=p�C=�RC>
=C>\)C>��C>��C?
=C?G�C?��C?�HC@(�C@p�C@�C@�HCA�CA\)CA�CA��CB=qCB�CB�CB�CC33CC�\CC�
CD{CD\)CD�\CD��CE
=CEQ�CE��CE�CF33CFp�CF�CF�HCG�CG\)CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                       @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�A�r�A�t�A�x�A�z�A�z�A�|�A�|�A�z�A�|�A�x�A�x�A�r�A�v�A�|�A�r�A�r�A�l�A�v�A�dZA�bNA�bNA�M�A�M�A�7LA�5?A�5?A�7LA�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�9XA�9XA�7LA�33A�{Aٙ�A�XA�/A�Q�AӉ7A�1A�l�AЩ�A�VA�"�ÁAʲ-A�oA�G�A�{A���A��A�A�A�|�A��FA��jA���A�=qA���A�7LA�1A�ZA��FA�bNA��uA�9XA��A�dZA�{A�XA��A��A���A��A��!A�hsA�r�A���A��7A�(�A���A��A��A�ĜA�/A���A���A���A��DA�I�A�"�A�=qA��A���A�n�A���A��jA�K�A�%A�r�A���A���A�-AS�A|v�AyC�Avz�AtbNAo/Ai&�Ac��Ab�uAa�A_K�A[��AXZAW�AU�;AR�!AO/AL��AJ��AI�mAG��AE+AD(�AB�/A@1'A?x�A?33A>jA;��A:I�A9dZA8�DA7�#A6�A5�A3�;A1�hA0��A/&�A.ffA-��A,jA*~�A(��A(��A(  A&~�A%�wA%%A$VA${A#+A"(�A �HA 9XAdZA��A1A�A��A;dA��AO�A��A�A�A{A�wAv�A-A+A�TA�A�AXA�A�An�A�A+A�mA
A�A	�hA$�A|�AJA�jA^5A�Al�A�A��A ��A b@��@��m@�G�@�bN@��m@�ƨ@���@��9@�O�@�b@���@�|�@�33@��@�C�@��u@�\@��@�ƨ@���@�{@陚@�z�@�&�@��
@�@�t�@�w@�@�F@�Z@�K�@��@�\)@�ȴ@�7L@�%@���@�@� �@�@�ȴ@�G�@���@�j@�Z@� �@ߥ�@�J@�r�@�v�@֟�@�@��@�bN@��
@�l�@��y@�V@���@�&�@�%@�Ĝ@�  @��@��#@�Z@� �@�dZ@�;d@��y@�ff@���@˅@�5?@Ɂ@�V@��@�~�@Ɵ�@�@�7L@Ĵ9@�A�@� �@�bN@��@�Z@�j@�A�@��m@��H@�{@�p�@��@��@�r�@�Z@��@�X@�I�@�t�@�+@��+@��!@�\)@��@�"�@�$�@�1@�  @�  @�  @�r�@�(�@�1@�ƨ@���@��y@�E�@��!@���@���@��\@�~�@��@��@��j@�z�@�b@�9X@�  @��
@��@�ƨ@�1@�l�@�|�@�;d@��T@�z�@�1@�ƨ@�@��y@���@�ȴ@��!@�^5@��T@���@��/@��@�(�@��m@���@��w@�l�@���@�^5@�V@�-@���@���@��@��-@�&�@��j@���@��u@�(�@�b@��@�j@�j@� �@��@�\)@��@��!@�M�@�-@��@��@��u@�A�@�  @���@�K�@��@��@��-@���@�hs@���@��`@��/@��/@���@�Ĝ@��@��@�b@��F@��F@�t�@��@��R@���@�ff@�~�@�5?@�@��^@���@�hs@��@���@���@���@��D@�1'@���@�dZ@��y@��\@�^5@�E�@�=q@���@�@���@�G�@���@���@�bN@�1'@��
@�l�@�
=@���@�-@�@�G�@�V@�Ĝ@�j@�A�@� �@��@�o@���@�v�@��\@�$�@���@�&�@���@��/@���@��D@���@��@�A�@��@��F@�\)@��@���@�n�@�M�@�V@���@�x�@��@��u@��u@��D@��@�z�@�r�@�bN@�bN@�Q�@�1'@�(�@�|�@�^5@���@�`B@�/@�V@��`@���@�bN@� �@���@�|�@�@��@���@���@�^5@�@��T@��^@���@��7@�x�@�G�@��@���@��9@��m@��@�\)@�K�@�+@���@�M�@�=q@�E�@�5?@�$�@���@�G�@��/@���@���@�r�@� �@��@���@�|�@�C�@��@�n�@�=q@�@��h@�X@�%@���@�I�@�b@�w@�P@~�R@~�+@~V@~@}�T@}�-@}�h@}`B@}/@|�j@|�@{�@z��@y�^@y%@xĜ@x��@yX@x��@x�@xb@w��@w\)@w;d@w;d@w�@v�y@u�h@tZ@s��@s�@sS�@s@r�@r�@s@r�\@rJ@q�#@q��@p��@p�@pb@o�w@ol�@n�y@n�R@n��@nff@n{@m�@m��@m@m�h@m/@l��@lZ@l(�@k�m@kdZ@k@j�!@j~�@jM�@i��@i��@ix�@i7L@h�`@h�9@h��@h�@hr�@hQ�@g��@gl�@f�y@f�+@fff@f5?@f@e�-@e`B@d��@d9X@c��@c33@b��@b��@b~�@b��@bn�@a��@a�7@aX@a&�@`  @_�@_l�@_+@^��@^@]��@]?}@\��@\j@\�@[�
@[�F@[��@[C�@Z��@Z��@Z�\@ZM�@Y��@X��@X1'@W�w@V��@V�+@V5?@U�-@U/@T�D@T(�@TI�@T1@SS�@R��@Rn�@RJ@Q��@QX@QG�@Q&�@P��@P��@P�u@PbN@O|�@N�R@NV@N5?@M�@M�-@M�@M?}@L��@L�/@L��@Lz�@KdZ@J�@J��@J^5@I��@I��@I�7@IG�@I�@H��@H�`@H��@H1'@G��@G\)@G;d@F�@F5?@F{@F@F@E�-@E?}@E�@D��@D��@D�j@D��@Dj@D9X@D�@C��@Co@B^5@B�@A��@A%@@�`@@Ĝ@@�@@A�@?�@?�@?|�@?K�@?+@>ȴ@>ff@>$�@>@=�T@=��@=`B@=O�@<�@<Z@<(�@;�m@;�F@;�F@;�F@;o@9�@9x�@8��@8�@8A�@8 �@7�@7�;@7�P@7\)@7;d@7+@7+@7�@6��@6ȴ@6��@6��@6v�@6V@6{@5�@5�@4�/@4�@4��@4��@4z�@41@3C�@2��@2^5@1�@1��@1hs@0Ĝ@0Q�@0 �@0 �@0  @/|�@/;d@/�@.�R@.v�@.$�@-�-@-�@-V@,�/@,z�@,�@+�F@+dZ@+o@*�@*�H@*��@*��@*~�@*=q@*J@)��@)�7@)X@(��@(�`@(��@(�@(bN@(1'@'�@'�@'�P@'|�@'K�@'
=@&�y@&�y@&ȴ@&v�@&ff@&V@&V@&E�@&5?@%�@%`B@%O�@$��@$j@$I�@$1@#ƨ@#�@#C�@"�@"�\@"-@!�#@!�^@!hs@ ��@ �@ 1'@ b@��@��@|�@\)@K�@;d@+@�@
=@��@ff@5?@$�@{@��@�h@O�@�@�j@j@9X@�
@��@��@C�@o@@�H@��@�!@��@M�@-@�@�#@��@��@x�@G�@7L@�@�`@�@1'@��@��@\)@�@�y@ȴ@�R@�+@E�@{@@�-@p�@/@V@�@��@�j@�@I�@�@�
@�
@ƨ@�F@��@t�@dZ@S�@@�\@-@�@�@�^@�7@�7@X@�@�`@��@�u@bN@1'@�;@��@��@�P@�P@l�@K�@;d@�@�y@ȴ@��@ff@{@@�@`B@�/@��@j@I�@9X@(�@�@�m@��@�@t�@S�@"�@
�@
�!@
��@
~�@
~�@
n�@
^5A�dZA�jA�ffA�p�A�p�A�n�A�x�A�t�A�t�A�t�A�v�A�v�A�r�A�x�A�t�A�z�A�x�A�x�A�~�A�x�A�z�A�~�A�x�A�~�A�z�A�|�A�|�A�x�A�|�A�z�A�x�A�|�A�z�A�~�A�x�A�x�A�|�A�x�A�|�A�v�A�r�A�r�A�p�A�p�A�x�A�p�A�t�A�p�A�p�A�t�A�v�A�~�A�x�A�~�A�x�A�~�A�|�A�|�A�~�A�n�A�r�A�p�A�p�A�t�A�p�A�t�A�n�A�t�A�n�A�t�A�n�A�v�A�t�A�p�A�x�A�r�A�bNA�Q�A�r�A�r�A�|�A�v�A�z�A�n�A�t�A�dZA�K�A�bNA�jA�r�A�p�A�r�A�hsA�bNA�I�A�I�A�G�A�\)A�dZA�v�A�r�A�n�A�E�A�O�A�I�A�Q�A�K�A�S�A�O�A�VA�K�A�Q�A�I�A�E�A�A�A�7LA�;dA�33A�9XA�33A�9XA�33A�7LA�5?A�5?A�7LA�33A�9XA�33A�7LA�7LA�5?A�9XA�33A�9XA�5?A�5?A�7LA�33A�7LA�7LA�33A�7LA�5?A�5?A�9XA�33A�9XA�5?A�5?A�9XA�33A�9XA�5?A�5?A�7LA�1'A�7LA�33A�33A�7LA�1'A�33A�7LA�33A�7LA�7LA�33A�7LA�33A�33A�9XA�5?A�33A�9XA�33A�5?A�33A�5?A�5?A�33A�9XA�33A�7LA�5?A�5?A�9XA�33A�7LA�33A�33A�9XA�5?A�5?A�9XA�5?A�5?A�9XA�33A�9XA�33A�7LA�9XA�33A�9XA�7LA�5?A�9XA�5?A�7LA�9XA�5?A�;dA�5?A�9XA�7LA�5?A�;dA�5?A�;dA�7LA�9XA�;dA�5?A�;dA�7LA�7LA�=qA�9XA�;dA�;dA�7LA�=qA�9XA�7LA�9XA�33A�7LA�7LA�33A�7LA�33A�1'A�7LA�/A�33A�1'A�+A�-A�+A� �A�oA��TA�t�A��A�VAٗ�A�1A�JA�JA�%A���A�z�A�7LA���Aכ�A�p�A�(�A�hsA���A�ȴA�~�A�
=A�A�|�A�K�A�9XA�1A��;A��TA���AӰ!AӇ+A�(�A��`A�jA� �A�oA�
=A�ȴAѣ�AсA�n�A�hsA�jA�jA�ffA�jA�ZA�bAоwA�\)Aϡ�A��;A�|�A� �A�VA���A͡�A�S�A�&�A�"�A�$�A�"�A��A��A��A��A�bA�JA��A̲-A��A�`BA�A���A��HA���A���A���Aʰ!Aʣ�AʓuAʉ7A�~�A�p�A�l�A�bNA�E�A�33A�&�A��A�A��mA���A���Aɰ!Aɥ�Aə�Aɇ+A�v�A�l�A�`BA�K�A�33A�1A��mA���AȲ-AȓuAȇ+A�p�A�Q�A�;dA�bA��HAǺ^AǍPA�~�A�v�A�I�A�&�A�%A��yA���A�AƶFAƣ�Aƕ�AƏ\AƁA�x�A�r�A�XA�=qA�-A�$�A��A� �A�%A��`A��/A��
A���A�ƨAżjAŮAť�A�~�A�?}A�bA��TAĺ^Aģ�Aĕ�A�v�A�C�A��A���AøRA�M�A��A��A���A¥�A\A�XA�1A��#A�A��A���A��\A�v�A�`BA�O�A�C�A�=qA�7LA�+A�{A��A��A�p�A���A��A��;A��^A��!A��!A���A���A���A���A��hA��\A��A�v�A�hsA�ZA�VA�VA�E�A�1'A�-A�"�A��A��A�%A���A��A���A��^A���A�ffA�\)A�S�A�E�A�?}A��A�1A���A���A�(�A��#A���A�ƨA��-A��DA�x�A�l�A�\)A�VA�E�A�"�A��A��
A�ƨA��jA��A���A��DA�|�A�hsA�&�A�oA��A���A�M�A��A��A��9A��PA�M�A���A��A�t�A�dZA�VA�5?A�A��/A��jA���A��DA�p�A�XA�;dA�/A�&�A��A�bA�%A���A��A��;A���A��!A��A�O�A��yA�ƨA��RA��PA�M�A���A�jA�%A��FA���A�XA�bA��/A��+A�  A�ȴA��!A��+A�K�A�1A���A��A��yA��mA��`A��/A���A�A�bNA���A�n�A�
=A�ƨA��uA�|�A�Q�A�bA��
A�A��9A���A���A��A�z�A�r�A�jA�hsA�`BA�I�A�33A�+A�$�A��A�VA���A��A��mA���A��9A���A��PA�p�A�dZA�E�A�-A��A���A�bA���A�^5A�(�A���A��`A��;A���A�ƨA���A�l�A�VA�7LA�ȴA�VA��A��A�XA�A���A�(�A��#A��PA�XA�bA���A���A���A���A��PA�n�A�G�A�&�A�oA��A��^A��A�VA�$�A�A��TA��!A��A�v�A�(�A��+A�A���A���A�hsA�(�A�JA���A��A��;A�ȴA��9A��\A�S�A�A���A���A�~�A�bNA�O�A�E�A�E�A�C�A�;dA�7LA�+A�$�A�+A�bA�VA�
=A���A��TA���A�ƨA�ȴA�ƨA��^A��!A���A���A��uA��hA��PA��A�r�A�VA�5?A�/A�{A�JA��TA�hsA���A�"�A���A��/A���A��A��9A�
=A�|�A�v�A�dZA��A�r�A���A��A�jA�VA�?}A��A�A���A��`A��HA��HA��HA��;A��;A��A�ȴA�ƨA�A��FA��9A��A��A��A���A���A���A���A���A���A��uA��DA��DA��DA��A�t�A�p�A�bNA�VA�I�A�A�A�7LA��A���A���A�n�A��A���A���A��A�G�A��A�ĜA���A�n�A�+A���A�v�A�E�A��A�A���A��A��;A��
A�ĜA��jA��9A���A���A��\A��\A��+A��A�z�A�v�A�r�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                       A�l�A�r�A�t�A�x�A�z�A�z�A�|�A�|�A�z�A�|�A�x�A�x�A�r�A�v�A�|�A�r�A�r�A�l�A�v�A�dZA�bNA�bNA�M�A�M�A�7LA�5?A�5?A�7LA�5?A�5?A�5?A�5?A�5?A�5?A�5?A�5?A�7LA�7LA�7LA�7LA�7LA�9XA�9XA�7LA�33A�{Aٙ�A�XA�/A�Q�AӉ7A�1A�l�AЩ�A�VA�"�ÁAʲ-A�oA�G�A�{A���A��A�A�A�|�A��FA��jA���A�=qA���A�7LA�1A�ZA��FA�bNA��uA�9XA��A�dZA�{A�XA��A��A���A��A��!A�hsA�r�A���A��7A�(�A���A��A��A�ĜA�/A���A���A���A��DA�I�A�"�A�=qA��A���A�n�A���A��jA�K�A�%A�r�A���A���A�-AS�A|v�AyC�Avz�AtbNAo/Ai&�Ac��Ab�uAa�A_K�A[��AXZAW�AU�;AR�!AO/AL��AJ��AI�mAG��AE+AD(�AB�/A@1'A?x�A?33A>jA;��A:I�A9dZA8�DA7�#A6�A5�A3�;A1�hA0��A/&�A.ffA-��A,jA*~�A(��A(��A(  A&~�A%�wA%%A$VA${A#+A"(�A �HA 9XAdZA��A1A�A��A;dA��AO�A��A�A�A{A�wAv�A-A+A�TA�A�AXA�A�An�A�A+A�mA
A�A	�hA$�A|�AJA�jA^5A�Al�A�A��A ��A b@��@��m@�G�@�bN@��m@�ƨ@���@��9@�O�@�b@���@�|�@�33@��@�C�@��u@�\@��@�ƨ@���@�{@陚@�z�@�&�@��
@�@�t�@�w@�@�F@�Z@�K�@��@�\)@�ȴ@�7L@�%@���@�@� �@�@�ȴ@�G�@���@�j@�Z@� �@ߥ�@�J@�r�@�v�@֟�@�@��@�bN@��
@�l�@��y@�V@���@�&�@�%@�Ĝ@�  @��@��#@�Z@� �@�dZ@�;d@��y@�ff@���@˅@�5?@Ɂ@�V@��@�~�@Ɵ�@�@�7L@Ĵ9@�A�@� �@�bN@��@�Z@�j@�A�@��m@��H@�{@�p�@��@��@�r�@�Z@��@�X@�I�@�t�@�+@��+@��!@�\)@��@�"�@�$�@�1@�  @�  @�  @�r�@�(�@�1@�ƨ@���@��y@�E�@��!@���@���@��\@�~�@��@��@��j@�z�@�b@�9X@�  @��
@��@�ƨ@�1@�l�@�|�@�;d@��T@�z�@�1@�ƨ@�@��y@���@�ȴ@��!@�^5@��T@���@��/@��@�(�@��m@���@��w@�l�@���@�^5@�V@�-@���@���@��@��-@�&�@��j@���@��u@�(�@�b@��@�j@�j@� �@��@�\)@��@��!@�M�@�-@��@��@��u@�A�@�  @���@�K�@��@��@��-@���@�hs@���@��`@��/@��/@���@�Ĝ@��@��@�b@��F@��F@�t�@��@��R@���@�ff@�~�@�5?@�@��^@���@�hs@��@���@���@���@��D@�1'@���@�dZ@��y@��\@�^5@�E�@�=q@���@�@���@�G�@���@���@�bN@�1'@��
@�l�@�
=@���@�-@�@�G�@�V@�Ĝ@�j@�A�@� �@��@�o@���@�v�@��\@�$�@���@�&�@���@��/@���@��D@���@��@�A�@��@��F@�\)@��@���@�n�@�M�@�V@���@�x�@��@��u@��u@��D@��@�z�@�r�@�bN@�bN@�Q�@�1'@�(�@�|�@�^5@���@�`B@�/@�V@��`@���@�bN@� �@���@�|�@�@��@���@���@�^5@�@��T@��^@���@��7@�x�@�G�@��@���@��9@��m@��@�\)@�K�@�+@���@�M�@�=q@�E�@�5?@�$�@���@�G�@��/@���@���@�r�@� �@��@���@�|�@�C�@��@�n�@�=q@�@��h@�X@�%@���@�I�@�b@�w@�P@~�R@~�+@~V@~@}�T@}�-@}�h@}`B@}/@|�j@|�@{�@z��@y�^@y%@xĜ@x��@yX@x��@x�@xb@w��@w\)@w;d@w;d@w�@v�y@u�h@tZ@s��@s�@sS�@s@r�@r�@s@r�\@rJ@q�#@q��@p��@p�@pb@o�w@ol�@n�y@n�R@n��@nff@n{@m�@m��@m@m�h@m/@l��@lZ@l(�@k�m@kdZ@k@j�!@j~�@jM�@i��@i��@ix�@i7L@h�`@h�9@h��@h�@hr�@hQ�@g��@gl�@f�y@f�+@fff@f5?@f@e�-@e`B@d��@d9X@c��@c33@b��@b��@b~�@b��@bn�@a��@a�7@aX@a&�@`  @_�@_l�@_+@^��@^@]��@]?}@\��@\j@\�@[�
@[�F@[��@[C�@Z��@Z��@Z�\@ZM�@Y��@X��@X1'@W�w@V��@V�+@V5?@U�-@U/@T�D@T(�@TI�@T1@SS�@R��@Rn�@RJ@Q��@QX@QG�@Q&�@P��@P��@P�u@PbN@O|�@N�R@NV@N5?@M�@M�-@M�@M?}@L��@L�/@L��@Lz�@KdZ@J�@J��@J^5@I��@I��@I�7@IG�@I�@H��@H�`@H��@H1'@G��@G\)@G;d@F�@F5?@F{@F@F@E�-@E?}@E�@D��@D��@D�j@D��@Dj@D9X@D�@C��@Co@B^5@B�@A��@A%@@�`@@Ĝ@@�@@A�@?�@?�@?|�@?K�@?+@>ȴ@>ff@>$�@>@=�T@=��@=`B@=O�@<�@<Z@<(�@;�m@;�F@;�F@;�F@;o@9�@9x�@8��@8�@8A�@8 �@7�@7�;@7�P@7\)@7;d@7+@7+@7�@6��@6ȴ@6��@6��@6v�@6V@6{@5�@5�@4�/@4�@4��@4��@4z�@41@3C�@2��@2^5@1�@1��@1hs@0Ĝ@0Q�@0 �@0 �@0  @/|�@/;d@/�@.�R@.v�@.$�@-�-@-�@-V@,�/@,z�@,�@+�F@+dZ@+o@*�@*�H@*��@*��@*~�@*=q@*J@)��@)�7@)X@(��@(�`@(��@(�@(bN@(1'@'�@'�@'�P@'|�@'K�@'
=@&�y@&�y@&ȴ@&v�@&ff@&V@&V@&E�@&5?@%�@%`B@%O�@$��@$j@$I�@$1@#ƨ@#�@#C�@"�@"�\@"-@!�#@!�^@!hs@ ��@ �@ 1'@ b@��@��@|�@\)@K�@;d@+@�@
=@��@ff@5?@$�@{@��@�h@O�@�@�j@j@9X@�
@��@��@C�@o@@�H@��@�!@��@M�@-@�@�#@��@��@x�@G�@7L@�@�`@�@1'@��@��@\)@�@�y@ȴ@�R@�+@E�@{@@�-@p�@/@V@�@��@�j@�@I�@�@�
@�
@ƨ@�F@��@t�@dZ@S�@@�\@-@�@�@�^@�7@�7@X@�@�`@��@�u@bN@1'@�;@��@��@�P@�P@l�@K�@;d@�@�y@ȴ@��@ff@{@@�@`B@�/@��@j@I�@9X@(�@�@�m@��@�@t�@S�@"�@
�@
�!@
��@
~�@
~�@
n�G�O�A�dZA�jA�ffA�p�A�p�A�n�A�x�A�t�A�t�A�t�A�v�A�v�A�r�A�x�A�t�A�z�A�x�A�x�A�~�A�x�A�z�A�~�A�x�A�~�A�z�A�|�A�|�A�x�A�|�A�z�A�x�A�|�A�z�A�~�A�x�A�x�A�|�A�x�A�|�A�v�A�r�A�r�A�p�A�p�A�x�A�p�A�t�A�p�A�p�A�t�A�v�A�~�A�x�A�~�A�x�A�~�A�|�A�|�A�~�A�n�A�r�A�p�A�p�A�t�A�p�A�t�A�n�A�t�A�n�A�t�A�n�A�v�A�t�A�p�A�x�A�r�A�bNA�Q�A�r�A�r�A�|�A�v�A�z�A�n�A�t�A�dZA�K�A�bNA�jA�r�A�p�A�r�A�hsA�bNA�I�A�I�A�G�A�\)A�dZA�v�A�r�A�n�A�E�A�O�A�I�A�Q�A�K�A�S�A�O�A�VA�K�A�Q�A�I�A�E�A�A�A�7LA�;dA�33A�9XA�33A�9XA�33A�7LA�5?A�5?A�7LA�33A�9XA�33A�7LA�7LA�5?A�9XA�33A�9XA�5?A�5?A�7LA�33A�7LA�7LA�33A�7LA�5?A�5?A�9XA�33A�9XA�5?A�5?A�9XA�33A�9XA�5?A�5?A�7LA�1'A�7LA�33A�33A�7LA�1'A�33A�7LA�33A�7LA�7LA�33A�7LA�33A�33A�9XA�5?A�33A�9XA�33A�5?A�33A�5?A�5?A�33A�9XA�33A�7LA�5?A�5?A�9XA�33A�7LA�33A�33A�9XA�5?A�5?A�9XA�5?A�5?A�9XA�33A�9XA�33A�7LA�9XA�33A�9XA�7LA�5?A�9XA�5?A�7LA�9XA�5?A�;dA�5?A�9XA�7LA�5?A�;dA�5?A�;dA�7LA�9XA�;dA�5?A�;dA�7LA�7LA�=qA�9XA�;dA�;dA�7LA�=qA�9XA�7LA�9XA�33A�7LA�7LA�33A�7LA�33A�1'A�7LA�/A�33A�1'A�+A�-A�+A� �A�oA��TA�t�A��A�VAٗ�A�1A�JA�JA�%A���A�z�A�7LA���Aכ�A�p�A�(�A�hsA���A�ȴA�~�A�
=A�A�|�A�K�A�9XA�1A��;A��TA���AӰ!AӇ+A�(�A��`A�jA� �A�oA�
=A�ȴAѣ�AсA�n�A�hsA�jA�jA�ffA�jA�ZA�bAоwA�\)Aϡ�A��;A�|�A� �A�VA���A͡�A�S�A�&�A�"�A�$�A�"�A��A��A��A��A�bA�JA��A̲-A��A�`BA�A���A��HA���A���A���Aʰ!Aʣ�AʓuAʉ7A�~�A�p�A�l�A�bNA�E�A�33A�&�A��A�A��mA���A���Aɰ!Aɥ�Aə�Aɇ+A�v�A�l�A�`BA�K�A�33A�1A��mA���AȲ-AȓuAȇ+A�p�A�Q�A�;dA�bA��HAǺ^AǍPA�~�A�v�A�I�A�&�A�%A��yA���A�AƶFAƣ�Aƕ�AƏ\AƁA�x�A�r�A�XA�=qA�-A�$�A��A� �A�%A��`A��/A��
A���A�ƨAżjAŮAť�A�~�A�?}A�bA��TAĺ^Aģ�Aĕ�A�v�A�C�A��A���AøRA�M�A��A��A���A¥�A\A�XA�1A��#A�A��A���A��\A�v�A�`BA�O�A�C�A�=qA�7LA�+A�{A��A��A�p�A���A��A��;A��^A��!A��!A���A���A���A���A��hA��\A��A�v�A�hsA�ZA�VA�VA�E�A�1'A�-A�"�A��A��A�%A���A��A���A��^A���A�ffA�\)A�S�A�E�A�?}A��A�1A���A���A�(�A��#A���A�ƨA��-A��DA�x�A�l�A�\)A�VA�E�A�"�A��A��
A�ƨA��jA��A���A��DA�|�A�hsA�&�A�oA��A���A�M�A��A��A��9A��PA�M�A���A��A�t�A�dZA�VA�5?A�A��/A��jA���A��DA�p�A�XA�;dA�/A�&�A��A�bA�%A���A��A��;A���A��!A��A�O�A��yA�ƨA��RA��PA�M�A���A�jA�%A��FA���A�XA�bA��/A��+A�  A�ȴA��!A��+A�K�A�1A���A��A��yA��mA��`A��/A���A�A�bNA���A�n�A�
=A�ƨA��uA�|�A�Q�A�bA��
A�A��9A���A���A��A�z�A�r�A�jA�hsA�`BA�I�A�33A�+A�$�A��A�VA���A��A��mA���A��9A���A��PA�p�A�dZA�E�A�-A��A���A�bA���A�^5A�(�A���A��`A��;A���A�ƨA���A�l�A�VA�7LA�ȴA�VA��A��A�XA�A���A�(�A��#A��PA�XA�bA���A���A���A���A��PA�n�A�G�A�&�A�oA��A��^A��A�VA�$�A�A��TA��!A��A�v�A�(�A��+A�A���A���A�hsA�(�A�JA���A��A��;A�ȴA��9A��\A�S�A�A���A���A�~�A�bNA�O�A�E�A�E�A�C�A�;dA�7LA�+A�$�A�+A�bA�VA�
=A���A��TA���A�ƨA�ȴA�ƨA��^A��!A���A���A��uA��hA��PA��A�r�A�VA�5?A�/A�{A�JA��TA�hsA���A�"�A���A��/A���A��A��9A�
=A�|�A�v�A�dZA��A�r�A���A��A�jA�VA�?}A��A�A���A��`A��HA��HA��HA��;A��;A��A�ȴA�ƨA�A��FA��9A��A��A��A���A���A���A���A���A���A��uA��DA��DA��DA��A�t�A�p�A�bNA�VA�I�A�A�A�7LA��A���A���A�n�A��A���A���A��A�G�A��A�ĜA���A�n�A�+A���A�v�A�E�A��A�A���A��A��;A��
A�ĜA��jA��9A���A���A��\A��\A��+A��A�z�A�v�A�r�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�hB��B��B�3B��B��B�3B��B��B��B�nB��B�3B�3B��B��B��B�3B��B��B�hB�aB��B��B�3B�hB��B��B��B��B��B��B�tB�B�tB�tB��B�FB�B�FB�B�zB�B�B�FB��B�B:^BqAB��B��B��B��B�kB��B��B��B�qB�B�B��B�eB�B��B��B��B�+B�:B��B��B��B�iB~]Bm)Bd�B`vB[#BOBI�B=�B5tB1�B-�BqB�B��B�sB��BǮB�<B�tB��B��B��B��By	Be�Ba�B_;B\�BX�BQ�B>�B&�B�BkB:B
�`B
��B
�zB
�-B
�B
}�B
pB
T,B
>B
2�B
OB
uB	�vB	�B	�B	��B	��B	�xB	��B	l�B	cTB	Z�B	TaB	;0B	7B	%B	"hB	�B	4B	
=B	�B��B��B��B�ZB��B�>B�B�BB��B�B��B��B��B��B�UB��B��B��B�B��B�B�B�kB��B��B��B��B�B��B��B�OB��B�	B��B��B��B��B��B�kB�YB�+B��B��B��B�YB�MB�_B��B�.B�JB�B��B��B��B�	B��B�bB�hB��B�SB�uB�=B�kB��B�=B�'B��B�tB�_B�B��B�bB��B�:B��B�[B��B��B�B��B�B�OB��B��B�B�B�DBרB�BƨB˒BуB��B�dB�vB��B	�B	DB	kB	'RB	5�B	:*B	8RB	0�B	2�B	49B	;0B	?}B	FtB	FtB	GEB	K^B	FB	D�B	M6B	OBB	RTB	T�B	TaB	O�B	QB	C�B	=B	9�B	8B	9�B	=B	>�B	?HB	E�B	UgB	bB	hsB	iDB	h�B	iB	kQB	t�B	t�B	wfB	v�B	t�B	s�B	o5B	qvB	n/B	u�B	o�B	o�B	r|B	t�B	u�B	v�B	w�B	w2B	|�B	��B	��B	��B	�B	�@B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�~B	�B	�B	�tB	�0B	�CB	�B	�B	��B	��B	��B	�eB	�9B	��B	�B	�B	�LB	��B	��B	�qB	��B	�wB	��B	�B	�OB	�HB	�B	��B	��B	��B	�mB	�gB	�?B	��B	��B	��B	�6B	�B	�NB	�B	�0B	�<B	�pB	ҽB	��B	��B	ݘB	�;B	��B	��B	��B	ݘB	�B	ޞB	ߤB	�B	��B	�ZB	�B	�B	�B	�fB	�B	�B	�B	�QB	�QB	�"B	�WB	��B	� B	�;B	��B	��B	�TB	�B	��B	�+B	�ZB	�B	�+B	�fB	��B	��B	��B	�>B	��B	�DB	�xB	��B	�>B	��B	��B	��B	��B	��B	��B
 4B
�B
AB
B
�B
{B
MB
B
B
B
B
�B
SB
	7B
�B
�B
�B
	B
1B
�B
_B
�B
�B
JB
~B
�B
PB
"B
"B
VB
"B
�B
�B
�B
�B
hB
:B
�B
oB
�B
:B
�B
�B
B
FB
{B
{B
MB
�B
�B
�B
B
�B
B
�B
�B
�B
�B
B
MB
�B
B
MB
�B
�B
eB
1B
eB
�B
�B
�B
�B
B
qB
�B
kB
�B
�B
xB
�B
B
B
�B
B
�B
�B
 �B
"hB
"�B
 'B
 'B
�B
�B
!B
!B
 'B
 �B
 �B
 �B
!�B
!�B
!�B
#B
%�B
&B
%�B
%zB
&B
&�B
&�B
'B
'RB
'RB
'RB
'�B
'RB
&�B
'B
'�B
(�B
(�B
'�B
(�B
)_B
)�B
+6B
+B
*0B
)�B
)�B
*0B
*0B
+B
,�B
.�B
.�B
1'B
2�B
1�B
1[B
33B
3�B
4B
4nB
49B
3�B
3�B
4B
5tB
5�B
5�B
7B
7B
6�B
7B
7�B
8�B
9XB
9�B
7�B
8�B
7�B
7�B
7B
7LB
8RB
:*B
9�B
:*B
:�B
=B
=qB
=qB
=�B
=qB
=<B
=qB
<�B
<6B
<�B
>BB
>wB
>�B
?B
?�B
@�B
@�B
@�B
@�B
A�B
B'B
B[B
B�B
B�B
CaB
C-B
C�B
C�B
D3B
C�B
C�B
C�B
C�B
D�B
EB
EmB
E�B
F?B
F�B
FtB
F�B
F�B
F�B
GB
GEB
GzB
G�B
HKB
H�B
H�B
IB
H�B
IB
I�B
JXB
K)B
K�B
K�B
K�B
K�B
LdB
LdB
L0B
L0B
K�B
LdB
M6B
NB
M�B
N�B
OB
OBB
OB
OB
OBB
QB
P}B
P�B
P�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
S&B
S&B
S&B
S�B
S�B
S�B
S[B
S�B
T,B
T�B
T�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
S�B
T�B
U�B
VmB
V9B
VB
VB
V9B
U�B
U�B
V9B
VmB
VmB
V�B
V�B
W�B
WsB
WsB
WsB
W�B
W�B
XB
XEB
XEB
XEB
XB
XyB
Z�B
Z�B
[#B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]dB
]dB
^5B
^�B
^�B
^�B
_pB
`B
`B
`B
`BB
`�B
`�B
`�B
aB
`�B
`�B
`�B
aB
aB
aB
a�B
bB
b�B
b�B
b�B
c�B
cTB
c�B
dZB
d�B
dZB
d�B
d�B
d�B
e,B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
g�B
g�B
h
B
h>B
hsB
h�B
iB
i�B
h�B
hsB
iB
iyB
i�B
jB
jKB
jB
jKB
jB
jKB
jKB
jKB
jKB
jKB
jKB
j�B
jB
j�B
jB
kQB
k�B
m�B
ncB
ncB
ncB
n/B
n�B
o5B
oiB
o B
n�B
o B
o B
n�B
oiB
o�B
o�B
o�B
p;B
p�B
poB
p�B
qAB
qB
q�B
q�B
q�B
r|B
rGB
r�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
uZB
u�B
u�B
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
w2B
w2B
wfB
w�B
w�B
w�B
x8B
y>B
y>B
yrB
y�B
y�B
y�B
zDB
yrB
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
z�B
{B
{B
{JB
{B
{�B
|B
|�B
|�B
|�B
|�B
}"B
}"B
}"B
}VB
}VB
}�B
}�B
}�B
~�B
~]B
~�B
~�B
~�B
.B
cB
�B
� B
�4B
�4B
� B
��B
��B
��B
�;B
�oB
��B
��B
��B
��B
��B
�uB
�uB
��B
�B
�GB
�{B
��B
�MB
�MB
��B
��B
��B
��B
�SB
�SB
��B
�%B
�%B
�%B
�%B
��B
��B
��B
��B
��B
�+B
�_B
�_B
��B
��B
��B
��B
��B
�fB
��B
��B
��B
�B
�B
�B
�7B
�B
��B
�rB
�B
�B
�B
�xB
�xB
��B
�JB
��B
�~B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�"B
��B
��B
��B
�(B
��B
��B
�\B
�\B
�.B
�.B
�bB
�bB
�bB
�bB
��B
��B
�4B
�4B
� B
��B
��B
�B
�:B
�:B
�:B
�oB
�oB
�:B��B��B��B��B��B��B�aB�9B��B�9B�hB�hB�B�aB�B�aB�3B�9B�aB�B�9B��B��B��B��B��B�hB��B�3B��B��B��B�nB��B��B��B��B��B�hB�B�B�aB�hB�3B�hB�nB��B�tB��B�hB��B��B��B��B�?B��B��B��B��B��B��B��B�hB��B�B��B�B�-B�nB�'B��B�aB��B��B�aB��B��B�9B��B�hB�'B��B��B��B�[B��B�B��B�-B��B��B��B��B�?B��B��B��B�B��B��B�3B��B��B��B��B�[B��B��B��B�[B��B��B�B�9B��B��B��B��B��B�nB�aB�nB��B��B�3B�3B�B�-B�nB��B�3B�9B��B��B�-B�B��B��B��B�hB�hB�B��B�B�9B��B�tB��B�?B�?B�B�B��B�?B��B��B��B�B�B�tB�hB��B�?B�B��B��B�nB�B�B�B�B��B�tB��B�B��B�?B��B��B�tB�FB�nB�FB�B��B��B��B�FB�B�FB��B�B�zB�B�?B�B�zB�B�B�B��B�B��B�B��B��B�B�?B��B��B�?B�LB��B�LB��B��B�LB�B��B�B��B�FB��B��B��B��B��B��B��B�tB�B�LB�tB��B�B�B�B��B�B��B�B��B�B�B��B�tB�B�B�LB��B�*B��B�BݘB�KB�B�B�BxBqBxB/OB6�BAUBGBX�BQBW�B|Bw2Bq�B�B�_B�%B��B��B�DB�B��B�	B�oB�JB�=B��B�VB�UB�@B�B�IB��B�B��B�hB��B��B��B�VB�xB�VB��B�$B�}B��B��B��B��B�eB�B��B��B�B��B��B��B��B��B��B�_B��B��B�eB�UBбB��B��B��B��B�wB�0B�qB�CB�kB�wB�qB��B�B��B�eB��B�qB�0B�CB��B�kB�qB�*B��B��B�kB�kB�B��B��B�B��B�CB�6B��B�6B��B��B�0B��B�eB�CB�}B�B�B��B�_B��B��B��B��B�6B��B��B��B��B�B��B��B�LB��B�$B��B��B�B��B�*B��B��B��B�B�tB�nB�tB�hB��B��B�zB�B��B��B��B��B�@B�hB�\B��B�B��B�'B�IB�VB�7B�FB�nB��B�B�=B�B��B�	B��B��B��B�B��B�B��B�_B��B��B��B��B�B��B�oB�4B��B�B�4B�hB��B��B�:B�oB��B�hB��B��B��B�bB�"B��B�(B��B��B��B�PB��B��B��B�.B�lB��B�rB�1B�	B�=B�~B��B��B��B�B��B��B�YB�B�oB�AB�B�;B�SB�ABcB�B}�B}�B}"B|PB{JB{�B��B.B�KB�BtTBtBs�BrBo5Br�Bp�BqvBjBf�BffBi�Bm�Bf2BiBiyBcTBf2Bf�BbBc�Bb�BbBa�Ba|B_;BaB_�B_BaHBbNBb�Be�BZ�BYB\�B]dBd�B\�BaHBU2BO�Bc�BQBXyB_pBZ�BK�BMBL0BS�BK�BGEBGEBHBE�BD�BEmBE�BE�BWsBP�BP�BIRBB�B@B>B?HBAUB>�B8�B8B9XB8RB7�B5tB5B6B5B6zB7�B4�B2�B33B3�B2�B4nB2-B0!B2�B.�B0�B/�B.�B,B-B-CB33B2�B6�B1[B 'B%�B �BOB�BBBqB�BuBYB�B�B�B�B�B�B�B�B�B�VB�.B 4B�lB�oB�B�5B�|B��B�B��B�B�cB�vB�B�DB��B�B��B�ZB�5B��B�>B�oB֡B�TB��BӏB��BȀBɆB��B�mBŢB�9B�B̘BȴB�}BB��B�B�B�B��B�tB�B��B��B��B��B�$B�3B��B��B�FB�3B�UB�B�B�OB��B��B�IB��B��B��B�B�kB�}B�eB�FB�LB��B��B��B�wB�@B��B��B� B�MB�B��Bz�Bw�Bv�B��B��Bv�B��Bh�Bf2BkQBl"Bd�Be`Bg�Bd�Bc�Ba|B`�Ba|Ba|B`�Ba�Ba�Bd�B_�B`BB^5B^�B^jB^�B`�B`�B^B\�B^jB]/B[�B[�BZ�B[�B[�B[�BZ�BY�BT�BU�BY�BX�BVmBcTBV�BJ�BI�BM6BM�BJXB@�B<�B=<B@B?B:�B6zB,�B'�B"�B&�B#�B$�B(�B$tB!�B�B�B#�B�BqBIB�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022111604021520221116040215IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022112523314420221125233144QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022112523314420221125233144QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                