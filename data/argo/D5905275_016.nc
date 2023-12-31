CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-06-22T04:45:11Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180622044511  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_016                 7316_008644_016                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�lCsB��@�lCsB��11  @�lC�/�W@�lC�/�W@)z$�LD|@)z$�LD|�d#������d#�����11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�G�@�  @�  @�G�A ��A  A ��A+�A?\)A`��A�Q�A�  A��A�  A���AУ�A��A�  B   B  B�
B�
B�
B((�B0  B8  B@  BG�
BO�
BX  B`  Bg�
Bo�Bx  B�  B�  B�  B�  B�  B�  B�{B�{B�  B�Q�B��B��B��B�{B�{B�  B�  B�  B�  B�  B�{B�  B��
B�  B�  B�  B�  B�  B�  B�  B�{B�  C   C
=C
=C{C  C
  C  C  C��C  C��C��C��C��C  C  C 
=C"  C#��C%��C'��C*
=C,
=C.
=C0  C1�C3��C6  C7��C:  C<  C=��C@  CA��CD  CF
=CH
=CJ
=CL  CM�CO�CQ��CT
=CV{CX  CZ
=C\{C^
=C`
=Ca��Cc�Ce��Cg��Ci��Ck��Cn  Cp
=Cr
=Ct  Cu��Cw��Cy��C|  C~  C�  C���C���C���C���C�  C�C�  C�  C�
=C�
=C�C�  C�  C�  C�  C���C�  C���C���C���C���C���C���C�  C���C���C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�C�
=C�  C���C�  C�C���C�  C���C�  C�
=C�  C���C�C�C�  C���C�  C�C�  C�  C�C�C�C�C�  C�  C�  C�C�
=C�
=C�C�C�C�C�  C���C�  C�  C���C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�C�C�C�  C�  C�  C���C���C�C�C���C���C�  C�C�  C�C�
=C�C�  C�C�
=C�C�  D   D ��D  D� D�qD� D�D��D�D� D  D}qD�qD}qD  D��D�qD��D	D	��D
  D
��DD� D�qD� D�D��D  D}qD�qD� D�D�D  D}qD��D}qD  D� D�D��D�D�DD�D��Dz�D�D��D�D��D  D��DD��D�qD}qD  D��D�qD� D  D��D�qD z�D �qD!}qD"  D"��D"�qD#z�D#�qD$��D%D%� D%�qD&� D'D'� D'�qD(� D)�D)��D)�qD*z�D*�qD+� D,�D,� D,�qD-� D.�D.��D/D/��D/�qD0}qD1  D1� D2  D2� D3�D3� D4  D4� D4�qD5}qD5�qD6}qD6�qD7� D7�qD8xRD9  D9��D:�D:z�D;�D;��D;�qD<z�D<��D=z�D=��D>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDBz�DB�qDCz�DD  DD��DE�DE��DFDF��DGDG�DH�DH��DI  DI��DJ�DJ��DKDK� DK�qDL��DM  DM� DM�qDN}qDN�qDO� DP�DP� DP�qDQ� DR�DR��DS  DS}qDT  DT��DU�DU� DV  DV� DV�qDW}qDX  DX� DY  DY��DZ  DZ}qD[  D[}qD[�qD\}qD]  D]��D^�D^� D^�qD_� D`�D`� Da�Da� Da�qDb}qDc  Dc� DdDd� Dd�qDe� De�qDf� Dg�Dg��Dh�Dh� Dh�qDi}qDi��Dj}qDk  Dk}qDk�qDl� Dm  Dm}qDn  Dn� Dn�qDo� Do�qDp}qDq�Dq��Dq�qDr}qDs  Ds}qDt  Dt��Du�Du�Dv�Dv� Dw�Dw� Dw�qDx}qDx�qDy� DzDz�D{�D{}qD|  D|� D}  D}��D~�D~��D  Dz�D�qD�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�@ D�~�D��qD���D�AHD��HD���D��qD�>�D�� D��HD��D�AHD�� D�� D��D�B�D��HD��HD�HD�@ D�}qD�� D�  D�=qD�� D��HD���D�@ D�~�D�� D�HD�@ D�~�D��qD��qD�=qD�~�D��qD��qD�>�D�� D���D�  D�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D���D�AHD�� D�� D��D�B�D�� D���D���D�@ D�� D��HD�HD�AHD��HD��HD���D�@ D�� D���D���D�@ D�� D��HD�HD�B�D�� D���D���D�@ D�� D��HD���D�=qD�� D�� D���D�>�D�~�D�� D���D�=qD�~�D���D�  D�@ D��HD�D�  D�=qD�� D�� D�HD�AHD�� D�� D�  D�AHD��HD�D��D�@ D�� D�� D��qD�=qD�}qD�� D�HD�AHD�� D�� D���D�>�D��HD���D�HD�@ D�� D��HD�  D�@ D�� D�� D�  D�AHD��HD���D��qD�@ D�� D�� D�  D�@ D�� D���D�HD�AHD��HD�D��D�@ D�}qD�� D�HD�AHD�� D���D�  D�@ D��HD��HD��D�B�D��HD���D���D�>�D�~�D���D���D�>�D�� D��HD���D�>�D�~�D�� D�HD�AHD��HD��HD�  D�@ D�~�D���D���D�AHD�� D��qD�  D�B�D�� D�� D��D�@ D�~�D���D�  D�@ D�� D���D��qD�>�D�� D�� D���D�=qD�~�D�� D�HD�AHD�� D�� D�HD�@ D��HD�� D���D�@ D�~�D��qD��qD�>�D�~�D���D�  D�AHD�� D�� D���D�=qD�~�D���D�  D�AHD�� D��qD�  D�@ D�� D�� D�HD�B�DHD�� D���D�>�D�~�Dþ�D���D�=qD�}qDĽqD���D�>�D�~�D�� D�  D�=qDƀ D��HD�  D�@ D�~�D�� D�HD�AHDȀ DȾ�D�HD�AHDɀ Dɾ�D��qD�>�Dʀ D��HD�HD�AHDˀ D�� D�  D�@ D�~�D̾�D���D�>�D̀ D��HD�  D�@ D�}qD�� D�HD�>�DρHD��HD�HD�@ D�~�D�� D�  D�AHDсHDѾ�D���D�@ DҁHD��HD�  D�B�DӁHD��HD�  D�@ DԀ D�� D�  D�AHDՁHD��HD��D�@ D�~�D�� D�  D�@ D�~�D׾�D�  D�AHD؁HDؾ�D���D�@ DفHD�� D�HD�@ Dڀ D��HD�  D�>�Dۀ D�� D��qD�@ D܁HD�� D�  D�@ D�~�Dݾ�D���D�>�Dހ D��HD�HD�@ D�~�D�� D�  D�=qD�~�D�� D�HD�@ D� D�� D�  D�>�D�~�D�� D��D�AHD� D�qD���D�>�D� D�� D�  D�>�D� D徸D�  D�AHD�HD�D�  D�AHD�HD�� D���D�AHD� D辸D���D�@ D� D��HD�  D�>�D�~�D�� D���D�>�D� D��HD��D�B�D�HD��HD�  D�@ D�~�D�� D�HD�AHD� D�� D���D�>�D�~�D�� D�HD�AHD�� D�� D���D�>�D�~�D�D���D�>�D� D�� D�HD�AHD�D��HD�  D�@ D�D�� D���D�@ D�� D���D��qD�>�D��HD�� D���D�@ D��HD��HD�  D�AHD��HD��HD�  D�>�D��HD�� D���D�AHD�� D�� D��)D�H�?�?L��?�  ?�z�?�33?�
=?�@�@��@&ff@333@G�@W
=@fff@u@��\@���@�33@�(�@�ff@���@�z�@��R@�ff@���@�@�  @�@�\)@�Q�A ��A�
A��Ap�AG�A�A=qA\)A!�A'
=A+�A0  A3�
A8��A=p�AAG�AE�AJ=qAO\)AR�\AW�A\��A`��Adz�Ah��An{Ar�\AvffAz�HA�  A�=qA�(�A�
=A���A��HA�p�A��A���A��
A�{A���A��\A���A�
=A���A��A�A�  A�=qA�z�A�ffA�G�A��A�p�A��A�=qA�z�A��RA�G�AÅA�AǮA�=qA���AθRA���AӅA�A׮A�=qA���A�
=A���A�A�{A�  A�=qA���A�
=A���A�A�{A�  A�=qA�z�A�
=B z�BB�HB  B�BffB�B��B	��B
=BQ�BG�BffB�B��BB
=BQ�B��B�RB�B��B=qB33Bz�B�B
=B   B!G�B"�RB#�B$��B%�B'\)B(z�B)��B*�RB,  B-G�B.=qB/\)B0��B1B2�HB4(�B5p�B6ffB7�B9�B:{B;33B<z�B=B>�HB?�
BAp�BB�\BC�BD��BF{BG\)BHQ�BI��BJ�HBL(�BM�BNffBO�
BP��BQ�BS33BTz�BU��BV�\BW�
BYG�BZffB[\)B\��B]�B_33B`(�Bap�Bb�RBc�Bd��Bf{Bg\)Bhz�Bip�Bj�RBl  Bm�Bn{Bo\)Bp��BqBr�RBt  BuG�Bv=qBw\)Bx��ByBz�RB{�
B}�B~=qB33B�(�B���B�G�B�B�Q�B���B��B�  B�z�B�
=B��B�=qB���B�33B��
B�Q�B���B�G�B��B�z�B���B�p�B�  B���B�33B��B�(�B��RB�33B���B�=qB��HB�\)B��
B�ffB��HB�p�B��B�ffB���B��B��B�ffB��HB��B�  B�ffB��HB�p�B��B�Q�B��HB�p�B��B�Q�B��RB�G�B�B�=qB���B���B�p�B��B�z�B���B�33B��B�{B�z�B���B�33B���B��B�{B�(�B�z�B��\B��\B��\B���B���B��\B�ffB�Q�B�ffB�ffB�=qB�(�B�(�B�(�B�(�B�{B�  B�  B�  B�  B�  B��B��B��B�  B��B��B�  B�{B�{B�  B�  B�(�B�=qB�=qB�=qB�Q�B�z�B���B��RB��RB���B�
=B�
=B��B�G�B�p�B���B��B�B��B�{B�=qB�Q�B�ffB��\B��RB���B��B�G�B�G�B�p�B��B��
B��B�(�B�=qB�ffB���B���B���B��B�G�B��B�B��
B�  B�(�B�Q�B��\B���B���B��B�G�B�p�B��B��B�(�B�Q�B��\B���B��HB��B�p�B���B�B�  B�=qB��\B���B��B�\)B���B��B�{B�ffB��RB��B��B��
B�{B�Q�B���B��B��B��
B�(�B�ffB��RB��B��B��B�Q�B��\B��HB�33B���B�  B�ffB��RB�
=B�G�B���B�  B�Q�B��RB��B��B�B�{B�Q�B���B�
=B�p�B�B�{B�ffB��RB���B�\)B�B�{B�ffB��RB�
=B�G�B���B��B�Q�B£�B���B�G�BÙ�B��B�=qBď\B���B�
=B�\)B�B�(�B�z�B��HB��B�p�B�B�  B�Q�BȸRB��B�p�B��
B�(�B�z�B���B��B�p�BˮB�{B�ffB���B�33B͙�B��B�Q�BθRB��B�\)B�B�{B�z�B��HB�33BхB�  B�ffB���B�33Bә�B�  B�Q�BԸRB���B�\)BծB�(�B֏\B���B�p�B��
B�=qB؏\B���B�G�BٮB�{Bڏ\B���B�\)B��
B�=qBܣ�B�
=B�p�B�B�=qBޏ\B�
=B�\)B�B�(�B��\B��HB�G�BᙚB�{B�ffB��HB�G�B�B�{B�z�B��HB�\)B�B�(�B��B�
=B�p�B��
B�=qB�RB�
=B�B��
B�Q�B�RB�33B�B�{B�z�B���B�p�B��B�ffB��HB�G�B�B�Q�B���B�G�B�B�=qB�RB�33B�B�(�B��RB�33B�B�(�B��RB�33B��B�=qB��RB�G�B��
B�Q�B��HB�\)B��B�ffB�
=B���B�{B���B�33B�C (�C ffC �C  C=qC�C��C{C\)C��C�C=qC�C��C
=CQ�C��C�HC33Cp�CC
=CQ�C��C�HC(�Cz�CC  CG�C�\C�HC	�C	ffC	�C	��C
=qC
�\C
�
C(�Cp�C�RC
=CQ�C��C�HC33C�C��C{C\)C��C�C33C�C��C{CffC�RC  CQ�C��C��CG�C��C�C=qC�\C�C=qC�\C�HC33C�\C�HC(�Cz�C�
C(�C�C�
C=qC�\C�HC33C�\C�HC33C�\C�HC33C�\C�CG�C��C��CG�C��C�C33C�\C�HC=qC�\C�C =qC �\C �HC!33C!z�C!�
C"�C"p�C"��C#�C#p�C#C$�C$p�C$��C%�C%ffC%�RC&
=C&Q�C&�C'
=C'\)C'�C'��C(G�C(�\C(�HC)33C)�C)�HC*33C*�C*�
C+�C+p�C+C,�C,z�C,C-{C-ffC-�C.  C.Q�C.�C/  C/G�C/�\C/�C033C0�C0�HC1=qC1�\C1�HC2(�C2z�C2C3�C3z�C3�
C4(�C4p�C4��C5{C5ffC5C6�C6p�C6C7
=C7\)C7�RC8{C8p�C8�RC9
=C9\)C9�C:  C:Q�C:��C;  C;Q�C;��C;�C<=qC<�C<�
C=33C=�C=�
C>�C>p�C>�RC?
=C?\)C?�C@
=C@Q�C@��C@�HCA33CA�CA�HCB33CB�\CB�
CC(�CCp�CC��CD(�CD�CD�
CE�CEp�CECF�CFz�CF�
CG(�CGp�CGCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ?�  @   @B�\@�G�@�  @�  @�G�A ��A  A ��A+�A?\)A`��A�Q�A�  A��A�  A���AУ�A��A�  B   B  B�
B�
B�
B((�B0  B8  B@  BG�
BO�
BX  B`  Bg�
Bo�Bx  B�  B�  B�  B�  B�  B�  B�{B�{B�  B�Q�B��B��B��B�{B�{B�  B�  B�  B�  B�  B�{B�  B��
B�  B�  B�  B�  B�  B�  B�  B�{B�  C   C
=C
=C{C  C
  C  C  C��C  C��C��C��C��C  C  C 
=C"  C#��C%��C'��C*
=C,
=C.
=C0  C1�C3��C6  C7��C:  C<  C=��C@  CA��CD  CF
=CH
=CJ
=CL  CM�CO�CQ��CT
=CV{CX  CZ
=C\{C^
=C`
=Ca��Cc�Ce��Cg��Ci��Ck��Cn  Cp
=Cr
=Ct  Cu��Cw��Cy��C|  C~  C�  C���C���C���C���C�  C�C�  C�  C�
=C�
=C�C�  C�  C�  C�  C���C�  C���C���C���C���C���C���C�  C���C���C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�C�
=C�  C���C�  C�C���C�  C���C�  C�
=C�  C���C�C�C�  C���C�  C�C�  C�  C�C�C�C�C�  C�  C�  C�C�
=C�
=C�C�C�C�C�  C���C�  C�  C���C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�C�C�C�  C�  C�  C���C���C�C�C���C���C�  C�C�  C�C�
=C�C�  C�C�
=C�C�  D   D ��D  D� D�qD� D�D��D�D� D  D}qD�qD}qD  D��D�qD��D	D	��D
  D
��DD� D�qD� D�D��D  D}qD�qD� D�D�D  D}qD��D}qD  D� D�D��D�D�DD�D��Dz�D�D��D�D��D  D��DD��D�qD}qD  D��D�qD� D  D��D�qD z�D �qD!}qD"  D"��D"�qD#z�D#�qD$��D%D%� D%�qD&� D'D'� D'�qD(� D)�D)��D)�qD*z�D*�qD+� D,�D,� D,�qD-� D.�D.��D/D/��D/�qD0}qD1  D1� D2  D2� D3�D3� D4  D4� D4�qD5}qD5�qD6}qD6�qD7� D7�qD8xRD9  D9��D:�D:z�D;�D;��D;�qD<z�D<��D=z�D=��D>}qD>�qD?}qD?�qD@}qD@�qDA}qDA�qDBz�DB�qDCz�DD  DD��DE�DE��DFDF��DGDG�DH�DH��DI  DI��DJ�DJ��DKDK� DK�qDL��DM  DM� DM�qDN}qDN�qDO� DP�DP� DP�qDQ� DR�DR��DS  DS}qDT  DT��DU�DU� DV  DV� DV�qDW}qDX  DX� DY  DY��DZ  DZ}qD[  D[}qD[�qD\}qD]  D]��D^�D^� D^�qD_� D`�D`� Da�Da� Da�qDb}qDc  Dc� DdDd� Dd�qDe� De�qDf� Dg�Dg��Dh�Dh� Dh�qDi}qDi��Dj}qDk  Dk}qDk�qDl� Dm  Dm}qDn  Dn� Dn�qDo� Do�qDp}qDq�Dq��Dq�qDr}qDs  Ds}qDt  Dt��Du�Du�Dv�Dv� Dw�Dw� Dw�qDx}qDx�qDy� DzDz�D{�D{}qD|  D|� D}  D}��D~�D~��D  Dz�D�qD�@ D��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�@ D�~�D��qD���D�AHD��HD���D��qD�>�D�� D��HD��D�AHD�� D�� D��D�B�D��HD��HD�HD�@ D�}qD�� D�  D�=qD�� D��HD���D�@ D�~�D�� D�HD�@ D�~�D��qD��qD�=qD�~�D��qD��qD�>�D�� D���D�  D�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D���D�AHD�� D�� D��D�B�D�� D���D���D�@ D�� D��HD�HD�AHD��HD��HD���D�@ D�� D���D���D�@ D�� D��HD�HD�B�D�� D���D���D�@ D�� D��HD���D�=qD�� D�� D���D�>�D�~�D�� D���D�=qD�~�D���D�  D�@ D��HD�D�  D�=qD�� D�� D�HD�AHD�� D�� D�  D�AHD��HD�D��D�@ D�� D�� D��qD�=qD�}qD�� D�HD�AHD�� D�� D���D�>�D��HD���D�HD�@ D�� D��HD�  D�@ D�� D�� D�  D�AHD��HD���D��qD�@ D�� D�� D�  D�@ D�� D���D�HD�AHD��HD�D��D�@ D�}qD�� D�HD�AHD�� D���D�  D�@ D��HD��HD��D�B�D��HD���D���D�>�D�~�D���D���D�>�D�� D��HD���D�>�D�~�D�� D�HD�AHD��HD��HD�  D�@ D�~�D���D���D�AHD�� D��qD�  D�B�D�� D�� D��D�@ D�~�D���D�  D�@ D�� D���D��qD�>�D�� D�� D���D�=qD�~�D�� D�HD�AHD�� D�� D�HD�@ D��HD�� D���D�@ D�~�D��qD��qD�>�D�~�D���D�  D�AHD�� D�� D���D�=qD�~�D���D�  D�AHD�� D��qD�  D�@ D�� D�� D�HD�B�DHD�� D���D�>�D�~�Dþ�D���D�=qD�}qDĽqD���D�>�D�~�D�� D�  D�=qDƀ D��HD�  D�@ D�~�D�� D�HD�AHDȀ DȾ�D�HD�AHDɀ Dɾ�D��qD�>�Dʀ D��HD�HD�AHDˀ D�� D�  D�@ D�~�D̾�D���D�>�D̀ D��HD�  D�@ D�}qD�� D�HD�>�DρHD��HD�HD�@ D�~�D�� D�  D�AHDсHDѾ�D���D�@ DҁHD��HD�  D�B�DӁHD��HD�  D�@ DԀ D�� D�  D�AHDՁHD��HD��D�@ D�~�D�� D�  D�@ D�~�D׾�D�  D�AHD؁HDؾ�D���D�@ DفHD�� D�HD�@ Dڀ D��HD�  D�>�Dۀ D�� D��qD�@ D܁HD�� D�  D�@ D�~�Dݾ�D���D�>�Dހ D��HD�HD�@ D�~�D�� D�  D�=qD�~�D�� D�HD�@ D� D�� D�  D�>�D�~�D�� D��D�AHD� D�qD���D�>�D� D�� D�  D�>�D� D徸D�  D�AHD�HD�D�  D�AHD�HD�� D���D�AHD� D辸D���D�@ D� D��HD�  D�>�D�~�D�� D���D�>�D� D��HD��D�B�D�HD��HD�  D�@ D�~�D�� D�HD�AHD� D�� D���D�>�D�~�D�� D�HD�AHD�� D�� D���D�>�D�~�D�D���D�>�D� D�� D�HD�AHD�D��HD�  D�@ D�D�� D���D�@ D�� D���D��qD�>�D��HD�� D���D�@ D��HD��HD�  D�AHD��HD��HD�  D�>�D��HD�� D���D�AHD�� D�� D��)G�O�?�?L��?�  ?�z�?�33?�
=?�@�@��@&ff@333@G�@W
=@fff@u@��\@���@�33@�(�@�ff@���@�z�@��R@�ff@���@�@�  @�@�\)@�Q�A ��A�
A��Ap�AG�A�A=qA\)A!�A'
=A+�A0  A3�
A8��A=p�AAG�AE�AJ=qAO\)AR�\AW�A\��A`��Adz�Ah��An{Ar�\AvffAz�HA�  A�=qA�(�A�
=A���A��HA�p�A��A���A��
A�{A���A��\A���A�
=A���A��A�A�  A�=qA�z�A�ffA�G�A��A�p�A��A�=qA�z�A��RA�G�AÅA�AǮA�=qA���AθRA���AӅA�A׮A�=qA���A�
=A���A�A�{A�  A�=qA���A�
=A���A�A�{A�  A�=qA�z�A�
=B z�BB�HB  B�BffB�B��B	��B
=BQ�BG�BffB�B��BB
=BQ�B��B�RB�B��B=qB33Bz�B�B
=B   B!G�B"�RB#�B$��B%�B'\)B(z�B)��B*�RB,  B-G�B.=qB/\)B0��B1B2�HB4(�B5p�B6ffB7�B9�B:{B;33B<z�B=B>�HB?�
BAp�BB�\BC�BD��BF{BG\)BHQ�BI��BJ�HBL(�BM�BNffBO�
BP��BQ�BS33BTz�BU��BV�\BW�
BYG�BZffB[\)B\��B]�B_33B`(�Bap�Bb�RBc�Bd��Bf{Bg\)Bhz�Bip�Bj�RBl  Bm�Bn{Bo\)Bp��BqBr�RBt  BuG�Bv=qBw\)Bx��ByBz�RB{�
B}�B~=qB33B�(�B���B�G�B�B�Q�B���B��B�  B�z�B�
=B��B�=qB���B�33B��
B�Q�B���B�G�B��B�z�B���B�p�B�  B���B�33B��B�(�B��RB�33B���B�=qB��HB�\)B��
B�ffB��HB�p�B��B�ffB���B��B��B�ffB��HB��B�  B�ffB��HB�p�B��B�Q�B��HB�p�B��B�Q�B��RB�G�B�B�=qB���B���B�p�B��B�z�B���B�33B��B�{B�z�B���B�33B���B��B�{B�(�B�z�B��\B��\B��\B���B���B��\B�ffB�Q�B�ffB�ffB�=qB�(�B�(�B�(�B�(�B�{B�  B�  B�  B�  B�  B��B��B��B�  B��B��B�  B�{B�{B�  B�  B�(�B�=qB�=qB�=qB�Q�B�z�B���B��RB��RB���B�
=B�
=B��B�G�B�p�B���B��B�B��B�{B�=qB�Q�B�ffB��\B��RB���B��B�G�B�G�B�p�B��B��
B��B�(�B�=qB�ffB���B���B���B��B�G�B��B�B��
B�  B�(�B�Q�B��\B���B���B��B�G�B�p�B��B��B�(�B�Q�B��\B���B��HB��B�p�B���B�B�  B�=qB��\B���B��B�\)B���B��B�{B�ffB��RB��B��B��
B�{B�Q�B���B��B��B��
B�(�B�ffB��RB��B��B��B�Q�B��\B��HB�33B���B�  B�ffB��RB�
=B�G�B���B�  B�Q�B��RB��B��B�B�{B�Q�B���B�
=B�p�B�B�{B�ffB��RB���B�\)B�B�{B�ffB��RB�
=B�G�B���B��B�Q�B£�B���B�G�BÙ�B��B�=qBď\B���B�
=B�\)B�B�(�B�z�B��HB��B�p�B�B�  B�Q�BȸRB��B�p�B��
B�(�B�z�B���B��B�p�BˮB�{B�ffB���B�33B͙�B��B�Q�BθRB��B�\)B�B�{B�z�B��HB�33BхB�  B�ffB���B�33Bә�B�  B�Q�BԸRB���B�\)BծB�(�B֏\B���B�p�B��
B�=qB؏\B���B�G�BٮB�{Bڏ\B���B�\)B��
B�=qBܣ�B�
=B�p�B�B�=qBޏ\B�
=B�\)B�B�(�B��\B��HB�G�BᙚB�{B�ffB��HB�G�B�B�{B�z�B��HB�\)B�B�(�B��B�
=B�p�B��
B�=qB�RB�
=B�B��
B�Q�B�RB�33B�B�{B�z�B���B�p�B��B�ffB��HB�G�B�B�Q�B���B�G�B�B�=qB�RB�33B�B�(�B��RB�33B�B�(�B��RB�33B��B�=qB��RB�G�B��
B�Q�B��HB�\)B��B�ffB�
=B���B�{B���B�33B�C (�C ffC �C  C=qC�C��C{C\)C��C�C=qC�C��C
=CQ�C��C�HC33Cp�CC
=CQ�C��C�HC(�Cz�CC  CG�C�\C�HC	�C	ffC	�C	��C
=qC
�\C
�
C(�Cp�C�RC
=CQ�C��C�HC33C�C��C{C\)C��C�C33C�C��C{CffC�RC  CQ�C��C��CG�C��C�C=qC�\C�C=qC�\C�HC33C�\C�HC(�Cz�C�
C(�C�C�
C=qC�\C�HC33C�\C�HC33C�\C�HC33C�\C�CG�C��C��CG�C��C�C33C�\C�HC=qC�\C�C =qC �\C �HC!33C!z�C!�
C"�C"p�C"��C#�C#p�C#C$�C$p�C$��C%�C%ffC%�RC&
=C&Q�C&�C'
=C'\)C'�C'��C(G�C(�\C(�HC)33C)�C)�HC*33C*�C*�
C+�C+p�C+C,�C,z�C,C-{C-ffC-�C.  C.Q�C.�C/  C/G�C/�\C/�C033C0�C0�HC1=qC1�\C1�HC2(�C2z�C2C3�C3z�C3�
C4(�C4p�C4��C5{C5ffC5C6�C6p�C6C7
=C7\)C7�RC8{C8p�C8�RC9
=C9\)C9�C:  C:Q�C:��C;  C;Q�C;��C;�C<=qC<�C<�
C=33C=�C=�
C>�C>p�C>�RC?
=C?\)C?�C@
=C@Q�C@��C@�HCA33CA�CA�HCB33CB�\CB�
CC(�CCp�CC��CD(�CD�CD�
CE�CEp�CECF�CFz�CF�
CG(�CGp�CGCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�_@�'G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�PA�t�AރAݟ�A�+A���A���AܼjAܴ9Aܥ�Aܟ�A܏\A܉7A�|�A�dZA�VA�S�A�O�A�Q�A�VA�^5A�XA�VA�Q�A�M�A�I�A�I�A�I�A�Q�A�VA�M�A�C�A�A�A�A�A�A�A�K�A�ZA�ZA�ZA�XA�S�A�Q�A�O�A�C�A�-A��A��yA���A���Aˉ7A�O�A���A�-A�hsA���A��A��
A���A���A�v�A�t�A�+A�{A�ƨA��TA��A��/A�hsA�ffA���A�7LA���A���A�A��+A��A���A�x�A�p�A�O�A��A��PA��A~��A}x�A{hsAuC�ArI�ApffAn��Ai��Ae�Aa��A[�AV��AUl�AR��AN�HAI��AF�/ACƨAAl�A>��A;`BA:r�A:  A9&�A7S�A6��A5�A3VA0�DA/�A0�A1�A2{A1K�A0A�A.�!A,�A,A+/A*�A*v�A)33A(1'A'�^A'dZA&��A&n�A&=qA& �A&�A&  A$��A#�A#l�A#�A"�A"ĜA"��A"-A!XA �A ��A �A ��A ^5AoA�^A\)A�yAz�AbAdZA�`A�+A1'A�
A��Ar�AVAx�A&�Ar�A=qA�A�hAx�A?}AC�AVA�/A�9A�+Av�A=qA-A �A��A�PA�A��A��A��AK�A�/AffA1'A�mAx�AoA��A�uA-AƨAdZA�AVA�`Av�A �A�^A"�A��Ar�A�mAC�A
��A
�RA
�uA
v�A
VA
A	�#A	��A	A	�A	dZA	/A��AQ�A{A  A�#A�hAS�A33AA��A�uAbNAE�A{AXA��A��A~�A9XA��Al�AA�/AĜA��A�A�mA�7A �A �RA bNA �@��@��!@�G�@�1@��H@�V@�E�@���@�hs@�j@��
@�+@���@�&�@�9X@�|�@�v�@��@�O�@��`@�j@�z�@��m@�S�@@�7@��/@�@�1'@띲@�E�@�O�@��@�%@��@�F@��@�ȴ@�ff@�@�9X@�F@��H@��@�G�@�j@���@��@��@�`B@��@ܼj@ܛ�@�z�@�I�@��m@���@�~�@�ff@�E�@�-@��@أ�@ץ�@ם�@�\)@�@֧�@�G�@��/@ԃ@�  @ӍP@�+@��@�V@��@�O�@Ѓ@��m@ϥ�@�t�@�K�@�
=@��@���@�ȴ@θR@Η�@�V@�@�V@̓u@�1'@��@�C�@�v�@�p�@��@��@ȴ9@�  @�dZ@Ɨ�@�{@���@�hs@��@���@���@�I�@�M�@�=q@�-@��-@�`B@��@��/@��D@�I�@��
@�|�@�S�@�t�@��P@���@��P@��@�n�@�-@�$�@�{@���@�`B@��/@�j@�9X@���@�C�@���@�v�@�$�@���@�/@�r�@��@���@�t�@�S�@��R@��-@�7L@���@�r�@�I�@�1@��w@�dZ@��!@�M�@�{@��h@�?}@��9@�j@�b@��;@��F@�\)@�
=@���@�v�@��@���@��@�O�@��@��9@�  @�ƨ@�"�@��H@���@�ff@�V@�{@���@�O�@�X@�7L@��j@��m@��@���@��@���@��+@�ff@�$�@��#@��-@�G�@��@��u@��@�bN@�1'@�1@��w@��@�l�@�ȴ@�-@�x�@�7L@�%@��`@�bN@�"�@�ȴ@��+@�^5@���@�V@��/@�Ĝ@��9@�r�@�1'@��@�t�@�S�@�"�@��@���@���@�^5@�@���@��@�j@���@�ƨ@��P@�K�@�"�@��@��H@���@�ff@�=q@�$�@�@���@��h@�`B@���@���@��D@��@�1@��m@���@�C�@�"�@��@�
=@���@�V@�@���@��7@��@��@�z�@�A�@�b@�|�@�33@�
=@��!@�5?@��@���@��`@���@�z�@�Z@�b@���@��w@��w@��F@�o@�E�@�5?@�{@�J@�{@��@��@�X@��@��`@��@��@�j@�bN@�b@�;d@��H@���@�M�@���@�@�p�@�`B@�`B@�G�@���@���@�r�@�I�@��@���@��;@��F@�\)@�"�@���@��y@��@���@��+@��@��@���@�`B@�?}@���@��@���@���@��D@�z�@�bN@� �@�b@��@��@�(�@�b@
=@~v�@}��@}�@|z�@{�
@{"�@z�H@z��@z-@yX@yx�@y��@yG�@xĜ@x�@xA�@xA�@w�@w
=@wl�@w�P@w��@w�P@wl�@w;d@v�R@vv�@v$�@u�T@u`B@uV@t�@tz�@t�@s��@r�\@rJ@q�^@q��@qx�@q&�@p��@p1'@o�@n5?@m�@mO�@l�/@lz�@k�m@k�@kS�@jn�@i�#@i�7@iG�@i�@iG�@iG�@h��@hr�@g�;@g\)@fv�@f$�@e�-@eO�@d��@dj@d�@c�
@c��@cdZ@c"�@b~�@b�@a�^@aX@`Ĝ@_�@_�w@_�P@_
=@^�@^V@^@]��@]`B@\��@\z�@\9X@[�F@[�@[o@Z~�@Y�@Y&�@X�9@XQ�@W��@W;d@W�@Vv�@U�T@U�h@UO�@T��@T�D@S��@SdZ@R�H@R�\@R~�@Q�#@Q&�@P��@Pb@O�;@O�@O��@O��@Ol�@O;d@O+@M�@M?}@L�@LZ@K�
@K��@K��@KS�@J��@J��@J-@I��@IX@H�`@Hr�@H�9@Hr�@H1'@G�@F�R@F@E�h@E?}@E�@D�/@D��@Dj@C��@C��@CC�@B�@B�!@B��@B~�@B-@A�#@Ax�@A&�@@��@@b@?��@?l�@?;d@>�@>��@>V@=@=`B@=?}@=�@=V@=V@<��@<�@<��@<�@<j@;ƨ@;S�@;@:��@:n�@:-@9�7@8��@8�u@8bN@7�@7K�@6�R@6��@6v�@6@5�@5p�@5/@4�j@49X@41@3�
@3dZ@2�H@2��@2~�@1��@1�^@1&�@0r�@0b@0  @/�;@/l�@.ȴ@.�+@.V@.E�@.5?@.5?@-�h@,��@,Z@+��@+�m@+�m@+�
@+��@+�@+dZ@+33@+o@*~�@*-@)��@)hs@(�@(Q�@(1'@( �@(1'@(bN@(1'@'�;@'�;@'��@'l�@'
=@&��@&��@&v�@&ff@&V@&V@&E�@&$�@&@%�@%�-@%�h@%��@%�h@%/@%�@%�@%�@%�@$��@$�j@$��@$z�@$I�@$�@#�F@#�@#dZ@#C�@"�@"��@"�!@"��@"n�@"=q@!�@!��@!x�@!%@ r�@ 1'@   @�;@�@�@E�@@V@��@j@Z@(�@�m@ƨ@��@�@��@�#@x�@G�@7L@%@�`@�9@r�@b@
=@��@E�@$�@�T@��@�-@�@O�@�@��@�@�@�@��@�@��@��@(�@ƨ@t�@C�@"�@�@��@�\@n�@J@�#@�#@��@X@�@�9@��@r�@ �@�@�;@�P@��@�R@$�@{@v�@V@�T@��@`B@/@�@��@j@j@j@j@I�@(�@�
@�F@o@
��@
��@
n�@
M�@
-@
J@	��@	��@	��@	�7@	x�@	X@	&�@	�@	�@�u@bN@Q�@A�@ �@  @��@�@��@��@l�@;d@
=@��A��yA��TA��A�n�A�ZA�7LA�VA�=qA�x�A���Aޛ�A�jA�C�A�oAݺ^A�v�A�hsA�Q�A�&�A��A��A�{A��A��mA��/A��A���A�ƨA�ĜA�ĜAܸRAܸRAܼjAܸRAܲ-AܮAܩ�Aܥ�Aܟ�Aܡ�Aܟ�Aܛ�Aܗ�AܓuA܋DA܅A܉7AܓuA܍PA܏\AܓuA܏\A܅A�~�A܃A܃A�|�A�x�A�|�A܅A�~�A�|�A�z�A�hsA�hsA�hsA�`BA�`BA�bNA�`BA�ZA�VA�XA�ZA�XA�Q�A�S�A�XA�S�A�O�A�Q�A�VA�Q�A�O�A�Q�A�Q�A�M�A�K�A�Q�A�O�A�M�A�O�A�S�A�S�A�O�A�S�A�VA�S�A�O�A�S�A�S�A�Q�A�S�A�\)A�\)A�VA�ZA�bNA�`BA�^5A�^5A�ZA�Q�A�S�A�XA�XA�XA�\)A�\)A�S�A�VA�ZA�XA�VA�VA�XA�Q�A�O�A�S�A�VA�Q�A�O�A�Q�A�S�A�Q�A�K�A�K�A�O�A�K�A�G�A�K�A�K�A�G�A�G�A�K�A�K�A�G�A�I�A�K�A�I�A�G�A�E�A�G�A�I�A�E�A�E�A�I�A�K�A�M�A�Q�A�Q�A�K�A�M�A�O�A�S�A�S�A�VA�XA�XA�S�A�XA�ZA�S�A�O�A�Q�A�Q�A�Q�A�I�A�K�A�K�A�C�A�A�A�E�A�E�A�A�A�A�A�G�A�G�A�C�A�A�A�A�A�A�A�=qA�=qA�A�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�A�A�=qA�A�A�E�A�G�A�C�A�G�A�K�A�O�A�I�A�Q�A�\)A�ZA�XA�ZA�^5A�ZA�XA�\)A�\)A�XA�XA�\)A�\)A�VA�VA�ZA�\)A�ZA�VA�XA�ZA�\)A�VA�VA�ZA�ZA�XA�VA�ZA�ZA�VA�S�A�S�A�VA�VA�Q�A�Q�A�VA�S�A�Q�A�Q�A�VA�S�A�O�A�O�A�S�A�VA�O�A�K�A�M�A�O�A�M�A�G�A�I�A�M�A�M�A�E�A�C�A�9XA�9XA�5?A�5?A�1'A�1'A�/A�&�A�(�A�+A�-A�(�A�&�A�$�A�$�A�"�A��A�oA�A��A��;A۶FAۍPA�&�AځAى7A���A��A׾wA�jA�C�A�A�A�E�A�I�A�S�A�bNA�t�AׅA׋DAדuAן�Aװ!Aײ-A׶FA׼jA׾wA�ƨA���A׸RA״9A׬Aס�A׉7A�ffA�S�A�E�A�E�A�7LA�/A��A��mA֛�A�x�A�XA�(�A�bA��yA���Aհ!A�v�A��TA�VA��`A�v�A�%A��AҰ!Aҝ�A�|�A�l�A�9XAѴ9A��AмjA�t�A�33A�VA���A��A�ƨAϾwA϶FAϩ�A�x�AϓuAσA�t�A�l�A�bNA�S�A�?}A� �A�1A��A���AΧ�A΅A�p�A�`BA�G�A�-A�bA��yA���Aʹ9Aͩ�A͝�A͉7A�x�A�r�A�dZA�Q�A�?}A�(�A��A�JA��/A̾wA̧�A̓uA�|�A�9XA�VAˏ\A��A�S�A��AɑhA�t�A�-A���A�I�A��#AǋDA�Q�A��AƸRAƴ9A�p�A�I�A�A�A�;dA�/A��A��mA�A���Aź^AŴ9Aţ�AŇ+AŁAŇ+AŋDAŅA�z�A�dZA�M�A�33A��A���A��#A�ƨAĩ�AēuA�z�A�l�A�`BA�?}A���A�S�A��A²-A\A�l�A�C�A�-A�"�A�oA�A��yA��-A��+A�x�A�{A�1A��A�ZA�I�A�=qA�$�A��HA�-A��A��A�VA�/A��A���A���A��RA��!A��A���A���A�|�A�XA��#A��A��A��mA��FA�=qA���A�l�A�Q�A�C�A�?}A�&�A�{A��
A�ffA���A��-A�jA��A��A��HA���A��^A���A��DA�l�A�Q�A�-A�&�A���A��HA���A���A���A�ƨA��PA�M�A�9XA�$�A�oA���A�ƨA�O�A���A���A��RA���A�~�A�l�A�^5A�Q�A�E�A�5?A��A�%A��A���A��PA�E�A�
=A��A�K�A���A�Q�A� �A��A�/A�E�A�C�A�E�A��7A��FA���A��yA�G�A�n�A�E�A�33A�/A��A���A�A�A�r�A�A��A�7LA��9A�O�A���A���A�`BA�1A�K�A�n�A�A���A�G�A�JA��FA�|�A�A�A��A��A�~�A��A��-A�K�A��TA�\)A�=qA��A��yA���A�E�A�$�A�1A���A���A�ĜA��-A���A�`BA�/A��A�bNA�I�A�E�A�=qA�5?A�-A�"�A�JA���A��#A��
A���A���A��wA���A�ffA�&�A���A�ƨA��DA�A�A��;A���A�`BA�5?A���A�ĜA��A��A��A�|�A�{A���A��A�bA���A�O�A�G�A�ȴA�A�A��yA��A��#A���A� �A��wA��FA���A�=qA��\A�VA�ĜA�dZA�Q�A�-A�S�A���A���A��7A�Q�A��A�A��A�ĜA��A�l�A�7LA��A��A�33A���A�S�A���A�JA���A��TA���A���A��A�M�A��yA�1'A�x�A��A�ƨA�`BA�;A|�A�A~��A~��A~�RA~�A~��A~r�A}t�A|�A|��A|�DA|ZA|M�A|{A{�wAz�Ay�Aw�TAv�Au|�At-As��As��As/Ar��Arz�Aq�mAq�FAq�hAq;dAp��Ap�uAp5?Ap  Ao��Ao|�An��An�`An��An�AnM�Am�Am
=AlbAk"�Ah�!Ag��Ag�Af��Afr�Ae�mAeAe�7Aet�AdĜAc\)Ab=qAa`BAa+A_�;A^{A\�A\A�A[��AZ��AYG�AX1'AW�AW�AV��AV�RAV5?AU��AU��AU�PAUt�AUdZAU;dAU33AU
=AT�!AS�-ARZAQ�
AQ`BAP��APJAOl�AN��ANE�AM`BAK�PAK"�AJjAIO�AI�AH��AH�\AHE�AG��AG+AF^5AEp�AEdZAEl�AEK�ADVACVABv�AB5?AA��AA�FAA��AA|�AAC�AA�A@�HA@n�A?�^A>�`A=�A=33A<r�A<�A;�wA;�hA;x�A;�A:^5A9��A:bNA:��A:��A:�+A:ffA:Q�A:5?A:JA9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             A�PA�t�AރAݟ�A�+A���A���AܼjAܴ9Aܥ�Aܟ�A܏\A܉7A�|�A�dZA�VA�S�A�O�A�Q�A�VA�^5A�XA�VA�Q�A�M�A�I�A�I�A�I�A�Q�A�VA�M�A�C�A�A�A�A�A�A�A�K�A�ZA�ZA�ZA�XA�S�A�Q�A�O�A�C�A�-A��A��yA���A���Aˉ7A�O�A���A�-A�hsA���A��A��
A���A���A�v�A�t�A�+A�{A�ƨA��TA��A��/A�hsA�ffA���A�7LA���A���A�A��+A��A���A�x�A�p�A�O�A��A��PA��A~��A}x�A{hsAuC�ArI�ApffAn��Ai��Ae�Aa��A[�AV��AUl�AR��AN�HAI��AF�/ACƨAAl�A>��A;`BA:r�A:  A9&�A7S�A6��A5�A3VA0�DA/�A0�A1�A2{A1K�A0A�A.�!A,�A,A+/A*�A*v�A)33A(1'A'�^A'dZA&��A&n�A&=qA& �A&�A&  A$��A#�A#l�A#�A"�A"ĜA"��A"-A!XA �A ��A �A ��A ^5AoA�^A\)A�yAz�AbAdZA�`A�+A1'A�
A��Ar�AVAx�A&�Ar�A=qA�A�hAx�A?}AC�AVA�/A�9A�+Av�A=qA-A �A��A�PA�A��A��A��AK�A�/AffA1'A�mAx�AoA��A�uA-AƨAdZA�AVA�`Av�A �A�^A"�A��Ar�A�mAC�A
��A
�RA
�uA
v�A
VA
A	�#A	��A	A	�A	dZA	/A��AQ�A{A  A�#A�hAS�A33AA��A�uAbNAE�A{AXA��A��A~�A9XA��Al�AA�/AĜA��A�A�mA�7A �A �RA bNA �@��@��!@�G�@�1@��H@�V@�E�@���@�hs@�j@��
@�+@���@�&�@�9X@�|�@�v�@��@�O�@��`@�j@�z�@��m@�S�@@�7@��/@�@�1'@띲@�E�@�O�@��@�%@��@�F@��@�ȴ@�ff@�@�9X@�F@��H@��@�G�@�j@���@��@��@�`B@��@ܼj@ܛ�@�z�@�I�@��m@���@�~�@�ff@�E�@�-@��@أ�@ץ�@ם�@�\)@�@֧�@�G�@��/@ԃ@�  @ӍP@�+@��@�V@��@�O�@Ѓ@��m@ϥ�@�t�@�K�@�
=@��@���@�ȴ@θR@Η�@�V@�@�V@̓u@�1'@��@�C�@�v�@�p�@��@��@ȴ9@�  @�dZ@Ɨ�@�{@���@�hs@��@���@���@�I�@�M�@�=q@�-@��-@�`B@��@��/@��D@�I�@��
@�|�@�S�@�t�@��P@���@��P@��@�n�@�-@�$�@�{@���@�`B@��/@�j@�9X@���@�C�@���@�v�@�$�@���@�/@�r�@��@���@�t�@�S�@��R@��-@�7L@���@�r�@�I�@�1@��w@�dZ@��!@�M�@�{@��h@�?}@��9@�j@�b@��;@��F@�\)@�
=@���@�v�@��@���@��@�O�@��@��9@�  @�ƨ@�"�@��H@���@�ff@�V@�{@���@�O�@�X@�7L@��j@��m@��@���@��@���@��+@�ff@�$�@��#@��-@�G�@��@��u@��@�bN@�1'@�1@��w@��@�l�@�ȴ@�-@�x�@�7L@�%@��`@�bN@�"�@�ȴ@��+@�^5@���@�V@��/@�Ĝ@��9@�r�@�1'@��@�t�@�S�@�"�@��@���@���@�^5@�@���@��@�j@���@�ƨ@��P@�K�@�"�@��@��H@���@�ff@�=q@�$�@�@���@��h@�`B@���@���@��D@��@�1@��m@���@�C�@�"�@��@�
=@���@�V@�@���@��7@��@��@�z�@�A�@�b@�|�@�33@�
=@��!@�5?@��@���@��`@���@�z�@�Z@�b@���@��w@��w@��F@�o@�E�@�5?@�{@�J@�{@��@��@�X@��@��`@��@��@�j@�bN@�b@�;d@��H@���@�M�@���@�@�p�@�`B@�`B@�G�@���@���@�r�@�I�@��@���@��;@��F@�\)@�"�@���@��y@��@���@��+@��@��@���@�`B@�?}@���@��@���@���@��D@�z�@�bN@� �@�b@��@��@�(�@�b@
=@~v�@}��@}�@|z�@{�
@{"�@z�H@z��@z-@yX@yx�@y��@yG�@xĜ@x�@xA�@xA�@w�@w
=@wl�@w�P@w��@w�P@wl�@w;d@v�R@vv�@v$�@u�T@u`B@uV@t�@tz�@t�@s��@r�\@rJ@q�^@q��@qx�@q&�@p��@p1'@o�@n5?@m�@mO�@l�/@lz�@k�m@k�@kS�@jn�@i�#@i�7@iG�@i�@iG�@iG�@h��@hr�@g�;@g\)@fv�@f$�@e�-@eO�@d��@dj@d�@c�
@c��@cdZ@c"�@b~�@b�@a�^@aX@`Ĝ@_�@_�w@_�P@_
=@^�@^V@^@]��@]`B@\��@\z�@\9X@[�F@[�@[o@Z~�@Y�@Y&�@X�9@XQ�@W��@W;d@W�@Vv�@U�T@U�h@UO�@T��@T�D@S��@SdZ@R�H@R�\@R~�@Q�#@Q&�@P��@Pb@O�;@O�@O��@O��@Ol�@O;d@O+@M�@M?}@L�@LZ@K�
@K��@K��@KS�@J��@J��@J-@I��@IX@H�`@Hr�@H�9@Hr�@H1'@G�@F�R@F@E�h@E?}@E�@D�/@D��@Dj@C��@C��@CC�@B�@B�!@B��@B~�@B-@A�#@Ax�@A&�@@��@@b@?��@?l�@?;d@>�@>��@>V@=@=`B@=?}@=�@=V@=V@<��@<�@<��@<�@<j@;ƨ@;S�@;@:��@:n�@:-@9�7@8��@8�u@8bN@7�@7K�@6�R@6��@6v�@6@5�@5p�@5/@4�j@49X@41@3�
@3dZ@2�H@2��@2~�@1��@1�^@1&�@0r�@0b@0  @/�;@/l�@.ȴ@.�+@.V@.E�@.5?@.5?@-�h@,��@,Z@+��@+�m@+�m@+�
@+��@+�@+dZ@+33@+o@*~�@*-@)��@)hs@(�@(Q�@(1'@( �@(1'@(bN@(1'@'�;@'�;@'��@'l�@'
=@&��@&��@&v�@&ff@&V@&V@&E�@&$�@&@%�@%�-@%�h@%��@%�h@%/@%�@%�@%�@%�@$��@$�j@$��@$z�@$I�@$�@#�F@#�@#dZ@#C�@"�@"��@"�!@"��@"n�@"=q@!�@!��@!x�@!%@ r�@ 1'@   @�;@�@�@E�@@V@��@j@Z@(�@�m@ƨ@��@�@��@�#@x�@G�@7L@%@�`@�9@r�@b@
=@��@E�@$�@�T@��@�-@�@O�@�@��@�@�@�@��@�@��@��@(�@ƨ@t�@C�@"�@�@��@�\@n�@J@�#@�#@��@X@�@�9@��@r�@ �@�@�;@�P@��@�R@$�@{@v�@V@�T@��@`B@/@�@��@j@j@j@j@I�@(�@�
@�F@o@
��@
��@
n�@
M�@
-@
J@	��@	��@	��@	�7@	x�@	X@	&�@	�@	�@�u@bN@Q�@A�@ �@  @��@�@��@��@l�@;d@
=G�O�A��yA��TA��A�n�A�ZA�7LA�VA�=qA�x�A���Aޛ�A�jA�C�A�oAݺ^A�v�A�hsA�Q�A�&�A��A��A�{A��A��mA��/A��A���A�ƨA�ĜA�ĜAܸRAܸRAܼjAܸRAܲ-AܮAܩ�Aܥ�Aܟ�Aܡ�Aܟ�Aܛ�Aܗ�AܓuA܋DA܅A܉7AܓuA܍PA܏\AܓuA܏\A܅A�~�A܃A܃A�|�A�x�A�|�A܅A�~�A�|�A�z�A�hsA�hsA�hsA�`BA�`BA�bNA�`BA�ZA�VA�XA�ZA�XA�Q�A�S�A�XA�S�A�O�A�Q�A�VA�Q�A�O�A�Q�A�Q�A�M�A�K�A�Q�A�O�A�M�A�O�A�S�A�S�A�O�A�S�A�VA�S�A�O�A�S�A�S�A�Q�A�S�A�\)A�\)A�VA�ZA�bNA�`BA�^5A�^5A�ZA�Q�A�S�A�XA�XA�XA�\)A�\)A�S�A�VA�ZA�XA�VA�VA�XA�Q�A�O�A�S�A�VA�Q�A�O�A�Q�A�S�A�Q�A�K�A�K�A�O�A�K�A�G�A�K�A�K�A�G�A�G�A�K�A�K�A�G�A�I�A�K�A�I�A�G�A�E�A�G�A�I�A�E�A�E�A�I�A�K�A�M�A�Q�A�Q�A�K�A�M�A�O�A�S�A�S�A�VA�XA�XA�S�A�XA�ZA�S�A�O�A�Q�A�Q�A�Q�A�I�A�K�A�K�A�C�A�A�A�E�A�E�A�A�A�A�A�G�A�G�A�C�A�A�A�A�A�A�A�=qA�=qA�A�A�C�A�?}A�?}A�C�A�C�A�?}A�?}A�C�A�A�A�=qA�A�A�E�A�G�A�C�A�G�A�K�A�O�A�I�A�Q�A�\)A�ZA�XA�ZA�^5A�ZA�XA�\)A�\)A�XA�XA�\)A�\)A�VA�VA�ZA�\)A�ZA�VA�XA�ZA�\)A�VA�VA�ZA�ZA�XA�VA�ZA�ZA�VA�S�A�S�A�VA�VA�Q�A�Q�A�VA�S�A�Q�A�Q�A�VA�S�A�O�A�O�A�S�A�VA�O�A�K�A�M�A�O�A�M�A�G�A�I�A�M�A�M�A�E�A�C�A�9XA�9XA�5?A�5?A�1'A�1'A�/A�&�A�(�A�+A�-A�(�A�&�A�$�A�$�A�"�A��A�oA�A��A��;A۶FAۍPA�&�AځAى7A���A��A׾wA�jA�C�A�A�A�E�A�I�A�S�A�bNA�t�AׅA׋DAדuAן�Aװ!Aײ-A׶FA׼jA׾wA�ƨA���A׸RA״9A׬Aס�A׉7A�ffA�S�A�E�A�E�A�7LA�/A��A��mA֛�A�x�A�XA�(�A�bA��yA���Aհ!A�v�A��TA�VA��`A�v�A�%A��AҰ!Aҝ�A�|�A�l�A�9XAѴ9A��AмjA�t�A�33A�VA���A��A�ƨAϾwA϶FAϩ�A�x�AϓuAσA�t�A�l�A�bNA�S�A�?}A� �A�1A��A���AΧ�A΅A�p�A�`BA�G�A�-A�bA��yA���Aʹ9Aͩ�A͝�A͉7A�x�A�r�A�dZA�Q�A�?}A�(�A��A�JA��/A̾wA̧�A̓uA�|�A�9XA�VAˏ\A��A�S�A��AɑhA�t�A�-A���A�I�A��#AǋDA�Q�A��AƸRAƴ9A�p�A�I�A�A�A�;dA�/A��A��mA�A���Aź^AŴ9Aţ�AŇ+AŁAŇ+AŋDAŅA�z�A�dZA�M�A�33A��A���A��#A�ƨAĩ�AēuA�z�A�l�A�`BA�?}A���A�S�A��A²-A\A�l�A�C�A�-A�"�A�oA�A��yA��-A��+A�x�A�{A�1A��A�ZA�I�A�=qA�$�A��HA�-A��A��A�VA�/A��A���A���A��RA��!A��A���A���A�|�A�XA��#A��A��A��mA��FA�=qA���A�l�A�Q�A�C�A�?}A�&�A�{A��
A�ffA���A��-A�jA��A��A��HA���A��^A���A��DA�l�A�Q�A�-A�&�A���A��HA���A���A���A�ƨA��PA�M�A�9XA�$�A�oA���A�ƨA�O�A���A���A��RA���A�~�A�l�A�^5A�Q�A�E�A�5?A��A�%A��A���A��PA�E�A�
=A��A�K�A���A�Q�A� �A��A�/A�E�A�C�A�E�A��7A��FA���A��yA�G�A�n�A�E�A�33A�/A��A���A�A�A�r�A�A��A�7LA��9A�O�A���A���A�`BA�1A�K�A�n�A�A���A�G�A�JA��FA�|�A�A�A��A��A�~�A��A��-A�K�A��TA�\)A�=qA��A��yA���A�E�A�$�A�1A���A���A�ĜA��-A���A�`BA�/A��A�bNA�I�A�E�A�=qA�5?A�-A�"�A�JA���A��#A��
A���A���A��wA���A�ffA�&�A���A�ƨA��DA�A�A��;A���A�`BA�5?A���A�ĜA��A��A��A�|�A�{A���A��A�bA���A�O�A�G�A�ȴA�A�A��yA��A��#A���A� �A��wA��FA���A�=qA��\A�VA�ĜA�dZA�Q�A�-A�S�A���A���A��7A�Q�A��A�A��A�ĜA��A�l�A�7LA��A��A�33A���A�S�A���A�JA���A��TA���A���A��A�M�A��yA�1'A�x�A��A�ƨA�`BA�;A|�A�A~��A~��A~�RA~�A~��A~r�A}t�A|�A|��A|�DA|ZA|M�A|{A{�wAz�Ay�Aw�TAv�Au|�At-As��As��As/Ar��Arz�Aq�mAq�FAq�hAq;dAp��Ap�uAp5?Ap  Ao��Ao|�An��An�`An��An�AnM�Am�Am
=AlbAk"�Ah�!Ag��Ag�Af��Afr�Ae�mAeAe�7Aet�AdĜAc\)Ab=qAa`BAa+A_�;A^{A\�A\A�A[��AZ��AYG�AX1'AW�AW�AV��AV�RAV5?AU��AU��AU�PAUt�AUdZAU;dAU33AU
=AT�!AS�-ARZAQ�
AQ`BAP��APJAOl�AN��ANE�AM`BAK�PAK"�AJjAIO�AI�AH��AH�\AHE�AG��AG+AF^5AEp�AEdZAEl�AEK�ADVACVABv�AB5?AA��AA�FAA��AA|�AAC�AA�A@�HA@n�A?�^A>�`A=�A=33A<r�A<�A;�wA;�hA;x�A;�A:^5A9��A:bNA:��A:��A:�+A:ffA:Q�A:5?A:JA9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	%B	OB	�B	�B	\B	�B	DB	
�B	xB	
	B		7B	�B		�B	
	B	�B	�B	�B	B	�B	YB	fB	fB	�B	1B	1B	�B	�B	�B		�B	DB	
	B	1B	1B	�B	�B		lB	PB	�B	B	�B	~B	B	�B	
=B	%B	B	aB	�8B

	B
,qB
|B
�mB
�]B
�pB
�
B
ܒB
��B$B9�B*eBYB
�B
�8B?�B��B`�B��B|�B\�BL�B*0B �B�B
�`B
�,B
��B
^5B
6�B
B	� B	�B	�B	��B	��B	�B	|�B	iDB	V9B	N<B	FtB	B�B	&B	'B	B	�B��B�MB�B�pB�B��B��B�BרB�TB	;B	 �B	oB	�B	�B	�B	-�B	+B	C�B	}"B	��B	��B	��B	�B	��B	�B	��B	�B	��B	ŢB	�B	ҽB	ԕB	��B	چB	�B	��B	�ZB	��B	��B	��B	��B	�B	��B	��B	��B
B
�B
�B
 �B
 4B
 4B
oB
�B
oB
  B	��B	��B
 4B
B

rB
PB
�B
�B
�B
CB
)�B
:*B
9�B
=qB
=B
<B
:�B
=B
@�B
GB
G�B
F�B
F�B
GEB
H�B
H�B
IB
I�B
K�B
M�B
QNB
Q�B
T�B
VmB
W
B
XB
WsB
WsB
W�B
XB
WsB
W
B
W�B
W
B
VmB
V�B
VB
W�B
YB
XEB
XB
W�B
W
B
VB
VB
VB
UgB
T�B
S�B
S�B
T,B
S�B
T,B
S[B
S[B
S[B
T,B
S&B
RTB
R�B
PHB
N�B
OvB
Q�B
QB
O�B
OBB
N�B
M�B
MB
L�B
LdB
L�B
L�B
L0B
K^B
J�B
J�B
I�B
H�B
HKB
GEB
FtB
F?B
D�B
C-B
B�B
A�B
?�B
?HB
=B
;�B
9�B
6B
4nB
1�B
3�B
4B
5B
4nB
2-B
.�B
.IB
+�B
+B
'�B
%B
"�B
#B
#:B
"�B
"4B
!�B
 �B
�B
 �B
 \B
�B
�B
�B
B
�B
�B
IB
~B
IB
�B
�B
CB
qB
�B
�B
�B
�B
_B
�B
SB
�B
MB
@B
:B
�B
4B
�B
�B
.B
bB
�B
(B
VB
�B
�B
B
�B
B
xB
~B
~B
�B
�B
B
�B
�B
�B
�B
VB
�B
"B
�B
�B
�B
�B
�B
(B
\B
\B
\B
(B
(B
(B
�B
VB
�B
PB
�B
�B
VB
VB
�B
�B
�B
PB
�B
~B
JB

rB

�B

=B

	B
	7B
�B

�B
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
	B

=B
�B
B
�B
(B
�B
VB
"B
VB
�B
"B
VB
"B
�B
�B
(B
VB
�B
�B
(B
�B
�B
�B
(B
�B
�B
.B
.B
.B
.B
.B
bB
�B
hB
�B
:B
:B
�B
�B
B
B
�B
uB
@B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
hB
hB
4B
�B
�B
oB
{B
�B
�B
�B
FB
�B
B
�B
$B
_B
�B
�B
�B
�B
eB
7B
7B
�B
qB
�B
�B
�B
�B
�B
�B
�B
�B
CB
xB
�B
�B
�B
xB
�B
B
IB
�B
�B
�B
�B
�B
B
�B
B
IB
IB
B
B
~B
B
�B
�B
�B
�B
 'B
 \B
 �B
 �B
 �B
"�B
"�B
"�B
!�B
!bB
!�B
"�B
"�B
#:B
#�B
#:B
#�B
$tB
$@B
$@B
$�B
%B
%B
$�B
$�B
%zB
%�B
%�B
&LB
'B
'RB
'B
&�B
&�B
'�B
'B
'B
&�B
(�B
'�B
'�B
(�B
)_B
)�B
)_B
)�B
)�B
)�B
*�B
*�B
,�B
,�B
+�B
+�B
,�B
,qB
,�B
-wB
-�B
-wB
-�B
-�B
.�B
.�B
.�B
.IB
/�B
0UB
0UB
0UB
1'B
1[B
1�B
2aB
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3hB
3hB
3�B
4nB
4�B
4�B
4�B
4�B
4�B
5B
4�B
5B
5tB
5tB
6B
6B
6B
6B
6zB
6�B
7�B
8RB
:*B
:�B
;�B
;�B
<6B
=qB
<�B
=B
<6B
<6B
<6B
<B
;�B
;dB
=qB
=�B
<�B
<jB
?�B
?}B
>�B
>BB
>BB
?B
>�B
>�B
@�B
A�B
B�B
B�B
B�B
C-B
C�B
C�B
C�B
C�B
D3B
DgB
DgB
DgB
D3B
D�B
DgB
D3B
C�B
D�B
FtB
FtB
F?B
F�B
FtB
FB
E�B
F�B
GB
F�B
GEB
F�B
F�B
G�B
G�B
G�B
G�B
HB
I�B
I�B
J�B
J�B
K)B
K�B
K�B
K^B
K�B
K^B
K�B
K�B
K�B
K�B
L0B
LdB
L�B
L�B
L�B
L�B
M6B
M6B
M6B
M6B
MjB
MjB
MjB
MjB
MjB
M�B
N<B
NB
NB
N<B
NpB
M�B
NpB
OB
OBB
OvB
OBB
PB
O�B
O�B
O�B
P�B
P�B
P�B
QNB
QB
QNB
Q�B
Q�B
Q�B
R B
R�B
S�B
S�B
S�B
T�B
T�B
U2B
U2B
U�B
U�B
U�B
W
B
W�B
VmB
VmB
V�B
V�B
V�B
W
B
X�B
XB
XEB
YKB
XyB
YKB
XEB
XyB
Y�B
[WB
Z�B
[#B
Z�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZQB
ZQB
Z�B
[#B
[�B
[WB
[�B
[�B
[�B
\]B
\)B
\)B
\�B
]/B
]dB
]dB
]�B
^5B
^�B
^�B
_B
_B
_;B
_;B
_;B
_pB
_;B
_pB
_pB
`B
`�B
aB
a�B
bB
bB
bB
bNB
b�B
bNB
b�B
c B
cTB
c�B
c�B
c�B
d�B
dZB
d&B
d�B
ffB
f�B
gB
gmB
g�B
f�B
gB
gB
gB
f�B
f�B
f�B
f�B
f�B
g8B
g�B
g8B
g8B
gB
gB
gB
f�B
g�B
g�B
hsB
hsB
h�B
h�B
iDB
jB
j�B
jB
j�B
jKB
kB
jB
kB
k�B
k�B
k�B
l�B
lWB
l�B
ncB
o B
o�B
o�B
poB
p�B
pB
pB
p�B
p�B
pB
poB
p�B
pB
p�B
qB
p�B
qB
q�B
qAB
p�B
rB
sB
s�B
tB
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
v+B
v�B
v�B
v�B
v�B
v+B
v+B
v�B
wfB
v�B
w�B
w�B
y	B
y	B
y	B
xlB
xB
x�B
yrB
xB
y>B
yrB
x�B
y>B
y�B
yrB
yrB
zB
y�B
z�B
{B
zxB
{JB
z�B
{JB
{B
{�B
z�B
{B
zB
zDB
y�B
zB
zxB
zDB
zDB
{JB
{JB
{B
|PB
|PB
}�B
}�B
~�B
~�B
}�B
}�B
}�B
}�B
}�B
}"B
}�B
}"B
}�B
~�B
~]B
~�B
cB
�4B
��B
��B
��B
�;B
��B
�uB
�B
�GB
�GB
��B
��B
�uB
�AB
�AB
��B
��B
��B
��B
��B
��B
��B
��B
�YB
�YB
�YB
�YB
��B
��B
�YB
�YB
�SB
�B
��B
��B
�SB
��B
��B
��B
��B
�%B
��B
�YB
��B
�+B
��B
��B
��B
�_B
��B
�+B
��B
��B
��B
�fB
��B
�1B
�B
��B
��B
�7B	&�B	B	�B	!�B	@�B	B	�B	4B	�B	0�B	�B	�B	�B	�B	!B	�B	�B	�B	�B	�B	"B	�B	"B	�B	PB	
�B	DB	�B	DB	
=B	DB	B	
=B	DB	~B	xB	
rB	
	B	B	1B	�B	B		�B	�B	1B	�B	_B	�B	�B		�B		lB		lB	B	B	fB	1B		lB	
�B	�B		7B	JB	fB		�B	_B	�B	�B	B	�B	{B	�B	�B	�B	�B	�B	MB	�B	B	{B	MB	�B	�B	�B	�B	�B	MB	�B	SB	�B	B	B	�B	B	�B	�B	_B	B	�B	�B	�B	{B	B	�B	+B	�B	�B	_B	�B	YB		�B		7B		�B	1B		lB	_B	�B	1B	�B	�B	�B		�B		B	_B		lB		�B	�B	�B	�B		7B	�B	�B	fB		7B	�B	_B	�B		lB		lB	�B	fB		B	�B	�B	fB	�B	_B	+B	�B	1B	�B	+B	�B	�B	_B	_B	1B	1B	�B	fB	1B	xB	fB		B		�B	�B	�B	xB	
�B		�B	
�B	~B	B	
rB	
�B	�B	
�B	B	
�B	
�B		7B	�B		7B		B	�B	+B		B		7B	_B	_B		�B		lB	�B	%B	1B	fB	_B	�B	fB	�B	_B	�B	�B	�B	�B	_B	�B	fB	1B	%B	
�B		�B		�B	
=B	
=B	�B	xB	�B	�B	�B	�B	�B	�B	�B	B	VB	"B	�B	~B	�B	"B	�B	JB	�B	"B	�B	~B	JB	"B	�B	~B	B	B	�B	�B	B	PB	PB	�B	DB	xB	PB	PB	B	�B	~B	�B	DB	�B	�B	�B	B	B	PB	�B	xB	
�B	�B	�B	xB	
rB	
=B	JB	~B	�B	�B	�B	�B	fB	�B	%B	+B	�B	�B	MB	SB	�B	�B	�B	B	�B	�B	 iB��B��B	YB	�B	 'B	.IB	K^B	I�B	Q�B	PB	V9B	[WB	Y�B	YKB	YB	YB	WsB	S�B	R B	S&B	R�B	Q�B	OB	N�B	N<B	MB	K)B	GzB	GEB	IRB	H�B	G�B	GzB	I�B	R�B	U2B	U�B	R�B	Q�B	PB	NB	T�B	a�B	a�B	c�B	iB	h>B	jB	jB	l�B	sMB	��B	��B	��B	��B	��B	�gB	�#B	�0B	�6B	�EB	��B	�B	��B	��B	�B	�B	�|B	�MB	�`B	�B	��B	�fB	�lB
1B	��B	�PB	��B	�JB	�PB	��B	��B
 �B	�.B
 �B
B
�B
�B
SB
�B
SB
�B
	B
B

�B
	B
1B
	lB
�B
�B
DB
~B
�B
PB
�B
�B
�B
B
oB
:B
B
�B
�B
�B
+B
<jB
OB
TaB
ZQB
R�B
XEB
]�B
j�B
gB
aHB
e�B
m)B
}�B
��B
�B
��B
��B
�B
�B
��B
��B
ĜB
�RB
�^B
�TB
��B
�gB
ҽB
҉B
֡B
�yB
�)B
�/B
��B
�)B
�]B
ܒB
��B
��B
�&B
��B
��B
�TB
ݘB
�B
��B
�B
�BB
��B
�KB
�B
�B
ҽB
�B
уB
� B
�2B
רB
�[B
��B
��B
�yB
�EB
�gB
ΥB
�<B
��B
�gB
�B
��B
�;B
��B
�B
�#B
��B
��B
ݘB
�B
ݘB
�B
�pB
��B
� B
��B
�B
�"B
�2B
��B@B1B
	BPB	lB�B�B
�B�B.�B8�B:*B@OBB�B:�B7B6�B:�B5�B6�B6B5?B.�B-B2aB)*BR BB�B�BSB�B4B�B�B	7B�B\B�B�B
�>B
��B
��B
�B
�B
�B
�B
�;B
�B
�B
�vB
��B
��B
�B 4B
�rB  B
�DB
��B
��B
�B
��B
��B�B�BSB
��BH�BEBMBN<B�B��B��B��B�BB��B��B�$B��B��B��BncBrGBk�B[�BP}B2aBD�B_�B�B�\B�B��B��B��B��B��B�DB��B�+B��B|�By�Bq�BcTBh�Bd�Be�Bb�BW
BS�BQ�BLdBN<BJ#BFtBU2Bc B?HB:�B.�B,B+�B)_B(�B)�B(�B'�B#�B!bB�B�B�B�B%B�B�BB B�BxBfB
��B
�B
��B
��B
�B
�B
��B
�B
��B
�9B
ޞB
�tB
��B
�kB
�CB
��B
��B
��B
�xB
��B
�;B
m�B
O�B
K)B
LdB
Q�B
R B
:�B
/�B
5?B
K^B
9�B
&�B
+B
�B	��B	�(B	�`B	�ZB	�MB	�8B	�B
  B	�B	�fB	��B	�HB	ںB	�B	�%B	�NB	�aB	ΥB	��B	�)B	��B	ɺB	��B	�NB	��B	��B	�FB	��B	��B	�B	�SB	�lB	��B	�_B	��B	��B	��B	�-B	|PB	�B	z�B	z�B	u�B	|�B	zB	��B	�MB	zDB	n�B	v�B	e�B	W�B	S�B	b�B	T�B	W
B	`�B	OB	QNB	U�B	O�B	Q�B	N�B	JXB	F?B	R B	F�B	@B	G�B	>�B	B�B	V�B	B�B	IRB	I�B	R�B	<B	1�B	(�B	3hB	1�B	%B	�B	�B	7�B	+�B	/�B	B	{B	=qB	$�B	SB	�B	�B	#�B	�B	�B	MB	�B	
�B��B	�B�rB��B�B��B�GB�%B��B�|B�B	GB�JB��B�&B�)B��B�>B�B��B�ZB�yB̘B�B�vB��BŢB��B��BB�3B�pBȀB��B�[B�dB�B��B��B˒B�0B�jB�6B�&B��B��B�5BܒB��B�mB�AB�NB�pB�&B֡BѷB� B��B�
B�#B��B��B	 �B	;B	GB	B	AB	oB	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             B	9�B	/iB	!�B	�B	�B	\B	B	^B	B	
�B		�B	�B	
=B	
�B	�B	�B	�B	B	�B	YB	�B	�B	�B	fB	KB	B	�B	�B		�B	xB	
XB	KB	KB	�B	�B		7B	PB	�B	6B	B	�B	0B	0B	B		RB	�B	��B
�B
�B
B�B
��B
ɠB
�B
��B
��B
�>B
�qB$tBG�B:�B�B
��B
��BGzBȀBp�B�nB�Bf2BVmB-]B(sB�B�B
�B
�UB
t�B
NpB
	�B	��B	��B	�yB	��B	�&B	��B	�HB	sMB	\�B	T�B	T�B	PbB	5ZB	<jB	#�B	EB��B	oB��B��B��B��B��B�wB�qB�B	gB	�B	�B	�B	�B	;B	0�B	(
B	?�B	|�B	��B	��B	��B	��B	�5B	��B	�B	��B	�"B	�B	ΥB	��B	�B	��B	�=B	�B	�&B	�B	��B	�rB	��B	��B	�B	�VB	�<B	�wB
�B
B
[B
 �B
 �B
oB
B
tB
�B
�B
 iB	��B
�B
�B
�B
�B
uB
�B
B
kB
(XB
;B
<jB
>BB
>(B
=<B
:�B
=�B
AB
G�B
H�B
GEB
GEB
G�B
IRB
I7B
IlB
KB
MB
OBB
Q�B
SB
WsB
X_B
X�B
Y�B
XEB
X�B
Y1B
YeB
W�B
X�B
YKB
XyB
W�B
W�B
VSB
X_B
[	B
Y�B
Y�B
Y�B
XyB
W?B
XB
XEB
V�B
U�B
T{B
T,B
T�B
UB
T�B
S�B
S�B
S�B
U2B
S�B
S@B
T�B
Q4B
OB
PB
R�B
Q�B
P.B
O�B
O�B
N�B
M�B
MPB
MPB
O\B
NB
MPB
K�B
K�B
LdB
KB
J	B
H�B
G�B
GEB
G�B
EmB
D�B
EB
BuB
@�B
@OB
>(B
=qB
<PB
8lB
6zB
2�B
3�B
4�B
6+B
6+B
3MB
0UB
0oB
-CB
,�B
(�B
&�B
$B
#�B
#�B
#:B
"�B
"�B
"B
 'B
"�B
!�B
�B
�B
 �B
 �B
!-B
5B
~B
�B
pB
�B
�B
B
/B
B
�B
�B
�B
�B
_B
�B
�B
�B
FB
B
B
�B
B
 B
B
B
�B
\B
�B
�B
<B
\B
}B
JB
�B
6B
PB
B
�B
�B
�B
�B
�B
<B
BB
VB
BB
bB
�B
BB
\B
BB
�B
�B
�B
vB
\B
vB
�B
�B
�B
�B
"B
�B
�B
�B
.B
�B
B
jB
�B
�B
�B
6B
B
�B

�B

XB
	�B

	B
�B
1B
�B
�B
fB
�B
tB
YB
B
YB
tB
B
�B
�B

#B
0B
VB
�B
�B
�B
�B
�B
BB
vB
�B
�B
�B
�B
�B
�B
B
vB
�B
�B
�B
.B
�B
�B
�B
�B
B
 B
�B
}B
�B
 B
�B
�B
�B
�B
@B
@B
�B
�B
�B
B
�B
�B
FB
aB
aB
�B
FB
,B
FB
@B
uB
�B
TB
�B
 B
�B
�B
�B
�B
�B
TB
oB
�B
�B
yB
�B
�B
�B
SB
sB
sB
�B
eB
eB
�B
eB
QB
kB
�B
	B
�B
]B
CB
)B
�B
�B
5B
dB
B
�B
�B
�B
�B
/B
�B
B
B
�B
B
B
/B
dB
�B
~B
/B
~B
�B
�B
~B
�B
5B
�B
�B
�B
�B
!B
 �B
 �B
!B
 �B
!bB
# B
#:B
# B
!�B
"B
"NB
"�B
#B
#�B
$&B
#�B
$�B
$�B
$�B
$�B
%zB
%`B
%,B
%B
%�B
&B
&LB
&B
&�B
(>B
'�B
'�B
'RB
'B
(�B
'�B
'�B
'�B
)_B
(>B
(�B
*eB
)�B
*B
)�B
*KB
)�B
*0B
*�B
+B
.B
.IB
+�B
,B
,�B
,qB
-CB
./B
.B
-�B
.B
.B
/ B
/B
.�B
/ B
1B
1B
0�B
1B
1�B
1�B
2aB
2|B
1�B
1�B
2|B
2�B
3hB
3B
33B
3MB
3�B
3�B
4TB
4�B
4�B
4�B
4�B
5B
5%B
5�B
5?B
5�B
5�B
5�B
6�B
6FB
6`B
6`B
6�B
7B
7�B
8�B
:DB
:�B
;�B
;�B
<�B
>wB
=<B
=�B
<�B
<�B
<�B
<�B
<B
;�B
>B
>]B
<�B
<6B
@B
@ B
?.B
>�B
>]B
@B
>�B
>�B
@�B
A�B
B�B
CB
C-B
C�B
C�B
C�B
DB
DB
D�B
D�B
D�B
D�B
D�B
E�B
D�B
D�B
D3B
EB
F�B
F�B
F�B
G�B
GEB
FYB
F?B
GB
GzB
GzB
G�B
F�B
G�B
H1B
G�B
G�B
HB
G�B
I�B
JXB
K)B
K)B
K�B
L�B
K�B
K�B
K�B
K�B
L0B
K�B
LB
LB
LdB
L�B
M6B
M6B
M6B
MB
M�B
NB
MjB
MjB
M�B
M�B
M�B
M�B
M�B
N"B
N�B
N�B
NVB
N�B
N�B
NVB
OB
O�B
O�B
O�B
O�B
P�B
PHB
PB
P}B
Q4B
Q4B
Q4B
Q�B
Q�B
Q�B
R B
R:B
Q�B
R:B
S&B
T�B
TB
T�B
T�B
T�B
UMB
U2B
U�B
U�B
VB
X+B
XEB
V�B
W
B
WYB
V�B
V�B
WYB
YeB
X+B
X�B
Y�B
X�B
Y�B
X�B
XEB
Z7B
[�B
[=B
\CB
[qB
Z�B
Z7B
Y�B
ZB
Z7B
Y�B
Z7B
Z�B
Z�B
[=B
[qB
[�B
[�B
[�B
\B
\)B
\�B
\�B
\�B
]dB
]dB
]�B
]�B
^B
^�B
_;B
_;B
_!B
_!B
_VB
_;B
_VB
_�B
_VB
_�B
_�B
`�B
aB
abB
bB
bNB
bhB
b�B
b�B
b�B
b�B
cB
c�B
c�B
c�B
c�B
d@B
eB
dtB
dtB
e`B
f�B
f�B
g8B
g�B
h$B
gB
g8B
g�B
gRB
g8B
gRB
gB
f�B
f�B
g�B
hsB
g�B
gmB
gB
gB
gB
g8B
hXB
hXB
h�B
h�B
h�B
h�B
iyB
j�B
j�B
j�B
kB
j�B
kkB
j�B
k�B
lWB
k�B
k�B
l�B
lWB
l�B
n�B
oOB
o�B
o�B
p�B
qAB
poB
p!B
p�B
p�B
p!B
poB
p�B
p;B
p�B
q'B
q'B
q'B
q�B
q[B
qB
r-B
sB
s�B
tB
s�B
t�B
uB
t�B
t�B
t�B
t�B
t�B
utB
vFB
v�B
v�B
v�B
v�B
v`B
v`B
wB
w�B
wfB
x8B
xlB
y>B
y>B
y$B
x�B
x�B
y�B
y�B
x�B
y�B
y�B
x�B
yrB
zB
y�B
y�B
zDB
zxB
{B
{�B
z�B
{dB
{B
{dB
{JB
|B
{dB
|jB
z�B
z�B
y�B
zDB
z�B
z^B
zxB
{B
{B
{�B
|6B
|PB
}�B
}�B
~�B
~�B
~(B
~(B
~(B
}�B
}�B
}VB
}�B
}VB
~B
~�B
~�B
.B
cB
�iB
� B
��B
�;B
�UB
�B
��B
�GB
�aB
��B
�3B
��B
��B
�[B
��B
��B
��B
�B
��B
��B
��B
�%B
�%B
�YB
�YB
�YB
�tB
��B
�EB
��B
��B
��B
�SB
��B
��B
�mB
��B
��B
��B
�B
�?B
��B
��B
��B
�EB
�B
�zB
��B
�zB
��B
�EB
��B
�1B
�B
��B
��B
�fB
�7B
��B
��G�O�B	&�B	B	�B	!�B	@�B	B	�B	4B	�B	0�B	�B	�B	�B	�B	!B	�B	�B	�B	�B	�B	"B	�B	"B	�B	PB	
�B	DB	�B	DB	
=B	DB	B	
=B	DB	~B	xB	
rB	
	B	B	1B	�B	B		�B	�B	1B	�B	_B	�B	�B		�B		lB		lB	B	B	fB	1B		lB	
�B	�B		7B	JB	fB		�B	_B	�B	�B	B	�B	{B	�B	�B	�B	�B	�B	MB	�B	B	{B	MB	�B	�B	�B	�B	�B	MB	�B	SB	�B	B	B	�B	B	�B	�B	_B	B	�B	�B	�B	{B	B	�B	+B	�B	�B	_B	�B	YB		�B		7B		�B	1B		lB	_B	�B	1B	�B	�B	�B		�B		B	_B		lB		�B	�B	�B	�B		7B	�B	�B	fB		7B	�B	_B	�B		lB		lB	�B	fB		B	�B	�B	fB	�B	_B	+B	�B	1B	�B	+B	�B	�B	_B	_B	1B	1B	�B	fB	1B	xB	fB		B		�B	�B	�B	xB	
�B		�B	
�B	~B	B	
rB	
�B	�B	
�B	B	
�B	
�B		7B	�B		7B		B	�B	+B		B		7B	_B	_B		�B		lB	�B	%B	1B	fB	_B	�B	fB	�B	_B	�B	�B	�B	�B	_B	�B	fB	1B	%B	
�B		�B		�B	
=B	
=B	�B	xB	�B	�B	�B	�B	�B	�B	�B	B	VB	"B	�B	~B	�B	"B	�B	JB	�B	"B	�B	~B	JB	"B	�B	~B	B	B	�B	�B	B	PB	PB	�B	DB	xB	PB	PB	B	�B	~B	�B	DB	�B	�B	�B	B	B	PB	�B	xB	
�B	�B	�B	xB	
rB	
=B	JB	~B	�B	�B	�B	�B	fB	�B	%B	+B	�B	�B	MB	SB	�B	�B	�B	B	�B	�B	 iB��B��B	YB	�B	 'B	.IB	K^B	I�B	Q�B	PB	V9B	[WB	Y�B	YKB	YB	YB	WsB	S�B	R B	S&B	R�B	Q�B	OB	N�B	N<B	MB	K)B	GzB	GEB	IRB	H�B	G�B	GzB	I�B	R�B	U2B	U�B	R�B	Q�B	PB	NB	T�B	a�B	a�B	c�B	iB	h>B	jB	jB	l�B	sMB	��B	��B	��B	��B	��B	�gB	�#B	�0B	�6B	�EB	��B	�B	��B	��B	�B	�B	�|B	�MB	�`B	�B	��B	�fB	�lB
1B	��B	�PB	��B	�JB	�PB	��B	��B
 �B	�.B
 �B
B
�B
�B
SB
�B
SB
�B
	B
B

�B
	B
1B
	lB
�B
�B
DB
~B
�B
PB
�B
�B
�B
B
oB
:B
B
�B
�B
�B
+B
<jB
OB
TaB
ZQB
R�B
XEB
]�B
j�B
gB
aHB
e�B
m)B
}�B
��B
�B
��B
��B
�B
�B
��B
��B
ĜB
�RB
�^B
�TB
��B
�gB
ҽB
҉B
֡B
�yB
�)B
�/B
��B
�)B
�]B
ܒB
��B
��B
�&B
��B
��B
�TB
ݘB
�B
��B
�B
�BB
��B
�KB
�B
�B
ҽB
�B
уB
� B
�2B
רB
�[B
��B
��B
�yB
�EB
�gB
ΥB
�<B
��B
�gB
�B
��B
�;B
��B
�B
�#B
��B
��B
ݘB
�B
ݘB
�B
�pB
��B
� B
��B
�B
�"B
�2B
��B@B1B
	BPB	lB�B�B
�B�B.�B8�B:*B@OBB�B:�B7B6�B:�B5�B6�B6B5?B.�B-B2aB)*BR BB�B�BSB�B4B�B�B	7B�B\B�B�B
�>B
��B
��B
�B
�B
�B
�B
�;B
�B
�B
�vB
��B
��B
�B 4B
�rB  B
�DB
��B
��B
�B
��B
��B�B�BSB
��BH�BEBMBN<B�B��B��B��B�BB��B��B�$B��B��B��BncBrGBk�B[�BP}B2aBD�B_�B�B�\B�B��B��B��B��B��B�DB��B�+B��B|�By�Bq�BcTBh�Bd�Be�Bb�BW
BS�BQ�BLdBN<BJ#BFtBU2Bc B?HB:�B.�B,B+�B)_B(�B)�B(�B'�B#�B!bB�B�B�B�B%B�B�BB B�BxBfB
��B
�B
��B
��B
�B
�B
��B
�B
��B
�9B
ޞB
�tB
��B
�kB
�CB
��B
��B
��B
�xB
��B
�;B
m�B
O�B
K)B
LdB
Q�B
R B
:�B
/�B
5?B
K^B
9�B
&�B
+B
�B	��B	�(B	�`B	�ZB	�MB	�8B	�B
  B	�B	�fB	��B	�HB	ںB	�B	�%B	�NB	�aB	ΥB	��B	�)B	��B	ɺB	��B	�NB	��B	��B	�FB	��B	��B	�B	�SB	�lB	��B	�_B	��B	��B	��B	�-B	|PB	�B	z�B	z�B	u�B	|�B	zB	��B	�MB	zDB	n�B	v�B	e�B	W�B	S�B	b�B	T�B	W
B	`�B	OB	QNB	U�B	O�B	Q�B	N�B	JXB	F?B	R B	F�B	@B	G�B	>�B	B�B	V�B	B�B	IRB	I�B	R�B	<B	1�B	(�B	3hB	1�B	%B	�B	�B	7�B	+�B	/�B	B	{B	=qB	$�B	SB	�B	�B	#�B	�B	�B	MB	�B	
�B��B	�B�rB��B�B��B�GB�%B��B�|B�B	GB�JB��B�&B�)B��B�>B�B��B�ZB�yB̘B�B�vB��BŢB��B��BB�3B�pBȀB��B�[B�dB�B��B��B˒B�0B�jB�6B�&B��B��B�5BܒB��B�mB�AB�NB�pB�&B֡BѷB� B��B�
B�#B��B��B	 �B	;B	GB	B	AB	oB	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             <�K<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��=#W<<z�<j4�<�}e<#�
<#�
<6��<s��<���<K�_<�.�<d�<q��<�.�<#�
<N��<#�
<#�
<̅<��c<j4�<V4j<'�[<0�<#�
<#�
<?y�<��,<ǉ�<�y�<��<��<#�
<mMe<8;<���<2�<#�
<#�
<�V�<3�p<#�
<#�
<v_<h<��=<�O�<k9<#�
<#�
<o�e<��%<:Q0<>җ<#�
<2� <1_<#�
<#�
<#�
<#�
<#�
<#�
</��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018062204451120180622044511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018070207034220180702070342QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018070207034220180702070342QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550820190521075508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                