CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-05-13T21:06:11Z creation; 2020-07-07T21:55:47Z DMQC;      
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  d@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20200513210611  20210429202812  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_133                 6810_008521_133                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�3���%@�3���%11  @�4-�@�4-�@3;�D=G@3;�D=G�e+�V�ϫ�e+�V�ϫ11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�@E�@z�H@��R@�G�@�  @��RA��A!G�A,��A@  A_\)A�  A�  A��A�Q�A�  A�  A�Q�A�Q�A��B�
B  B(�B (�B(  B/�
B7�
B?�
BG�
BP  BX(�B`  Bg�
Bo�Bw�B�  B��B��B�  B�  B�{B�{B�  B��B�{B�{B�  B�  B��B�  B�(�B�{B�{B�{B�  B�{B�  B�  B�  B�{B�{B�{B�  B��
B��B�  B�  B��C
=C
=C  C  C

=C
=C  C��C
=C  C
=C{C��C��C
=C {C"  C#��C%��C'��C*  C+��C-��C0  C1��C4  C5��C8  C:
=C<  C>
=C@  CB
=CD  CE��CH
=CJ
=CL  CM��CO��CQ�CS��CU��CW��CZ
=C\  C]��C`  Cb  Cd  Cf
=Ch  Ci��Cl  Cm��Cp  Cr  Ct
=Cv  Cw��Cz  C|
=C~  C�  C���C���C�C���C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C���C�C���C���C�C�  C�
=C���C���C�  C���C���C���C�  C�C�  C�C�  C�  C���C�  C���C���C���C�  C�C�C�C�
=C�  C���C���C���C�C�
=C�
=C�C���C�C�  C���C�  C�C�
=C�C���C�C�\C�  C�  C�C�  C�  C�C�
=C�C���C���C�  C�  C�C�C�C�  C�C�  C�C�C�  C�C�  C�C�C���C�C�C���C�  C�  C�  C�  C���C���C���C�  C���C���C�  C�  C���C�  C�  C���C���C���C�  C�C�  C�  C�C�C�
=C�
=C�  C���C�C�
=C�  C�  C�  C���C���D � D  D� D�D��D  D� D�D��D  D��D�qD� D  D� D�qD� D�qD	� D
  D
}qD
�qD}qD  D� D  D� D�qD}qD  D��D�D� D  D� D  D��DD�D�D}qD�qDz�D��D}qD  D��D�D� D�D��DD��D�qD� D  D� D  D� DD� D  D��D �D � D �qD!}qD"�D"��D#�D#��D$�D$}qD$�qD%� D&  D&� D&�qD'xRD'�qD(� D)  D)��D*  D*��D+  D+� D,  D,}qD,�qD-� D-�qD.� D/�D/}qD0  D0�D1  D1� D2�D2}qD2�qD3}qD3�qD4� D5  D5}qD6  D6}qD6�qD7}qD8�D8��D9�D9�D:D:� D;�D;� D<  D<�D=�D=��D>�D>� D>�qD?� D@  D@}qD@�qDA}qDB  DB� DB�qDC� DD  DD}qDD�qDE}qDF  DF� DG  DG��DH�DH��DI  DI� DJ�DJ� DK�DK�DLDL��DM  DM� DN  DN}qDN��DO� DPDP�DQ�DQ� DQ�qDR� DS  DS}qDT  DT}qDT��DU}qDU�qDV� DWDW}qDX  DX}qDX�qDY}qDY�qDZ}qD[  D[� D\�D\� D\�qD]��D^�D^� D_  D_��D`  D`� D`�qDa� Db�Db��DcDc� Dd  Dd� De  De� De�qDf� Dg�Dg� Dh  Dh��Di�Di��Di�qDj}qDj�qDk}qDl�Dl��Dm  Dmz�Dm��Dn� Do�Do� Do�qDp��Dq�Dq� Dr  Dr� Ds  Ds� Dt�Dt��Du�Du� Du�qDv� Dv�qDw� Dx  Dx}qDy  Dy��Dy�qDz� D{�D{�D|D|��D}  D}}qD}��D~z�D  D��D�HD�@ D�� D�D�  D�AHD�~�D���D�  D�@ D�� D��HD�HD�AHD�~�D�� D�HD�@ D�}qD�� D�HD�AHD��HD�D�  D�>�D�~�D���D��qD�>�D�~�D�� D��D�C�D���D�� D���D�@ D�� D��HD�HD�AHD��HD�� D�HD�AHD�� D�� D�  D�AHD�~�D��qD�  D�@ D�~�D�� D���D�=qD�� D��HD�  D�>�D�}qD���D�  D�>�D�� D�� D���D�=qD�� D�� D�  D�@ D�� D��HD��D�AHD�� D�� D���D�@ D��HD�D�  D�=qD�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D���D�  D�@ D�� D��HD�  D�>�D��HD�� D��qD�>�D�� D��HD���D�=qD�� D��HD�  D�AHD�� D�� D�HD�AHD�� D�� D���D�=qD�~�D��HD�  D�>�D�� D�� D�  D�>�D�}qD�� D�HD�AHD�� D��HD�HD�@ D�� D�D�HD�@ D�� D�� D�  D�=qD�}qD���D���D�=qD�~�D�� D���D�@ D�� D���D���D�@ D���D��HD�  D�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D��qD��qD�=qD�� D�� D��qD�>�D�� D�� D�  D�@ D�� D��HD���D�=qD�~�D��HD��D�AHD�� D��HD��D�AHD��HD�� D�  D�B�D��HD�� D�  D�@ D�� D�� D�  D�@ D�}qD���D�HD�AHD�� D�� D�HD�AHD���D�� D�  D�AHD�~�D���D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�@ D�� D��HD���D�@ D�~�D��qD��qD�>�D�� D���D��qD�>�D��HD��HD�HD�AHD��HD���D���D�@ D��HD�� D�  D�AHD�� D��HD�HD�@ D��HD��HD�  D�@ D D�� D�  D�@ D�~�DýqD��qD�>�D�~�DĽqD�  D�@ Dŀ D�� D�  D�@ DƁHD��HD�  D�@ Dǀ D�� D�  D�@ DȁHD��HD�HD�>�Dɀ Dɾ�D�  D�AHD�~�D�� D�  D�@ DˁHD�� D�  D�AHD̂�D��HD���D�>�D�~�D;�D�  D�AHD΁HD�� D���D�@ D�~�DϾ�D���D�@ DЁHD��HD��D�B�Dр DѾ�D�  D�AHDҁHD�� D�  D�AHDӀ D��HD�HD�@ D�~�D��HD�HD�@ DՀ D�� D���D�>�Dր D�� D�  D�@ D׀ D��HD�HD�AHD؁HD��HD�HD�AHDـ D��HD�  D�>�Dڀ D��HD�  D�@ Dۀ D�� D�HD�@ D܀ D��HD��D�B�D݁HD��HD�  D�>�D�~�D�� D���D�>�D߀ D��HD�  D�@ D�� D��HD�HD�B�DႏD�� D���D�>�D�~�D�� D�  D�@ D� D�� D���D�=qD� D�� D���D�>�D� D�� D���D�>�D� D�� D�  D�@ D� D�� D�  D�AHD肏D��HD�HD�>�D�}qD龸D���D�@ D� D�� D���D�AHD낏D�� D���D�@ D�HD쾸D���D�@ D�~�D�� D�  D�>�D�HD�D�HD�@ D�~�D�qD�  D�AHD��HD��HD�  D�@ D�HD�D��D�@ D�~�D��HD�  D�@ D�~�D�� D�HD�AHD� D���D�  D�>�D�~�D�� D�  D�AHD��HD��HD�  D�>�D�� D��HD�  D�AHD��HD���D���D�@ D�k�?��?8Q�?aG�?�z�?�p�?�G�@   @\)@�R@5@J=q@Y��@fff@z�H@��@���@�Q�@��R@���@�33@�p�@�ff@˅@�@�G�@�=q@��@�Q�AG�AffA�A  A�
A
=A(�A!G�A&ffA*=qA-p�A1G�A7�A<(�A?\)AC33AHQ�AN{AR�\AUAY��A^�RAc�
AhQ�Amp�AqG�Atz�Ax��A~�RA���A��A��A�\)A�=qA�z�A��RA�  A��A�z�A�
=A���A�33A��A�\)A��A�z�A��RA���A��\A�z�A�
=A�G�A��
A�{A�\)A���A�(�A��RA���A\A�z�AƸRAə�A��
A�AϮAљ�A�(�AָRA�G�A�33A��A�\)AᙚA���A�RA��A��HA��A�A�\A�z�A�ffA���A��A�B (�B ��B{B\)B��B�B
=B  B	G�B
ffB�
B��BB�HB(�B��B�\B�B��B=qB�B��Bp�B�\B�
BG�BffB�B ��B!��B"�RB$(�B%p�B&�RB'�B(z�B*{B+\)B,Q�B-p�B.�\B/�
B1�B2�\B3�B4��B5�B6�HB8Q�B9�B;33B<Q�B=p�B>ffB?�B@��BB�\BC�
BD��BE�BG
=BHQ�BI��BK33BLQ�BM�BNffBO�BQ�BRffBS�BT��BU��BV�RBX(�BY��BZ�HB[�
B\��B^=qB_�Ba�Bb=qBc�Bd��BeBg
=Bhz�Bi�Bk33BlQ�Bm��Bn�RBo�
BqG�Br�RBt(�BuG�BvffBw�Bx��BzffB{�
B|��B}�B33B�Q�B���B��B�Q�B���B�p�B�  B���B�\)B�{B��RB�G�B��
B�ffB��B��
B��\B��B���B�(�B��HB���B�=qB��RB�G�B��
B�z�B��B���B�{B�z�B�
=B���B�(�B�z�B���B�33B��B�(�B��RB��B�p�B��
B�ffB���B�33B��B��B�z�B��HB�p�B��B�Q�B��RB�
=B��B�{B���B�
=B�\)B��B�(�B��RB�33B��B�(�B��\B��HB�G�B�B�Q�B���B�33B���B��B�Q�B��HB�\)B��
B�Q�B��RB�
=B�\)B��
B�=qB��HB�\)B�B�{B�z�B��HB�G�B��
B�Q�B���B�G�B���B��
B�=qB���B�33B��B�{B�z�B���B��B��B�{B��\B���B�33B��B�{B��\B���B�\)B�B�{B�ffB��HB�G�B��
B�=qB��\B���B�33B�B�(�B���B���B�p�B��B�  B�ffB��HB�\)B��
B�(�B�ffB��RB�33B��B�{B\B���B��B�p�B��
B�Q�BĸRB��B�\)BŮB�  B�Q�BƸRB�33BǙ�B��B�(�B�z�B���B��BɅB��
B�=qBʏ\B���B�\)B�B�(�B�ffB̸RB�
=B�\)BͮB�  B�ffB��HB�G�BϮB�  B�Q�BЏ\B���B�33BхB��B�ffB���B�
=B�\)Bә�B��B�=qBԏ\B���B�\)BծB�{B�Q�B֏\B��HB�33BׅB�  B�ffBظRB�
=B�\)BٮB��B�=qB�z�B���B��BۅB�  B�Q�Bܣ�B���B�\)BݮB�  B�=qBޏ\B���B�p�B��
B�=qB��B���B�G�BᙚB��B�Q�B�\B�
=B�p�B��B�Q�B��B���B�G�B�B��B�Q�B�RB�33B�B�  B�Q�B��B���B�G�B陚B�{B�\B���B�\)B�B�  B�Q�B�RB��B홚B�  B�z�B��HB�33B�B��
B�=qB�RB�33B�B�=qB�RB���B�\)B�B�(�B���B��B���B�{B��\B���B�\)B��B�{B��\B�
=B��B�  B�ffB��HB�G�B���B�{B��\B���B�\)B�  B�=qB��RB�33B���C 
=C =qC �C �RC ��C(�Cp�C��C�HC
=C=qCp�C��C�HC
=CG�Cz�C�C��C(�CffC��CC��C�C\)C��C�C(�CffC��C�HC�CQ�C�\CC  C33Cp�C��C�C	(�C	ffC	�RC
  C
=qC
�C
C  CG�Cz�CC��C33CffC��C��C
=CG�Cz�CC  C=qCz�C�RC�C(�CffC��C�
C  C33Cp�C�C�C(�CffC��C�
C
=CG�Cz�C�C�HC�C\)C�\CC  CG�C�\C��C{C\)C��C�
C  C=qCz�CC  CG�C�C��C
=CQ�C�\CC  C=qCp�C��C�HC(�CffC�C��C33Cp�C�C�HC{C\)C��C�HC(�Cp�C�RC�C33CffC��C��C
=CQ�C��C�HC �C Q�C �\C �RC ��C!33C!p�C!�RC!��C"=qC"z�C"�C"�C#�C#Q�C#�\C#��C$
=C$G�C$��C$�
C%�C%Q�C%�\C%��C&
=C&Q�C&��C&�HC'33C'p�C'�C'�HC(�C(\)C(��C(�C)33C)z�C)C*  C*33C*p�C*�C*�C+(�C+p�C+��C+�C,33C,z�C,C-{C-Q�C-�\C-��C.  C.=qC.�C.C/  C/G�C/��C/�
C0�C0\)C0�\C0��C1  C1G�C1�C1�
C2{C2\)C2��C2�HC3(�C3p�C3�C3��C4=qC4p�C4�C4�C5�C5\)C5��C5�HC6(�C6ffC6�C6�C7(�C7z�C7�C8  C8G�C8�\C8�
C9�C9ffC9��C9�HC:�C:\)C:��C:�
C;�C;ffC;��C;�C<33C<�C<C=
=C=Q�C=�\C=�
C>�C>\)C>��C>�
C?�C?\)C?��C?�HC@(�C@ffC@�C@�CA=qCA�CACB
=CBQ�CB��CB�
CC�CC\)CC��CC�
CD�CDffCD��CD�CE(�CEffCE�CE�CF�CFffCF��CF�CG=qCG�CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                      ?�  @�@E�@z�H@��R@�G�@�  @��RA��A!G�A,��A@  A_\)A�  A�  A��A�Q�A�  A�  A�Q�A�Q�A��B�
B  B(�B (�B(  B/�
B7�
B?�
BG�
BP  BX(�B`  Bg�
Bo�Bw�B�  B��B��B�  B�  B�{B�{B�  B��B�{B�{B�  B�  B��B�  B�(�B�{B�{B�{B�  B�{B�  B�  B�  B�{B�{B�{B�  B��
B��B�  B�  B��C
=C
=C  C  C

=C
=C  C��C
=C  C
=C{C��C��C
=C {C"  C#��C%��C'��C*  C+��C-��C0  C1��C4  C5��C8  C:
=C<  C>
=C@  CB
=CD  CE��CH
=CJ
=CL  CM��CO��CQ�CS��CU��CW��CZ
=C\  C]��C`  Cb  Cd  Cf
=Ch  Ci��Cl  Cm��Cp  Cr  Ct
=Cv  Cw��Cz  C|
=C~  C�  C���C���C�C���C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C���C�C���C���C�C�  C�
=C���C���C�  C���C���C���C�  C�C�  C�C�  C�  C���C�  C���C���C���C�  C�C�C�C�
=C�  C���C���C���C�C�
=C�
=C�C���C�C�  C���C�  C�C�
=C�C���C�C�\C�  C�  C�C�  C�  C�C�
=C�C���C���C�  C�  C�C�C�C�  C�C�  C�C�C�  C�C�  C�C�C���C�C�C���C�  C�  C�  C�  C���C���C���C�  C���C���C�  C�  C���C�  C�  C���C���C���C�  C�C�  C�  C�C�C�
=C�
=C�  C���C�C�
=C�  C�  C�  C���C���D � D  D� D�D��D  D� D�D��D  D��D�qD� D  D� D�qD� D�qD	� D
  D
}qD
�qD}qD  D� D  D� D�qD}qD  D��D�D� D  D� D  D��DD�D�D}qD�qDz�D��D}qD  D��D�D� D�D��DD��D�qD� D  D� D  D� DD� D  D��D �D � D �qD!}qD"�D"��D#�D#��D$�D$}qD$�qD%� D&  D&� D&�qD'xRD'�qD(� D)  D)��D*  D*��D+  D+� D,  D,}qD,�qD-� D-�qD.� D/�D/}qD0  D0�D1  D1� D2�D2}qD2�qD3}qD3�qD4� D5  D5}qD6  D6}qD6�qD7}qD8�D8��D9�D9�D:D:� D;�D;� D<  D<�D=�D=��D>�D>� D>�qD?� D@  D@}qD@�qDA}qDB  DB� DB�qDC� DD  DD}qDD�qDE}qDF  DF� DG  DG��DH�DH��DI  DI� DJ�DJ� DK�DK�DLDL��DM  DM� DN  DN}qDN��DO� DPDP�DQ�DQ� DQ�qDR� DS  DS}qDT  DT}qDT��DU}qDU�qDV� DWDW}qDX  DX}qDX�qDY}qDY�qDZ}qD[  D[� D\�D\� D\�qD]��D^�D^� D_  D_��D`  D`� D`�qDa� Db�Db��DcDc� Dd  Dd� De  De� De�qDf� Dg�Dg� Dh  Dh��Di�Di��Di�qDj}qDj�qDk}qDl�Dl��Dm  Dmz�Dm��Dn� Do�Do� Do�qDp��Dq�Dq� Dr  Dr� Ds  Ds� Dt�Dt��Du�Du� Du�qDv� Dv�qDw� Dx  Dx}qDy  Dy��Dy�qDz� D{�D{�D|D|��D}  D}}qD}��D~z�D  D��D�HD�@ D�� D�D�  D�AHD�~�D���D�  D�@ D�� D��HD�HD�AHD�~�D�� D�HD�@ D�}qD�� D�HD�AHD��HD�D�  D�>�D�~�D���D��qD�>�D�~�D�� D��D�C�D���D�� D���D�@ D�� D��HD�HD�AHD��HD�� D�HD�AHD�� D�� D�  D�AHD�~�D��qD�  D�@ D�~�D�� D���D�=qD�� D��HD�  D�>�D�}qD���D�  D�>�D�� D�� D���D�=qD�� D�� D�  D�@ D�� D��HD��D�AHD�� D�� D���D�@ D��HD�D�  D�=qD�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D���D�  D�@ D�� D��HD�  D�>�D��HD�� D��qD�>�D�� D��HD���D�=qD�� D��HD�  D�AHD�� D�� D�HD�AHD�� D�� D���D�=qD�~�D��HD�  D�>�D�� D�� D�  D�>�D�}qD�� D�HD�AHD�� D��HD�HD�@ D�� D�D�HD�@ D�� D�� D�  D�=qD�}qD���D���D�=qD�~�D�� D���D�@ D�� D���D���D�@ D���D��HD�  D�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D��qD��qD�=qD�� D�� D��qD�>�D�� D�� D�  D�@ D�� D��HD���D�=qD�~�D��HD��D�AHD�� D��HD��D�AHD��HD�� D�  D�B�D��HD�� D�  D�@ D�� D�� D�  D�@ D�}qD���D�HD�AHD�� D�� D�HD�AHD���D�� D�  D�AHD�~�D���D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�@ D�� D��HD���D�@ D�~�D��qD��qD�>�D�� D���D��qD�>�D��HD��HD�HD�AHD��HD���D���D�@ D��HD�� D�  D�AHD�� D��HD�HD�@ D��HD��HD�  D�@ D D�� D�  D�@ D�~�DýqD��qD�>�D�~�DĽqD�  D�@ Dŀ D�� D�  D�@ DƁHD��HD�  D�@ Dǀ D�� D�  D�@ DȁHD��HD�HD�>�Dɀ Dɾ�D�  D�AHD�~�D�� D�  D�@ DˁHD�� D�  D�AHD̂�D��HD���D�>�D�~�D;�D�  D�AHD΁HD�� D���D�@ D�~�DϾ�D���D�@ DЁHD��HD��D�B�Dр DѾ�D�  D�AHDҁHD�� D�  D�AHDӀ D��HD�HD�@ D�~�D��HD�HD�@ DՀ D�� D���D�>�Dր D�� D�  D�@ D׀ D��HD�HD�AHD؁HD��HD�HD�AHDـ D��HD�  D�>�Dڀ D��HD�  D�@ Dۀ D�� D�HD�@ D܀ D��HD��D�B�D݁HD��HD�  D�>�D�~�D�� D���D�>�D߀ D��HD�  D�@ D�� D��HD�HD�B�DႏD�� D���D�>�D�~�D�� D�  D�@ D� D�� D���D�=qD� D�� D���D�>�D� D�� D���D�>�D� D�� D�  D�@ D� D�� D�  D�AHD肏D��HD�HD�>�D�}qD龸D���D�@ D� D�� D���D�AHD낏D�� D���D�@ D�HD쾸D���D�@ D�~�D�� D�  D�>�D�HD�D�HD�@ D�~�D�qD�  D�AHD��HD��HD�  D�@ D�HD�D��D�@ D�~�D��HD�  D�@ D�~�D�� D�HD�AHD� D���D�  D�>�D�~�D�� D�  D�AHD��HD��HD�  D�>�D�� D��HD�  D�AHD��HD���D���D�@ G�O�?��?8Q�?aG�?�z�?�p�?�G�@   @\)@�R@5@J=q@Y��@fff@z�H@��@���@�Q�@��R@���@�33@�p�@�ff@˅@�@�G�@�=q@��@�Q�AG�AffA�A  A�
A
=A(�A!G�A&ffA*=qA-p�A1G�A7�A<(�A?\)AC33AHQ�AN{AR�\AUAY��A^�RAc�
AhQ�Amp�AqG�Atz�Ax��A~�RA���A��A��A�\)A�=qA�z�A��RA�  A��A�z�A�
=A���A�33A��A�\)A��A�z�A��RA���A��\A�z�A�
=A�G�A��
A�{A�\)A���A�(�A��RA���A\A�z�AƸRAə�A��
A�AϮAљ�A�(�AָRA�G�A�33A��A�\)AᙚA���A�RA��A��HA��A�A�\A�z�A�ffA���A��A�B (�B ��B{B\)B��B�B
=B  B	G�B
ffB�
B��BB�HB(�B��B�\B�B��B=qB�B��Bp�B�\B�
BG�BffB�B ��B!��B"�RB$(�B%p�B&�RB'�B(z�B*{B+\)B,Q�B-p�B.�\B/�
B1�B2�\B3�B4��B5�B6�HB8Q�B9�B;33B<Q�B=p�B>ffB?�B@��BB�\BC�
BD��BE�BG
=BHQ�BI��BK33BLQ�BM�BNffBO�BQ�BRffBS�BT��BU��BV�RBX(�BY��BZ�HB[�
B\��B^=qB_�Ba�Bb=qBc�Bd��BeBg
=Bhz�Bi�Bk33BlQ�Bm��Bn�RBo�
BqG�Br�RBt(�BuG�BvffBw�Bx��BzffB{�
B|��B}�B33B�Q�B���B��B�Q�B���B�p�B�  B���B�\)B�{B��RB�G�B��
B�ffB��B��
B��\B��B���B�(�B��HB���B�=qB��RB�G�B��
B�z�B��B���B�{B�z�B�
=B���B�(�B�z�B���B�33B��B�(�B��RB��B�p�B��
B�ffB���B�33B��B��B�z�B��HB�p�B��B�Q�B��RB�
=B��B�{B���B�
=B�\)B��B�(�B��RB�33B��B�(�B��\B��HB�G�B�B�Q�B���B�33B���B��B�Q�B��HB�\)B��
B�Q�B��RB�
=B�\)B��
B�=qB��HB�\)B�B�{B�z�B��HB�G�B��
B�Q�B���B�G�B���B��
B�=qB���B�33B��B�{B�z�B���B��B��B�{B��\B���B�33B��B�{B��\B���B�\)B�B�{B�ffB��HB�G�B��
B�=qB��\B���B�33B�B�(�B���B���B�p�B��B�  B�ffB��HB�\)B��
B�(�B�ffB��RB�33B��B�{B\B���B��B�p�B��
B�Q�BĸRB��B�\)BŮB�  B�Q�BƸRB�33BǙ�B��B�(�B�z�B���B��BɅB��
B�=qBʏ\B���B�\)B�B�(�B�ffB̸RB�
=B�\)BͮB�  B�ffB��HB�G�BϮB�  B�Q�BЏ\B���B�33BхB��B�ffB���B�
=B�\)Bә�B��B�=qBԏ\B���B�\)BծB�{B�Q�B֏\B��HB�33BׅB�  B�ffBظRB�
=B�\)BٮB��B�=qB�z�B���B��BۅB�  B�Q�Bܣ�B���B�\)BݮB�  B�=qBޏ\B���B�p�B��
B�=qB��B���B�G�BᙚB��B�Q�B�\B�
=B�p�B��B�Q�B��B���B�G�B�B��B�Q�B�RB�33B�B�  B�Q�B��B���B�G�B陚B�{B�\B���B�\)B�B�  B�Q�B�RB��B홚B�  B�z�B��HB�33B�B��
B�=qB�RB�33B�B�=qB�RB���B�\)B�B�(�B���B��B���B�{B��\B���B�\)B��B�{B��\B�
=B��B�  B�ffB��HB�G�B���B�{B��\B���B�\)B�  B�=qB��RB�33B���C 
=C =qC �C �RC ��C(�Cp�C��C�HC
=C=qCp�C��C�HC
=CG�Cz�C�C��C(�CffC��CC��C�C\)C��C�C(�CffC��C�HC�CQ�C�\CC  C33Cp�C��C�C	(�C	ffC	�RC
  C
=qC
�C
C  CG�Cz�CC��C33CffC��C��C
=CG�Cz�CC  C=qCz�C�RC�C(�CffC��C�
C  C33Cp�C�C�C(�CffC��C�
C
=CG�Cz�C�C�HC�C\)C�\CC  CG�C�\C��C{C\)C��C�
C  C=qCz�CC  CG�C�C��C
=CQ�C�\CC  C=qCp�C��C�HC(�CffC�C��C33Cp�C�C�HC{C\)C��C�HC(�Cp�C�RC�C33CffC��C��C
=CQ�C��C�HC �C Q�C �\C �RC ��C!33C!p�C!�RC!��C"=qC"z�C"�C"�C#�C#Q�C#�\C#��C$
=C$G�C$��C$�
C%�C%Q�C%�\C%��C&
=C&Q�C&��C&�HC'33C'p�C'�C'�HC(�C(\)C(��C(�C)33C)z�C)C*  C*33C*p�C*�C*�C+(�C+p�C+��C+�C,33C,z�C,C-{C-Q�C-�\C-��C.  C.=qC.�C.C/  C/G�C/��C/�
C0�C0\)C0�\C0��C1  C1G�C1�C1�
C2{C2\)C2��C2�HC3(�C3p�C3�C3��C4=qC4p�C4�C4�C5�C5\)C5��C5�HC6(�C6ffC6�C6�C7(�C7z�C7�C8  C8G�C8�\C8�
C9�C9ffC9��C9�HC:�C:\)C:��C:�
C;�C;ffC;��C;�C<33C<�C<C=
=C=Q�C=�\C=�
C>�C>\)C>��C>�
C?�C?\)C?��C?�HC@(�C@ffC@�C@�CA=qCA�CACB
=CBQ�CB��CB�
CC�CC\)CC��CC�
CD�CDffCD��CD�CE(�CEffCE�CE�CF�CFffCF��CF�CG=qCG�CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�A���A��A��A��A��A��A��A��A��A��`A��;AоwAЇ+A�ZA�K�A�dZAЛ�AП�AП�AГuAЍPAЍPAЇ+AЅAЅA�~�A�t�A�p�A�p�A�n�A�ffA�^5A�XA�Q�A�O�A�Q�A�Q�A�S�A�G�A�&�A��AυA�?}A��A��A��A���AΕ�A��;A�&�A̅A��A˴9A��AʼjA�$�A��A��#AɅA�A�A�A�O�Aư!AŸRA���A���AÁA�oA¾wA���A�dZA���A��A�33A��RA�K�A�XA�Q�A�v�A��A��hA��+A���A��7A�A�ƨA��DA���A��A�ƨA�ZA��\A��A�v�A��A�jA���A���A�~�A�?}A�VA�`BA�XA���A��yA�\)A�JA��A��PA�\)A��A�5?A�O�A��TA�7LA��A���A��!A���A��#A���A�z�A�ĜA��A��!A�A�A���A�1'A}�Az�yAyƨAv�9As�-Arv�AoG�Am��Ak�Aj5?Aj�Aj$�Ai�Ai�AhJAfĜAc��Aa��A`��A`ZA_�FA_
=A^(�A\�\AY�AY%AX(�AWVAU�AU�PARr�AQC�APVAO�hAO�AL�yAK/AJI�AG
=AE��ACG�ABbNAA�^A?p�A?
=A>1'A;l�A9��A8�DA7XA6�DA6bNA6�A5hsA3��A3|�A3C�A3A2~�A1�
A1K�A0�HA/7LA.z�A.A-�hA,9XA+p�A)�^A(��A'hsA&jA%33A$�+A#"�A!��A�+A{A��A�TA�A�`A�\A�A�yA�A�AAl�A�9A/AffAdZAQ�A|�A9XA`BA&�A�yAn�A9XA�TA�!A	ƨA�`A�9A�9A�uAƨA;dA+AoA�9A-A1A33A �@��!@�&�@���@���@�A�@��#@��+@���@���@�9X@�l�@��@�\)@�;d@�ff@��@���@� �@�^@�&�@�%@���@�1'@�33@���@��@��
@޸R@��@�I�@�l�@�ȴ@ڗ�@ڇ+@�V@�J@ج@�l�@�@�V@�z�@Ӿw@�V@���@�{@́@�x�@�hs@�O�@�G�@�G�@�O�@�&�@�%@�Ĝ@̃@�bN@��@̃@̃@��
@�o@��#@��@�Z@ǍP@�33@š�@��@��@�S�@�
=@���@§�@\@�n�@�M�@��@�7L@��@�9X@���@���@�  @��m@��
@���@��P@�;d@���@���@�X@�%@���@�1'@� �@�1@���@��
@���@�+@��H@�~�@�=q@�$�@��@��^@�&�@���@��;@�+@���@��!@�^5@�V@�5?@���@���@�hs@��`@��D@���@�33@���@�-@���@��@�;d@��@���@�ȴ@��!@��\@�V@�5?@�J@��7@�7L@��`@�z�@��@��P@�C�@���@�~�@�ff@�{@���@�?}@��/@�z�@�bN@�A�@��w@��@��H@���@��#@�x�@���@��u@��@�r�@�bN@�Z@�Q�@�I�@�Q�@�Q�@�I�@��@�t�@�K�@�;d@���@��y@��H@���@�n�@�{@��@��@���@���@�`B@�?}@�%@���@�Q�@��@�1@���@��@���@���@���@��P@��!@�$�@�@�x�@�&�@��/@���@�(�@���@��w@��@�dZ@�C�@�
=@��@��y@��@�v�@���@�@���@�x�@�V@��9@�z�@� �@�ƨ@�\)@���@��!@��+@�=q@�{@�@��T@�@�x�@���@��D@�Q�@��@���@��F@�\)@�o@��y@��H@��@���@�^5@�@���@�X@�G�@�V@��@���@��u@�A�@���@��F@�|�@�C�@�
=@���@�E�@�@���@��-@��h@�x�@�7L@��@�(�@�  @��@�t�@�K�@�o@�@��@��!@�v�@�ff@�V@�M�@�M�@�=q@�5?@�-@�O�@�z�@�Z@��@��@�ƨ@���@��@��y@���@���@��R@��!@�~�@��@��-@���@���@���@��h@��@�p�@�?}@���@�Ĝ@��u@��@��@��m@�@�ȴ@��R@��!@��!@��!@�=q@�$�@��@�J@��T@���@��@��/@��D@��@�  @�@�;@��@��@|�@~�y@~��@~V@}�@}@|�@z~�@y�^@y7L@y%@x�`@x��@x��@x�9@x�@x1'@w��@v�R@v�R@vE�@u@u/@tj@t9X@t(�@t1@s�F@sS�@r��@r�\@r=q@qX@p�9@pr�@p �@o��@oK�@n�@nE�@m��@m�h@l(�@kt�@kdZ@kdZ@k33@j��@i�@ix�@hĜ@h �@g��@gl�@f��@f�R@f��@fff@fE�@f5?@f@e�@e/@d�/@dj@c��@cdZ@b�@ax�@`��@`bN@`Q�@`Q�@`1'@_�;@_\)@^v�@^{@]�-@\�D@[�m@[t�@[o@Z��@Z�\@Zn�@Zn�@ZM�@Z-@Z�@Z�@Y�@Y��@YG�@X�9@X1'@Xb@W�@W�;@W�@Wl�@W\)@WK�@WK�@W;d@W
=@V�@U�@U�-@U��@U�h@UV@T��@TI�@T9X@T1@S�@St�@SS�@SS�@S@Rn�@Q��@Q�^@QG�@Q�@Q%@P�`@P��@P�9@P�u@Pr�@P1'@O�@O�w@O�@O;d@N��@Nv�@Nv�@Nff@Nff@N$�@M�T@M�@MV@L��@LI�@K�m@Kt�@K"�@J�H@J��@J~�@J~�@JM�@I�@I��@I%@Hr�@H  @G�@G�P@G\)@G\)@G;d@G;d@G�@F��@F�y@Fȴ@F��@F�+@FV@F5?@F$�@E�T@E/@D�/@D�D@DZ@D�@C��@CdZ@C33@C@B��@B�\@Bn�@Bn�@Bn�@BM�@B=q@BJ@A�@A��@A�7@Ax�@A7L@@�@>�@>��@>ff@=��@=p�@=/@<�/@<�D@<I�@<9X@<�@;�F@:��@:�@9&�@8r�@7�;@7�w@7��@7�P@7l�@6�+@4��@4�/@4��@4�j@4��@4�D@4Z@3�m@1�@1%@0 �@/��@/�@/\)@.�@.��@.�+@.v�@.V@.{@-@-��@-`B@-�@,�/@,j@,�@,�@,1@+�
@+��@*��@*^5@*=q@*-@*J@*J@)��@)��@)hs@)�@)%@(�`@(Ĝ@(�u@(Q�@(1'@(b@(  @'�@'��@'l�@';d@&�R@&$�@%�@%p�@%O�@$�@$�D@$j@$Z@$I�@$9X@#ƨ@#��@#33@"�\@"M�@"J@!�#@!��@!hs@!hs@!X@!X@!X@!x�@!�7@!�7@!�7@!�7@!�@ �`@ ��@ Q�@ b@|�@;d@��@�@�+@5?@�@��@`B@�@�@I�@�m@�
@�F@t�@S�@S�@S�@33@o@��@��@�\@~�@M�@M�@�@�#@��@hs@7L@��@ �@�;@��@�@��@+@ȴ@�R@�R@�R@��@��@��@��@@�-@`B@?}@V@��@��@1@��@t�@C�@33@"�@@�H@��@^5@��@��@x�@X@X@G�@G�@G�@G�@G�@&�@��@��@Ĝ@Ĝ@Ĝ@��@�9@�9@�u@bN@Q�@Q�@A�@1'@b@��@�@�P@K�@�R@�+@�+@v�@E�@@�T@�@�@�m@�m@�m@�
@ƨ@��@��@S�A�1A�%A�A���A���A���A�%A�%A�A���A���A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��mA��mA��`A��TA��HA��TA��`A��`A��;A��#A��#A��/A��/A��#A��
A���A�ȴA�ƨA���Aд9AЧ�AХ�AЛ�AЙ�AГuAЉ7A�|�A�n�A�hsA�bNA�\)A�ZA�VA�VA�VA�VA�S�A�O�A�M�A�K�A�K�A�K�A�I�A�E�A�G�A�M�A�VA�ffA�v�AЅAЇ+AЕ�AН�AН�AН�AЛ�AЛ�AП�AС�AП�AН�AЛ�AН�AС�AС�AП�AП�AП�AУ�AУ�AП�AЙ�AЙ�AЙ�AЙ�AЕ�AЏ\AЋDAЋDAЍPAЍPAЋDAЉ7AЋDAЍPAБhAЏ\AЍPAЍPAЍPAЍPAЉ7AЅAЇ+AЉ7AЉ7AЇ+AЅAЃAЅAЇ+AЇ+AЇ+AЃAЁAЃAЅAЇ+AЇ+AЅAЃAЇ+AЇ+AЅA�~�A�|�A�|�A�|�A�~�A�~�A�z�A�v�A�r�A�r�A�r�A�t�A�t�A�r�A�l�A�n�A�p�A�r�A�r�A�r�A�n�A�l�A�n�A�p�A�r�A�p�A�l�A�l�A�n�A�n�A�l�A�jA�jA�ffA�dZA�dZA�ffA�ffA�`BA�`BA�`BA�`BA�`BA�\)A�\)A�XA�VA�XA�XA�XA�ZA�XA�S�A�Q�A�M�A�M�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�Q�A�S�A�Q�A�M�A�M�A�Q�A�Q�A�S�A�S�A�Q�A�O�A�O�A�O�A�Q�A�S�A�VA�S�A�Q�A�O�A�Q�A�VA�VA�Q�A�K�A�E�A�=qA�;dA�9XA�1'A�(�A�$�A�"�A� �A��A�oA�%A���A��A��A��A��HA��#A���A�ĜA���Aϥ�AϓuA�z�A�p�A�l�A�hsA�bNA�\)A�XA�Q�A�I�A�G�A�E�A�A�A�1'A�1A�  A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��/A���A���A���A���A���A���A���A���A���AμjAδ9AζFAάAΣ�AΝ�AΏ\A�|�A�r�A�n�A�dZA�K�A�
=A���A�ȴA�ĜA�ĜAͺ^Aʹ9Aͥ�A̓ÁAͅA�x�A�E�A�oA��mA��#A���A�ĜA̺^A̲-A̙�ȂhȀ\Ȧ+ÁA�x�A�jA�S�A�G�A�E�A�C�A�C�A�7LA�JA�A���A���A���A���A���A���A��A��/A˗�AˋDAˋDAˉ7Aˇ+AˁA�jA�K�A�7LA�(�A�$�A��A�%A�A���A���A���A���A��A��;A��A��
A���A�ȴAʰ!Aʣ�Aʝ�Aʏ\AʅA�z�A�t�A�dZA�+A��A��A�VA�A�A�  A�A�A�A�  A���A���A��A��A��yA��yA��yA��A��A��yA��yA��`A��`A��HA��/A��#A��
A��A���A���A���A���A�ȴA�ȴA�A���AɾwAɶFAɮAɣ�Aə�A��A���AȓuA�x�A�z�A�t�A�l�A�Q�A�C�A�1'A�oA�%A�A�A�1A�1A�1A�1A�%A�%A�A�A�  A���A��mA��HA���Aǟ�A�S�A�5?A�{A�JA�1A�%A�A�A���A��TA���A�ȴA���AƮAƁA�M�A�O�A�;dA�+A� �A��A�VA��yAŮA�jA�=qAİ!A�Q�A�1'A��A�A��yA��/A��#A��
A��
A��
A���A��
A���A���A���A���A���A���A���Aú^Aá�AÏ\AÉ7A�~�A�l�A�jA�\)A�G�A�7LA�1'A��A�bA�1A�A���A���A��A��`A��#A���A���Aº^A´9A©�A£�A�AAA�dZA�oA���A�jA�\)A�r�A�jA�l�A�t�A��DA��`A�1A�-A�bNA��\A�XA�
=A�A��A��HA��/A���A�JA�bA�VA�1A���A��mA��-A�z�A�hsA�hsA�\)A�S�A�I�A�1'A�+A�&�A�(�A�&�A��A��A���A���A���A�A���A���A�v�A�O�A�E�A��A���A��hA�$�A��A���A�p�A�5?A�-A�oA�%A���A�dZA�"�A��FA���A�ZA��A�%A���A��RA�dZA� �A��A�1'A���A��A���A�bNA�`BA�?}A�bA�A��yA��A�  A���A�A�A���A���A���A��A��A�ƨA�ĜA���A�\)A�1A��;A�A���A��A�r�A�v�A�x�A�p�A�r�A�x�A�(�A���A��HA���A���A���A��A���A���A���A���A��DA�~�A�r�A�ffA�S�A�?}A�7LA�7LA��A��A���A�ƨA��FA��!A��A���A�jA�Q�A���A�A�Q�A���A��TA���A��uA�r�A�VA�33A��A�1A���A��A��#A���A�O�A��TA��A���A��RA���A��A�t�A�XA�G�A�A�A�9XA�"�A�VA��#A���A�ZA�?}A�A��A��A�~�A�A�A�JA��A���A�ƨA�ƨA���A���A���A�~�A�ffA�9XA�+A��A�%A���A��yA���A���A�ĜA��^A��^A��9A��!A���A�x�A��A��+A��A�z�A�n�A�/A���A��A�A�A�^5A�33A�%A���A�33A�%A��`A��;A���A�A�A��FA���A���A�p�A�5?A���A��#A���A���A���A�A��RA��!A���A���A�v�A�r�A�l�A�jA�jA�dZA�VA�O�A�K�A�=qA�A�A�5?A��TA��DA�n�A�^5A�M�A�I�A�A�A��A���A���A�x�A�VA�S�A�E�A�1'A�VA��7A�|�A��uA��\A�7LA��A�A���A��A��A��TA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                      A�  A�A���A��A��A��A��A��A��A��A��A��`A��;AоwAЇ+A�ZA�K�A�dZAЛ�AП�AП�AГuAЍPAЍPAЇ+AЅAЅA�~�A�t�A�p�A�p�A�n�A�ffA�^5A�XA�Q�A�O�A�Q�A�Q�A�S�A�G�A�&�A��AυA�?}A��A��A��A���AΕ�A��;A�&�A̅A��A˴9A��AʼjA�$�A��A��#AɅA�A�A�A�O�Aư!AŸRA���A���AÁA�oA¾wA���A�dZA���A��A�33A��RA�K�A�XA�Q�A�v�A��A��hA��+A���A��7A�A�ƨA��DA���A��A�ƨA�ZA��\A��A�v�A��A�jA���A���A�~�A�?}A�VA�`BA�XA���A��yA�\)A�JA��A��PA�\)A��A�5?A�O�A��TA�7LA��A���A��!A���A��#A���A�z�A�ĜA��A��!A�A�A���A�1'A}�Az�yAyƨAv�9As�-Arv�AoG�Am��Ak�Aj5?Aj�Aj$�Ai�Ai�AhJAfĜAc��Aa��A`��A`ZA_�FA_
=A^(�A\�\AY�AY%AX(�AWVAU�AU�PARr�AQC�APVAO�hAO�AL�yAK/AJI�AG
=AE��ACG�ABbNAA�^A?p�A?
=A>1'A;l�A9��A8�DA7XA6�DA6bNA6�A5hsA3��A3|�A3C�A3A2~�A1�
A1K�A0�HA/7LA.z�A.A-�hA,9XA+p�A)�^A(��A'hsA&jA%33A$�+A#"�A!��A�+A{A��A�TA�A�`A�\A�A�yA�A�AAl�A�9A/AffAdZAQ�A|�A9XA`BA&�A�yAn�A9XA�TA�!A	ƨA�`A�9A�9A�uAƨA;dA+AoA�9A-A1A33A �@��!@�&�@���@���@�A�@��#@��+@���@���@�9X@�l�@��@�\)@�;d@�ff@��@���@� �@�^@�&�@�%@���@�1'@�33@���@��@��
@޸R@��@�I�@�l�@�ȴ@ڗ�@ڇ+@�V@�J@ج@�l�@�@�V@�z�@Ӿw@�V@���@�{@́@�x�@�hs@�O�@�G�@�G�@�O�@�&�@�%@�Ĝ@̃@�bN@��@̃@̃@��
@�o@��#@��@�Z@ǍP@�33@š�@��@��@�S�@�
=@���@§�@\@�n�@�M�@��@�7L@��@�9X@���@���@�  @��m@��
@���@��P@�;d@���@���@�X@�%@���@�1'@� �@�1@���@��
@���@�+@��H@�~�@�=q@�$�@��@��^@�&�@���@��;@�+@���@��!@�^5@�V@�5?@���@���@�hs@��`@��D@���@�33@���@�-@���@��@�;d@��@���@�ȴ@��!@��\@�V@�5?@�J@��7@�7L@��`@�z�@��@��P@�C�@���@�~�@�ff@�{@���@�?}@��/@�z�@�bN@�A�@��w@��@��H@���@��#@�x�@���@��u@��@�r�@�bN@�Z@�Q�@�I�@�Q�@�Q�@�I�@��@�t�@�K�@�;d@���@��y@��H@���@�n�@�{@��@��@���@���@�`B@�?}@�%@���@�Q�@��@�1@���@��@���@���@���@��P@��!@�$�@�@�x�@�&�@��/@���@�(�@���@��w@��@�dZ@�C�@�
=@��@��y@��@�v�@���@�@���@�x�@�V@��9@�z�@� �@�ƨ@�\)@���@��!@��+@�=q@�{@�@��T@�@�x�@���@��D@�Q�@��@���@��F@�\)@�o@��y@��H@��@���@�^5@�@���@�X@�G�@�V@��@���@��u@�A�@���@��F@�|�@�C�@�
=@���@�E�@�@���@��-@��h@�x�@�7L@��@�(�@�  @��@�t�@�K�@�o@�@��@��!@�v�@�ff@�V@�M�@�M�@�=q@�5?@�-@�O�@�z�@�Z@��@��@�ƨ@���@��@��y@���@���@��R@��!@�~�@��@��-@���@���@���@��h@��@�p�@�?}@���@�Ĝ@��u@��@��@��m@�@�ȴ@��R@��!@��!@��!@�=q@�$�@��@�J@��T@���@��@��/@��D@��@�  @�@�;@��@��@|�@~�y@~��@~V@}�@}@|�@z~�@y�^@y7L@y%@x�`@x��@x��@x�9@x�@x1'@w��@v�R@v�R@vE�@u@u/@tj@t9X@t(�@t1@s�F@sS�@r��@r�\@r=q@qX@p�9@pr�@p �@o��@oK�@n�@nE�@m��@m�h@l(�@kt�@kdZ@kdZ@k33@j��@i�@ix�@hĜ@h �@g��@gl�@f��@f�R@f��@fff@fE�@f5?@f@e�@e/@d�/@dj@c��@cdZ@b�@ax�@`��@`bN@`Q�@`Q�@`1'@_�;@_\)@^v�@^{@]�-@\�D@[�m@[t�@[o@Z��@Z�\@Zn�@Zn�@ZM�@Z-@Z�@Z�@Y�@Y��@YG�@X�9@X1'@Xb@W�@W�;@W�@Wl�@W\)@WK�@WK�@W;d@W
=@V�@U�@U�-@U��@U�h@UV@T��@TI�@T9X@T1@S�@St�@SS�@SS�@S@Rn�@Q��@Q�^@QG�@Q�@Q%@P�`@P��@P�9@P�u@Pr�@P1'@O�@O�w@O�@O;d@N��@Nv�@Nv�@Nff@Nff@N$�@M�T@M�@MV@L��@LI�@K�m@Kt�@K"�@J�H@J��@J~�@J~�@JM�@I�@I��@I%@Hr�@H  @G�@G�P@G\)@G\)@G;d@G;d@G�@F��@F�y@Fȴ@F��@F�+@FV@F5?@F$�@E�T@E/@D�/@D�D@DZ@D�@C��@CdZ@C33@C@B��@B�\@Bn�@Bn�@Bn�@BM�@B=q@BJ@A�@A��@A�7@Ax�@A7L@@�@>�@>��@>ff@=��@=p�@=/@<�/@<�D@<I�@<9X@<�@;�F@:��@:�@9&�@8r�@7�;@7�w@7��@7�P@7l�@6�+@4��@4�/@4��@4�j@4��@4�D@4Z@3�m@1�@1%@0 �@/��@/�@/\)@.�@.��@.�+@.v�@.V@.{@-@-��@-`B@-�@,�/@,j@,�@,�@,1@+�
@+��@*��@*^5@*=q@*-@*J@*J@)��@)��@)hs@)�@)%@(�`@(Ĝ@(�u@(Q�@(1'@(b@(  @'�@'��@'l�@';d@&�R@&$�@%�@%p�@%O�@$�@$�D@$j@$Z@$I�@$9X@#ƨ@#��@#33@"�\@"M�@"J@!�#@!��@!hs@!hs@!X@!X@!X@!x�@!�7@!�7@!�7@!�7@!�@ �`@ ��@ Q�@ b@|�@;d@��@�@�+@5?@�@��@`B@�@�@I�@�m@�
@�F@t�@S�@S�@S�@33@o@��@��@�\@~�@M�@M�@�@�#@��@hs@7L@��@ �@�;@��@�@��@+@ȴ@�R@�R@�R@��@��@��@��@@�-@`B@?}@V@��@��@1@��@t�@C�@33@"�@@�H@��@^5@��@��@x�@X@X@G�@G�@G�@G�@G�@&�@��@��@Ĝ@Ĝ@Ĝ@��@�9@�9@�u@bN@Q�@Q�@A�@1'@b@��@�@�P@K�@�R@�+@�+@v�@E�@@�T@�@�@�m@�m@�m@�
@ƨ@��@��G�O�A�1A�%A�A���A���A���A�%A�%A�A���A���A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��mA��mA��`A��TA��HA��TA��`A��`A��;A��#A��#A��/A��/A��#A��
A���A�ȴA�ƨA���Aд9AЧ�AХ�AЛ�AЙ�AГuAЉ7A�|�A�n�A�hsA�bNA�\)A�ZA�VA�VA�VA�VA�S�A�O�A�M�A�K�A�K�A�K�A�I�A�E�A�G�A�M�A�VA�ffA�v�AЅAЇ+AЕ�AН�AН�AН�AЛ�AЛ�AП�AС�AП�AН�AЛ�AН�AС�AС�AП�AП�AП�AУ�AУ�AП�AЙ�AЙ�AЙ�AЙ�AЕ�AЏ\AЋDAЋDAЍPAЍPAЋDAЉ7AЋDAЍPAБhAЏ\AЍPAЍPAЍPAЍPAЉ7AЅAЇ+AЉ7AЉ7AЇ+AЅAЃAЅAЇ+AЇ+AЇ+AЃAЁAЃAЅAЇ+AЇ+AЅAЃAЇ+AЇ+AЅA�~�A�|�A�|�A�|�A�~�A�~�A�z�A�v�A�r�A�r�A�r�A�t�A�t�A�r�A�l�A�n�A�p�A�r�A�r�A�r�A�n�A�l�A�n�A�p�A�r�A�p�A�l�A�l�A�n�A�n�A�l�A�jA�jA�ffA�dZA�dZA�ffA�ffA�`BA�`BA�`BA�`BA�`BA�\)A�\)A�XA�VA�XA�XA�XA�ZA�XA�S�A�Q�A�M�A�M�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�Q�A�S�A�Q�A�M�A�M�A�Q�A�Q�A�S�A�S�A�Q�A�O�A�O�A�O�A�Q�A�S�A�VA�S�A�Q�A�O�A�Q�A�VA�VA�Q�A�K�A�E�A�=qA�;dA�9XA�1'A�(�A�$�A�"�A� �A��A�oA�%A���A��A��A��A��HA��#A���A�ĜA���Aϥ�AϓuA�z�A�p�A�l�A�hsA�bNA�\)A�XA�Q�A�I�A�G�A�E�A�A�A�1'A�1A�  A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��/A���A���A���A���A���A���A���A���A���AμjAδ9AζFAάAΣ�AΝ�AΏ\A�|�A�r�A�n�A�dZA�K�A�
=A���A�ȴA�ĜA�ĜAͺ^Aʹ9Aͥ�A̓ÁAͅA�x�A�E�A�oA��mA��#A���A�ĜA̺^A̲-A̙�ȂhȀ\Ȧ+ÁA�x�A�jA�S�A�G�A�E�A�C�A�C�A�7LA�JA�A���A���A���A���A���A���A��A��/A˗�AˋDAˋDAˉ7Aˇ+AˁA�jA�K�A�7LA�(�A�$�A��A�%A�A���A���A���A���A��A��;A��A��
A���A�ȴAʰ!Aʣ�Aʝ�Aʏ\AʅA�z�A�t�A�dZA�+A��A��A�VA�A�A�  A�A�A�A�  A���A���A��A��A��yA��yA��yA��A��A��yA��yA��`A��`A��HA��/A��#A��
A��A���A���A���A���A�ȴA�ȴA�A���AɾwAɶFAɮAɣ�Aə�A��A���AȓuA�x�A�z�A�t�A�l�A�Q�A�C�A�1'A�oA�%A�A�A�1A�1A�1A�1A�%A�%A�A�A�  A���A��mA��HA���Aǟ�A�S�A�5?A�{A�JA�1A�%A�A�A���A��TA���A�ȴA���AƮAƁA�M�A�O�A�;dA�+A� �A��A�VA��yAŮA�jA�=qAİ!A�Q�A�1'A��A�A��yA��/A��#A��
A��
A��
A���A��
A���A���A���A���A���A���A���Aú^Aá�AÏ\AÉ7A�~�A�l�A�jA�\)A�G�A�7LA�1'A��A�bA�1A�A���A���A��A��`A��#A���A���Aº^A´9A©�A£�A�AAA�dZA�oA���A�jA�\)A�r�A�jA�l�A�t�A��DA��`A�1A�-A�bNA��\A�XA�
=A�A��A��HA��/A���A�JA�bA�VA�1A���A��mA��-A�z�A�hsA�hsA�\)A�S�A�I�A�1'A�+A�&�A�(�A�&�A��A��A���A���A���A�A���A���A�v�A�O�A�E�A��A���A��hA�$�A��A���A�p�A�5?A�-A�oA�%A���A�dZA�"�A��FA���A�ZA��A�%A���A��RA�dZA� �A��A�1'A���A��A���A�bNA�`BA�?}A�bA�A��yA��A�  A���A�A�A���A���A���A��A��A�ƨA�ĜA���A�\)A�1A��;A�A���A��A�r�A�v�A�x�A�p�A�r�A�x�A�(�A���A��HA���A���A���A��A���A���A���A���A��DA�~�A�r�A�ffA�S�A�?}A�7LA�7LA��A��A���A�ƨA��FA��!A��A���A�jA�Q�A���A�A�Q�A���A��TA���A��uA�r�A�VA�33A��A�1A���A��A��#A���A�O�A��TA��A���A��RA���A��A�t�A�XA�G�A�A�A�9XA�"�A�VA��#A���A�ZA�?}A�A��A��A�~�A�A�A�JA��A���A�ƨA�ƨA���A���A���A�~�A�ffA�9XA�+A��A�%A���A��yA���A���A�ĜA��^A��^A��9A��!A���A�x�A��A��+A��A�z�A�n�A�/A���A��A�A�A�^5A�33A�%A���A�33A�%A��`A��;A���A�A�A��FA���A���A�p�A�5?A���A��#A���A���A���A�A��RA��!A���A���A�v�A�r�A�l�A�jA�jA�dZA�VA�O�A�K�A�=qA�A�A�5?A��TA��DA�n�A�^5A�M�A�I�A�A�A��A���A���A�x�A�VA�S�A�E�A�1'A�VA��7A�|�A��uA��\A�7LA��A�A���A��A��A��TA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	�UB	��B	�!B	��B	��B	�!B	��B	�UB	�9B	�$B	��B	�&B	ߤB	�B	��B
�B
"�B
,B
7�B
FB
OB
VmB
XB
Y�B
Z�B
Z�B
ZB
ZQB
Y�B
Y�B
Y�B
Y�B
ZQB
ZQB
ZQB
Z�B
Z�B
\�B
b�B
o B
�{B
�B
��B
�-B
�9B
�B
�jB
�fB
�GB
��B
��BB�B)�BU�BsBu�B��B�zB�XB�B��B�B@�B@BB�B?�B5�BOBBDB�BB(B�B�ZB�B��B�BB�B"�B'RB,qB5�B'B'B�B	B�B�B1BSB�VB�B��B�B��B�6B��B�B�HB�qB�qB�FB��B��B��B�B�B~�BtTBp�BjKB\�BR�BE�B0�B�B
��B
�`B
�^B
�[B
��B
�kB
��B
�PB
|�B
aB
U�B
H�B
6�B
(XB
�B
B
AB	��B	��B	�ZB	�TB	�B	�>B	�B	уB	�gB	�<B	��B	�?B	��B	��B	�B	��B	��B	��B	�;B	y�B	tB	m�B	^B	Y�B	R�B	N<B	L�B	;�B	=qB	-CB	*eB	IB	�B	FB	�B	�B	1B�xB��B�]B��B�B��B�B�|B�WB�B�
B�?BԕBҽB��B��B�jB�9B�[B��B�BB��B�B��B��B��B�6B��B��B��B��B��B��B�eB��B�*B��B�jB�yB�B�cB�iB�vB�;B�B��B�MB��B�"B	YB	MB	SB	+B	�B	xB	PB	4B	�B	�B	B	:B	B	uB	�B	�B	�B	�B	�B	�B	B	�B	�B	SB	�B	�B	�B	_B	 4B��B��B��B�GB��B��B��B��B��B��B��B�B�2B��B��B��B�B�B	+B	
rB	JB	�B	@B	�B	qB	xB	B	B	�B	"4B	$tB	$@B	$@B	#�B	"�B	!-B	 \B	$�B	(�B	*�B	-B	-CB	.B	/OB	1�B	5tB	7�B	9�B	:�B	<B	C-B	M�B	U�B	V�B	W�B	YB	YB	bNB	c�B	c�B	h�B	m]B	r�B	w2B	xB	y�B	zxB	z�B	{B	|�B	�B	�{B	�B	��B	�B	��B	�_B	��B	�1B	��B	�7B	�lB	��B	��B	�bB	�.B	�:B	��B	��B	�IB	��B	�OB	��B	��B	�$B	��B	��B	��B	�eB	��B	�6B	�=B	��B	��B	�B	�B	�B	��B	�B	��B	�LB	�B	��B	�6B	�B	��B	�'B	ÖB	�B	�<B	�}B	��B	��B	��B	�B	уB	� B	� B	ҽB	��B	��B	�mB	�B	خB	��B	ݘB	�BB	�BB	�BB	�|B	��B	�B	��B	�B	�B	�2B	��B	�B	��B	�KB	�"B	��B	�)B	�]B	�B	�B	��B	��B	��B	�cB	��B	� B	�5B	��B	�B	�vB	��B	�B	�|B	�|B	�|B	�MB	�B	�B	�MB	�B	�B	�`B	��B	��B	��B	��B	�xB	�xB	��B	�xB	�JB	��B	�B	�B	��B	��B	��B
  B
 4B
;B
B
�B
AB
�B
GB
{B
�B
�B
�B
MB
B
�B
�B
�B
%B
�B
�B
�B
+B
�B
�B
	7B
�B
	�B
	�B

rB

�B

�B
DB
�B
�B
\B
�B
�B
.B
�B
B
B
B
uB
uB
@B
uB
�B
MB
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
eB
eB
eB
7B
�B
�B
CB
�B
B
~B
�B
�B
�B
�B
�B
!B
!B
!B
!B
!B
�B
OB
!-B
�B
 'B
�B
�B
 'B
 'B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
#�B
#nB
#:B
#nB
#nB
#nB
#nB
#�B
$B
$�B
$�B
$�B
%�B
%FB
%�B
'RB
'B
'B
'B
'B
&�B
($B
($B
'�B
'�B
(XB
(�B
)�B
)�B
*�B
+B
+6B
+B
+6B
+6B
+6B
+�B
,=B
,qB
,�B
,qB
,=B
-CB
-wB
-�B
-�B
.B
.B
.B
.B
.B
.IB
.�B
/�B
/�B
/�B
0UB
0UB
0�B
1�B
1[B
1[B
1�B
1�B
2�B
3�B
49B
4nB
5tB
5?B
5tB
5tB
5�B
5�B
5�B
6B
5�B
5�B
6�B
7B
7LB
7B
7�B
8RB
8�B
8RB
9XB
9�B
9�B
:^B
:�B
:�B
;0B
:�B
;0B
:�B
;0B
;�B
;�B
<B
<jB
<�B
<�B
=�B
=qB
=�B
>BB
>BB
>B
>B
>BB
>�B
?�B
?�B
@B
A�B
A�B
A�B
B'B
B'B
B[B
B�B
B[B
B�B
B�B
B�B
B'B
B�B
B[B
B�B
C�B
D�B
D�B
D�B
D�B
EB
EB
E9B
E9B
E9B
EmB
EmB
EmB
F�B
F�B
FtB
FtB
GzB
GB
GzB
GEB
GzB
G�B
HB
HB
HB
H�B
IB
IRB
I�B
I�B
J#B
I�B
J#B
J#B
JXB
J�B
J�B
J�B
K)B
K^B
K)B
K�B
L�B
LdB
L0B
L0B
L0B
LdB
L�B
M6B
MjB
M�B
N<B
N<B
N�B
OBB
OB
OvB
OvB
OBB
O�B
O�B
PB
P}B
P�B
QNB
QB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
Q�B
R B
RTB
R B
R B
R�B
S&B
S&B
S[B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U2B
U2B
UgB
U2B
U2B
U2B
VB
WsB
V�B
W?B
W�B
W�B
XB
XyB
X�B
X�B
X�B
X�B
YB
Y�B
ZB
[WB
[�B
\)B
\)B
\)B
[�B
[�B
]�B
^B
^B
^5B
^B
^B
^B
]�B
^jB
`�B
a|B
bNB
bNB
bNB
b�B
cTB
c B
c B
cTB
cTB
c�B
d&B
d&B
dZB
d�B
d�B
e,B
e`B
e,B
e,B
e`B
e`B
gB
f�B
f�B
gB
gB
gB
gB
gmB
g�B
h
B
g�B
h
B
h>B
hsB
h�B
h�B
iB
h�B
iB
iB
iyB
iyB
jB
j�B
k�B
kQB
kB
k�B
l"B
l"B
lWB
lWB
lWB
l�B
lWB
m)B
m]B
m�B
m�B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
ncB
ncB
n�B
ncB
n/B
o B
n�B
o5B
o5B
oiB
o�B
o�B
o�B
pB
poB
p�B
qB
qAB
qAB
qvB
q�B
rB
rGB
r|B
r�B
sB
sB
sB
sB
s�B
s�B
tB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
uZB
u�B
uZB
u�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
x8B
x8B
x8B
x8B
x8B
x8B
xB
yrB
y>B
y�B
zB
y�B
zDB
zxB
{B
{�B
{�B
|B
{�B
|B
|B
|PB
|PB
|�B
|�B
|�B
}"B
|�B
|�B
}"B
|�B
|�B
|�B
|�B
}"B
}"B
}VB
}VB
}"B
}"B
}"B
}VB
}VB
}VB
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~]B
~]B
~�B
~�B
�B
.B
.B
cB
�B
cB
��B
��B
�oB
�oB
�oB
�oB
�oB
��B
��B
��B	�'B	��B	�'B	�zB	��B	��B	�CB	�'B	��B	��B	�B	�!B	��B	��B	��B	�B	�!B	��B	�'B	�!B	��B	��B	��B	��B	�IB	��B	��B	��B	�[B	�UB	�OB	��B	��B	��B	��B	�OB	�B	�OB	��B	��B	�'B	�B	��B	�zB	��B	��B	�B	��B	�^B	�dB	�*B	�*B	�^B	�6B	�HB	��B	��B	ĜB	�B	˒B	��B	ΥB	�<B	�BB	� B	՛B	��B	یB	�/B	��B	��B	�B	�BB	��B	�B	�B	�B	�,B	�`B	�B	�fB	�B	�DB	�B	�"B	��B	�AB	��B
B
�B

�B
\B
uB
{B
�B
=B
IB
�B
�B
#:B
$tB
$@B
#�B
$B
&�B
'RB
(�B
*�B
,�B
/B
2-B
49B
4nB
5?B
6�B
9XB
:�B
<jB
@B
E�B
F�B
IB
HKB
G�B
GB
G�B
K)B
L�B
P�B
P�B
TaB
VmB
V�B
U�B
U2B
VB
W?B
W�B
W
B
V�B
V�B
V�B
XyB
Y�B
Y�B
YB
X�B
XyB
ZQB
Z�B
Y�B
YB
Z�B
\)B
[�B
Z�B
Z�B
YB
Y�B
Z�B
[#B
[�B
Z�B
Y�B
X�B
YKB
ZB
[#B
Z�B
ZQB
YB
X�B
YB
Z�B
[WB
Z�B
ZQB
YB
Y�B
Z�B
Z�B
ZQB
YKB
YB
YB
YKB
Z�B
Z�B
ZQB
X�B
X�B
Z�B
[#B
Z�B
Y�B
YB
Y�B
YB
Z�B
Z�B
ZB
ZQB
YB
X�B
Y�B
ZB
Z�B
[WB
[WB
YB
YKB
Y�B
Z�B
[#B
[�B
Y�B
X�B
Y�B
[WB
[WB
ZQB
Y�B
YKB
YKB
ZB
Z�B
[�B
[�B
Z�B
Y�B
YKB
ZB
Z�B
[�B
[WB
ZQB
ZQB
Z�B
\�B
]�B
^�B
^�B
^�B
aHB
c�B
cTB
c B
cTB
d�B
ffB
i�B
m�B
n/B
m]B
ncB
q�B
sB
uZB
w�B
w�B
}"B
�iB
��B
��B
�+B
�lB
�B
�JB
��B
��B
��B
� B
��B
��B
�_B
�zB
��B
��B
��B
��B
��B
�}B
��B
��B
�!B
�!B
��B
��B
��B
�3B
�-B
�[B
�[B
�aB
�3B
��B
��B
��B
�[B
��B
��B
�B
�$B
�B
�BB
��B
��B
��B
�B
�B
�}B
�aB
��B
��B
�gB
�EB
�)B
��B
�}B
��B
�2B
��B
�9B
یB
�B
�B
�sB
�
B
�B
�B
�B
�WB
�B
�5B
�iB
��B
�rB
�B
�B
�B
�B
�;B
�cB
�B
�B
��B
�ZB
�`B
��B
��B
�>B
��B
��B
��B
�>B
��B
�rB
��B
��B
��B
�(B
��B
��B
�VB
��B
��BBxB�BB�B�B�BJBhB�B�B�BxB �B �B"�B"�B#B#:B%B&B%�B%zB%�B&�B)�B,qB,�B0!B1'B1�B2aB5BE�BH�BLdB[�BjBqvBq�BqvBqvBp�BqABqvBr|BsMBsBu%Bt�Bs�Bs�BsMBsMBsMBt�BtTBuZBv+Bv`Bu�Bu�Bv�BwfBv�Bw2Bx8Bx�By�By�By>BzxB{�B~�B�AB��B��B�\B��B��B��B�4B�tB��B�tB�_B�-B�?B�zBBǮBǮB�zBǮBȀB�RB�XB��B��B��B�KB�|B�B�B�dB�pB�pB�B�BB�B��B�B�B�2B�fB�sB�ABB�VB��B�]B��B��B��B��BBBB�BEB=�B9�B?}B@�BF?BC�BA�B?�B@OB@�B?}B?�B?�B@�B@�B@�B?�B?B?}BA�BB'BCaBA�BC�BF�BB�BB'BEBDgBAUBE9B@B?HB=<B=�B:�B=�B8�B:^B8B5?B7�B4�B2-B3hB0�B0�B+B'�B6FB.IBDB�B�B	�B:B=B1B/B��BBB�B�B��B�BBDB
�B	7B�B�B+B�BeB_B�B!-BBB�BoB�B�B�B�B.B�B�B:B�B�B�BB�B�B
rB�B
	B�BDB4B�BB;B�BB�	B�5B�B��B�(B�B�MB��B�DB�B� BیB��B��B�B��B�B��B��B�B�B�B�iB��B�B��B��B�B	lBJB�B�B�B�B�BVB:BBJB{B�BYB�B�B7B�B~B 'B#:B%zB#:B%B=B�BqBxBxB�B'B"�B#B"�B$�B($B)_B)�B+�B-B/OB-B,B/�B(�B,=B&�B'�B&�B&�B&�B3hB*�B;dB}�B($B 'B�B 'B0�B*�B)�B(XB)_B&B$�B#B%FB+kB*�B,qB �B"�B�B"hBqBCB!bBCB�BeBeB=B�B �BuBB�B B�B�B�B{BoBbBB
�BB�B	BB
�B�BfBxB	�BYB
rBB�B�B{BuB �B �BBBOB�JB�lB�lB�8B�.B�B�B�B;B�B�;B�8B�
B�sB�`B�B�NB��B�jB�|B�pB��B��B�B�,B��B�BרB�EB��B�EB�2B�B�,B�BB�B��B�jB�B�jB�BB��B̘B�vB�EB͟B��B��B�gB��B�aB��B� B�tB�B� B��B��B�B�B��B��B�/B��B��B��B�B�BB�jB�qB��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202007072154312020070721543120200707215431202007072154312020070721543120200707215431SI  SI  ARFMARFM                                                                                                                                                2020051321061120200513210611IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020052400004220200524000042QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020052400004220200524000042QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020070612390520200706123905IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20200707214331              CF      PSAL                            ?�  G�O�D�k�G�O�?�  G�O�Bad PSAL Drift                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20200707214828    CF                  PSAL            G�O�?��G�O�CH�G�O�?�                  Bad PSAL Drift  SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                