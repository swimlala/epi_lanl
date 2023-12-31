CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-10-19T04:19:44Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20181019041944  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_028                 7316_008644_028                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؊�M�@؊�M�11  @؊"h	�@؊"h	�@+h�/D�*@+h�/D�*�dɯ��{�dɯ��{11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@=p�@}p�@�  @\@�G�@��RA\)A   A,(�A@  A`  A�  A�  A�Q�A���A�Q�A�Q�A�Q�A�  B Q�BQ�BQ�B(�B�
B(  B/�
B7�B?�
BH  BP(�BX(�B`(�Bh  Bo�
Bx  B�{B�{B�z�B�  B��B�  B�{B�{B��B�  B�{B�  B��B��B�  B�{B�  B�  B�  B�  B�{B�  B��B�  B�  B��B�{B�{B�  B��B�  B�  C   C
=C
=C��C  C

=C
=C
=C  C  C  C  C  C  C  C  C��C"  C$
=C&{C(  C)��C+��C-��C0  C2  C4
=C6  C7��C9�C<  C>{C@
=CA��CD  CF
=CH{CJ{CL
=CM��CO��CQ��CT  CV
=CW��CZ  C\  C]�C_�Cb  Cd  Cf  Ch
=Cj{Cl  Cn  Cp  Cr  Ct
=Cv
=Cx  Cz
=C|
=C~  C�  C���C���C���C�  C�  C�  C���C���C�  C�  C�C�
=C�C�  C�  C�C�C���C���C�  C���C���C�  C�C�  C���C�  C�  C�  C�  C�C�C�C���C�  C���C���C���C�  C�C�C���C�  C�  C�  C���C�  C���C���C���C���C�  C���C���C�  C�  C�C�  C���C���C���C�  C�  C�  C�C�C�C�C�C�C�  C�  C�C�C�C���C���C���C���C�  C�  C�  C�  C�C�C�C�  C���C�  C�C�  C�  C�C�C�C�  C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C���C���C�  C�  C���D � D�D��D  D��D�qD� D  D}qD�D��D  D� D  D� D�qD}qD	  D	� D	�qD
� D  D� D�D� D  D� D  D� D  D� D�D��D�qD� D  D}qD  D� D�qD}qD��D� D  D� D  D� D  D��D�D� D  D� D  D��D�D� D  D��D�D��D  D}qD   D � D!  D!��D"  D"}qD"�qD#� D$D$��D%  D%� D&  D&}qD&�qD'}qD(  D(��D)�D)}qD)�qD*� D+  D+��D+�qD,� D-D-��D.  D.��D/  D/xRD/�qD0��D1�D1��D2D2��D3  D3}qD3��D4� D4�qD5}qD6�D6�D7�D7� D7�qD8� D9�D9��D:  D:��D;�D;� D;��D<}qD=  D=�D>D>��D?�D?��D@  D@��DADA}qDA�qDB��DC�DC��DD�DD� DD�qDE}qDE�qDF� DG  DG� DH�DH� DI  DI}qDI��DJz�DJ��DKz�DK�qDL� DL�qDMz�DM�qDN� DN�qDO��DPDP��DQ  DQ}qDQ�qDRz�DS  DS��DT  DT� DU  DU� DV�DV��DW�DW�DX  DXz�DX��DYz�DZ�DZ��DZ��D[}qD[�qD\� D]  D]��D^�D^� D_  D_� D`  D`}qD`��Da}qDb  Db� Dc  Dc��Dd�Dd��Dd�qDe� Df�Df��Df��Dgz�Dh�Dh��Di�Di��Di��Djz�Dk  Dk�Dl�Dl� Dm  Dm� Dm�qDn}qDo  Do� Dp�Dp}qDp�qDq� Dr�Dr� Ds  Ds� Dt  Dt��Du  Du}qDv  Dv��Dw�Dw}qDw�qDx� Dy  Dy��Dz  Dz}qD{  D{� D|�D|� D|�qD}� D~D~��D�D� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�~�D���D���D�@ D�� D���D���D�@ D�~�D��qD���D�@ D�~�D���D���D�>�D�� D��HD�HD�@ D�~�D���D���D�=qD�� D�� D���D�@ D�� D���D�  D�AHD�� D�� D�HD�AHD��HD��HD�HD�@ D�� D��HD�  D�=qD�� D��HD�  D�>�D�~�D��HD�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�AHD��HD�� D���D�@ D��HD�� D���D�@ D�~�D���D���D�=qD�� D�� D�  D�AHD�� D�� D���D�@ D��HD�� D�HD�AHD�� D���D���D�>�D�� D�D��D�AHD��HD�� D���D�>�D�~�D���D���D�>�D�� D�D��D�AHD�� D�� D�  D�=qD�~�D���D���D�>�D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�� D���D���D�@ D�� D��HD�  D�>�D�� D��HD�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�B�D��HD�� D�  D�@ D�� D�D��D�@ D�~�D��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�HD�AHD�~�D�� D�  D�AHD��HD���D���D�@ D�� D�� D�HD�AHD�� D��qD���D�>�D�� D��HD���D�>�D��HD�D�  D�>�D�� D��HD���D�AHD���D��HD�  D�AHD�� D���D�HD�@ D��HD�D��D�AHD�� D���D���D�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�AHD��HD���D�  D�AHD��HD��HD�  D�AHD�� D��qD��qD�>�D�~�D�� D�HD�>�D�~�D�� D�  D�@ D D�� D���D�@ DÁHD�� D�  D�@ D�~�D�� D�  D�>�Dŀ D��HD���D�>�DƁHD�� D�  D�@ Dǀ D�� D�  D�@ DȁHD��HD�  D�>�DɁHDɾ�D�  D�B�Dʀ Dʾ�D�  D�>�Dˀ D��HD�HD�AHD̀ D��HD�HD�AHD́HD;�D�  D�>�D�~�D��HD�  D�>�D�~�D�� D���D�>�DЀ D�� D���D�AHDсHD��HD�  D�@ D�~�D�� D�  D�AHDӁHD�� D�HD�@ DԁHD�� D�  D�>�D�~�D�� D�HD�B�Dւ�D��HD���D�>�D׀ D��HD�  D�>�D�~�DؽqD���D�@ D�~�D�� D�HD�@ DځHD��HD�  D�>�Dۀ D��HD�  D�@ D܁HD��HD�  D�@ D�~�Dݾ�D�  D�AHDހ D�� D�  D�@ D�~�D߾�D�  D�>�D�~�D�� D�  D�AHD� DᾸD�  D�@ D�HD�D�  D�>�D� D��HD�  D�=qD�~�D侸D�  D�AHD� D�� D�  D�@ D悏D�D�HD�@ D� D羸D�  D�@ D� D�� D���D�>�D� D�� D�HD�AHD� D꾸D�  D�@ D� D뾸D�HD�>�D� D쾸D���D�@ D� D��HD�HD�@ D� DD�  D�>�D�HD��HD�  D�@ D��HD�D�HD�@ D� D��HD���D�AHD�HD��HD���D�@ D�D��HD�  D�@ D�~�D�� D��D�B�D���D�D�  D�=qD�~�D���D�  D�@ D��HD�D�  D�>�D�~�D�� D�  D�AHD��HD��HD�HD�>�D��\>�\)>��
?.{?�  ?���?��@�\@��@+�@E�@Y��@n{@�  @���@�Q�@��\@���@��H@��@�\)@�p�@�@��A   AA
=qA  A�A��A"�\A(Q�A/\)A5�A:�HA@��AHQ�AN{AS33AY��A`��AfffAk�Aq�Ax��A~{A�=qA�p�A���A�33A�{A�G�A���A��A�=qA�p�A�Q�A��
A�ffA���A�(�A�\)A�=qA��A�  A��A�ffA�G�A��
A�
=Aʏ\A�A�  A�33A�ffA��A�z�A�\)A�=qA�A��A�A�ffA�A���A��A��\A��B Q�B�B33B��B=qB�
B	�B
ffB�
BG�B�\B�B��B�B�HB�B��B�B�HB�Bz�B��B�\B\)B(�B�B=qB
=B�
B ��B!B"�RB#\)B$Q�B%p�B&=qB'
=B'�
B(��B)�B*�\B+\)B,z�B-G�B-�B.�RB/�B0��B1p�B2=qB3
=B4  B4��B5G�B6=qB733B8  B8��B9p�B:�\B;\)B<(�B<��B=B>�RB?�B@��BAG�BB=qBC\)BDQ�BEG�BE�BF�HBH  BI�BJ{BJ�HBK�BL��BM�BN�HBP  BP��BQ��BR�RBS�
BT��BU�BV�HBW�BX��BY�B[
=B\  B\��B]B_
=B`Q�BaG�Bb{Bc33BdQ�Bep�Bf�\Bg�BhQ�Bi��Bj�\Bk�
Bl��BmBn�RBo�Bp��Br{Bs
=Bt  Bt��Bv{Bw33Bxz�ByBz�HB{�
B|��B~=qB�B�ffB���B��B�{B���B�33B��
B�ffB���B��B��B�z�B��HB�\)B��
B�=qB��\B���B���B�33B��B�B��
B�{B�=qB�ffB���B���B��B�\)B���B�B��B�{B�Q�B��\B���B���B�33B�p�B��B�B�  B�Q�B�z�B��RB��HB�
=B�G�B��B�B�{B�Q�B��\B���B���B�33B�p�B�B�  B�Q�B��\B���B�
=B�\)B��B�  B�=qB���B���B��B�\)B��B�  B�Q�B���B�
=B�\)B��B�{B�Q�B��\B��HB�33B���B�  B�ffB���B��B�\)B��B��B�Q�B��RB�
=B�p�B��
B�(�B��\B���B��B�p�B�B�(�B��\B���B�\)B�B�{B�ffB��RB�
=B�p�B��
B�Q�B��RB�
=B�p�B�B�{B�z�B���B�33B��B��B�Q�B���B�
=B�p�B��
B�(�B��\B��HB�G�B��B�  B�Q�B��RB�
=B�\)B�B�{B��\B���B�\)B�B�{B��\B��HB�G�B��B�  B�ffB��RB��B��B��B�Q�B��RB�33B��B�  B�ffB���B�G�B��B�{B��\B���B�\)B�B�(�B���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB���B�
=B��B��
B�=qB¸RB��BÅB��B�Q�BĸRB��BŅB��B�Q�B���B�33BǙ�B�  B�z�B��HB�\)B�B�(�Bʣ�B�
=B˅B�  B�z�B��HB�\)B��
B�=qBΣ�B��BυB�  B�ffB��HB�\)B��
B�Q�B���B�33BӮB�(�Bԏ\B�
=BՅB�{B�z�B���B�p�B��
B�ffB���B�G�B�B�Q�B���B�G�B�B�ffB���B�\)B��B�ffB���B�p�B�  B�z�B�
=B�B�{B�\B��B㙚B�(�B��B��B�B�(�B�\B��B癚B�{B�z�B�
=B�B�  B�ffB���B�B�  B�ffB���B�p�B�  B�ffB��HB�\)B�B�(�B��B�
=B�B�  B�z�B���B�p�B�  B�\B��B���B�(�B���B�33B��B�=qB���B��B���B�{B���B�
=B���B�{B��\B�33B��B�(�B��RB�33B��C {C Q�C �\C �
C{CQ�C�\C�
C�C\)C��C�HC(�CffC��C�HC�CffC��C�HC(�Cp�C�RC  C=qC�CC  C=qCz�CC
=CQ�C��C�HC	(�C	p�C	�C	��C
33C
p�C
�RC
��CG�C�C��C{C\)C��C�
C�C\)C��C�HC(�Cp�C�RC��C33Cp�C�RC��CG�C�\C�
C
=CQ�C�\C�
C(�Cp�C�C�C(�Cp�C�C  CG�C�\CC
=CG�C��C�HC33CffC�C��C=qC�\C��C{CQ�C��C�C33Cp�C�C  CQ�C��C�
C{C\)C�C��C=qCz�C��C{C\)C��C�HC=qCz�C�RC
=C\)C��C�
C �C z�C C!  C!G�C!��C!�HC"�C"ffC"�C#  C#=qC#�C#�
C$�C$\)C$��C$��C%G�C%�\C%�
C&�C&z�C&�RC&��C'=qC'��C'�HC(�C(p�C(C)  C)Q�C)��C)�C*33C*p�C*C+{C+\)C+��C+��C,G�C,�\C,��C-�C-p�C-�RC.  C.Q�C.��C.�C/=qC/�\C/�
C0�C0p�C0C1
=C1\)C1�C1��C2G�C2��C2��C333C3�C3�HC433C4z�C4�
C5�C5p�C5C6�C6ffC6C7{C7\)C7�C8
=C8\)C8�C9
=C9\)C9��C:  C:\)C:��C;  C;\)C;�C<  C<\)C<��C=  C=\)C=�C=��C>ffC>�C?  C?ffC?�C@  C@ffC@�CA  CAffCA�RCB
=CBp�CBCC
=CCp�CC�RCD
=CDp�CD�RCE
=CEz�CECF{CFz�CFCG�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                ?u?��H@=p�@}p�@�  @\@�G�@��RA\)A   A,(�A@  A`  A�  A�  A�Q�A���A�Q�A�Q�A�Q�A�  B Q�BQ�BQ�B(�B�
B(  B/�
B7�B?�
BH  BP(�BX(�B`(�Bh  Bo�
Bx  B�{B�{B�z�B�  B��B�  B�{B�{B��B�  B�{B�  B��B��B�  B�{B�  B�  B�  B�  B�{B�  B��B�  B�  B��B�{B�{B�  B��B�  B�  C   C
=C
=C��C  C

=C
=C
=C  C  C  C  C  C  C  C  C��C"  C$
=C&{C(  C)��C+��C-��C0  C2  C4
=C6  C7��C9�C<  C>{C@
=CA��CD  CF
=CH{CJ{CL
=CM��CO��CQ��CT  CV
=CW��CZ  C\  C]�C_�Cb  Cd  Cf  Ch
=Cj{Cl  Cn  Cp  Cr  Ct
=Cv
=Cx  Cz
=C|
=C~  C�  C���C���C���C�  C�  C�  C���C���C�  C�  C�C�
=C�C�  C�  C�C�C���C���C�  C���C���C�  C�C�  C���C�  C�  C�  C�  C�C�C�C���C�  C���C���C���C�  C�C�C���C�  C�  C�  C���C�  C���C���C���C���C�  C���C���C�  C�  C�C�  C���C���C���C�  C�  C�  C�C�C�C�C�C�C�  C�  C�C�C�C���C���C���C���C�  C�  C�  C�  C�C�C�C�  C���C�  C�C�  C�  C�C�C�C�  C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C���C���C�  C�  C���D � D�D��D  D��D�qD� D  D}qD�D��D  D� D  D� D�qD}qD	  D	� D	�qD
� D  D� D�D� D  D� D  D� D  D� D�D��D�qD� D  D}qD  D� D�qD}qD��D� D  D� D  D� D  D��D�D� D  D� D  D��D�D� D  D��D�D��D  D}qD   D � D!  D!��D"  D"}qD"�qD#� D$D$��D%  D%� D&  D&}qD&�qD'}qD(  D(��D)�D)}qD)�qD*� D+  D+��D+�qD,� D-D-��D.  D.��D/  D/xRD/�qD0��D1�D1��D2D2��D3  D3}qD3��D4� D4�qD5}qD6�D6�D7�D7� D7�qD8� D9�D9��D:  D:��D;�D;� D;��D<}qD=  D=�D>D>��D?�D?��D@  D@��DADA}qDA�qDB��DC�DC��DD�DD� DD�qDE}qDE�qDF� DG  DG� DH�DH� DI  DI}qDI��DJz�DJ��DKz�DK�qDL� DL�qDMz�DM�qDN� DN�qDO��DPDP��DQ  DQ}qDQ�qDRz�DS  DS��DT  DT� DU  DU� DV�DV��DW�DW�DX  DXz�DX��DYz�DZ�DZ��DZ��D[}qD[�qD\� D]  D]��D^�D^� D_  D_� D`  D`}qD`��Da}qDb  Db� Dc  Dc��Dd�Dd��Dd�qDe� Df�Df��Df��Dgz�Dh�Dh��Di�Di��Di��Djz�Dk  Dk�Dl�Dl� Dm  Dm� Dm�qDn}qDo  Do� Dp�Dp}qDp�qDq� Dr�Dr� Ds  Ds� Dt  Dt��Du  Du}qDv  Dv��Dw�Dw}qDw�qDx� Dy  Dy��Dz  Dz}qD{  D{� D|�D|� D|�qD}� D~D~��D�D� D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�~�D���D���D�@ D�� D���D���D�@ D�~�D��qD���D�@ D�~�D���D���D�>�D�� D��HD�HD�@ D�~�D���D���D�=qD�� D�� D���D�@ D�� D���D�  D�AHD�� D�� D�HD�AHD��HD��HD�HD�@ D�� D��HD�  D�=qD�� D��HD�  D�>�D�~�D��HD�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�AHD��HD�� D���D�@ D��HD�� D���D�@ D�~�D���D���D�=qD�� D�� D�  D�AHD�� D�� D���D�@ D��HD�� D�HD�AHD�� D���D���D�>�D�� D�D��D�AHD��HD�� D���D�>�D�~�D���D���D�>�D�� D�D��D�AHD�� D�� D�  D�=qD�~�D���D���D�>�D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�� D���D���D�@ D�� D��HD�  D�>�D�� D��HD�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�B�D��HD�� D�  D�@ D�� D�D��D�@ D�~�D��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�HD�AHD�~�D�� D�  D�AHD��HD���D���D�@ D�� D�� D�HD�AHD�� D��qD���D�>�D�� D��HD���D�>�D��HD�D�  D�>�D�� D��HD���D�AHD���D��HD�  D�AHD�� D���D�HD�@ D��HD�D��D�AHD�� D���D���D�@ D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�AHD��HD���D�  D�AHD��HD��HD�  D�AHD�� D��qD��qD�>�D�~�D�� D�HD�>�D�~�D�� D�  D�@ D D�� D���D�@ DÁHD�� D�  D�@ D�~�D�� D�  D�>�Dŀ D��HD���D�>�DƁHD�� D�  D�@ Dǀ D�� D�  D�@ DȁHD��HD�  D�>�DɁHDɾ�D�  D�B�Dʀ Dʾ�D�  D�>�Dˀ D��HD�HD�AHD̀ D��HD�HD�AHD́HD;�D�  D�>�D�~�D��HD�  D�>�D�~�D�� D���D�>�DЀ D�� D���D�AHDсHD��HD�  D�@ D�~�D�� D�  D�AHDӁHD�� D�HD�@ DԁHD�� D�  D�>�D�~�D�� D�HD�B�Dւ�D��HD���D�>�D׀ D��HD�  D�>�D�~�DؽqD���D�@ D�~�D�� D�HD�@ DځHD��HD�  D�>�Dۀ D��HD�  D�@ D܁HD��HD�  D�@ D�~�Dݾ�D�  D�AHDހ D�� D�  D�@ D�~�D߾�D�  D�>�D�~�D�� D�  D�AHD� DᾸD�  D�@ D�HD�D�  D�>�D� D��HD�  D�=qD�~�D侸D�  D�AHD� D�� D�  D�@ D悏D�D�HD�@ D� D羸D�  D�@ D� D�� D���D�>�D� D�� D�HD�AHD� D꾸D�  D�@ D� D뾸D�HD�>�D� D쾸D���D�@ D� D��HD�HD�@ D� DD�  D�>�D�HD��HD�  D�@ D��HD�D�HD�@ D� D��HD���D�AHD�HD��HD���D�@ D�D��HD�  D�@ D�~�D�� D��D�B�D���D�D�  D�=qD�~�D���D�  D�@ D��HD�D�  D�>�D�~�D�� D�  D�AHD��HD��HD�HD�>�G�O�>�\)>��
?.{?�  ?���?��@�\@��@+�@E�@Y��@n{@�  @���@�Q�@��\@���@��H@��@�\)@�p�@�@��A   AA
=qA  A�A��A"�\A(Q�A/\)A5�A:�HA@��AHQ�AN{AS33AY��A`��AfffAk�Aq�Ax��A~{A�=qA�p�A���A�33A�{A�G�A���A��A�=qA�p�A�Q�A��
A�ffA���A�(�A�\)A�=qA��A�  A��A�ffA�G�A��
A�
=Aʏ\A�A�  A�33A�ffA��A�z�A�\)A�=qA�A��A�A�ffA�A���A��A��\A��B Q�B�B33B��B=qB�
B	�B
ffB�
BG�B�\B�B��B�B�HB�B��B�B�HB�Bz�B��B�\B\)B(�B�B=qB
=B�
B ��B!B"�RB#\)B$Q�B%p�B&=qB'
=B'�
B(��B)�B*�\B+\)B,z�B-G�B-�B.�RB/�B0��B1p�B2=qB3
=B4  B4��B5G�B6=qB733B8  B8��B9p�B:�\B;\)B<(�B<��B=B>�RB?�B@��BAG�BB=qBC\)BDQ�BEG�BE�BF�HBH  BI�BJ{BJ�HBK�BL��BM�BN�HBP  BP��BQ��BR�RBS�
BT��BU�BV�HBW�BX��BY�B[
=B\  B\��B]B_
=B`Q�BaG�Bb{Bc33BdQ�Bep�Bf�\Bg�BhQ�Bi��Bj�\Bk�
Bl��BmBn�RBo�Bp��Br{Bs
=Bt  Bt��Bv{Bw33Bxz�ByBz�HB{�
B|��B~=qB�B�ffB���B��B�{B���B�33B��
B�ffB���B��B��B�z�B��HB�\)B��
B�=qB��\B���B���B�33B��B�B��
B�{B�=qB�ffB���B���B��B�\)B���B�B��B�{B�Q�B��\B���B���B�33B�p�B��B�B�  B�Q�B�z�B��RB��HB�
=B�G�B��B�B�{B�Q�B��\B���B���B�33B�p�B�B�  B�Q�B��\B���B�
=B�\)B��B�  B�=qB���B���B��B�\)B��B�  B�Q�B���B�
=B�\)B��B�{B�Q�B��\B��HB�33B���B�  B�ffB���B��B�\)B��B��B�Q�B��RB�
=B�p�B��
B�(�B��\B���B��B�p�B�B�(�B��\B���B�\)B�B�{B�ffB��RB�
=B�p�B��
B�Q�B��RB�
=B�p�B�B�{B�z�B���B�33B��B��B�Q�B���B�
=B�p�B��
B�(�B��\B��HB�G�B��B�  B�Q�B��RB�
=B�\)B�B�{B��\B���B�\)B�B�{B��\B��HB�G�B��B�  B�ffB��RB��B��B��B�Q�B��RB�33B��B�  B�ffB���B�G�B��B�{B��\B���B�\)B�B�(�B���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB���B�
=B��B��
B�=qB¸RB��BÅB��B�Q�BĸRB��BŅB��B�Q�B���B�33BǙ�B�  B�z�B��HB�\)B�B�(�Bʣ�B�
=B˅B�  B�z�B��HB�\)B��
B�=qBΣ�B��BυB�  B�ffB��HB�\)B��
B�Q�B���B�33BӮB�(�Bԏ\B�
=BՅB�{B�z�B���B�p�B��
B�ffB���B�G�B�B�Q�B���B�G�B�B�ffB���B�\)B��B�ffB���B�p�B�  B�z�B�
=B�B�{B�\B��B㙚B�(�B��B��B�B�(�B�\B��B癚B�{B�z�B�
=B�B�  B�ffB���B�B�  B�ffB���B�p�B�  B�ffB��HB�\)B�B�(�B��B�
=B�B�  B�z�B���B�p�B�  B�\B��B���B�(�B���B�33B��B�=qB���B��B���B�{B���B�
=B���B�{B��\B�33B��B�(�B��RB�33B��C {C Q�C �\C �
C{CQ�C�\C�
C�C\)C��C�HC(�CffC��C�HC�CffC��C�HC(�Cp�C�RC  C=qC�CC  C=qCz�CC
=CQ�C��C�HC	(�C	p�C	�C	��C
33C
p�C
�RC
��CG�C�C��C{C\)C��C�
C�C\)C��C�HC(�Cp�C�RC��C33Cp�C�RC��CG�C�\C�
C
=CQ�C�\C�
C(�Cp�C�C�C(�Cp�C�C  CG�C�\CC
=CG�C��C�HC33CffC�C��C=qC�\C��C{CQ�C��C�C33Cp�C�C  CQ�C��C�
C{C\)C�C��C=qCz�C��C{C\)C��C�HC=qCz�C�RC
=C\)C��C�
C �C z�C C!  C!G�C!��C!�HC"�C"ffC"�C#  C#=qC#�C#�
C$�C$\)C$��C$��C%G�C%�\C%�
C&�C&z�C&�RC&��C'=qC'��C'�HC(�C(p�C(C)  C)Q�C)��C)�C*33C*p�C*C+{C+\)C+��C+��C,G�C,�\C,��C-�C-p�C-�RC.  C.Q�C.��C.�C/=qC/�\C/�
C0�C0p�C0C1
=C1\)C1�C1��C2G�C2��C2��C333C3�C3�HC433C4z�C4�
C5�C5p�C5C6�C6ffC6C7{C7\)C7�C8
=C8\)C8�C9
=C9\)C9��C:  C:\)C:��C;  C;\)C;�C<  C<\)C<��C=  C=\)C=�C=��C>ffC>�C?  C?ffC?�C@  C@ffC@�CA  CAffCA�RCB
=CBp�CBCC
=CCp�CC�RCD
=CDp�CD�RCE
=CEz�CECF{CFz�CFCG�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�4G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�A�r�A�XA� �A��#A韾A�\)A�7LA��A���A��A��#A���A�ȴA�wA�FA蛦A�Q�A�M�A���A�A�O�A��A�A�r�A�\)A�7LA�1'A��A�r�A۰!AڋDA�?}A�  A�oA�v�A�x�A��A�=qA̲-A��
A�Q�Ağ�A� �A���A��+A���A���A��A���A�bNA���A���A���A�ĜA���A��^A���A���A��wA�$�A�  A��A���A���A���A��^A�E�A���A��yA��PA��HA�r�A�$�A��PA��A�5?A��A���A��jA�^A{�mAx��Au�;Ar5?AnA�AhI�AchsAa�A`�`A]�A[��AY\)AQ��AP(�AN�AMC�AJȴAE�AB(�A?��A=A;��A9��A7�A7?}A6��A4��A3��A3p�A3`BA2�A2{A1�PA0�`A0$�A.��A-XA,�\A+��A+x�A*ĜA*Q�A)�
A)�7A)��A)x�A(1'A&��A%��A%hsA&1'A%�A$5?A#�A#�hA#�A#+A"�9A"jA!�#A!�A �`A ��A ��A ZA ~�A 1'A�hA��A�\A��AK�AVA��A�AM�A�
Ap�AȴA�+A�yA
=AA��A(�A��A��AO�A&�A�9A=qA1'A=qAA�
A�mA�A�A�A�RAM�A��A��Ap�A7LA��AbAS�A7LA�HAbAt�AO�A/A%AȴA��A1A�FA��AG�A�A�A-A�
A��A|�A
�A
�RA
jA	��A	ƨA	\)A�yA��A�!A�A�FA��A|�AAjAA�AA��A%A�+A �AdZA;dA��A �A�A�-A ��A �A ��A r�A I�A A�@��P@��@�^5@��@�Z@���@���@�@���@�x�@��9@��
@�S�@��-@�`B@���@�1'@�@��H@�J@�-@��@�z�@��;@�"�@�@���@�V@�A�@�C�@��T@�&�@��m@�\)@�
=@�+@�n�@�{@䛦@�w@��@㕁@�@�l�@�33@���@�=q@�X@���@���@�r�@�  @�t�@�@��@އ+@��@�7L@�Z@ۅ@ڇ+@��@�x�@�7L@��/@؛�@�9X@���@�@պ^@�?}@�r�@�1'@�1'@�1'@�t�@ҏ\@�O�@�z�@��@�|�@�;d@ΰ!@�J@Ͳ-@�`B@̴9@�1@�|�@��@�E�@��@ȋD@�I�@�1@���@Ǯ@�33@Ɵ�@�V@�{@ř�@�Ĝ@�A�@Å@��@�V@��@�&�@���@�Q�@�b@�t�@�"�@�-@�O�@��j@��D@�I�@�(�@�1@��w@�K�@���@�@���@���@�O�@��j@�Z@�Q�@�1'@���@�K�@��@��+@�n�@�=q@��@���@�X@�/@��/@��u@�r�@�  @�t�@��y@�v�@�ff@�5?@���@��h@�/@���@�1'@��w@�;d@�
=@�~�@�5?@�J@��@���@���@�p�@�X@�?}@�7L@�&�@���@��9@��
@�S�@�~�@�$�@��@���@��-@��7@�X@��`@��@���@�dZ@�;d@�"�@���@��\@�=q@�{@��@��^@�G�@���@��@�(�@��;@�ƨ@���@��@���@�t�@�+@��@�ȴ@��!@�~�@�{@��-@�x�@�V@�r�@��w@�S�@�
=@��H@���@�n�@�J@��#@�x�@�/@���@���@��@�I�@�  @���@��
@�"�@���@��!@�n�@�M�@�M�@���@�p�@�?}@�V@���@���@���@���@�r�@���@���@�C�@�@���@��!@�^5@�@���@���@�x�@�?}@���@��@�Q�@��m@���@�l�@��H@�=q@��-@�x�@��@�Ĝ@��u@�z�@���@��F@��@���@��@��@�~�@�=q@�-@�$�@���@��@��@���@���@�%@��j@��@�(�@�  @���@��@��
@�\)@��!@�V@�J@���@�?}@�%@��`@��9@�Z@�  @��
@���@�dZ@�C�@���@��!@���@���@�E�@��@��^@���@��h@�x�@�?}@��`@�z�@�I�@�9X@��@�1@��m@��@�l�@�C�@�@�~�@�5?@�$�@�@��7@�7L@���@��/@��j@��D@��@|�@K�@
=@~v�@}�T@}`B@|��@|�@|�D@{�m@{t�@{C�@z~�@y��@y�^@y��@y�7@x�`@w�@wK�@w+@vȴ@v�+@v$�@u�T@u�h@t��@t(�@t�@s�
@st�@so@r~�@q��@pbN@o�@o�w@o�P@n��@n$�@m�T@m�h@m/@l�@lZ@l(�@l1@k�@j�!@i�#@i%@h��@hb@g�P@g+@f�+@f5?@e�@ep�@dZ@cdZ@b�!@bn�@b�@a��@a��@a7L@`��@`Ĝ@`r�@`1'@`  @_�@_��@_|�@^�+@]p�@]`B@]/@\�/@\(�@[ƨ@[dZ@[@Z�H@Z~�@Y��@Y��@Y7L@X�u@XQ�@X1'@W�@W|�@Vȴ@V��@VV@V$�@V{@U��@T��@T(�@S��@S�@SdZ@SS�@S@R�\@R�\@R�\@R�@QG�@P�9@P��@P��@P��@P�@Pb@O��@O\)@N��@N$�@Mp�@L�j@K��@K��@J�@JM�@I��@Ihs@H�`@H��@H�u@H�@H�@Hr�@HbN@HQ�@HA�@Hb@G��@GK�@G
=@F�+@FV@FE�@E��@EV@D��@D(�@C�
@C�F@Ct�@C33@Co@B��@Bn�@A�@A��@Ax�@A&�@@Q�@?�w@?;d@>�@>��@>ff@>5?@>@=`B@=?}@<��@<��@<�D@<z�@<I�@;ƨ@;o@:n�@:M�@:�@9�@9��@9x�@9hs@9&�@8��@8�9@8�@7�@7K�@7�@6ȴ@6$�@6@5�T@5��@5?}@4��@4Z@3ƨ@3S�@3"�@2�@2��@2~�@2-@1�@1��@1X@1&�@0�u@0  @/�w@/�P@/|�@/K�@/+@/�@/�@/�@.V@-�-@-`B@-O�@-�@-�@-`B@-/@-V@,�j@,�D@,j@,�@+�
@+�@+o@*��@*�\@)��@)hs@)X@)7L@)7L@)�@)�@)%@(��@(�`@(��@(Q�@'�;@'�P@'�@&�y@&�@&�@&ȴ@&ȴ@&�R@&��@&��@&ff@&ff@&E�@%�@%�-@$�@$��@$j@$9X@$(�@$�@$1@#��@#ƨ@#��@#S�@#o@"��@"n�@!��@!�^@!�7@!X@!X@!&�@ Ĝ@ �u@ Q�@  �@   @��@��@��@�@|�@K�@��@E�@$�@��@�-@�h@�@p�@?}@/@�@��@�@�@��@z�@j@I�@1@ƨ@�F@��@�@"�@o@�H@�H@�!@=q@�@�#@��@��@G�@�`@Ĝ@�@b@�;@�@|�@l�@K�@�@�y@��@5?@@��@�@`B@?}@V@�/@��@�D@j@9X@(�@1@�m@�@S�@33@�@��@�\@n�@M�@=q@-@=q@�@�#@�^@��@��@hs@G�@&�@�@%@��@�9@�@Q�@b@�;@��@K�@;d@�@�@
=@��@�@ȴ@��@�+@v�@{@��@@�h@p�@`B@�@��@�/@�j@��@I�@9X@(�@1@��@��@��@t�@dZ@S�@C�@@
��@
��@
�\A�z�A�z�A�|�A�z�A�|�A�~�A�A�A�~�A�jA�\)A�O�A�^5A�ZA�-A��A���A�1A�A�jA�9A镁A�p�A�^5A�I�A�?}A�33A�(�A��A�VA�A���A��A��A��yA��yA��HA��;A��HA��/A���A���A��
A���A���A���A���A���A�ȴA�ȴA���A�ȴA�ĜA���A�A���A�wA�FA�RA�^A�^A�FA�!A�-A�-A�A��A�PA�XA��`A�+A�RA��A�+A�r�A�VA�G�A�C�A�?}A�5?A�(�A��A���A�ĜA䗍A�PA�7A�A�~�A�z�A�x�A�p�A�ffA�ZA�1'A���A���A�^A��A㛦A�uA�7A�+A�+A�A�A�|�A�A�A�|�A�z�A�z�A�~�A�|�A�v�A�p�A�p�A�p�A�jA�ffA�hsA�dZA�^5A�ZA�^5A�`BA�ZA�Q�A�VA�Q�A�M�A�I�A�E�A�A�A�9XA�(�A��A�oA��A��A���A���A��A�A�A�1AᗍA�dZA�-A���A�jA��A�r�A�9XA���A�ȴA�~�A�E�A���A�v�A��A�r�A�JA��A��;A���AܼjA܏\A�`BA��A��/Aۏ\A�{A���AڸRAڰ!Aک�Aڡ�AڍPAڅA�|�A�hsA�M�A�5?A���A�XA�  A�ƨAضFA؛�A؋DA�r�A�ZA�9XA�oA׬A�n�A�(�A��
A֛�A�7LA���A���AլA�dZA��/A�t�A�VAӋDA�7LA��AҍPA��AѼjAѓuA�x�A�bNA�O�A�E�A�9XA�
=A�|�A�{AϬA�jA��#AζFAΩ�AΩ�AΩ�AΗ�A�hsA�I�A�=qA�$�A�VA��;Aͥ�Aͣ�A͗�A͕�A͉7A�|�A�t�A�ffA�=qA��A�%A���A��
A̍PA�^5A�/A��A�ĜA˴9A˕�A�\)A�=qA�-A�(�A�"�A�{A�bA�oA�oA�{A�VA���A���Aʛ�A�hsA�G�A�9XA�-A�"�A���A���Aɏ\A�ffA�9XA��Aȗ�A�=qAǾwA���Aƛ�AƑhAƍPAƇ+A�|�A�^5A��/A�
=AĶFA�\)A�"�A��A�ĜAÉ7A�Q�A�$�A���A���A�A�Q�A�
=A���A��jA��RA��!A��RA���A���A���A���A���A�ƨA�ƨA���A���A�A���A��PA��A��+A���A��hA�JA��7A�JA�A���A��7A�~�A�|�A�|�A��uA���A���A���A��uA��hA��A���A��
A��
A���A���A���A���A���A���A�ȴA�ƨA���A��jA��-A���A���A��DA�dZA�C�A��A�ffA���A�~�A�E�A��A�A��A��TA��A�ȴA��RA��hA�I�A���A��
A���A��A�A�A��A���A�t�A�jA�M�A��A��yA�A��RA��FA���A�~�A�hsA�^5A�I�A�/A�A��wA���A��7A�|�A�n�A�jA�`BA�ZA�I�A��A��;A��RA���A���A�|�A�9XA���A���A��+A�E�A�A��;A��A�\)A�XA�S�A�K�A�A�A�;dA��A��mA��^A��+A�XA���A��wA��A�G�A�VA��mA���A���A�l�A�A�A�-A�{A�"�A�=qA��A�JA��A���A��hA���A�C�A��`A��jA���A�l�A�/A��HA�S�A�A�/A���A�~�A�jA�\)A�G�A�;dA�5?A�1'A�+A�&�A�"�A��A�A�  A�A�A���A�  A���A�  A���A���A��A��yA��mA��A�  A���A�  A��;A��A��A��
A��
A���A�ȴA�ȴA�ĜA�A��^A��9A��-A��A���A���A��PA��A�G�A�+A���A��A���A�ZA��^A���A�r�A�/A��
A��A�`BA�&�A�A���A��A��TA��A�bNA�JA��A�A�A�JA��FA�v�A�5?A�$�A��DA��A�t�A��;A�/A��A��A�ȴA���A�G�A�7LA�p�A���A���A�A��-A�z�A���A�+A�Q�A��DA���A��A�XA�/A��A�1A���A��TA���A���A�O�A��A��PA��A�E�A� �A��A��A�oA�%A���A���A��HA���A��FA��\A�dZA��mA��-A�I�A���A�t�A�G�A��A���A���A�=qA�r�A��PA�  A�bNA���A��9A�hsA�+A��mA�ffA��`A�jA���A��A��A�/A��RA�(�A�x�A���A�%A���A��\A�|�A�&�A�A�bNA�A��
A���A�z�A�M�A�A?}A}�A|�A|z�A|ZA|(�A{�A{��A{�A{�Az�uAz-Ay�-Ay�Ax�\Ax$�Aw|�AwoAv�\Av1'Au�TAu��AuO�Au%At�jAtv�As��ArbNAq;dApjAoAol�An��An�\An~�AnjAn�Am�Alr�Ak7LAjbAh�Ag�Ag7LAfM�Ae?}Ad��Ad �AcdZAcVAb�Ab�Ab~�Ab=qAbbAa�Aa��Aa��AaƨAa��Aa�-Aa�AaVA`�A`=qA_�A_�^A_G�A^��A^�A]
=A\n�A\-A\JA[��A[�A[��A[hsA[O�AZ^5AZv�AZ�jAZbNAY�
AW�AW�hAS�hASAR�AR{AQ33AP��AP�APffAPI�AP9XAPM�AP{AO�wAOhsAOAN��AN�AN-AN  AM�AM�-AM`BAMK�AM�AL��AL  AKG�AJ�/AJ�9AJ�DAJ�AH��AG�FAF^5AE��AE"�ADVAC�AC+AB��ABr�AA��AAG�A@�\A@5?A@1A?�wA?G�A>�`A=��A=`BA=�A<�A<�A<�A<Q�A<-A<A;��A;�PA;dZA;+A:��A:�\A:A8-A7�wA7��A7�hA7�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                A�z�A�A�r�A�XA� �A��#A韾A�\)A�7LA��A���A��A��#A���A�ȴA�wA�FA蛦A�Q�A�M�A���A�A�O�A��A�A�r�A�\)A�7LA�1'A��A�r�A۰!AڋDA�?}A�  A�oA�v�A�x�A��A�=qA̲-A��
A�Q�Ağ�A� �A���A��+A���A���A��A���A�bNA���A���A���A�ĜA���A��^A���A���A��wA�$�A�  A��A���A���A���A��^A�E�A���A��yA��PA��HA�r�A�$�A��PA��A�5?A��A���A��jA�^A{�mAx��Au�;Ar5?AnA�AhI�AchsAa�A`�`A]�A[��AY\)AQ��AP(�AN�AMC�AJȴAE�AB(�A?��A=A;��A9��A7�A7?}A6��A4��A3��A3p�A3`BA2�A2{A1�PA0�`A0$�A.��A-XA,�\A+��A+x�A*ĜA*Q�A)�
A)�7A)��A)x�A(1'A&��A%��A%hsA&1'A%�A$5?A#�A#�hA#�A#+A"�9A"jA!�#A!�A �`A ��A ��A ZA ~�A 1'A�hA��A�\A��AK�AVA��A�AM�A�
Ap�AȴA�+A�yA
=AA��A(�A��A��AO�A&�A�9A=qA1'A=qAA�
A�mA�A�A�A�RAM�A��A��Ap�A7LA��AbAS�A7LA�HAbAt�AO�A/A%AȴA��A1A�FA��AG�A�A�A-A�
A��A|�A
�A
�RA
jA	��A	ƨA	\)A�yA��A�!A�A�FA��A|�AAjAA�AA��A%A�+A �AdZA;dA��A �A�A�-A ��A �A ��A r�A I�A A�@��P@��@�^5@��@�Z@���@���@�@���@�x�@��9@��
@�S�@��-@�`B@���@�1'@�@��H@�J@�-@��@�z�@��;@�"�@�@���@�V@�A�@�C�@��T@�&�@��m@�\)@�
=@�+@�n�@�{@䛦@�w@��@㕁@�@�l�@�33@���@�=q@�X@���@���@�r�@�  @�t�@�@��@އ+@��@�7L@�Z@ۅ@ڇ+@��@�x�@�7L@��/@؛�@�9X@���@�@պ^@�?}@�r�@�1'@�1'@�1'@�t�@ҏ\@�O�@�z�@��@�|�@�;d@ΰ!@�J@Ͳ-@�`B@̴9@�1@�|�@��@�E�@��@ȋD@�I�@�1@���@Ǯ@�33@Ɵ�@�V@�{@ř�@�Ĝ@�A�@Å@��@�V@��@�&�@���@�Q�@�b@�t�@�"�@�-@�O�@��j@��D@�I�@�(�@�1@��w@�K�@���@�@���@���@�O�@��j@�Z@�Q�@�1'@���@�K�@��@��+@�n�@�=q@��@���@�X@�/@��/@��u@�r�@�  @�t�@��y@�v�@�ff@�5?@���@��h@�/@���@�1'@��w@�;d@�
=@�~�@�5?@�J@��@���@���@�p�@�X@�?}@�7L@�&�@���@��9@��
@�S�@�~�@�$�@��@���@��-@��7@�X@��`@��@���@�dZ@�;d@�"�@���@��\@�=q@�{@��@��^@�G�@���@��@�(�@��;@�ƨ@���@��@���@�t�@�+@��@�ȴ@��!@�~�@�{@��-@�x�@�V@�r�@��w@�S�@�
=@��H@���@�n�@�J@��#@�x�@�/@���@���@��@�I�@�  @���@��
@�"�@���@��!@�n�@�M�@�M�@���@�p�@�?}@�V@���@���@���@���@�r�@���@���@�C�@�@���@��!@�^5@�@���@���@�x�@�?}@���@��@�Q�@��m@���@�l�@��H@�=q@��-@�x�@��@�Ĝ@��u@�z�@���@��F@��@���@��@��@�~�@�=q@�-@�$�@���@��@��@���@���@�%@��j@��@�(�@�  @���@��@��
@�\)@��!@�V@�J@���@�?}@�%@��`@��9@�Z@�  @��
@���@�dZ@�C�@���@��!@���@���@�E�@��@��^@���@��h@�x�@�?}@��`@�z�@�I�@�9X@��@�1@��m@��@�l�@�C�@�@�~�@�5?@�$�@�@��7@�7L@���@��/@��j@��D@��@|�@K�@
=@~v�@}�T@}`B@|��@|�@|�D@{�m@{t�@{C�@z~�@y��@y�^@y��@y�7@x�`@w�@wK�@w+@vȴ@v�+@v$�@u�T@u�h@t��@t(�@t�@s�
@st�@so@r~�@q��@pbN@o�@o�w@o�P@n��@n$�@m�T@m�h@m/@l�@lZ@l(�@l1@k�@j�!@i�#@i%@h��@hb@g�P@g+@f�+@f5?@e�@ep�@dZ@cdZ@b�!@bn�@b�@a��@a��@a7L@`��@`Ĝ@`r�@`1'@`  @_�@_��@_|�@^�+@]p�@]`B@]/@\�/@\(�@[ƨ@[dZ@[@Z�H@Z~�@Y��@Y��@Y7L@X�u@XQ�@X1'@W�@W|�@Vȴ@V��@VV@V$�@V{@U��@T��@T(�@S��@S�@SdZ@SS�@S@R�\@R�\@R�\@R�@QG�@P�9@P��@P��@P��@P�@Pb@O��@O\)@N��@N$�@Mp�@L�j@K��@K��@J�@JM�@I��@Ihs@H�`@H��@H�u@H�@H�@Hr�@HbN@HQ�@HA�@Hb@G��@GK�@G
=@F�+@FV@FE�@E��@EV@D��@D(�@C�
@C�F@Ct�@C33@Co@B��@Bn�@A�@A��@Ax�@A&�@@Q�@?�w@?;d@>�@>��@>ff@>5?@>@=`B@=?}@<��@<��@<�D@<z�@<I�@;ƨ@;o@:n�@:M�@:�@9�@9��@9x�@9hs@9&�@8��@8�9@8�@7�@7K�@7�@6ȴ@6$�@6@5�T@5��@5?}@4��@4Z@3ƨ@3S�@3"�@2�@2��@2~�@2-@1�@1��@1X@1&�@0�u@0  @/�w@/�P@/|�@/K�@/+@/�@/�@/�@.V@-�-@-`B@-O�@-�@-�@-`B@-/@-V@,�j@,�D@,j@,�@+�
@+�@+o@*��@*�\@)��@)hs@)X@)7L@)7L@)�@)�@)%@(��@(�`@(��@(Q�@'�;@'�P@'�@&�y@&�@&�@&ȴ@&ȴ@&�R@&��@&��@&ff@&ff@&E�@%�@%�-@$�@$��@$j@$9X@$(�@$�@$1@#��@#ƨ@#��@#S�@#o@"��@"n�@!��@!�^@!�7@!X@!X@!&�@ Ĝ@ �u@ Q�@  �@   @��@��@��@�@|�@K�@��@E�@$�@��@�-@�h@�@p�@?}@/@�@��@�@�@��@z�@j@I�@1@ƨ@�F@��@�@"�@o@�H@�H@�!@=q@�@�#@��@��@G�@�`@Ĝ@�@b@�;@�@|�@l�@K�@�@�y@��@5?@@��@�@`B@?}@V@�/@��@�D@j@9X@(�@1@�m@�@S�@33@�@��@�\@n�@M�@=q@-@=q@�@�#@�^@��@��@hs@G�@&�@�@%@��@�9@�@Q�@b@�;@��@K�@;d@�@�@
=@��@�@ȴ@��@�+@v�@{@��@@�h@p�@`B@�@��@�/@�j@��@I�@9X@(�@1@��@��@��@t�@dZ@S�@C�@@
��@
��G�O�A�z�A�z�A�|�A�z�A�|�A�~�A�A�A�~�A�jA�\)A�O�A�^5A�ZA�-A��A���A�1A�A�jA�9A镁A�p�A�^5A�I�A�?}A�33A�(�A��A�VA�A���A��A��A��yA��yA��HA��;A��HA��/A���A���A��
A���A���A���A���A���A�ȴA�ȴA���A�ȴA�ĜA���A�A���A�wA�FA�RA�^A�^A�FA�!A�-A�-A�A��A�PA�XA��`A�+A�RA��A�+A�r�A�VA�G�A�C�A�?}A�5?A�(�A��A���A�ĜA䗍A�PA�7A�A�~�A�z�A�x�A�p�A�ffA�ZA�1'A���A���A�^A��A㛦A�uA�7A�+A�+A�A�A�|�A�A�A�|�A�z�A�z�A�~�A�|�A�v�A�p�A�p�A�p�A�jA�ffA�hsA�dZA�^5A�ZA�^5A�`BA�ZA�Q�A�VA�Q�A�M�A�I�A�E�A�A�A�9XA�(�A��A�oA��A��A���A���A��A�A�A�1AᗍA�dZA�-A���A�jA��A�r�A�9XA���A�ȴA�~�A�E�A���A�v�A��A�r�A�JA��A��;A���AܼjA܏\A�`BA��A��/Aۏ\A�{A���AڸRAڰ!Aک�Aڡ�AڍPAڅA�|�A�hsA�M�A�5?A���A�XA�  A�ƨAضFA؛�A؋DA�r�A�ZA�9XA�oA׬A�n�A�(�A��
A֛�A�7LA���A���AլA�dZA��/A�t�A�VAӋDA�7LA��AҍPA��AѼjAѓuA�x�A�bNA�O�A�E�A�9XA�
=A�|�A�{AϬA�jA��#AζFAΩ�AΩ�AΩ�AΗ�A�hsA�I�A�=qA�$�A�VA��;Aͥ�Aͣ�A͗�A͕�A͉7A�|�A�t�A�ffA�=qA��A�%A���A��
A̍PA�^5A�/A��A�ĜA˴9A˕�A�\)A�=qA�-A�(�A�"�A�{A�bA�oA�oA�{A�VA���A���Aʛ�A�hsA�G�A�9XA�-A�"�A���A���Aɏ\A�ffA�9XA��Aȗ�A�=qAǾwA���Aƛ�AƑhAƍPAƇ+A�|�A�^5A��/A�
=AĶFA�\)A�"�A��A�ĜAÉ7A�Q�A�$�A���A���A�A�Q�A�
=A���A��jA��RA��!A��RA���A���A���A���A���A�ƨA�ƨA���A���A�A���A��PA��A��+A���A��hA�JA��7A�JA�A���A��7A�~�A�|�A�|�A��uA���A���A���A��uA��hA��A���A��
A��
A���A���A���A���A���A���A�ȴA�ƨA���A��jA��-A���A���A��DA�dZA�C�A��A�ffA���A�~�A�E�A��A�A��A��TA��A�ȴA��RA��hA�I�A���A��
A���A��A�A�A��A���A�t�A�jA�M�A��A��yA�A��RA��FA���A�~�A�hsA�^5A�I�A�/A�A��wA���A��7A�|�A�n�A�jA�`BA�ZA�I�A��A��;A��RA���A���A�|�A�9XA���A���A��+A�E�A�A��;A��A�\)A�XA�S�A�K�A�A�A�;dA��A��mA��^A��+A�XA���A��wA��A�G�A�VA��mA���A���A�l�A�A�A�-A�{A�"�A�=qA��A�JA��A���A��hA���A�C�A��`A��jA���A�l�A�/A��HA�S�A�A�/A���A�~�A�jA�\)A�G�A�;dA�5?A�1'A�+A�&�A�"�A��A�A�  A�A�A���A�  A���A�  A���A���A��A��yA��mA��A�  A���A�  A��;A��A��A��
A��
A���A�ȴA�ȴA�ĜA�A��^A��9A��-A��A���A���A��PA��A�G�A�+A���A��A���A�ZA��^A���A�r�A�/A��
A��A�`BA�&�A�A���A��A��TA��A�bNA�JA��A�A�A�JA��FA�v�A�5?A�$�A��DA��A�t�A��;A�/A��A��A�ȴA���A�G�A�7LA�p�A���A���A�A��-A�z�A���A�+A�Q�A��DA���A��A�XA�/A��A�1A���A��TA���A���A�O�A��A��PA��A�E�A� �A��A��A�oA�%A���A���A��HA���A��FA��\A�dZA��mA��-A�I�A���A�t�A�G�A��A���A���A�=qA�r�A��PA�  A�bNA���A��9A�hsA�+A��mA�ffA��`A�jA���A��A��A�/A��RA�(�A�x�A���A�%A���A��\A�|�A�&�A�A�bNA�A��
A���A�z�A�M�A�A?}A}�A|�A|z�A|ZA|(�A{�A{��A{�A{�Az�uAz-Ay�-Ay�Ax�\Ax$�Aw|�AwoAv�\Av1'Au�TAu��AuO�Au%At�jAtv�As��ArbNAq;dApjAoAol�An��An�\An~�AnjAn�Am�Alr�Ak7LAjbAh�Ag�Ag7LAfM�Ae?}Ad��Ad �AcdZAcVAb�Ab�Ab~�Ab=qAbbAa�Aa��Aa��AaƨAa��Aa�-Aa�AaVA`�A`=qA_�A_�^A_G�A^��A^�A]
=A\n�A\-A\JA[��A[�A[��A[hsA[O�AZ^5AZv�AZ�jAZbNAY�
AW�AW�hAS�hASAR�AR{AQ33AP��AP�APffAPI�AP9XAPM�AP{AO�wAOhsAOAN��AN�AN-AN  AM�AM�-AM`BAMK�AM�AL��AL  AKG�AJ�/AJ�9AJ�DAJ�AH��AG�FAF^5AE��AE"�ADVAC�AC+AB��ABr�AA��AAG�A@�\A@5?A@1A?�wA?G�A>�`A=��A=`BA=�A<�A<�A<�A<Q�A<-A<A;��A;�PA;dZA;+A:��A:�\A:A8-A7�wA7��A7�hA7�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�BB	�B	�}B	��B	��B	�0B	��B	��B	��B	��B	�RB	�LB	�B	�FB	�B	��B	�nB	��B	�'B	�B	��B	��B	�jB	�B	�sB	��B	چB	��B	� B
�B
�B
kB
B
�B
"hB
(�B
D3B
O�B
g�B
�B
��B
��B
�]B
��B&LBM6BrGBz�B��B��B��B�2BxBVB�BB�B&�BF�BD3B�B�B�B�B  B��B�DB�aB�kB��B:*B
�B
�9B
��B
�lB
�rB
oiB
PB
�B	�B	��B	�hB	wfB	n�B	]dB	WsB	B'B	6FB	\B	�B	 �B��B�vB�/B��B�TB�TB�jB�<BרB��B�tB�?B��B�#B��B�}B��B҉B�KB��B	�B	)�B	9�B	C�B	F?B	K�B	UgB	aHB	e`B	x8B	��B	�B	�RB	��B	�9B	��B	�B	��B	�>B	�xB	��B
�B
�B
�B
SB
!�B
'RB
-�B
4nB
8�B
;0B
;0B
;0B
8�B
8�B
:�B
CaB
H�B
EmB
N�B
K�B
EB
@B
?}B
?B
C�B
B�B
?HB
=�B
;dB
;dB
C�B
G�B
OvB
P�B
OvB
J#B
>B
:�B
>�B
<�B
:*B
B[B
G�B
GzB
HKB
K�B
M�B
OBB
NpB
O�B
S�B
O�B
OB
OB
N�B
LdB
K)B
IB
I�B
K�B
IB
IB
J�B
LdB
K�B
J�B
J�B
J�B
I�B
I�B
J#B
H�B
GzB
F?B
D�B
C�B
B�B
C�B
B�B
C�B
C�B
CaB
D�B
B'B
AUB
@�B
@�B
>�B
>B
=�B
=qB
:�B
:�B
9�B
8�B
6�B
5?B
4�B
1[B
0�B
1[B
-CB
,qB
+�B
-B
)_B
)�B
,qB
,B
,=B
+�B
)�B
)�B
(�B
(�B
#�B
%zB
$B
#:B
"�B
"4B
 \B
!bB
!B
 'B
 �B
 �B
�B
 �B
�B
VB
!B
�B
�B
=B
7B
�B
+B
�B
1B
�B
�B
�B
�B
�B
B
uB
uB
B
�B
4B
�B
�B
�B
.B
�B
\B
�B
�B
oB
B
B
:B
B
�B
:B
B
�B
VB
(B
:B
B
�B
�B
4B
bB
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
~B
�B
"B
PB
PB
�B
B
~B
~B
�B
xB
B

�B
B
B
xB
DB
xB
xB
DB
B
�B
�B
B
JB
JB
�B
�B
B
�B
~B
JB
B
B
�B
B
xB
�B
�B
B
PB
�B
PB
B
PB
PB
�B
�B
�B
�B
(B
bB
�B
\B
�B
�B
�B
hB
 B
�B
 B
 B
�B
hB
�B
�B
�B
4B
:B
B
�B
�B
B
�B
oB
oB
oB
@B
�B
oB
�B
�B
�B
�B
�B
uB
uB
B
FB
FB
FB
B
B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
$B
�B
_B
_B
+B
+B
�B
1B
�B
�B
_B
�B
1B
�B
�B
�B
�B
�B
B
7B
7B
7B
7B
B
�B
�B
�B
B
�B
�B
7B
B
�B
7B
�B
�B
7B
kB
qB
�B
�B
xB
CB
CB
�B
�B
B
�B
�B
 'B
VB
�B
 �B
 �B
 \B
!�B
!�B
!�B
!�B
"4B
"hB
"�B
"4B
"�B
#nB
#�B
$tB
$@B
$�B
$�B
%zB
%�B
%�B
&B
&LB
&�B
'B
&�B
'�B
'�B
($B
'�B
(�B
)_B
)_B
)_B
)�B
)�B
*0B
*eB
+6B
+B
*�B
*�B
*�B
+�B
+�B
+�B
+kB
+kB
+�B
+�B
+�B
+�B
+kB
-B
,�B
-B
-wB
-B
,�B
,�B
,�B
-B
,�B
-B
,qB
-B
.B
-wB
-wB
-CB
.B
.B
.B
.B
.B
-�B
.}B
.�B
.�B
.}B
/OB
/�B
/�B
/�B
/�B
/�B
/�B
0�B
1[B
1�B
1�B
1�B
1�B
1�B
2-B
2-B
2-B
2�B
3hB
2�B
2�B
2-B
2�B
2�B
2�B
2aB
2aB
2-B
33B
2�B
2aB
2�B
33B
3�B
49B
3�B
3�B
49B
4�B
4�B
5B
5�B
6FB
5�B
5�B
5�B
6�B
7�B
7�B
7�B
8B
8B
8RB
8RB
8RB
9�B
9�B
9XB
9�B
:*B
9�B
:*B
;dB
<6B
<6B
<6B
<6B
=qB
=qB
=�B
=�B
>B
>BB
>�B
>�B
>wB
?B
?�B
@�B
@�B
@�B
AUB
AUB
A�B
B'B
A�B
B[B
B[B
C�B
DgB
D�B
D�B
EB
EB
EmB
E�B
E�B
E�B
F?B
FB
FB
FB
E�B
E�B
G�B
GEB
GEB
GEB
HB
HKB
H�B
H�B
IB
H�B
IRB
IRB
I�B
I�B
JXB
J#B
I�B
J#B
J�B
J�B
JXB
J�B
J�B
JXB
K)B
K)B
LdB
K�B
K�B
K�B
K�B
L0B
L�B
LdB
K�B
L�B
M�B
M�B
MjB
M6B
M6B
MjB
NB
M�B
N<B
NpB
OBB
OBB
PB
P}B
PHB
QB
QB
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
RTB
R�B
R�B
S[B
S[B
S�B
T,B
S�B
S�B
T�B
T�B
UgB
U�B
VB
VB
VmB
VmB
V�B
V�B
W
B
W�B
W�B
W�B
XEB
YKB
YB
Y�B
ZQB
ZB
ZQB
Z�B
Z�B
[WB
[#B
[�B
[�B
[�B
[WB
[�B
[�B
\]B
\�B
\�B
\�B
]/B
]dB
]dB
]dB
]�B
]�B
^B
]�B
_B
_pB
_pB
`B
`BB
`BB
`BB
`vB
`�B
aHB
a�B
bB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
dZB
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
f2B
f�B
hsB
iB
h�B
h�B
h�B
i�B
i�B
jB
jB
jB
jB
j�B
kB
kQB
j�B
kB
j�B
kB
l�B
l�B
l�B
l�B
lWB
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m�B
m�B
n/B
ncB
n�B
n�B
n�B
n�B
n�B
n/B
ncB
n�B
n/B
n�B
ncB
n�B
o�B
o�B
pB
p;B
o�B
p�B
p;B
poB
qB
poB
p�B
qB
q�B
q�B
rGB
rB
q�B
q�B
rGB
r�B
sB
r�B
r�B
r�B
s�B
tB
s�B
sB
tB
tB
t�B
u�B
t�B
t�B
u�B
uZB
uZB
uZB
uZB
u�B
u�B
v`B
v`B
u�B
v�B
v�B
v�B
wfB
wfB
w�B
w�B
wfB
v�B
wfB
x8B
w�B
xlB
x8B
w�B
y	B
x�B
x�B
xlB
yrB
x�B
y�B
yrB
zB
z�B
zDB
{JB
z�B
z�B
{B
{�B
{�B
{�B
|B
|�B
|�B
}"B
}VB
}"B
}"B
}�B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
~�B
~]B
~�B
cB
� B
�4B
��B
�;B
��B
�B
�iB
�B
��B
�;B
�oB
�;B
�B
��B
�B
��B
��B
�AB
�B
�AB
��B
�AB
�B
�uB
�GB
��B
�{B
�B
�{B
�GB
�{B
�{B
��B
�B
��B
�MB
�MB
�MB
��B
��B
��B
�SB
�B
�B
�B
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
��B
�YB
�+B
�_B
��B
�_B	�<B	�qB	�BB	�B	��B	��B	�B	�B	��B	�-B	� B	��B	��B	�dB	��B	��B	��B	��B	�B	�0B	��B	�HB	��B	�RB	��B	��B	�RB	��B	��B	��B	��B	�FB	��B	�B	�B	��B	��B	��B	�FB	�zB	��B	��B	�tB	��B	�LB	��B	�FB	��B	�B	�zB	�B	�B	�B	��B	�FB	��B	�B	��B	�?B	�B	��B	��B	�?B	��B	��B	�-B	��B	��B	�[B	�qB	ޞB	��B	�?B	�3B	��B	��B	�IB	��B	�B	��B	�IB	�UB	��B	�RB	��B	��B	��B	��B	�*B	�qB	��B	ɆB	�XB	�B	՛B	��B	�9B	�KB	�B	��B	�9B	רB	�sB	�mB	�B	�
B	�EB	�
B	��B	خB	�KB	��B	�sB	��B	چB	�WB	ںB	�KB	�B	��B	��B	�KB	�#B	یB	�QB	�B	چB	�)B	چB	�B	�#B	یB	��B	یB	یB	��B	�pB	�B	�B	�ZB	�B	�TB	�B	��B	��B	��B	�DB	��B
 4B
 4B	��B
{B
�B
SB
�B

=B
JB
�B
!bB
CB
%B
$B
�B
{B
B
�B
$B
$B
�B
�B
�B
&�B
�B
uB
FB
FB
�B
�B
{B
B
B
�B
�B
OB
%�B
�B
�B
�B
�B
�B
�B
�B
"hB
"4B
,�B
 �B
(�B
($B
'�B
-wB
%�B
$�B
%B
0!B
8�B
4B
A�B
?�B
AUB
J#B
N<B
X�B
W�B
N�B
OvB
L�B
NB
L�B
L�B
R�B
d�B
cTB
poB
q�B
{�B
poB
q�B
qAB
qB
xlB
��B
�	B
��B
��B
��B
��B
�YB
��B
�B
�uB
�B
��B
�B
��B
�VB
�B
�B
�4B
�$B
�=B
�0B
��B
��B
�aB
��B
�B
��B
�FB
�FB
��B
��B
�$B
��B
�^B
�XB
�RB
��B
�B
�3B
ʌB
�B
��B
�?B
�B
�B
�NB
�aB
�?B
רB
�)B
�pB
��B
��B
�B
��B
�vB
��B
�B
�B
��B
��B
�B
�B
��B
�ZB
�B
�B
��B
�rB
��B
�.B�B�BB(�B/OB0�B,�B/B.�B,�B9�BDgBE9BJ�BL0BLdBK^BL�BN<BT�BS�BR�BS[BT�B}�B�B�-B��B~(BqABgmBhsBf�BiDBk�Bg�BsBr�BrGBt�Bs�Bl�B��B��B��B��B�XB�eB�B��B��B�}B��B�IB�B�OB�UB��B�3B��B��BɺB�B�B��B�qB��B�RB��B�RB�?B�FB�?B��B�jB�*B��B�HB�|B��B��B�yB�WB�0B�5B�DB�lB��B 4BVB�B+B�B�B�BBBFB�B
=B"B�BDBBBB�B"B�BfB1B
=B�B_B
=B�B�B(BoB#B@BBoBoB@B�B$BMBFBB/�B7LB33B2aB0UB(�B#B$�B"hB"hB�B#�B+B'RBGzBM�BLdBMjBK�BT�BaHB[�BEB;0B<�BB�B9�B<�BAUB@�B5?B-CB�BB.B�BPB\BJB~BBJB
�B�B+B�B�B�B%B�B�B�B+B�BBoB�BoB�B��B�B�BBAB �B  B 4B��B�PB�B�B��B�B�fB��B��B��B�cB�lB�`B�B�TB�B��B��B�XB�LB�BB�XB��B�3B��B�B��B��B��B�B��B�$B��B��B��B�B�.B�B|B�@Bw2BjKB`�B\]BG�BA�B �B"�B$tB<jB
�.B
�`B
�|B
ޞB
�yB
�B
�
B
��B
�[B
�&B
��B
��B
�B
��B
�CB
�1B
�1B
��B
�MB
��B
�!B
��B
�1B
��B
��B
�B
�YB
�YB
�1B
�7B
�+B
��B
��B
��B
�lB
��B
��B
��B
�JB
zxB
}�B
r�B
e�B
h�B
b�B
_;B
a|B
k�B
`B
O�B
PHB
7�B
7�B
2�B
*�B
+�B
-wB
�B
�B

rB	��B	�lB	��B	��B	�MB	�B	�B	�5B	��B	��B	��B
:B	��B	�nB	�~B	�uB	��B	�B	��B	�@B	�oB	�xB	��B	x�B	w�B	y>B	y>B	t�B	w�B	{�B	u�B	qB	y>B	m]B	kQB	f�B	j�B	a�B	f2B	`vB	]�B	[�B	X�B	XB	V�B	Q�B	Y�B	g�B	Z�B	X�B	MjB	H�B	OBB	>BB	;dB	;0B	>wB	I�B	FB	E�B	G�B	,=B	"4B	+�B	�B	*0B	�B	 B	�B	�B	
�B	YB	fB	�B	1B	{B	�B	 4B	 4B��B	B	�B	�B	B�B�.B��B�"B��B	 �B	B��B�`B��B�/B�/B�/B��B��B	�B�
B�B�ZB�KB	{BخB	;0B��BޞB�B�B֡B�EBуBѷB�}B�^B�[B��B��B�B�[B�BB��BѷB˒BϫB�HBɺB�HB�KB�;B�)B��B�zB��B��B� B�yB�vB�BٴBٴB��B��B��BĜBȴB�6BɆB��B�B�9B�RB��B�mB�RB�B��B��B��B��B��B��B��B��B�6B�qBB�[B�0B�5B�[B�[B��B�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                B	�]B	��B	��B	�B	��B	��B	��B	�B	�DB	�B	��B	��B	�fB	�zB	�`B	��B	�B	�wB	�B	��B	��B	��B	бB	��B	��B	چB	��B	� B	�DB
�B
"NB
 �B
�B
#:B
-�B
6�B
N�B
X�B
p�B
�\B
�B
�dB
�oB
�B)DBS�Bv�B{0B�B�B��BߊB B�BYBkB*0B-CBT�BS�B"�B�BYB�B�B�B�B��B��B��BO\B
��B
��B
��B
��B
��B
��B
c�B
/�B
�B	�B	��B	�UB	y$B	i�B	d�B	U2B	EB	�B	EB	
XB	�B�^B	B�KB�EB�?BּB��B��B�,B�\BʦB�SB�bB�B��B��B�mB�7B�B	#:B	*KB	;dB	FYB	I7B	PB	Z�B	dB	h
B	y�B	�=B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B
B
!�B
B
SB
!�B
(�B
/�B
5�B
:�B
<�B
=VB
<6B
8�B
9�B
:�B
D�B
J�B
E�B
RTB
O(B
F?B
AB
?�B
>�B
F�B
D�B
@�B
@ B
<6B
:DB
C�B
H1B
P�B
RoB
Q�B
P}B
?cB
;dB
@OB
>(B
:xB
BAB
HfB
HB
HB
K�B
N�B
Q�B
O\B
QNB
U�B
P.B
O�B
PB
Q B
NpB
M�B
I�B
J�B
N�B
KDB
I�B
KB
MB
L�B
K�B
MB
K�B
J	B
J�B
K^B
I�B
I7B
GzB
E�B
DMB
D�B
D�B
DB
E�B
DgB
D�B
F%B
B�B
A�B
B�B
BB
?cB
>�B
?cB
?}B
;B
;�B
:�B
:�B
8�B
6�B
7fB
2-B
2�B
3B
.B
-wB
.cB
.B
)�B
*0B
,�B
,WB
-�B
,�B
*�B
,=B
*eB
+B
$tB
&�B
$tB
#�B
$@B
#�B
!|B
$@B
�B
 �B
"4B
!�B
!|B
"NB
 BB
 vB
 \B
!B
/B
�B
�B
B
�B
�B
�B
B
1B
�B
9B
�B
aB
FB
B
�B
�B
NB
 B
 B
B
 B
B
 B
 B
:B
�B
�B
B
B
:B
�B
[B
uB
@B
�B
 B
[B
�B
 B
oB
�B
B
�B
HB
�B
�B
�B
jB
�B
�B
VB
vB
\B
B
�B
�B
�B
pB
�B
�B
6B
�B
B
~B
�B
JB
B
6B
�B
�B
�B
�B
0B
B
~B
dB
B
�B
PB
6B
�B
�B
<B
6B
B
B
�B
B
�B
jB
B
�B
�B
�B
�B
�B
�B
<B
�B
�B
(B
�B
BB
.B
B
�B
�B
}B
�B
�B
B
4B
4B
�B
�B
:B
�B
:B
 B
�B
 B
@B
&B
�B
�B
�B
uB
�B
@B
�B
B
�B
[B
&B
�B
,B
�B
�B
�B
�B
aB
{B
{B
aB
FB
aB
�B
mB
�B
�B
�B
$B

B
�B
?B
�B
sB
�B
�B
�B
�B
_B
�B
eB
�B
B
B
�B
�B
�B
�B
QB
QB
B
�B
7B
kB
�B
�B
�B
QB
B
B
kB
�B
QB
kB
qB
WB
�B
�B
B
QB
�B
#B
�B
xB
IB
/B
�B
�B
dB
B
5B
B
 BB
 �B
�B
 vB
 �B
 �B
!HB
"NB
"4B
"4B
"hB
"�B
"�B
"�B
"�B
#�B
$&B
$tB
$�B
$�B
$�B
%FB
&B
&B
&LB
&fB
&�B
'RB
'mB
'mB
(�B
(�B
(�B
)B
)�B
*eB
)�B
*B
*B
*eB
*B
+QB
+�B
+B
*�B
+B
+�B
,�B
,"B
+�B
+�B
+�B
+�B
+�B
+�B
,B
,�B
-�B
-]B
-�B
-�B
-)B
,�B
,�B
-�B
.IB
-�B
-�B
-B
.B
.}B
-�B
-�B
-�B
.�B
.cB
.}B
.�B
.cB
.}B
/ B
.�B
.�B
/B
0B
/�B
0B
0B
/�B
0;B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
2B
2�B
2�B
2�B
3�B
3�B
2�B
3�B
2�B
3hB
3�B
2�B
2�B
2�B
3B
3�B
2�B
2�B
3B
3�B
4TB
4�B
3�B
4B
4�B
5?B
5%B
5�B
6zB
6`B
5�B
5�B
6`B
8B
8RB
7�B
8RB
8lB
8�B
8�B
8�B
9>B
:^B
9�B
9�B
9�B
:�B
:�B
;B
<�B
<�B
<jB
<�B
="B
=�B
=�B
=�B
>B
>�B
>�B
?B
>�B
>�B
?�B
@�B
AUB
A B
A�B
A�B
A�B
B[B
BuB
BAB
B�B
CaB
D�B
EB
EB
EB
E9B
ESB
E�B
E�B
E�B
F%B
F�B
F?B
F%B
F?B
F?B
F�B
H�B
G_B
GzB
G�B
H�B
H�B
H�B
H�B
IRB
IB
J	B
I�B
I�B
JXB
J�B
JXB
J=B
J�B
K)B
J�B
J�B
K)B
J�B
J�B
K�B
K�B
L�B
L0B
LB
LB
LB
L�B
L�B
L~B
L~B
M�B
NVB
M�B
MjB
M6B
MjB
M�B
NpB
M�B
N�B
OBB
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
RTB
RoB
S&B
R�B
R�B
R�B
R�B
R�B
SB
RoB
S&B
S&B
S�B
S�B
TB
TaB
TB
TB
UMB
U�B
U�B
VB
V9B
VSB
V�B
V�B
V�B
W
B
W�B
W�B
XB
XB
YB
Y�B
ZB
ZQB
Z�B
ZQB
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\B
\xB
\�B
\�B
\�B
]/B
]~B
]�B
]~B
]�B
]�B
^B
^5B
^jB
_�B
_�B
_�B
`�B
`vB
`vB
`�B
`�B
a-B
a�B
bNB
b�B
b�B
b�B
c B
cB
c�B
dB
d@B
d�B
d�B
e`B
f�B
fB
e�B
fB
e�B
e�B
fLB
ffB
fLB
gRB
iB
i_B
h�B
h�B
h�B
i�B
i�B
jKB
j�B
j�B
j�B
kB
kkB
k�B
kB
kkB
kB
k�B
mB
l�B
mB
l�B
lqB
l�B
l�B
l�B
l�B
mB
mwB
m�B
m�B
m�B
ncB
n}B
n�B
n�B
n�B
n�B
n�B
nIB
n�B
n�B
ncB
n�B
n�B
o�B
p!B
o�B
p;B
pUB
o�B
p�B
pUB
p�B
q'B
p�B
p�B
q[B
r-B
r-B
r�B
rGB
rB
q�B
r|B
sB
sMB
r�B
sB
sB
s�B
tB
s�B
sMB
tTB
tnB
u?B
vB
u%B
u?B
u�B
utB
utB
utB
u�B
u�B
u�B
vzB
vzB
vFB
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
xRB
w�B
xlB
xlB
xB
yXB
x�B
x�B
x�B
y�B
y>B
y�B
y�B
zxB
z�B
zxB
{B
z�B
z�B
{�B
{�B
|B
|PB
|�B
}B
}B
}<B
}qB
}VB
}VB
}�B
}qB
}VB
~(B
~B
}�B
~(B
~]B
~�B
~�B
B
�B
�B
�OB
��B
�UB
��B
�B
��B
�UB
��B
�UB
��B
�oB
�'B
��B
� B
��B
��B
�uB
�AB
�uB
��B
�uB
�GB
��B
�aB
��B
�{B
�-B
��B
�aB
��B
��B
��B
�3B
�MB
��B
�gB
��B
��B
��B
�B
�mB
�9B
�9B
�SB
��B
��B
��B
��B
��B
��B
�B
��B
�tB
�tB
��B
��B
�_B
�zB
��G�O�B	�<B	�qB	�BB	�B	��B	��B	�B	�B	��B	�-B	� B	��B	��B	�dB	��B	��B	��B	��B	�B	�0B	��B	�HB	��B	�RB	��B	��B	�RB	��B	��B	��B	��B	�FB	��B	�B	�B	��B	��B	��B	�FB	�zB	��B	��B	�tB	��B	�LB	��B	�FB	��B	�B	�zB	�B	�B	�B	��B	�FB	��B	�B	��B	�?B	�B	��B	��B	�?B	��B	��B	�-B	��B	��B	�[B	�qB	ޞB	��B	�?B	�3B	��B	��B	�IB	��B	�B	��B	�IB	�UB	��B	�RB	��B	��B	��B	��B	�*B	�qB	��B	ɆB	�XB	�B	՛B	��B	�9B	�KB	�B	��B	�9B	רB	�sB	�mB	�B	�
B	�EB	�
B	��B	خB	�KB	��B	�sB	��B	چB	�WB	ںB	�KB	�B	��B	��B	�KB	�#B	یB	�QB	�B	چB	�)B	چB	�B	�#B	یB	��B	یB	یB	��B	�pB	�B	�B	�ZB	�B	�TB	�B	��B	��B	��B	�DB	��B
 4B
 4B	��B
{B
�B
SB
�B

=B
JB
�B
!bB
CB
%B
$B
�B
{B
B
�B
$B
$B
�B
�B
�B
&�B
�B
uB
FB
FB
�B
�B
{B
B
B
�B
�B
OB
%�B
�B
�B
�B
�B
�B
�B
�B
"hB
"4B
,�B
 �B
(�B
($B
'�B
-wB
%�B
$�B
%B
0!B
8�B
4B
A�B
?�B
AUB
J#B
N<B
X�B
W�B
N�B
OvB
L�B
NB
L�B
L�B
R�B
d�B
cTB
poB
q�B
{�B
poB
q�B
qAB
qB
xlB
��B
�	B
��B
��B
��B
��B
�YB
��B
�B
�uB
�B
��B
�B
��B
�VB
�B
�B
�4B
�$B
�=B
�0B
��B
��B
�aB
��B
�B
��B
�FB
�FB
��B
��B
�$B
��B
�^B
�XB
�RB
��B
�B
�3B
ʌB
�B
��B
�?B
�B
�B
�NB
�aB
�?B
רB
�)B
�pB
��B
��B
�B
��B
�vB
��B
�B
�B
��B
��B
�B
�B
��B
�ZB
�B
�B
��B
�rB
��B
�.B�B�BB(�B/OB0�B,�B/B.�B,�B9�BDgBE9BJ�BL0BLdBK^BL�BN<BT�BS�BR�BS[BT�B}�B�B�-B��B~(BqABgmBhsBf�BiDBk�Bg�BsBr�BrGBt�Bs�Bl�B��B��B��B��B�XB�eB�B��B��B�}B��B�IB�B�OB�UB��B�3B��B��BɺB�B�B��B�qB��B�RB��B�RB�?B�FB�?B��B�jB�*B��B�HB�|B��B��B�yB�WB�0B�5B�DB�lB��B 4BVB�B+B�B�B�BBBFB�B
=B"B�BDBBBB�B"B�BfB1B
=B�B_B
=B�B�B(BoB#B@BBoBoB@B�B$BMBFBB/�B7LB33B2aB0UB(�B#B$�B"hB"hB�B#�B+B'RBGzBM�BLdBMjBK�BT�BaHB[�BEB;0B<�BB�B9�B<�BAUB@�B5?B-CB�BB.B�BPB\BJB~BBJB
�B�B+B�B�B�B%B�B�B�B+B�BBoB�BoB�B��B�B�BBAB �B  B 4B��B�PB�B�B��B�B�fB��B��B��B�cB�lB�`B�B�TB�B��B��B�XB�LB�BB�XB��B�3B��B�B��B��B��B�B��B�$B��B��B��B�B�.B�B|B�@Bw2BjKB`�B\]BG�BA�B �B"�B$tB<jB
�.B
�`B
�|B
ޞB
�yB
�B
�
B
��B
�[B
�&B
��B
��B
�B
��B
�CB
�1B
�1B
��B
�MB
��B
�!B
��B
�1B
��B
��B
�B
�YB
�YB
�1B
�7B
�+B
��B
��B
��B
�lB
��B
��B
��B
�JB
zxB
}�B
r�B
e�B
h�B
b�B
_;B
a|B
k�B
`B
O�B
PHB
7�B
7�B
2�B
*�B
+�B
-wB
�B
�B

rB	��B	�lB	��B	��B	�MB	�B	�B	�5B	��B	��B	��B
:B	��B	�nB	�~B	�uB	��B	�B	��B	�@B	�oB	�xB	��B	x�B	w�B	y>B	y>B	t�B	w�B	{�B	u�B	qB	y>B	m]B	kQB	f�B	j�B	a�B	f2B	`vB	]�B	[�B	X�B	XB	V�B	Q�B	Y�B	g�B	Z�B	X�B	MjB	H�B	OBB	>BB	;dB	;0B	>wB	I�B	FB	E�B	G�B	,=B	"4B	+�B	�B	*0B	�B	 B	�B	�B	
�B	YB	fB	�B	1B	{B	�B	 4B	 4B��B	B	�B	�B	B�B�.B��B�"B��B	 �B	B��B�`B��B�/B�/B�/B��B��B	�B�
B�B�ZB�KB	{BخB	;0B��BޞB�B�B֡B�EBуBѷB�}B�^B�[B��B��B�B�[B�BB��BѷB˒BϫB�HBɺB�HB�KB�;B�)B��B�zB��B��B� B�yB�vB�BٴBٴB��B��B��BĜBȴB�6BɆB��B�B�9B�RB��B�mB�RB�B��B��B��B��B��B��B��B��B�6B�qBB�[B�0B�5B�[B�[B��B�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=��<f�x<#�
<#�
<#�
<#�
<=��<k�~<5�T<#�
<#�
<.T<<(�<s��<���<k�~<#�
<#�
<#�
<#�
<#�
<Q�<h�B<0(<#�
<#�
<4Ӄ<#�
<4Ӄ<#�
<m��<�1�<#�
<#�
<#�
<#�
<#�
<<��<�B3<.T<�8H<��N<��W<�J�<#�
<#�
<#�
<���<��W<��<�<�S<��.<P�l<)l</�6<P�l<[�L<���<s��<#�
<#�
<&Q�<#�
<#�
<�J�<#�
<#�
<#�
<#�
<�8H<K�"<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018101904194420181019041944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010220190106200102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010220190106200102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551120190521075511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                