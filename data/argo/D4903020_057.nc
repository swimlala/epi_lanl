CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-21T21:34:26Z creation; 2021-03-26T17:01:00Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20201121213426  20210326170210  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               9   9AA  AOAO7836_008777_057                 7836_008777_057                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�I5
�Sz@�I5
�Sz11  @�I5=�c@�I5=�c@<�Ǹ��@<�Ǹ���d��Ҳ��d��Ҳ�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�  @��R@��R@�G�A   A  A ��A,(�A@  A_\)A\)A�  A��A��A��AϮA�  A�  B   B(�B(�B�
B   B(  B/�
B7�
B?�
BG�
BP  BXQ�B`  Bg�
Bp  Bw�
B�
B��B�  B��
B��
B�  B��
B�  B�(�B�(�B�(�B�{B�  B��B�(�B�{B��B�  B�  B��B��
B�B��B�{B�{B�=qB�(�B�{B�(�B�(�B�=qB�(�B��C��C  C{C{C
  C
=C
=C��C�C��C  C�C�HC�HC�C�C!��C#��C&  C(
=C*  C,  C.  C0  C2  C4  C6
=C8
=C9��C<  C>{C?��CA��CD{CF{CH  CJ  CL{CN
=CP  CR  CT  CV
=CX
=CZ{C\{C^{C_�Cb  Cc��Ce��Ch
=Ci��Ck�Cm�Cp
=Cr
=Ct  Cv  Cw��Cy��C|
=C~  C��C���C�  C���C�  C���C���C���C�  C�C���C���C�  C�  C���C�  C�
=C�C�  C���C���C�  C�
=C�  C���C�  C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�  C�C�C���C���C�  C�
=C�C�C�C�  C���C���C�  C�C�C�  C���C���C�C�
=C�C�  C�  C�
=C���C���C�  C�C�C�  C���C�  C�  C�  C���C���C�  C�  C�  C���C���C�C�  C���C���C�  C���C�  C�  C���C�C���C�C�  C�  C�  C���C�  C�C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C���C�  C���C�  C�C�  C�C�C�C�  C�C�  C���C���C���C�C�C�C�D �D � D  D� D��Dz�D��D}qD�qD� D�D}qD�qD}qD�qD��DD��D	�D	}qD	�qD
� D
��DxRD�RDz�D�D}qD�qD}qD  D��D�qD� D  D}qD  D��D  Dz�D�qD��D��D}qD�qD}qD  D� DD�D  D� D�D�D�D}qD�qD��D  Dz�D��D}qD��Dz�D   D � D ��D!xRD!�qD"}qD#  D#��D$  D$� D$�qD%z�D&  D&� D'  D'� D(  D(��D)�D)� D*  D*�D+  D+}qD,  D,� D-�D-}qD-�qD.� D/  D/� D0  D0}qD0��D1z�D1��D2z�D2�qD3� D3�qD4}qD4�qD5}qD6  D6��D7�D7��D8�D8� D9  D9��D:  D:}qD;  D;}qD<  D<��D=  D=}qD>  D>� D>�qD?}qD@�D@��DA  DA��DB  DB}qDB�qDCz�DC�qDD��DEDE� DF  DF��DG  DG}qDH  DH� DIDIz�DI��DJ� DK  DK}qDK�qDL� DM�DM�DM�qDN� DO�DO}qDP  DP��DQ�DQ� DQ�qDR� DS  DS��DT�DT}qDU  DU��DU�qDV}qDW�DW� DW�qDX� DY  DY� DZ  DZz�DZ�qD[� D\  D\}qD\�qD]� D^�D^��D_  D_z�D`  D`��D`�qDaz�Db  Db��Db�qDc� Dd  Dd��De  De� De��Dfz�Df�qDg��Dh�Dh��Di�Di��Di�RDjz�Dj�qDkz�Dk�RDl}qDm  Dm� Dn�Dn��DoDo�Dp  Dp}qDp�qDq}qDq�qDr� DsDs��DtDt� Dt��Du}qDv�Dv��Dv�qDw}qDx  Dx��Dy  Dy}qDz  Dz��D{�D{}qD{�qD|z�D|�RD}}qD~�D~��D  D��D�  D�<)D�|)D��qD�  D�@ D�|)D�� D��D�B�D���D��HD�HD�@ D�� D��HD�HD�AHD��HD��HD��D�B�D��HD��qD��qD�>�D��HD��HD��qD�<)D�~�D�� D�HD�>�D�~�D��HD�HD�AHD��HD��HD���D�>�D�~�D��qD�  D�=qD�}qD�� D���D�>�D�}qD��qD��qD�>�D�� D�� D���D�@ D�� D���D�  D�B�D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D��HD��HD�HD�AHD��HD�� D�  D�>�D�� D��HD�  D�>�D�� D���D�  D�AHD�� D�� D�HD�>�D�� D���D���D�@ D�� D���D�  D�@ D�}qD��qD�  D�@ D�}qD���D�HD�AHD��HD��HD�HD�@ D�~�D���D��qD�>�D�~�D�� D�  D�>�D�}qD��qD���D�@ D�� D��HD�  D�=qD�~�D�� D���D�=qD�~�D��HD�HD�@ D�~�D���D�  D�>�D�� D�� D�  D�>�D�� D��HD��D�>�D�~�D��HD�HD�@ D�� D�� D�  D�AHD��HD��HD�  D�AHD��HD��HD�  D�@ D�� D��qD��qD�=qD�}qD��qD��qD�>�D�~�D�� D�HD�AHD��HD�� D���D�<)D�� D��HD�  D�@ D���D��HD�  D�>�D�~�D�� D�  D�AHD�� D�� D�  D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�� D�D��D�B�D�� D���D���D�@ D��HD�� D�  D�>�D�}qD���D��D�B�D���D��HD�  D�>�D�~�D��qD���D�>�D�}qD��qD���D�AHD�� D�� D�  D�>�D�}qD���D�  D�>�D��HD�� D��qD�>�D�� D���D�  D�AHD���D��HD�  D�AHD�~�D�� D��D�B�D��HD��HD�HD�@ D D�� D�  D�@ DÂ�D�D���D�>�DāHD��HD�  D�=qD�~�D�D��D�@ D�}qDƼ)D��qD�@ DǁHD�� D�  D�@ DȁHD�� D���D�AHDɂ�D��HD�HD�@ D�}qDʾ�D�  D�=qD�}qD˽qD���D�@ D̀ D��HD�HD�@ D́HD��HD�  D�@ D�}qDξ�D�  D�@ D�~�D�� D�  D�@ DЂ�D���D�HD�>�Dр D��HD�  D�@ DҀ DҽqD���D�AHDӁHD�� D�  D�AHD�~�D�� D�  D�@ D�~�Dվ�D�  D�AHDր D��HD�HD�@ DׁHD׾�D�  D�@ D�~�D�� D�  D�>�DفHD�� D���D�AHDڀ Dھ�D�HD�@ D�~�D�� D�  D�>�D܀ D�� D�  D�AHD݂�D�� D��qD�>�Dހ D�� D�HD�>�D�~�D�D��D�AHD�� DྸD���D�@ D� DᾸD�HD�@ D�~�D��HD�HD�>�D� D��HD��qD�<)D�~�D侸D�HD�B�D�HD徸D��qD�=qD�~�D���D��D�AHD炏D��HD�  D�@ D肏D���D�  D�@ D� D�� D���D�>�D�~�D��HD���D�@ D�~�D�� D�HD�AHD� D�qD�  D�@ D�}qD�� D�  D�@ D�HD�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�  D�>�D� D��HD��D�B�D� D�D���D�=qD�~�D�� D���D�=qD� D�� D��D�@ D�~�D��HD��D�AHD�~�D�� D�  D�AHD�� D�� D���D�@ D���D��HD�HD�=qD�}qD�� D�  D�AHD�z�?�?L��?��?��R?�33?��?��H@��@z�@&ff@.{@:�H@L��@^�R@s33@�  @��@��@�z�@��R@��@��@��@�Q�@�G�@�=q@�33@��H@�G�@�@�{@�A   A�A��A(�A�RA�\A
=A(�A ��A$z�A'
=A*=qA.�RA3�
A8Q�A=p�A@��ADz�AG�AL��AQ�AW
=AZ�HA^{Ab�\AeAj=qAp  Au�AxQ�A{�A\)A��A�z�A�
=A�G�A��A�p�A�\)A�G�A��A�{A�  A��HA�z�A��RA���A��\A��A�  A�=qA�z�A�ffA�Q�A�=qA�(�A��RA�G�A��
A�ffA���A��HA�z�AƸRAȣ�A��HA�AУ�A�33A�p�A׮Aٙ�A��
A�A�  A��HA��A�A��A�z�A�ffA��A��A�z�A�
=A���A��
A�ffB   B ��B�B
=Bz�BB�HB�B��B	��B
�RB  B�BffB�B��BB�\B\)B��B�B
=BQ�B��B�\B�Bz�Bp�B�\B�
B!�B"ffB#�B$��B%��B&�\B'�B(��B)B+
=B,(�B-p�B.ffB/�B0��B1B2�\B3�B4z�B5��B6�\B7�
B8��B:=qB;\)B<  B=�B=�B>�HB?�
BA�BB=qBC\)BDz�BE��BFffBG\)BHz�BIG�BJ=qBK
=BL  BMG�BNffBO�BPz�BQ��BRffBS33BT  BT��BUBV�\BW�BX��BYBZ�RB[�
B\��B]��B^�\B_\)B`(�B`��BaBbffBc33Bd(�Be�Be�Bf�HBg�
Bh��Bi�Bj�RBk33Bl(�Bl��Bm��BnffBo�BpQ�BqG�Br=qBs33Bt(�Bt��BuBv�\Bw�BxQ�Byp�BzffB{\)B|Q�B}�B~{B
=B�  B��\B���B��B�  B�z�B�
=B��B�{B���B��B��B�=qB��RB�33B�B�=qB��HB�p�B�{B���B��B��B�Q�B��HB��B�{B��RB�G�B��B�z�B��B��B�ffB��HB���B�=qB��HB��B�{B��RB�p�B�{B���B�p�B�{B���B�\)B��B���B�G�B��B��\B��B�B�ffB���B��B�=qB���B�p�B�  B���B�G�B��B���B�G�B��B��\B�33B��
B��\B�33B��B���B�33B��
B�ffB���B��B�(�B��HB��B�=qB��HB��B�ffB�
=B��B�=qB���B�p�B�  B���B�G�B�  B��RB�\)B�=qB���B��B�(�B���B�\)B�  B���B�p�B�(�B��HBÙ�B�=qB��HB�p�B�{B���B�p�B�=qB��HBɮB�ffB���B˙�B�(�B���BͅB�Q�B���B�B�z�B�33B�B�Q�B���Bә�B�ffB�33B�  B֣�B�\)B�  B؏\B�33B��BڸRB�p�B�=qB���B݅B�(�B���B߅B�Q�B�
=B��
B�Q�B��HB㙚B�ffB��B�  B��B�33B��
B�ffB��B�B�\B�G�B�{B�RB�G�B��
B�\B�\)B�(�B�RB�33B��
B��B�p�B�(�B��RB�G�B��B���B��B�=qB��HB�p�B�  B��RB���B�Q�B��HB��B�{B��HB��C 33C z�C C�C�C�HC=qC��C�C33C�C��CQ�C��C�HCG�C�RC
=C\)C��C��Cp�CC
=CffCC	(�C	�C	C
33C
��C
�HC33C�\C
=CG�C�\C  CffC�C�C\)C��C{C\)C��C(�Cz�C�RC33C�\C�
C(�C�\C�C�C�C�CG�C�C�
CG�C��C�HC(�C��C��C33C�\C  C=qC�\C  C=qC��C  C=qC��C
=C=qC��C
=CQ�C��C{C\)C��C
=CffC��C
=C\)C��C 
=C \)C ��C!�C!Q�C!��C"�C"\)C"�C#�C#ffC#��C${C$ffC$��C%
=C%p�C%�C%�C&Q�C&�RC'  C'33C'��C(  C(=qC(�\C(��C)G�C)�C)�C*Q�C*�C*�C+Q�C+�C+�C,G�C,�C,�C-=qC-�C-��C.=qC.�C.��C/=qC/�\C0  C033C0��C0��C133C1��C1��C2=qC2�C2�HC3=qC3��C3�HC4=qC4��C4�C5=qC5�C5�C633C6��C6��C733C7�C7�HC8Q�C8��C8��C9\)C9�\C:
=C:Q�C:��C;{C;=qC;�C;��C<Q�C<�C<�C=\)C=�C=��C>ffC>��C?  C?Q�C?��C@
=C@G�C@��CA
=CA=qCA��CB  CB=qCB�RCC  CC=qCC�RCC��CDG�CD�RCD�CEQ�CE�RCE�CFQ�CF�RCF��CGG�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                             ?�  @   @B�\@�  @��R@��R@�G�A   A  A ��A,(�A@  A_\)A\)A�  A��A��A��AϮA�  A�  B   B(�B(�B�
B   B(  B/�
B7�
B?�
BG�
BP  BXQ�B`  Bg�
Bp  Bw�
B�
B��B�  B��
B��
B�  B��
B�  B�(�B�(�B�(�B�{B�  B��B�(�B�{B��B�  B�  B��B��
B�B��B�{B�{B�=qB�(�B�{B�(�B�(�B�=qB�(�B��C��C  C{C{C
  C
=C
=C��C�C��C  C�C�HC�HC�C�C!��C#��C&  C(
=C*  C,  C.  C0  C2  C4  C6
=C8
=C9��C<  C>{C?��CA��CD{CF{CH  CJ  CL{CN
=CP  CR  CT  CV
=CX
=CZ{C\{C^{C_�Cb  Cc��Ce��Ch
=Ci��Ck�Cm�Cp
=Cr
=Ct  Cv  Cw��Cy��C|
=C~  C��C���C�  C���C�  C���C���C���C�  C�C���C���C�  C�  C���C�  C�
=C�C�  C���C���C�  C�
=C�  C���C�  C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�  C�C�C���C���C�  C�
=C�C�C�C�  C���C���C�  C�C�C�  C���C���C�C�
=C�C�  C�  C�
=C���C���C�  C�C�C�  C���C�  C�  C�  C���C���C�  C�  C�  C���C���C�C�  C���C���C�  C���C�  C�  C���C�C���C�C�  C�  C�  C���C�  C�C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C���C�  C���C�  C�C�  C�C�C�C�  C�C�  C���C���C���C�C�C�C�D �D � D  D� D��Dz�D��D}qD�qD� D�D}qD�qD}qD�qD��DD��D	�D	}qD	�qD
� D
��DxRD�RDz�D�D}qD�qD}qD  D��D�qD� D  D}qD  D��D  Dz�D�qD��D��D}qD�qD}qD  D� DD�D  D� D�D�D�D}qD�qD��D  Dz�D��D}qD��Dz�D   D � D ��D!xRD!�qD"}qD#  D#��D$  D$� D$�qD%z�D&  D&� D'  D'� D(  D(��D)�D)� D*  D*�D+  D+}qD,  D,� D-�D-}qD-�qD.� D/  D/� D0  D0}qD0��D1z�D1��D2z�D2�qD3� D3�qD4}qD4�qD5}qD6  D6��D7�D7��D8�D8� D9  D9��D:  D:}qD;  D;}qD<  D<��D=  D=}qD>  D>� D>�qD?}qD@�D@��DA  DA��DB  DB}qDB�qDCz�DC�qDD��DEDE� DF  DF��DG  DG}qDH  DH� DIDIz�DI��DJ� DK  DK}qDK�qDL� DM�DM�DM�qDN� DO�DO}qDP  DP��DQ�DQ� DQ�qDR� DS  DS��DT�DT}qDU  DU��DU�qDV}qDW�DW� DW�qDX� DY  DY� DZ  DZz�DZ�qD[� D\  D\}qD\�qD]� D^�D^��D_  D_z�D`  D`��D`�qDaz�Db  Db��Db�qDc� Dd  Dd��De  De� De��Dfz�Df�qDg��Dh�Dh��Di�Di��Di�RDjz�Dj�qDkz�Dk�RDl}qDm  Dm� Dn�Dn��DoDo�Dp  Dp}qDp�qDq}qDq�qDr� DsDs��DtDt� Dt��Du}qDv�Dv��Dv�qDw}qDx  Dx��Dy  Dy}qDz  Dz��D{�D{}qD{�qD|z�D|�RD}}qD~�D~��D  D��D�  D�<)D�|)D��qD�  D�@ D�|)D�� D��D�B�D���D��HD�HD�@ D�� D��HD�HD�AHD��HD��HD��D�B�D��HD��qD��qD�>�D��HD��HD��qD�<)D�~�D�� D�HD�>�D�~�D��HD�HD�AHD��HD��HD���D�>�D�~�D��qD�  D�=qD�}qD�� D���D�>�D�}qD��qD��qD�>�D�� D�� D���D�@ D�� D���D�  D�B�D�� D�� D�  D�@ D��HD��HD�  D�AHD��HD�� D���D�>�D��HD��HD�HD�AHD��HD�� D�  D�>�D�� D��HD�  D�>�D�� D���D�  D�AHD�� D�� D�HD�>�D�� D���D���D�@ D�� D���D�  D�@ D�}qD��qD�  D�@ D�}qD���D�HD�AHD��HD��HD�HD�@ D�~�D���D��qD�>�D�~�D�� D�  D�>�D�}qD��qD���D�@ D�� D��HD�  D�=qD�~�D�� D���D�=qD�~�D��HD�HD�@ D�~�D���D�  D�>�D�� D�� D�  D�>�D�� D��HD��D�>�D�~�D��HD�HD�@ D�� D�� D�  D�AHD��HD��HD�  D�AHD��HD��HD�  D�@ D�� D��qD��qD�=qD�}qD��qD��qD�>�D�~�D�� D�HD�AHD��HD�� D���D�<)D�� D��HD�  D�@ D���D��HD�  D�>�D�~�D�� D�  D�AHD�� D�� D�  D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�� D�D��D�B�D�� D���D���D�@ D��HD�� D�  D�>�D�}qD���D��D�B�D���D��HD�  D�>�D�~�D��qD���D�>�D�}qD��qD���D�AHD�� D�� D�  D�>�D�}qD���D�  D�>�D��HD�� D��qD�>�D�� D���D�  D�AHD���D��HD�  D�AHD�~�D�� D��D�B�D��HD��HD�HD�@ D D�� D�  D�@ DÂ�D�D���D�>�DāHD��HD�  D�=qD�~�D�D��D�@ D�}qDƼ)D��qD�@ DǁHD�� D�  D�@ DȁHD�� D���D�AHDɂ�D��HD�HD�@ D�}qDʾ�D�  D�=qD�}qD˽qD���D�@ D̀ D��HD�HD�@ D́HD��HD�  D�@ D�}qDξ�D�  D�@ D�~�D�� D�  D�@ DЂ�D���D�HD�>�Dр D��HD�  D�@ DҀ DҽqD���D�AHDӁHD�� D�  D�AHD�~�D�� D�  D�@ D�~�Dվ�D�  D�AHDր D��HD�HD�@ DׁHD׾�D�  D�@ D�~�D�� D�  D�>�DفHD�� D���D�AHDڀ Dھ�D�HD�@ D�~�D�� D�  D�>�D܀ D�� D�  D�AHD݂�D�� D��qD�>�Dހ D�� D�HD�>�D�~�D�D��D�AHD�� DྸD���D�@ D� DᾸD�HD�@ D�~�D��HD�HD�>�D� D��HD��qD�<)D�~�D侸D�HD�B�D�HD徸D��qD�=qD�~�D���D��D�AHD炏D��HD�  D�@ D肏D���D�  D�@ D� D�� D���D�>�D�~�D��HD���D�@ D�~�D�� D�HD�AHD� D�qD�  D�@ D�}qD�� D�  D�@ D�HD�� D�HD�@ D�~�D�� D�  D�>�D�~�D��HD�  D�>�D� D��HD��D�B�D� D�D���D�=qD�~�D�� D���D�=qD� D�� D��D�@ D�~�D��HD��D�AHD�~�D�� D�  D�AHD�� D�� D���D�@ D���D��HD�HD�=qD�}qD�� D�  D�AHG�O�?�?L��?��?��R?�33?��?��H@��@z�@&ff@.{@:�H@L��@^�R@s33@�  @��@��@�z�@��R@��@��@��@�Q�@�G�@�=q@�33@��H@�G�@�@�{@�A   A�A��A(�A�RA�\A
=A(�A ��A$z�A'
=A*=qA.�RA3�
A8Q�A=p�A@��ADz�AG�AL��AQ�AW
=AZ�HA^{Ab�\AeAj=qAp  Au�AxQ�A{�A\)A��A�z�A�
=A�G�A��A�p�A�\)A�G�A��A�{A�  A��HA�z�A��RA���A��\A��A�  A�=qA�z�A�ffA�Q�A�=qA�(�A��RA�G�A��
A�ffA���A��HA�z�AƸRAȣ�A��HA�AУ�A�33A�p�A׮Aٙ�A��
A�A�  A��HA��A�A��A�z�A�ffA��A��A�z�A�
=A���A��
A�ffB   B ��B�B
=Bz�BB�HB�B��B	��B
�RB  B�BffB�B��BB�\B\)B��B�B
=BQ�B��B�\B�Bz�Bp�B�\B�
B!�B"ffB#�B$��B%��B&�\B'�B(��B)B+
=B,(�B-p�B.ffB/�B0��B1B2�\B3�B4z�B5��B6�\B7�
B8��B:=qB;\)B<  B=�B=�B>�HB?�
BA�BB=qBC\)BDz�BE��BFffBG\)BHz�BIG�BJ=qBK
=BL  BMG�BNffBO�BPz�BQ��BRffBS33BT  BT��BUBV�\BW�BX��BYBZ�RB[�
B\��B]��B^�\B_\)B`(�B`��BaBbffBc33Bd(�Be�Be�Bf�HBg�
Bh��Bi�Bj�RBk33Bl(�Bl��Bm��BnffBo�BpQ�BqG�Br=qBs33Bt(�Bt��BuBv�\Bw�BxQ�Byp�BzffB{\)B|Q�B}�B~{B
=B�  B��\B���B��B�  B�z�B�
=B��B�{B���B��B��B�=qB��RB�33B�B�=qB��HB�p�B�{B���B��B��B�Q�B��HB��B�{B��RB�G�B��B�z�B��B��B�ffB��HB���B�=qB��HB��B�{B��RB�p�B�{B���B�p�B�{B���B�\)B��B���B�G�B��B��\B��B�B�ffB���B��B�=qB���B�p�B�  B���B�G�B��B���B�G�B��B��\B�33B��
B��\B�33B��B���B�33B��
B�ffB���B��B�(�B��HB��B�=qB��HB��B�ffB�
=B��B�=qB���B�p�B�  B���B�G�B�  B��RB�\)B�=qB���B��B�(�B���B�\)B�  B���B�p�B�(�B��HBÙ�B�=qB��HB�p�B�{B���B�p�B�=qB��HBɮB�ffB���B˙�B�(�B���BͅB�Q�B���B�B�z�B�33B�B�Q�B���Bә�B�ffB�33B�  B֣�B�\)B�  B؏\B�33B��BڸRB�p�B�=qB���B݅B�(�B���B߅B�Q�B�
=B��
B�Q�B��HB㙚B�ffB��B�  B��B�33B��
B�ffB��B�B�\B�G�B�{B�RB�G�B��
B�\B�\)B�(�B�RB�33B��
B��B�p�B�(�B��RB�G�B��B���B��B�=qB��HB�p�B�  B��RB���B�Q�B��HB��B�{B��HB��C 33C z�C C�C�C�HC=qC��C�C33C�C��CQ�C��C�HCG�C�RC
=C\)C��C��Cp�CC
=CffCC	(�C	�C	C
33C
��C
�HC33C�\C
=CG�C�\C  CffC�C�C\)C��C{C\)C��C(�Cz�C�RC33C�\C�
C(�C�\C�C�C�C�CG�C�C�
CG�C��C�HC(�C��C��C33C�\C  C=qC�\C  C=qC��C  C=qC��C
=C=qC��C
=CQ�C��C{C\)C��C
=CffC��C
=C\)C��C 
=C \)C ��C!�C!Q�C!��C"�C"\)C"�C#�C#ffC#��C${C$ffC$��C%
=C%p�C%�C%�C&Q�C&�RC'  C'33C'��C(  C(=qC(�\C(��C)G�C)�C)�C*Q�C*�C*�C+Q�C+�C+�C,G�C,�C,�C-=qC-�C-��C.=qC.�C.��C/=qC/�\C0  C033C0��C0��C133C1��C1��C2=qC2�C2�HC3=qC3��C3�HC4=qC4��C4�C5=qC5�C5�C633C6��C6��C733C7�C7�HC8Q�C8��C8��C9\)C9�\C:
=C:Q�C:��C;{C;=qC;�C;��C<Q�C<�C<�C=\)C=�C=��C>ffC>��C?  C?Q�C?��C@
=C@G�C@��CA
=CA=qCA��CB  CB=qCB�RCC  CC=qCC�RCC��CDG�CD�RCD�CEQ�CE�RCE�CFQ�CF�RCF��CGG�CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                             @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A�x�A�x�A�z�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�|�A�x�A�$�A�I�A�M�A�1A�ffA���A�A��A�l�A��A�p�A���A�n�A�5?A�ffA�A�G�A�XA���A�G�A��A�ffA��9A���A��A��A�7LA��RA�p�A�bA���A�E�A�E�A���A��#A�XA���A�1'A���A�A�A�oA���A�n�A��#A���A�A�9XA���A��A�&�A��A��A�r�A�E�A�ĜA���A�l�A��A��#A���A��A��A��A���A�A�A��yA�;dA��A`BA~A{t�Ay�Axv�Aw��AuhsArA�Ao��An{Al��Ak�-Ai?}Af��Af �Ae7LAdv�Ac�^Ab��A`  A^�A\�!A\-A[�AZE�AX��AV�AU��AU��AU`BAU�AT��AT��AS�AS|�AS&�ARz�ARE�AR9XAR1'ARbAQ|�AO��AN��AM/AK��AI+AGVAF��AFA�AE��AD$�ACK�AA�#AA|�A@^5A?A>bNA>�A=�
A=|�A<��A;�A:�DA8�A8�9A8�!A8��A8bNA6�`A5�TA5K�A4��A4ZA3hsA2��A2ZA1ƨA1`BA0n�A/A.��A-��A,�!A,bNA+�#A+A* �A)A)�-A)��A)VA't�A'
=A%A%hsA$�yA$�RA$�A#��A#VA"�RA"I�A!ƨA   A|�AS�AĜA��A�At�A�Av�A��A�uA�AA�PAO�A7LA��A�FAA��A�!A�DA$�Ap�A�HA=qA|�A�DA�;A��A�AG�A��A  A
�9A	��A	p�A�AK�A��AE�A�A�A�9AffA��A&�A(�A�AAC�A ~�@�C�@��@��@�O�@�Q�@��@��P@�M�@��@� �@��@�o@�`B@��@�V@�V@��;@�ȴ@��#@�x�@�7L@��@�  @�@��H@�7@ߍP@�@�hs@�/@�Z@�\)@�n�@�hs@� �@�C�@�/@��T@Ѓ@�Q�@��@�+@�p�@˕�@�5?@�`B@�Ĝ@ǶF@���@�$�@�`B@å�@��@+@�-@��7@�Ĝ@�K�@�v�@�5?@���@�%@���@��^@�9X@��F@���@�{@��@�G�@��/@���@�E�@��h@�Q�@���@�V@�O�@�Z@�ƨ@�ȴ@���@��@��u@�9X@���@�o@�@��j@��@�K�@��R@���@��`@���@�t�@�;d@��@�{@���@�V@��@�j@�I�@�  @���@�;d@�@��\@���@��@���@�Z@�b@�l�@��!@�~�@��@���@��-@�O�@��@���@�Z@���@�"�@��@�ȴ@���@�v�@�J@���@���@�p�@�7L@�Ĝ@�9X@���@���@�;d@�@���@�^5@��#@�X@�%@��`@��@�I�@��m@�;d@��\@�~�@�E�@��7@��/@�r�@�A�@�(�@�b@�b@���@��w@��P@�C�@���@�-@���@��T@���@�`B@�&�@��@��`@��D@�  @���@�dZ@���@�-@���@���@�@��^@��h@��7@�x�@�`B@�&�@�%@���@���@���@���@�b@}�T@}V@|z�@|(�@{�m@{dZ@{C�@{33@z�@z~�@zJ@yx�@y&�@y�@x��@xA�@w��@w+@v�y@vȴ@v�+@v@u@u�@u�@tj@t9X@t(�@sƨ@r-@q�^@q��@qhs@q7L@p��@pQ�@p1'@p �@pb@o�w@o�@o|�@o
=@n�+@n$�@m@m��@m��@m�-@mp�@mO�@m�@m/@l��@l9X@k33@j��@j�\@j-@i��@i��@ix�@iX@i&�@i�@h�`@hbN@g�@g��@g�@g|�@g\)@g
=@f�@f�R@fv�@f�+@fv�@fff@fV@e�T@ep�@d�@d�j@dz�@c��@c��@c@bM�@a��@a��@a�^@a�^@a�^@a7L@`�`@`Ĝ@`Ĝ@`��@`r�@` �@_��@_;d@^�@^E�@]?}@]/@]?}@]�@\�j@\z�@\�@[�
@[ƨ@[��@[S�@Z�!@Zn�@Z=q@ZJ@Y�@Y��@Y�7@Y7L@Y%@XĜ@XQ�@Xb@W�;@W�@W
=@V�+@V{@U�@U�T@U�@TI�@T�@S��@Sƨ@SS�@R��@R=q@R-@R�@Q�#@Q��@Q��@Qx�@Q�@P�@Pb@O��@O�@N�R@N�+@M�T@M�-@M�h@M/@L��@L��@L�@LZ@L(�@L�@K�m@K�F@K�@K33@K@J�H@JM�@J�@JJ@I��@I�#@IG�@I�@H�`@H�@HA�@Hb@G��@G��@Gl�@GK�@G;d@G�@F�R@Fv�@FE�@F{@E��@E`B@D�/@D�j@DZ@C��@Cƨ@C��@C�@C"�@B��@B~�@B=q@A�#@AX@A&�@A%@@r�@@b@?�;@?��@?l�@?;d@?�@>��@>$�@=�-@=p�@=?}@=�@<��@<��@<1@;"�@:�@:�@:�H@:��@:��@:n�@:=q@9�^@9��@9�7@9�7@9hs@9G�@9G�@97L@9G�@9G�@9&�@8��@8�`@8��@8Ĝ@8Ĝ@8�9@8��@8 �@7�;@7�@7l�@7�@6�@6�+@6@5�T@5@5�-@5�h@5`B@5/@4�@4��@4�@4�D@4I�@4�@3ƨ@3��@3��@3t�@3C�@3@2��@2��@2n�@2=q@2J@1��@1x�@1X@17L@1�@0��@0��@0�u@0r�@0A�@0 �@/�w@/��@/+@.��@.{@.@.{@-��@-?}@,��@,��@,z�@+�
@+�@+t�@+C�@*��@*~�@*n�@*^5@*=q@)��@)�#@)��@)�@(�`@(��@(Ĝ@(�9@(�@(1'@'�@'�w@'l�@'+@&�y@&��@&E�@&5?@&$�@&$�@%�@%�h@%p�@%?}@%�@%�@$�@$(�@#�
@#ƨ@#ƨ@#�F@#S�@#o@"�H@"^5@"J@!��@!��@!X@ �`@ Ĝ@ �9@ ��@ �u@��@�P@|�@l�@\)@K�@
=@��@ff@�@�@�T@��@@�-@`B@�@��@j@I�@�@��@t�@C�@"�@�@��@�\@^5@M�@J@��@�7@x�@x�@X@&�@%@Ĝ@b@��@|�@\)@\)@K�@K�@;d@+@
=@��@v�@V@E�@$�@$�@@�T@@�-@�h@`B@�@��@�D@j@Z@(�@�m@�
@ƨ@dZ@o@@�@��@n�@-@��@��@�7@&�@�`@��@��@r�@ �@  @�;@�@��@�P@l�@�@�y@��@v�@$�@�@@��@�@`B@O�@?}@V@�/@�j@�@��@I�@�@�m@ƨ@�F@��@��@dZ@o@
�H@
��@
��@
�!@
n�@
M�@
-@
�@
J@	��@	��@	��@	hs@	7L@	&�@	&�@	�@�9@r�@bN@bN@Q�@Q�@ �@�;@�w@�w@�w@�@l�@;d@;d@�@��@�@�@ȴ@��@v�@ff@@�@@{@{@{@@��@�@O�@V@�/@�/@��@��@�@�D@z�@z�@z�@I�@9X@�@��@ƨ@ƨ@��@��@�@dZ@S�@33@�H@��@��@��A�v�A�v�A�x�A�z�A�v�A�v�A�x�A�z�A�z�A�x�A�x�A�t�A�v�A�x�A�z�A�~�A�z�A�z�A�z�A�z�A�|�A�|�A�z�A�z�A�z�A�|�A�~�A��A��A�~�A�~�A�|�A�|�A��A��A��A�~�A�|�A�|�A�|�A�~�A��A��A�~�A�|�A�|�A�~�A��A��A��A�|�A�|�A�~�A��A��A��A��A�~�A�~�A�~�A��A��A��A��A�~�A�~�A��A��A��A��A��A�~�A�~�A�~�A�~�A��A��A��A��A�~�A�~�A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A�~�A�~�A��A��A��A��A��A��A��A�~�A�|�A�~�A�~�A�~�A��A��A��A�~�A�|�A�|�A�~�A��A��A��A��A�~�A�|�A�~�A��A��A��A��A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��+A��+A��+A��A��A��A��A�~�A��A��A��A��+A��A��A�~�A�~�A�|�A�~�A�~�A��A��A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A��A�~�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�z�A�z�A�|�A�z�A�z�A�v�A�t�A�r�A�r�A�v�A�z�A�v�A�hsA�Q�A�E�A�E�A�A�A�%A��A��yA���A��A�-A���A��;A�r�A�I�A�K�A�$�A�|�A�1A�-A��/A�x�A�oA���A�v�A�+A���A��A�K�A��yA�z�A�O�A�  A��RA�dZA�oA���A���A���A�K�A� �A���A���A���A�~�A�hsA�O�A�=qA�1'A�bA���A��A�ȴA���A���A��hA��DA��A�v�A�bNA�/A�A��DA�?}A�bA��mA���A��jA��!A��A��PA�G�A��A���A�ĜA���A�VA�=qA���A��A��yA��
A�ȴA��wA��A���A���A��+A��A�|�A�S�A�%A��FA�~�A�+A��A��A���A��-A���A�|�A�\)A�/A���A��
A�~�A��A��-A��hA�z�A�n�A�\)A�I�A�A�A�;dA�7LA�(�A�A��A��A�%A���A��A��#A��jA���A�~�A�jA�\)A�S�A�I�A�A�A�?}A�9XA�&�A��A�  A��A��A�ĜA��9A���A��A�XA�/A���A���A�ƨA��wA��RA���A�z�A�VA�&�A��TA��wA��A���A���A��\A�|�A�t�A�ffA�?}A��A�%A��A��
A��RA���A�z�A�O�A�5?A���A��`A���A�ȴA���A��wA��9A���A��+A��A�|�A�r�A�\)A�G�A�9XA� �A��A�  A��A��A��yA��mA��TA�A��9A���A�t�A�dZA�XA�O�A�1'A���A��hA�r�A�Q�A�-A���A��A��A���A���A�x�A�\)A�-A�A��A�ȴA��A���A��uA��A�ffA�9XA� �A�oA�1A���A��A���A���A�t�A�O�A�E�A�9XA�(�A��A���A���A��wA��PA�z�A�hsA�`BA�XA�I�A�=qA�(�A��A��A��A��A�oA�JA���A��;A���A�ĜA��9A���A��PA��A��A�r�A�VA�1'A�VA��A��#A�A��wA��FA��A���A���A��hA��A�n�A�E�A��A��!A���A�p�A�K�A��A�A�/A���A�\)A�A�A�1'A��A��yA��\A�G�A���A��#A�ȴA���A��A�r�A�VA�7LA��A� �A��A��A��A���A��A��mA��A��TA��;A��jA��!A��A���A���A��\A��A��A�x�A�`BA�XA�S�A�O�A�G�A�C�A�G�A�G�A�(�A�bA��`A�ĜA��-A���A�`BA�-A��A��A�n�A�Q�A��A��9A�hsA�^5A�XA�M�A�;dA��A�%A���A��;A�ƨA���A�bNA��HA���A�ƨA��A�9XA��mA��!A���A��hA�|�A�5?A��A�1A���A��A��A��9A�z�A�/A�1A���A��TA���A�z�A� �A�1A�A���A��yA���A��wA���A��7A�x�A�ffA�O�A�G�A�;dA�7LA�-A�$�A�{A���A��A��TA���A���A�\)A�A�A�;dA�&�A��A�VA�1A�A�A�A�;A��A��AA|�A&�A~��A~��A~��A~z�A~$�A}��A}\)A}oA|��A{��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                             A�x�A�x�A�x�A�z�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�|�A�x�A�$�A�I�A�M�A�1A�ffA���A�A��A�l�A��A�p�A���A�n�A�5?A�ffA�A�G�A�XA���A�G�A��A�ffA��9A���A��A��A�7LA��RA�p�A�bA���A�E�A�E�A���A��#A�XA���A�1'A���A�A�A�oA���A�n�A��#A���A�A�9XA���A��A�&�A��A��A�r�A�E�A�ĜA���A�l�A��A��#A���A��A��A��A���A�A�A��yA�;dA��A`BA~A{t�Ay�Axv�Aw��AuhsArA�Ao��An{Al��Ak�-Ai?}Af��Af �Ae7LAdv�Ac�^Ab��A`  A^�A\�!A\-A[�AZE�AX��AV�AU��AU��AU`BAU�AT��AT��AS�AS|�AS&�ARz�ARE�AR9XAR1'ARbAQ|�AO��AN��AM/AK��AI+AGVAF��AFA�AE��AD$�ACK�AA�#AA|�A@^5A?A>bNA>�A=�
A=|�A<��A;�A:�DA8�A8�9A8�!A8��A8bNA6�`A5�TA5K�A4��A4ZA3hsA2��A2ZA1ƨA1`BA0n�A/A.��A-��A,�!A,bNA+�#A+A* �A)A)�-A)��A)VA't�A'
=A%A%hsA$�yA$�RA$�A#��A#VA"�RA"I�A!ƨA   A|�AS�AĜA��A�At�A�Av�A��A�uA�AA�PAO�A7LA��A�FAA��A�!A�DA$�Ap�A�HA=qA|�A�DA�;A��A�AG�A��A  A
�9A	��A	p�A�AK�A��AE�A�A�A�9AffA��A&�A(�A�AAC�A ~�@�C�@��@��@�O�@�Q�@��@��P@�M�@��@� �@��@�o@�`B@��@�V@�V@��;@�ȴ@��#@�x�@�7L@��@�  @�@��H@�7@ߍP@�@�hs@�/@�Z@�\)@�n�@�hs@� �@�C�@�/@��T@Ѓ@�Q�@��@�+@�p�@˕�@�5?@�`B@�Ĝ@ǶF@���@�$�@�`B@å�@��@+@�-@��7@�Ĝ@�K�@�v�@�5?@���@�%@���@��^@�9X@��F@���@�{@��@�G�@��/@���@�E�@��h@�Q�@���@�V@�O�@�Z@�ƨ@�ȴ@���@��@��u@�9X@���@�o@�@��j@��@�K�@��R@���@��`@���@�t�@�;d@��@�{@���@�V@��@�j@�I�@�  @���@�;d@�@��\@���@��@���@�Z@�b@�l�@��!@�~�@��@���@��-@�O�@��@���@�Z@���@�"�@��@�ȴ@���@�v�@�J@���@���@�p�@�7L@�Ĝ@�9X@���@���@�;d@�@���@�^5@��#@�X@�%@��`@��@�I�@��m@�;d@��\@�~�@�E�@��7@��/@�r�@�A�@�(�@�b@�b@���@��w@��P@�C�@���@�-@���@��T@���@�`B@�&�@��@��`@��D@�  @���@�dZ@���@�-@���@���@�@��^@��h@��7@�x�@�`B@�&�@�%@���@���@���@���@�b@}�T@}V@|z�@|(�@{�m@{dZ@{C�@{33@z�@z~�@zJ@yx�@y&�@y�@x��@xA�@w��@w+@v�y@vȴ@v�+@v@u@u�@u�@tj@t9X@t(�@sƨ@r-@q�^@q��@qhs@q7L@p��@pQ�@p1'@p �@pb@o�w@o�@o|�@o
=@n�+@n$�@m@m��@m��@m�-@mp�@mO�@m�@m/@l��@l9X@k33@j��@j�\@j-@i��@i��@ix�@iX@i&�@i�@h�`@hbN@g�@g��@g�@g|�@g\)@g
=@f�@f�R@fv�@f�+@fv�@fff@fV@e�T@ep�@d�@d�j@dz�@c��@c��@c@bM�@a��@a��@a�^@a�^@a�^@a7L@`�`@`Ĝ@`Ĝ@`��@`r�@` �@_��@_;d@^�@^E�@]?}@]/@]?}@]�@\�j@\z�@\�@[�
@[ƨ@[��@[S�@Z�!@Zn�@Z=q@ZJ@Y�@Y��@Y�7@Y7L@Y%@XĜ@XQ�@Xb@W�;@W�@W
=@V�+@V{@U�@U�T@U�@TI�@T�@S��@Sƨ@SS�@R��@R=q@R-@R�@Q�#@Q��@Q��@Qx�@Q�@P�@Pb@O��@O�@N�R@N�+@M�T@M�-@M�h@M/@L��@L��@L�@LZ@L(�@L�@K�m@K�F@K�@K33@K@J�H@JM�@J�@JJ@I��@I�#@IG�@I�@H�`@H�@HA�@Hb@G��@G��@Gl�@GK�@G;d@G�@F�R@Fv�@FE�@F{@E��@E`B@D�/@D�j@DZ@C��@Cƨ@C��@C�@C"�@B��@B~�@B=q@A�#@AX@A&�@A%@@r�@@b@?�;@?��@?l�@?;d@?�@>��@>$�@=�-@=p�@=?}@=�@<��@<��@<1@;"�@:�@:�@:�H@:��@:��@:n�@:=q@9�^@9��@9�7@9�7@9hs@9G�@9G�@97L@9G�@9G�@9&�@8��@8�`@8��@8Ĝ@8Ĝ@8�9@8��@8 �@7�;@7�@7l�@7�@6�@6�+@6@5�T@5@5�-@5�h@5`B@5/@4�@4��@4�@4�D@4I�@4�@3ƨ@3��@3��@3t�@3C�@3@2��@2��@2n�@2=q@2J@1��@1x�@1X@17L@1�@0��@0��@0�u@0r�@0A�@0 �@/�w@/��@/+@.��@.{@.@.{@-��@-?}@,��@,��@,z�@+�
@+�@+t�@+C�@*��@*~�@*n�@*^5@*=q@)��@)�#@)��@)�@(�`@(��@(Ĝ@(�9@(�@(1'@'�@'�w@'l�@'+@&�y@&��@&E�@&5?@&$�@&$�@%�@%�h@%p�@%?}@%�@%�@$�@$(�@#�
@#ƨ@#ƨ@#�F@#S�@#o@"�H@"^5@"J@!��@!��@!X@ �`@ Ĝ@ �9@ ��@ �u@��@�P@|�@l�@\)@K�@
=@��@ff@�@�@�T@��@@�-@`B@�@��@j@I�@�@��@t�@C�@"�@�@��@�\@^5@M�@J@��@�7@x�@x�@X@&�@%@Ĝ@b@��@|�@\)@\)@K�@K�@;d@+@
=@��@v�@V@E�@$�@$�@@�T@@�-@�h@`B@�@��@�D@j@Z@(�@�m@�
@ƨ@dZ@o@@�@��@n�@-@��@��@�7@&�@�`@��@��@r�@ �@  @�;@�@��@�P@l�@�@�y@��@v�@$�@�@@��@�@`B@O�@?}@V@�/@�j@�@��@I�@�@�m@ƨ@�F@��@��@dZ@o@
�H@
��@
��@
�!@
n�@
M�@
-@
�@
J@	��@	��@	��@	hs@	7L@	&�@	&�@	�@�9@r�@bN@bN@Q�@Q�@ �@�;@�w@�w@�w@�@l�@;d@;d@�@��@�@�@ȴ@��@v�@ff@@�@@{@{@{@@��@�@O�@V@�/@�/@��@��@�@�D@z�@z�@z�@I�@9X@�@��@ƨ@ƨ@��@��@�@dZ@S�@33@�H@��@��G�O�A�v�A�v�A�x�A�z�A�v�A�v�A�x�A�z�A�z�A�x�A�x�A�t�A�v�A�x�A�z�A�~�A�z�A�z�A�z�A�z�A�|�A�|�A�z�A�z�A�z�A�|�A�~�A��A��A�~�A�~�A�|�A�|�A��A��A��A�~�A�|�A�|�A�|�A�~�A��A��A�~�A�|�A�|�A�~�A��A��A��A�|�A�|�A�~�A��A��A��A��A�~�A�~�A�~�A��A��A��A��A�~�A�~�A��A��A��A��A��A�~�A�~�A�~�A�~�A��A��A��A��A�~�A�~�A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A�~�A�~�A��A��A��A��A��A��A��A�~�A�|�A�~�A�~�A�~�A��A��A��A�~�A�|�A�|�A�~�A��A��A��A��A�~�A�|�A�~�A��A��A��A��A�~�A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��+A��+A��+A��A��A��A��A�~�A��A��A��A��+A��A��A�~�A�~�A�|�A�~�A�~�A��A��A��A��A��A��A��A��A��A�~�A��A��A��A��A��A��A��A��A��A�~�A�|�A�|�A�|�A�~�A�~�A�~�A�~�A�~�A�~�A�|�A�~�A�|�A�z�A�z�A�|�A�z�A�z�A�v�A�t�A�r�A�r�A�v�A�z�A�v�A�hsA�Q�A�E�A�E�A�A�A�%A��A��yA���A��A�-A���A��;A�r�A�I�A�K�A�$�A�|�A�1A�-A��/A�x�A�oA���A�v�A�+A���A��A�K�A��yA�z�A�O�A�  A��RA�dZA�oA���A���A���A�K�A� �A���A���A���A�~�A�hsA�O�A�=qA�1'A�bA���A��A�ȴA���A���A��hA��DA��A�v�A�bNA�/A�A��DA�?}A�bA��mA���A��jA��!A��A��PA�G�A��A���A�ĜA���A�VA�=qA���A��A��yA��
A�ȴA��wA��A���A���A��+A��A�|�A�S�A�%A��FA�~�A�+A��A��A���A��-A���A�|�A�\)A�/A���A��
A�~�A��A��-A��hA�z�A�n�A�\)A�I�A�A�A�;dA�7LA�(�A�A��A��A�%A���A��A��#A��jA���A�~�A�jA�\)A�S�A�I�A�A�A�?}A�9XA�&�A��A�  A��A��A�ĜA��9A���A��A�XA�/A���A���A�ƨA��wA��RA���A�z�A�VA�&�A��TA��wA��A���A���A��\A�|�A�t�A�ffA�?}A��A�%A��A��
A��RA���A�z�A�O�A�5?A���A��`A���A�ȴA���A��wA��9A���A��+A��A�|�A�r�A�\)A�G�A�9XA� �A��A�  A��A��A��yA��mA��TA�A��9A���A�t�A�dZA�XA�O�A�1'A���A��hA�r�A�Q�A�-A���A��A��A���A���A�x�A�\)A�-A�A��A�ȴA��A���A��uA��A�ffA�9XA� �A�oA�1A���A��A���A���A�t�A�O�A�E�A�9XA�(�A��A���A���A��wA��PA�z�A�hsA�`BA�XA�I�A�=qA�(�A��A��A��A��A�oA�JA���A��;A���A�ĜA��9A���A��PA��A��A�r�A�VA�1'A�VA��A��#A�A��wA��FA��A���A���A��hA��A�n�A�E�A��A��!A���A�p�A�K�A��A�A�/A���A�\)A�A�A�1'A��A��yA��\A�G�A���A��#A�ȴA���A��A�r�A�VA�7LA��A� �A��A��A��A���A��A��mA��A��TA��;A��jA��!A��A���A���A��\A��A��A�x�A�`BA�XA�S�A�O�A�G�A�C�A�G�A�G�A�(�A�bA��`A�ĜA��-A���A�`BA�-A��A��A�n�A�Q�A��A��9A�hsA�^5A�XA�M�A�;dA��A�%A���A��;A�ƨA���A�bNA��HA���A�ƨA��A�9XA��mA��!A���A��hA�|�A�5?A��A�1A���A��A��A��9A�z�A�/A�1A���A��TA���A�z�A� �A�1A�A���A��yA���A��wA���A��7A�x�A�ffA�O�A�G�A�;dA�7LA�-A�$�A�{A���A��A��TA���A���A�\)A�A�A�;dA�&�A��A�VA�1A�A�A�A�;A��A��AA|�A&�A~��A~��A~��A~z�A~$�A}��A}\)A}oA|��A{��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�QB�B�B��BٴB�KB�KB�B�BخB�B�gB�yB�B��BרB�sB�
B��B֡B�9B��B՛B�2B��B��B�,B��B�[BҽB��B�HBΥB�^B��B��B�tBjKB>BB1�B �B7B�BoB�B{B��B��B�vB��B�/BΥB�jB�UB��B��B��B��B�B��B�uB��B�uB}�Bx�Bs�Bn/Ba|BWsBL0BC�B<jB2aB,B#�B�B=B�B�B�B�B�B�TB�wB�B�UB�CB��B�-B�B�VB{�Bp�BffBR�BC�B;0B.IB$�BxB�B\B+BB��B�B�)BҽBɆB�'B��B�B�~B�SBzBx8Bd�B\�BVBQNBJ�BF?B:�B/�B%B�B�B�B	7BGB
�xB
��B
��B
��B
��B
�|B
��B
��B
��B
��B
�
B
�B
��B
�`B
��B
ںB
�&B
�pB
�gB
�0B
�B
��B
��B
�zB
��B
�kB
��B
��B
�PB
��B
�B
�B
}�B
z�B
xlB
s�B
n/B
h
B
d�B
dZB
b�B
aB
_pB
W
B
U�B
Q�B
PHB
MB
H�B
F�B
C�B
A B
?B
:�B
5B
3�B
,�B
)�B
(�B
#�B
"�B
B
B
=B
kB
B
\B
�B

=B
1B
+B
B
�B
 �B	��B	�B	�rB	��B	�oB	�;B	�;B	�B	�B	�>B	�B	�fB	��B	�/B	�)B	�QB	ԕB	ӏB	҉B	�NB	�TB	��B	��B	��B	��B	ȴB	�B	�aB	��B	��B	�6B	��B	�RB	��B	��B	�}B	�=B	��B	��B	�tB	�@B	�nB	�OB	�B	�B	��B	��B	��B	�SB	��B	�@B	��B	��B	��B	��B	�JB	��B	��B	��B	�B	��B	��B	� B	cB	{B	~]B	{JB	xlB	x�B	v�B	u�B	u�B	uZB	tB	s�B	r�B	rGB	r�B	p�B	p;B	q�B	qvB	pB	o5B	n/B	ncB	m�B	lWB	l�B	lWB	k�B	k�B	o�B	l�B	k�B	j�B	k�B	k�B	m�B	m)B	l�B	l�B	m�B	m)B	n�B	m�B	q�B	poB	poB	p�B	rB	rB	u�B	s�B	sMB	rGB	rGB	tTB	yrB	y�B	z�B	|B	|�B	}�B	|�B	|�B	�B	�{B	�uB	��B	�7B	�=B	��B	� B	�:B	�B	�$B	�_B	�1B	�7B	�qB	�B	�:B	�B	�$B	��B	��B	��B	��B	��B	�B	��B	��B	�<B	�BB	�UB	�aB	�aB	��B	�B	�tB	�B	ȴB	��B	�pB	� B	�&B	�[B	ԕB	�sB	�#B	��B	�B	�;B	�pB	�B	�B	�TB	�sB	�)B	� B	�B	��B	�GB	��B	��B	�`B	��B	��B	�rB	��B	��B
 �B
�B
�B
YB
fB
	�B
�B
 B
�B
B
B
SB
+B
IB
!bB
!-B
"�B
(�B
-B
/�B
1'B
1�B
2aB
2-B
2�B
4nB
5?B
7�B
<�B
?�B
A B
A�B
D3B
E�B
GB
GEB
HKB
J�B
OB
RTB
S[B
Z�B
]�B
_;B
`vB
`�B
aB
bNB
b�B
c B
c�B
e�B
ffB
f�B
f�B
g�B
h�B
m�B
w2B
z�B
}VB
~�B
.B
��B
�B
�uB
�{B
��B
��B
�=B
�xB
�xB
��B
��B
�:B
�B
��B
�B
�YB
��B
�7B
��B
��B
�B
�nB
��B
��B
�CB
��B
�}B
�B
��B
�UB
��B
��B
��B
�aB
�B
�B
��B
��B
��B
��B
��B
�6B
�jB
�<B
�B
��B
��B
�aB
ĜB
��B
�KB
�KB
��B
��B
̘B
�jB
��B
�pB
�BB
�HB
��B
ԕB
֡B
�?B
רB
خB
�B
�B
�QB
ںB
ںB
چB
چB
��B
�#B
ݘB
�vB
�B
�B
��B
�B
�"B
�]B
�5B
�B
�oB
��B
�B
�AB
�vB
�B
�B
��B
��B
�fB
�B
��B
�xB
�JB
��B
�]B
��B  BBuB{BB%B�B�B	�BB�B�B�B�B�B�B B�B:B@BB{B�B$B�B7B�BkBIB�B�B \B �B"4B$tB%FB%zB%�B&LB&�B&�B'B($B)_B*0B+B,�B.IB.B/�B0UB0�B1�B2-B2aB2�B3�B3�B4B4�B5B5?B5�B6�B6�B8�B8�B8�B8�B8RB9�B9�B:^B;dB;�B<�B=B=�B>BB>�B>�B?B?�BA BB�BC-BC�BD�BD�BE9BF?BGzBG�BHBHBH�BH�BJ#BJ�BK�BLdBL�BMBNpBOBO�BP}BQBQBQBR�BS�BT�BUgBU�BU�BV9BVmBWsBYKBYBYBY�BY�BZQBZ�BZ�B\)B\]B\]B\]B\�B\�B\�B\�B\�B\�B\�B]/B]/B]dB]dB]/B\�B\�B^5B^5B^�B_B_�B`B`�Ba�Ba�BbBbBb�Bb�Bc Bc�Bc�Bc�BdZBd�Bd�Be`Be�Be�Be�Bf�Bf�Bg8Bg�Bg�Bh>Bh>Bh�BiyBiyBi�BjBjBj�BkBkQBk�BkQBlWBk�Bm]Bm�Bo5Bn�Bn�BoiBpBpoBp�BqvBr|Br�Br�BsBtTBt�Bt�BtTBt�Bt�Bu%Bu�Bv�Bv�Bv�Bv�Bw2BwfBw�Bx8Bx�Bx�By>By�BzDBz�Bz�Bz�Bz�B{B{�B|B|PB|PB|B}�B~(B~�B~]B~�B~]B.BcB�B��B�B�oB��B�AB�B�B�B��B��B�B��B��B��B��B��B�SB�B��B��B��B��B��B��B��B��B��B��B�B�B�B�	B�=B�rB��B�B�B�xB��B��B�B��B��B��B��B��B�PB�PB��B��B�\B�\B��B��B��B��B��B��B��B��B��B��B� B� B�4B�hB��B��B��B��B�:B�:B�B�@B�@B�@B��B�B�B�B��B�MB�MB�MB��B�B��B��B��B�$B��B�_B��B��B��B��B�eB��B��B�B�B�B�kB��B�	B�qB��B�CB��B��B��B��B��B��B�IB�~B��B�~B��B�B�OB��B��B�!B�!B�!B��B�'B�\B��B�\B��B��B��B�-B�-B�bB��B�-B��B��B�hB�4B�hB��B�B�nB�:B�:B�nB��B�@B�@B�@B�@B�B��B��B��B��B��B��B��B��B�FB��B��B�LB��B��B��B��B��B��B��B��B��B��B�XB�XB�$B��B��B�$B�XB�XB��B��B�*B�*B�*B��B�_B��B��B��B��B��B��B��B�B�6B�6B�kBچB��BٴB�BچBچB�QB�B�B�B�B�WBںBچB�KB��B�B�QBںB�B��B�BٴB�B�QBٴBخB�yB�yB��BٴB��B��BخB�EB�yB�B�BٴB�KBخB��B�EB�BٴBٴBخBרBרB�B�KB�BخB�EB��BרB�EB�KB�B�EB�sB�
BרB��B�B��B�yB�B�?B�
B�sB�EB�yB��B�yB��B��B��BרB�EB�yB��B�?B֡B�mB֡B�?B��B�B�B�?B֡B�mB��B�9B�mBרB��BרB�?B֡B�B՛B՛B֡B�?B�?B��B�
B��B֡B��B�2B՛B֡B�
B�?B֡B�9B�2B��B՛B�9B�
B�9B��BԕB��B՛B�B�9B�mB՛B՛B�2B�aB��B�2B�2B�mB��B��B��B�,B��B�,B�aB�gB՛B�B�2BԕB��B��BӏB��B��B��B�gB��BԕB�aB��B��B��B�&B��B��B�,B��B��B�aB��B�&B�TB��B�TB҉B��B�&B��B�&B�TBҽB�BбB�BбB��B��BѷBҽB��B��BбB��BϫB�BB�pB�<B�vB�BϫB��B�BBΥB�<B͟B̘B��B�^B�^B��B�#BʌB�XBɆB��B��B�B�KBƨB��B��B��B�wB�B��B��B��B��B�FB�zB�UB�eB�B��B�?B��B�VB��B��B�qBzDB~�B�Bv�BR�BX�BJXBK�BS�BGB<jBGzBCaB9$B>�B5�B7B6�B3hB.}B;0B6FB'�B#B*�B*0B"�B�B!�BxB�BxB7BOBeB_B�B�B�B�B�B�B@B�BB(B"4B(B�B
�B	�B
	B�BB�B	lBYBuB�BGB�BoB�cB�B��B��B��B��B��B�2B��B��B�B�B��B��B��B�%B�B�QB��B�B��B�mB�yB�B��B��BޞB�QB�pBخB�,B��B��BҽB�}B�<B̘B��B��B�aB�B��B��B��B��B�[B�-B�[B�[B�B�6B��B��B��B�$B��B��B�RB��B�B�FB��B�-B��B�hB��B��B��B��B��B�LB��B��B�XB��B��B�RB��B��B��B��B��B�kB��B�kB��B��B��B��B�MB�:B��B��B�"B��B�lB��B��B�MB��B�4B��B�{B~�B~�B}"B~�B�B}"B|�Bv`Bw2BwfBv�Bt�BsMBrBr�Bu�BpoBt�Bo�Bl�BjBiBp�Bn�BkBc�B`Ba�B^BX�BW?BW�BZ�BS&BW
BS�BM�BL�BIRBK�BE�BF?BD3BE�BC�B@�B>B=qB<jB9XB<B>BB:^B5B0UB1'B1'B0�B2-B-wB+6B.�B($B&�B#�B$�B&�B$@B!�B#:B�B�B�B�BOB�B�BkB�B=B+BB�B:BB{B7BB	�B�B	�B%B�B�B�B�B�BB;BSB��BGB�B�8B��B�B��B�]B�sB��B��BѷB�BB�
B�B�BƨB��B��B�mB�^B�B�?B�dB��B��B��B�aB�aB��B��B�OB��B�=B��B�B�B��B��B�B��B�_B��B��B��B�FB��B��B�nB��B��B��B�~B��B�@B��B�=B��B��B�B�4B��B��B��B��B��B|PBt�BuZBv`Bs�Bw2Bp�Bo5Bp;Bo BkBqABh�B\�B\�B`vBjBZ�BT�BPBLdBMBW?BFtBD�BD3BA�BA�BD�BC-BA�B9XB6FB3�B7B:�B0�B+�B&�B(XB'RB)�B$�B&�B#�B�B�B!bB�BB=BBkB�B�BBBB_B7B
�B�B"B
�B
	B	B�B�BYB�B�B{BGB	7B�BB�"B�JB  B�B�"B�rB�B�lB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2020112121342620201121213426IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143120210220011431QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143120210220011431QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164320210325101643IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                