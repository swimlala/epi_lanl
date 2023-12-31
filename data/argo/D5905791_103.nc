CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-24T20:41:39Z creation; 2022-09-06T18:25:47Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20211224204139  20220907192127  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               g   gAA  AOAO7825_008765_103                 7825_008765_103                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @٪5z�@d@٪5z�@d11  @٪5���'@٪5���'@5�/�	@5�/�	�e\�$_�e\�$_11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?�@=p�@�  @��R@��R@�G�A   A  A   A*�HA?\)A`��A\)A��A�Q�A�  A�  A�Q�A�Q�A�  B Q�B  B  BQ�B Q�B(Q�B0Q�B8  B?�BG�BP  BX  B_�Bg�Bo�
Bw�
B�
B��B��B�{B�  B��B�  B�  B��B��
B�  B�{B�(�B�{B��
B�  B�{B��B��B�  B�  B��B�  B�  B�  B�  B�{B�  B�{B�{B��B��
B��C  C  C
=C{C
  C  C
=C
=C
=C��C��C�C�C  C  C   C"  C$
=C&
=C(
=C)��C+�C.  C0  C1��C4  C6
=C8  C9��C<  C>
=C?��CA��CC��CE��CG�CI��CL
=CN  CP  CR  CT
=CV  CW��CZ  C\  C^{C`  Cb  Cd
=Cf
=Ch  Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cx  Cy��C{��C~  C�  C�C�
=C�  C�  C�C�C�C���C���C�C���C���C�  C�  C�  C�C�C�  C�  C�C�  C���C�  C�  C�  C�
=C�\C�  C��C���C�  C�C�
=C�C���C�  C�  C�  C�  C�C���C���C���C���C���C�  C�  C�  C�  C���C�  C�  C���C�C�\C�\C�
=C�  C�  C�  C���C���C���C�  C�C�
=C�
=C���C���C�  C���C���C�
=C�
=C�  C�  C�C�  C���C�  C�  C���C���C�  C���C�  C�
=C�C�  C�  C�  C���C���C�  C���C��C��C��C�  C�C���C�  C�
=C�
=C�  C�  C�
=C�\C�
=C�  C�C���C���C�C�C�C�C�  C�  C�  C�C�
=C�\C�C���C���C���D   D �D  Dz�D�qD}qD�D�D�D� D�D�D�D}qD��D}qD  D��D	�D	� D
�D
� D�D�D�D� D  D� D�qDz�D��D}qD  Dz�D��D� D  D� D  D}qD�qD��DD� D��Dz�D�qD}qD  D}qD  D�D�D��DD}qD��Dz�D�qD� DD� D  D}qD   D ��D!  D!}qD!��D"}qD#�D#��D$  D$� D%  D%� D&  D&� D'  D'� D(�D(��D)  D)� D*  D*� D+�D+}qD+�qD,� D-�D-}qD.�D.�D/�D/� D0  D0��D1  D1� D2�D2�D3D3��D4�D4��D5  D5��D6�D6� D7  D7��D8�D8}qD8�qD9� D:  D:� D;  D;� D<�D<� D=  D=� D>�D>� D>�qD?� D@�D@��DA  DA� DB  DB��DC�DC� DD  DD� DE  DE��DF�DF}qDF�qDG}qDH  DH� DI  DI��DJDJ�DKDK� DL�DL�DMDM��DN  DN}qDO  DO��DP  DP}qDQ  DQ� DQ��DRz�DS  DS��DT�DT� DU  DU}qDU�qDV� DW�DW��DX�DX� DX��DY� DZ�DZ� D[  D[z�D\  D\� D\�qD]��D^D^��D_�D_�D`�D`��Da�Da� Db  Db� Dc�Dc� Dd  Dd}qDd�qDe��Df  Df}qDf�qDg}qDh�Dh� Di  Di}qDj  Dj� Dj�qDk}qDk��Dl� Dm�Dm� Dn  Dn��Do�Do� Dp  Dp��Dp�qDqz�Dr  Dr��Ds  Ds}qDt  Dt� Du�Du� Du�qDv}qDv�qDw��Dx�Dx}qDy  Dy� Dz  Dz��Dz�qD{� D|�D|��D}  D}� D~�D~��D  D� D��D�B�D�� D�� D��D�B�D���D�� D�  D�@ D�~�D���D���D�@ D��HD�D��D�AHD�� D�� D�HD�@ D�� D���D���D�=qD�}qD�� D���D�@ D�~�D���D�HD�AHD�~�D���D���D�>�D��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D��HD��D�AHD�� D�� D�  D�@ D��HD�D�  D�AHD��HD��HD���D�@ D��HD��HD�HD�AHD��HD��HD�HD�B�D�� D���D�  D�@ D�� D��qD���D�@ D�~�D�� D�  D�AHD�~�D�� D�HD�AHD���D��HD�  D�=qD�}qD�� D�HD�@ D�� D���D���D�AHD��HD���D���D�=qD�}qD���D�HD�@ D�� D��HD�HD�B�D���D�D�  D�@ D��HD���D�  D�@ D�� D��HD��D�@ D�~�D�� D�  D�B�D�� D�� D��D�B�D���D�D��D�@ D�~�D���D���D�>�D�� D�� D�HD�AHD��HD�� D�  D�>�D�}qD��qD���D�@ D��HD���D��D�@ D�~�D��HD�HD�@ D�~�D���D��qD�AHD��HD��qD�  D�AHD��HD���D���D�AHD��HD���D�  D�@ D��HD��HD���D�>�D�� D��HD�HD�>�D�� D��HD�  D�AHD��HD���D��qD�>�D�� D��qD��)D�@ D���D��HD�  D�@ D��HD��HD�  D�AHD��HD��qD��)D�=qD�� D��HD�HD�AHD�� D��HD�HD�@ D�~�D��qD��qD�@ D���D��HD�  D�>�D�� D���D���D�@ D�� D�� D���D�>�D�� D��HD�HD�@ D�~�D�� D�  D�>�D�� D�� D���D�@ D�� D���D��qD�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�AHDÀ Dþ�D�  D�@ DĀ Dľ�D�  D�@ D�~�D�� D�HD�=qD�}qDƽqD���D�@ D�~�DǾ�D�HD�@ D�}qD�� D�  D�@ D�~�DɽqD���D�@ DʁHD�� D��qD�>�Dˀ D˽qD��qD�>�D�}qD̾�D�  D�@ D̀ D�� D���D�>�D�~�Dξ�D���D�>�D�}qD�� D��D�>�D�~�D�� D�  D�@ Dр DѾ�D��qD�>�DҁHD��HD�  D�>�DӀ D�� D���D�@ DԁHD�D�  D�>�DՁHD�� D�  D�=qD�}qD־�D�HD�@ D�}qD׽qD��qD�=qD؀ D�� D���D�=qD�~�D��HD��D�B�DځHD�� D���D�@ Dۀ D۾�D��qD�=qD�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD޽qD��qD�@ D߁HD�� D�  D�>�D��HD�D�  D�=qD� DᾸD�  D�@ D�~�D�� D�HD�@ D� D��HD�  D�AHD�HD�� D�  D�AHD�HD�� D���D�>�D� D�� D�HD�@ D�~�D��HD��D�B�D�HD��HD�HD�=qD�}qD龸D��qD�>�D�~�D꾸D���D�@ D낏D��HD���D�AHD� D쾸D�  D�AHD�HD�� D���D�>�D�~�DD�HD�B�D�HD�� D���D�<)D�~�D��HD�HD�@ D�D��HD�  D�>�D�~�D�D���D�>�D�~�D�� D�  D�@ D� D�� D�HD�>�D�~�D�� D���D�>�D�~�D�� D��D�>�D�}qD���D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD�ff?��?8Q�?�  ?�=q?�33?Ǯ?�@�\@��@&ff@8Q�@J=q@Y��@n{@��\@���@��@�(�@��@���@�Q�@��
@���@�Q�@޸R@���@�33A ��A�A
�HA\)AA�HA   A%�A*=qA0��A5A:�HA@��AEAL(�AP��AW
=A[�Ab�\AfffAmp�Aq�Ax��A|��A�=qA�z�A�  A�=qA�{A�  A��A�A�G�A��
A�\)A�G�A��A�
=A��HA�z�A�Q�A��\A�p�A�  A�33A�{A���AÅA�ffAȣ�A��
A�{A�G�A��
A�ffA�G�A��
A�
=AᙚA�z�A�\)A��A���A�\)A�\A���A��A��\A��A��BG�B�RB�
Bp�BffB(�B	�B
�HB  BG�B�RB  BG�B�RB�
Bp�BffB(�B�B
=B�
B��BffB (�B!�B"�HB#�
B%��B&ffB((�B)�B*�HB+�
B-p�B.�\B0(�B1�B2�RB3�B5G�B6=qB8  B8��B:ffB;�B=�B>{B?�B@��BA�BC\)BDz�BEBG
=BHz�BI��BK
=BL(�BMBN�\BPQ�BQ�BR�RBS�BUG�BV=qBX  BX��BZffB[�B\��B^{B_\)B`��BaBc\)BdQ�Be�Bf�HBhQ�Bip�Bj�HBl  BmG�Bn�\Bp  Bp��BrffBs�Bt��Bv=qBw\)Bx��By�B{�B|z�B~{B
=B�Q�B���B���B�(�B��HB�p�B�(�B��RB�\)B�{B��\B�p�B��
B��RB��B�  B�ffB�33B��B�Q�B�
=B��B�Q�B��RB���B�  B��HB�G�B�{B��\B�G�B��B�ffB�G�B��B�z�B��HB�B�=qB��HB��B�{B��HB�G�B�{B��\B�\)B��
B��\B�
=B�B�Q�B���B��B�{B���B�G�B�{B��\B�G�B�B�ffB�
=B�p�B�Q�B��RB�\)B��B��\B�
=B��B�Q�B���B�p�B�B���B���B��B�=qB��HB�p�B�  B���B�
=B��B�Q�B�
=B�p�B�(�B���B�G�B��B�ffB��B��B�Q�B���B��B�{B��RB��B�  B��HB�G�B�  B���B�G�B�(�B£�BÅB�  B���B�p�B�{B��HB�G�B�(�BȸRBɅB�{BʸRB˙�B�{B���B�\)B�=qBθRBϙ�B�{B���BхB�  B��HB�\)B�(�B��HB�p�B�=qBָRBי�B�{B��HBمB�(�B�
=BۅB�ffB�
=BݮB�z�B�
=B��B�z�B�G�B��
B�\B�G�B�B�z�B��HB�p�B�{B�Q�B���B�33B�B�{B�Q�B���B��HB�\)B�B��B�ffB�z�B���B�33B�p�B�  B�{B�\B���B��B�B��B�(�B�Q�B�RB��B�G�B�B��B�Q�B�RB���B�p�B�B��B�Q�B�z�B�
=B��B�B�B�(�B�\B��RB�G�B�\)B��B�{B�z�B���B�
=B��B��B�(�B�Q�B��HB�
=B�p�B�B�  B��\B���B�33B�G�B�B�{B�ffB��HB�
=B��B��B�(�B�z�B���B�33B�p�B�C �C (�C ffC �\C �RC ��C  CG�CffC��CC�HC(�C33Cz�C��CC  C{CQ�Cp�C��C�HC��C33CQ�Cz�C�RC�
C{C33CffC��C�RC  C�C\)C��C�C��C�CG�C��C�C�C�CQ�C��C�C��C	{C	\)C	�\C	�RC
  C
�C
\)C
�\C
�RC  C�CffC��C�RC
=C33CQ�C��CC  C=qCQ�C��C��C��CG�CffC�\C�
C��C=qCp�C�C�
C�C33Cp�C�\C�
C  C33Cp�C�\C�
C
=C33Cz�C��C�
C�C=qC�C�C�
C�C=qCz�C�RC�
C{C\)Cp�C�RC�C{C\)Cz�C�RC  C�C\)C��C�RC
=C33C\)C�C�
C{C\)Cz�CC
=C(�CffC�C��C�CQ�C�C�
C  C(�Cp�C�C�
C(�CG�Cz�CC�C�Cz�C��C��C{C=qCp�C�RC�
C �C Q�C z�C ��C �C!�C!ffC!�C!�RC"  C"�C"Q�C"��C"C#  C#G�C#p�C#��C#�HC${C$=qC$�\C$�RC$�HC%33C%Q�C%�\C%�
C%��C&G�C&z�C&��C&�C'(�C'Q�C'��C'��C(  C(Q�C(z�C(�C)  C)(�C)\)C)�C)�
C*  C*Q�C*�C*�RC+  C+�C+ffC+�C+��C,�C,Q�C,�C,��C-
=C-(�C-z�C-C-�HC.(�C.p�C.��C.�HC/33C/Q�C/��C/�C0  C0Q�C0��C0�RC1
=C1Q�C1p�C1C1��C2�C2p�C2�C2�
C3(�C3\)C3�C3�
C4{C4=qC4p�C4��C4��C5(�C5z�C5�RC5�HC6�C6p�C6��C6�
C7(�C7G�C7�\C7�HC8
=C8G�C8��C8C9  C9Q�C9�\C9C:{C:G�C:z�C:�
C;
=C;33C;�\C;��C;�C<G�C<�C<�C=  C=G�C=ffC=�RC>  C>(�C>z�C>C>�C?=qC?�C?�C@  C@G�C@p�C@�RCA  CA33CAp�CACA��CB�CBp�CB�RCB�HCC(�CCp�CC��CC�
CD(�CDQ�CDz�CD�
CE
=CE33CEz�CECE�HCF{CFffCF��CFCG  CGG�CGp�CG��CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                ?�  ?�@=p�@�  @��R@��R@�G�A   A  A   A*�HA?\)A`��A\)A��A�Q�A�  A�  A�Q�A�Q�A�  B Q�B  B  BQ�B Q�B(Q�B0Q�B8  B?�BG�BP  BX  B_�Bg�Bo�
Bw�
B�
B��B��B�{B�  B��B�  B�  B��B��
B�  B�{B�(�B�{B��
B�  B�{B��B��B�  B�  B��B�  B�  B�  B�  B�{B�  B�{B�{B��B��
B��C  C  C
=C{C
  C  C
=C
=C
=C��C��C�C�C  C  C   C"  C$
=C&
=C(
=C)��C+�C.  C0  C1��C4  C6
=C8  C9��C<  C>
=C?��CA��CC��CE��CG�CI��CL
=CN  CP  CR  CT
=CV  CW��CZ  C\  C^{C`  Cb  Cd
=Cf
=Ch  Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cx  Cy��C{��C~  C�  C�C�
=C�  C�  C�C�C�C���C���C�C���C���C�  C�  C�  C�C�C�  C�  C�C�  C���C�  C�  C�  C�
=C�\C�  C��C���C�  C�C�
=C�C���C�  C�  C�  C�  C�C���C���C���C���C���C�  C�  C�  C�  C���C�  C�  C���C�C�\C�\C�
=C�  C�  C�  C���C���C���C�  C�C�
=C�
=C���C���C�  C���C���C�
=C�
=C�  C�  C�C�  C���C�  C�  C���C���C�  C���C�  C�
=C�C�  C�  C�  C���C���C�  C���C��C��C��C�  C�C���C�  C�
=C�
=C�  C�  C�
=C�\C�
=C�  C�C���C���C�C�C�C�C�  C�  C�  C�C�
=C�\C�C���C���C���D   D �D  Dz�D�qD}qD�D�D�D� D�D�D�D}qD��D}qD  D��D	�D	� D
�D
� D�D�D�D� D  D� D�qDz�D��D}qD  Dz�D��D� D  D� D  D}qD�qD��DD� D��Dz�D�qD}qD  D}qD  D�D�D��DD}qD��Dz�D�qD� DD� D  D}qD   D ��D!  D!}qD!��D"}qD#�D#��D$  D$� D%  D%� D&  D&� D'  D'� D(�D(��D)  D)� D*  D*� D+�D+}qD+�qD,� D-�D-}qD.�D.�D/�D/� D0  D0��D1  D1� D2�D2�D3D3��D4�D4��D5  D5��D6�D6� D7  D7��D8�D8}qD8�qD9� D:  D:� D;  D;� D<�D<� D=  D=� D>�D>� D>�qD?� D@�D@��DA  DA� DB  DB��DC�DC� DD  DD� DE  DE��DF�DF}qDF�qDG}qDH  DH� DI  DI��DJDJ�DKDK� DL�DL�DMDM��DN  DN}qDO  DO��DP  DP}qDQ  DQ� DQ��DRz�DS  DS��DT�DT� DU  DU}qDU�qDV� DW�DW��DX�DX� DX��DY� DZ�DZ� D[  D[z�D\  D\� D\�qD]��D^D^��D_�D_�D`�D`��Da�Da� Db  Db� Dc�Dc� Dd  Dd}qDd�qDe��Df  Df}qDf�qDg}qDh�Dh� Di  Di}qDj  Dj� Dj�qDk}qDk��Dl� Dm�Dm� Dn  Dn��Do�Do� Dp  Dp��Dp�qDqz�Dr  Dr��Ds  Ds}qDt  Dt� Du�Du� Du�qDv}qDv�qDw��Dx�Dx}qDy  Dy� Dz  Dz��Dz�qD{� D|�D|��D}  D}� D~�D~��D  D� D��D�B�D�� D�� D��D�B�D���D�� D�  D�@ D�~�D���D���D�@ D��HD�D��D�AHD�� D�� D�HD�@ D�� D���D���D�=qD�}qD�� D���D�@ D�~�D���D�HD�AHD�~�D���D���D�>�D��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D��HD��D�AHD�� D�� D�  D�@ D��HD�D�  D�AHD��HD��HD���D�@ D��HD��HD�HD�AHD��HD��HD�HD�B�D�� D���D�  D�@ D�� D��qD���D�@ D�~�D�� D�  D�AHD�~�D�� D�HD�AHD���D��HD�  D�=qD�}qD�� D�HD�@ D�� D���D���D�AHD��HD���D���D�=qD�}qD���D�HD�@ D�� D��HD�HD�B�D���D�D�  D�@ D��HD���D�  D�@ D�� D��HD��D�@ D�~�D�� D�  D�B�D�� D�� D��D�B�D���D�D��D�@ D�~�D���D���D�>�D�� D�� D�HD�AHD��HD�� D�  D�>�D�}qD��qD���D�@ D��HD���D��D�@ D�~�D��HD�HD�@ D�~�D���D��qD�AHD��HD��qD�  D�AHD��HD���D���D�AHD��HD���D�  D�@ D��HD��HD���D�>�D�� D��HD�HD�>�D�� D��HD�  D�AHD��HD���D��qD�>�D�� D��qD��)D�@ D���D��HD�  D�@ D��HD��HD�  D�AHD��HD��qD��)D�=qD�� D��HD�HD�AHD�� D��HD�HD�@ D�~�D��qD��qD�@ D���D��HD�  D�>�D�� D���D���D�@ D�� D�� D���D�>�D�� D��HD�HD�@ D�~�D�� D�  D�>�D�� D�� D���D�@ D�� D���D��qD�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�AHDÀ Dþ�D�  D�@ DĀ Dľ�D�  D�@ D�~�D�� D�HD�=qD�}qDƽqD���D�@ D�~�DǾ�D�HD�@ D�}qD�� D�  D�@ D�~�DɽqD���D�@ DʁHD�� D��qD�>�Dˀ D˽qD��qD�>�D�}qD̾�D�  D�@ D̀ D�� D���D�>�D�~�Dξ�D���D�>�D�}qD�� D��D�>�D�~�D�� D�  D�@ Dр DѾ�D��qD�>�DҁHD��HD�  D�>�DӀ D�� D���D�@ DԁHD�D�  D�>�DՁHD�� D�  D�=qD�}qD־�D�HD�@ D�}qD׽qD��qD�=qD؀ D�� D���D�=qD�~�D��HD��D�B�DځHD�� D���D�@ Dۀ D۾�D��qD�=qD�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD޽qD��qD�@ D߁HD�� D�  D�>�D��HD�D�  D�=qD� DᾸD�  D�@ D�~�D�� D�HD�@ D� D��HD�  D�AHD�HD�� D�  D�AHD�HD�� D���D�>�D� D�� D�HD�@ D�~�D��HD��D�B�D�HD��HD�HD�=qD�}qD龸D��qD�>�D�~�D꾸D���D�@ D낏D��HD���D�AHD� D쾸D�  D�AHD�HD�� D���D�>�D�~�DD�HD�B�D�HD�� D���D�<)D�~�D��HD�HD�@ D�D��HD�  D�>�D�~�D�D���D�>�D�~�D�� D�  D�@ D� D�� D�HD�>�D�~�D�� D���D�>�D�~�D�� D��D�>�D�}qD���D�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHG�O�?��?8Q�?�  ?�=q?�33?Ǯ?�@�\@��@&ff@8Q�@J=q@Y��@n{@��\@���@��@�(�@��@���@�Q�@��
@���@�Q�@޸R@���@�33A ��A�A
�HA\)AA�HA   A%�A*=qA0��A5A:�HA@��AEAL(�AP��AW
=A[�Ab�\AfffAmp�Aq�Ax��A|��A�=qA�z�A�  A�=qA�{A�  A��A�A�G�A��
A�\)A�G�A��A�
=A��HA�z�A�Q�A��\A�p�A�  A�33A�{A���AÅA�ffAȣ�A��
A�{A�G�A��
A�ffA�G�A��
A�
=AᙚA�z�A�\)A��A���A�\)A�\A���A��A��\A��A��BG�B�RB�
Bp�BffB(�B	�B
�HB  BG�B�RB  BG�B�RB�
Bp�BffB(�B�B
=B�
B��BffB (�B!�B"�HB#�
B%��B&ffB((�B)�B*�HB+�
B-p�B.�\B0(�B1�B2�RB3�B5G�B6=qB8  B8��B:ffB;�B=�B>{B?�B@��BA�BC\)BDz�BEBG
=BHz�BI��BK
=BL(�BMBN�\BPQ�BQ�BR�RBS�BUG�BV=qBX  BX��BZffB[�B\��B^{B_\)B`��BaBc\)BdQ�Be�Bf�HBhQ�Bip�Bj�HBl  BmG�Bn�\Bp  Bp��BrffBs�Bt��Bv=qBw\)Bx��By�B{�B|z�B~{B
=B�Q�B���B���B�(�B��HB�p�B�(�B��RB�\)B�{B��\B�p�B��
B��RB��B�  B�ffB�33B��B�Q�B�
=B��B�Q�B��RB���B�  B��HB�G�B�{B��\B�G�B��B�ffB�G�B��B�z�B��HB�B�=qB��HB��B�{B��HB�G�B�{B��\B�\)B��
B��\B�
=B�B�Q�B���B��B�{B���B�G�B�{B��\B�G�B�B�ffB�
=B�p�B�Q�B��RB�\)B��B��\B�
=B��B�Q�B���B�p�B�B���B���B��B�=qB��HB�p�B�  B���B�
=B��B�Q�B�
=B�p�B�(�B���B�G�B��B�ffB��B��B�Q�B���B��B�{B��RB��B�  B��HB�G�B�  B���B�G�B�(�B£�BÅB�  B���B�p�B�{B��HB�G�B�(�BȸRBɅB�{BʸRB˙�B�{B���B�\)B�=qBθRBϙ�B�{B���BхB�  B��HB�\)B�(�B��HB�p�B�=qBָRBי�B�{B��HBمB�(�B�
=BۅB�ffB�
=BݮB�z�B�
=B��B�z�B�G�B��
B�\B�G�B�B�z�B��HB�p�B�{B�Q�B���B�33B�B�{B�Q�B���B��HB�\)B�B��B�ffB�z�B���B�33B�p�B�  B�{B�\B���B��B�B��B�(�B�Q�B�RB��B�G�B�B��B�Q�B�RB���B�p�B�B��B�Q�B�z�B�
=B��B�B�B�(�B�\B��RB�G�B�\)B��B�{B�z�B���B�
=B��B��B�(�B�Q�B��HB�
=B�p�B�B�  B��\B���B�33B�G�B�B�{B�ffB��HB�
=B��B��B�(�B�z�B���B�33B�p�B�C �C (�C ffC �\C �RC ��C  CG�CffC��CC�HC(�C33Cz�C��CC  C{CQ�Cp�C��C�HC��C33CQ�Cz�C�RC�
C{C33CffC��C�RC  C�C\)C��C�C��C�CG�C��C�C�C�CQ�C��C�C��C	{C	\)C	�\C	�RC
  C
�C
\)C
�\C
�RC  C�CffC��C�RC
=C33CQ�C��CC  C=qCQ�C��C��C��CG�CffC�\C�
C��C=qCp�C�C�
C�C33Cp�C�\C�
C  C33Cp�C�\C�
C
=C33Cz�C��C�
C�C=qC�C�C�
C�C=qCz�C�RC�
C{C\)Cp�C�RC�C{C\)Cz�C�RC  C�C\)C��C�RC
=C33C\)C�C�
C{C\)Cz�CC
=C(�CffC�C��C�CQ�C�C�
C  C(�Cp�C�C�
C(�CG�Cz�CC�C�Cz�C��C��C{C=qCp�C�RC�
C �C Q�C z�C ��C �C!�C!ffC!�C!�RC"  C"�C"Q�C"��C"C#  C#G�C#p�C#��C#�HC${C$=qC$�\C$�RC$�HC%33C%Q�C%�\C%�
C%��C&G�C&z�C&��C&�C'(�C'Q�C'��C'��C(  C(Q�C(z�C(�C)  C)(�C)\)C)�C)�
C*  C*Q�C*�C*�RC+  C+�C+ffC+�C+��C,�C,Q�C,�C,��C-
=C-(�C-z�C-C-�HC.(�C.p�C.��C.�HC/33C/Q�C/��C/�C0  C0Q�C0��C0�RC1
=C1Q�C1p�C1C1��C2�C2p�C2�C2�
C3(�C3\)C3�C3�
C4{C4=qC4p�C4��C4��C5(�C5z�C5�RC5�HC6�C6p�C6��C6�
C7(�C7G�C7�\C7�HC8
=C8G�C8��C8C9  C9Q�C9�\C9C:{C:G�C:z�C:�
C;
=C;33C;�\C;��C;�C<G�C<�C<�C=  C=G�C=ffC=�RC>  C>(�C>z�C>C>�C?=qC?�C?�C@  C@G�C@p�C@�RCA  CA33CAp�CACA��CB�CBp�CB�RCB�HCC(�CCp�CC��CC�
CD(�CDQ�CDz�CD�
CE
=CE33CEz�CECE�HCF{CFffCF��CFCG  CGG�CGp�CG��CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aˉ7Aˉ7AˋDAˋDAˋDAˉ7A�~�A�x�A�~�A�~�A˃A�|�A�^5A�M�A��A���Aʺ^AʬAʧ�Aʧ�Aʩ�Aʧ�Aʥ�Aʥ�Aʣ�Aʝ�Aʙ�Aʗ�Aʗ�Aʏ\A�z�A�bNA�VA�O�A�M�A�;dA�/A�/A�+A� �A��A�oA��#A�\)A�33A�/A�(�A�"�A��A�ZA�E�A�?}A��!A���A���A�jA�O�A���A�;dA�bA���A��#A��jA�E�A���A��RA�~�A�;dA�ĜA��A���A��A���A�K�A���A���A�+A��FA���A�A���A�A�A���A��A���A�z�A�=qA�1A�bNA�`BA��
A���A��A��A��A��A��#A�=qA���A�z�A�ZA�A�A�-A�  A���A�1'A�
=A�1A��A�~�A�"�A�A��A�VA�VA�1A���A�XA�%A�\)A��TA�VA��`A�bA|-Az �AyO�Ay33Ax�\Awt�As�ApbAnE�AlA�Aj�Ah��Agx�Ad�uAb�RA^��A\^5A[AXffAWhsAT�AQ&�AK�AJn�AH{AD��AC�ACK�ABM�A@��A>��A;XA9;dA8�+A7t�A6bA4�RA3�7A1x�A.z�A-�TA-�mA-�mA-��A,�A,-A*�HA*bA)��A)A(��A'��A'A&�+A&�A%33A$5?A#�hA#;dA#VA"�yA"n�A!|�A M�A��A�HA�AhsAVAM�A��A|�AVA�AdZA�A��A�A-A��A�A�;AdZA�A��A�wA�wA�PA+A
��A
=A
��A
��A
��A
jA
{A�DA�A`BA%A�uAjAA�A  A�A=qAp�AAn�A�#A�wA�A?}A�A�A ��A VA -@��;@��@��@��P@��@�J@�Ĝ@��@�^5@�z�@��+@��@�/@�&�@��/@�A�@�o@�-@�%@�\@�  @�t�@�C�@��@�p�@��/@���@���@��m@��@���@�J@�ƨ@ݑh@�  @��@�ff@�p�@��/@���@֗�@�`B@�S�@��@��#@�x�@�ȴ@�@�`B@�r�@˕�@�K�@��@ɡ�@�1@�o@�5?@�?}@�z�@ÍP@��h@���@��@�bN@�  @��P@�^5@�@�p�@�X@�z�@���@�ƨ@���@�dZ@�@��y@�=q@��`@�I�@��;@��@��+@���@��;@�|�@�C�@�^5@�`B@��D@�A�@� �@�z�@��j@�Ĝ@��u@�(�@�1@��m@�\)@�ff@�{@��#@���@���@�bN@���@��@��R@���@�5?@�I�@���@��+@�ff@�5?@�@��@�G�@�V@���@��D@�I�@�1'@�b@�9X@���@�S�@�33@��@�"�@�+@�33@�+@�+@�+@�@��y@�@��@���@�ȴ@��\@�ff@�V@�-@��-@��@���@��#@�p�@��@�Ĝ@���@��9@�bN@�A�@�A�@�Z@�r�@���@��@��P@�+@��\@��+@��\@��+@�=q@�$�@�{@��#@���@�Ĝ@���@�"�@�n�@�{@���@���@�x�@�O�@�&�@���@��9@��@�bN@�Z@�Q�@�I�@� �@�ƨ@��@�t�@�t�@�t�@�l�@�\)@�o@�~�@�5?@�5?@�5?@�$�@��@���@��@��@���@��#@���@���@�p�@���@��@��@�v�@���@���@��7@�x�@��@�hs@�?}@���@��/@�Ĝ@��u@�z�@�j@�Z@�I�@�(�@��@�1@���@��m@�ƨ@��F@���@���@�|�@�"�@�n�@���@���@�/@��/@���@��u@��D@�z�@�bN@�I�@�1@��m@��F@�\)@�"�@�@��H@���@�^5@�E�@�$�@��@��#@���@�hs@�?}@�&�@�V@���@��j@�z�@�b@��
@���@�K�@�33@��@�E�@��@��-@��h@�p�@�&�@���@�Ĝ@��9@��@��D@�Z@�1@��;@��w@��P@�C�@��@��+@�M�@�-@�@��^@�hs@�?}@��@��`@���@���@��@K�@~��@~�@~ȴ@}��@|Z@{��@{S�@z�H@z~�@z�@y��@y�7@y&�@y%@xĜ@w��@v�y@v�@v��@vV@v5?@v5?@v$�@up�@t�@t(�@sƨ@s��@s"�@r�!@rn�@r^5@r=q@r�@rJ@q��@q�^@q7L@q&�@q%@p��@p��@n��@n@l�/@l9X@l�@k��@kƨ@kdZ@j��@j=q@i�@iX@h��@g�;@g�@fv�@e��@e?}@d�/@d�j@d��@d��@dz�@cƨ@co@b�!@b�\@b�\@b~�@b^5@bM�@b-@a�@ax�@a%@`Ĝ@`��@`�@`  @_�@^ȴ@^v�@^@]V@\�j@\z�@\Z@\I�@\9X@\9X@\�@[��@[�F@[�@[C�@Z��@Zn�@Z�@Y��@Y��@YX@Y7L@Y7L@Y&�@Y�@Y%@X��@X�u@X1'@W��@W�P@WK�@W�@V��@V��@V��@V��@V��@V�@V@UO�@U�@T�@TI�@T1@S�F@St�@SC�@R�@QX@PĜ@P1'@O�@O�;@O��@O�w@O�@O��@O|�@O|�@O
=@N$�@M`B@M�@L��@L��@K��@Ko@J�!@I�@I��@I�7@IX@HĜ@G�@G
=@F�+@Fff@F5?@F@E�@EV@Dz�@C�m@C��@Ct�@C"�@B��@B�!@B��@Bn�@B=q@B-@BJ@A��@A�7@Ahs@Ahs@AX@@��@@bN@@1'@?�;@?�P@?K�@>��@>5?@=?}@=V@<�/@<�@<�D@<Z@<1@;��@;C�@:��@:-@9��@9��@97L@8��@8�u@8�@8r�@8Q�@8 �@7��@7K�@6��@6@5��@5@5�-@5`B@49X@3��@3�F@3t�@3o@3@2�@2�H@2��@2��@2�\@2~�@2^5@2M�@2=q@2�@1�#@1G�@0��@0��@0r�@0bN@0A�@/��@/|�@/l�@/\)@/\)@/�@.�@.��@.{@-`B@-�@,��@,I�@+t�@+C�@+33@+33@+33@+o@*n�@)��@)�#@)��@)�^@)�^@)��@)��@)��@)hs@)7L@)%@(��@(bN@'�@'|�@&��@&5?@%�@%��@%��@%��@%O�@%?}@%?}@%?}@%�@$��@$�@$��@$z�@$z�@$j@$j@$j@$j@$j@$9X@$�@#��@#�@#t�@#"�@"�H@"��@"~�@"n�@"n�@"M�@"�@!�7@!&�@!&�@!�@ ��@ ��@ �@  �@�@;d@��@�@ȴ@ȴ@ȴ@��@�+@�+@�+@ff@�@��@O�@��@j@Z@�@1@ƨ@ƨ@ƨ@ƨ@�
@��@33@@��@n�@=q@�@x�@&�@Ĝ@  @�P@\)@K�@;d@��@�y@�@�@�@ȴ@�R@v�@V@E�@5?@{@�@@�-@�h@?}@�@V@��@�@�/@��@9X@�F@dZ@C�@@-@J@�@��@�7@�@�`@��@Ĝ@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@Q�@1'@  @��@|�@l�@l�@\)@l�@l�@l�@\)@K�@;d@;d@+@�@�R@��@ff@$�@@�-@��@�/@��@��@�D@j@I�@9X@(�@��@�F@dZ@"�@
^5@	�^@	X@	G�@	G�@	7L@	7L@	7L@	%@�`@bN@1'@b@  @�@�;@��@��A˅Aˇ+Aˇ+AˍPAˉ7AˍPAˉ7Aˉ7Aˇ+AˍPAˇ+Aˏ\AˋDAˏ\Aˇ+AˍPAˉ7Aˏ\AˋDAˍPAˋDAˏ\A�z�A�|�AˁAˁA�x�A�t�A�~�AˁA�z�A�~�A�z�A˅A�z�Aˉ7A�z�Aˉ7AˁA�~�A�t�A�t�A˅A˃A�S�A�G�A�I�A�O�A�O�A�O�A�Q�A�O�A�I�A�33A��A��A�9XA�oA��A��yA��
A���A���A�A���Aʺ^AʼjAʼjAʶFAʸRAʮAʲ-Aʩ�AʮAʧ�AʬAʥ�Aʩ�Aʥ�Aʩ�Aʣ�AʬAʣ�Aʩ�Aʥ�AʬAʥ�AʬAʧ�AʮAʩ�AʬAʧ�Aʩ�Aʥ�Aʩ�Aʥ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʥ�Aʧ�Aʣ�Aʥ�Aʡ�Aʣ�Aʣ�Aʝ�Aʙ�Aʙ�Aʙ�Aʛ�Aʗ�Aʙ�Aʗ�Aʙ�Aʗ�Aʙ�Aʙ�Aʗ�Aʗ�Aʗ�Aʙ�Aʗ�Aʙ�Aʗ�Aʗ�Aʕ�Aʗ�AʓuAʓuAʑhAʓuAʍPAʏ\Aʉ7AʋDAʁAʃA�p�A�r�A�ffA�n�A�bNA�jA�`BA�dZA�ZA�XA�VA�VA�XA�S�A�XA�O�A�VA�O�A�S�A�K�A�O�A�I�A�O�A�K�A�M�A�M�A�K�A�Q�A�G�A�G�A�A�A�9XA�33A�5?A�/A�33A�-A�33A�-A�1'A�-A�1'A�-A�/A�/A�/A�/A�-A�/A�-A�/A�(�A�(�A� �A�$�A��A�"�A��A� �A� �A��A� �A��A��A�{A��A�bA��A�oA�oA�oA�bA�oA�
=A�%A���A���AɋDAɉ7AɁA�|�A�p�A�A�A�9XA�33A�9XA�1'A�5?A�1'A�5?A�33A�/A�1'A�-A�1'A�+A�1'A�(�A�-A�(�A�+A�+A�(�A�(�A�$�A�+A�"�A�(�A�"�A� �A� �A��A��A�VA�A��A��/A���AȮAț�AȍPAȇ+A�n�A�ffA�VAǝ�A�&�A���A�G�A��AžwAţ�AőhAŁA�jA�S�A�G�A�
=A�^5A�C�A�A�VA�C�A��;A��PA�G�A�  A��
A��FA�p�A�E�A�(�A�{A���A���A��!A�t�A�r�A�hsA�l�A�hsA�hsA�jA�dZA�bNA�S�A�K�A�A�A�?}A�7LA�1'A�VA��A��A��-A��!A���A��A�5?A�bA���A��A�bNA�+A�oA��!A��DA�ZA��A��\A�7LA���A��DA��+A�E�A��HA��A�\)A�"�A��TA���A�ȴA��RA���A��7A�x�A�t�A�`BA�M�A�E�A�E�A�A�A�7LA�5?A�7LA��A��A��A��A�JA�A�  A���A���A���A��mA��A��HA��/A��A�ȴA���A�ĜA�ȴA���A��RA��-A��A��A���A���A���A���A��hA��A��DA�x�A�x�A�~�A�x�A�|�A�r�A�r�A�t�A�ZA�XA�O�A�Q�A�K�A�9XA�1'A�1'A�+A� �A�"�A��A��A�1A���A��yA��;A�ȴA��9A��!A���A��7A�r�A�`BA�S�A�;dA�=qA�7LA�(�A�(�A��A��A�JA�A���A��mA��TA���A���A���A���A���A��uA��PA��hA�|�A�p�A�hsA�\)A�E�A�(�A��A���A��yA��#A���A��jA���A��PA�r�A�G�A�Q�A�=qA�-A�9XA�5?A�5?A���A�K�A��RA��7A�z�A�/A���A��9A�-A���A�dZA�C�A�5?A� �A�JA��mA�ƨA��!A��uA�ffA�5?A�A��A��TA���A��RA��-A���A���A��hA��A�r�A�hsA�^5A�`BA�S�A�I�A�9XA� �A�bA���A��A��HA��HA��A��HA���A���A��9A���A���A�~�A�XA�$�A��HA��;A��#A���A���A��RA���A��A�XA�Q�A�;dA�"�A�JA��A��A��A���A���A��DA�I�A�{A���A��yA�ĜA��PA�`BA�(�A��;A�ƨA��FA�jA�S�A�9XA� �A��A���A��jA�dZA��9A��PA��A���A�  A���A��A���A��A��A��A��A��`A��
A��jA��^A��-A��A���A���A���A���A���A���A���A��uA��PA��A��+A�z�A�z�A�z�A�v�A�v�A�jA�bNA�bNA�ZA�O�A�S�A�M�A�I�A�I�A�A�A�1A��A�\)A�O�A�9XA�oA�A��yA�ȴA��FA�A���A��A��\A�r�A�x�A�dZA�VA�+A�  A�A��-A��PA�t�A�l�A�dZA�(�A�A���A��A��A��/A���A���A���A�ĜA�ĜA��^A��^A��RA��!A��A���A��uA��A�n�A�K�A�C�A�(�A�VA���A��`A���A���A��\A�z�A�`BA�;dA�$�A��A�A�~�A�S�A�JA���A���A��A��`A��`A��HA��#A��A�A�Q�A�"�A�  A���A��A��`A��#A���A��-A��7A�VA�bA���A�S�A�A�A�+A�jA�hsA���A���A�"�A�I�A�$�A�{A��A��!A���A��uA��hA��\A��uA��PA��PA��PA�|�A�v�A�t�A�x�A�t�A�t�A�l�A�hsA�l�A�jA�`BA�K�A�M�A�C�A�I�A�I�A�G�A�E�A�C�A�;dA�E�A�7LA�1'A�1'A�33A�+A�-A�-A�&�A�$�A�"�A��A�{A�
=A���A��A���A���A��A��/A���A�ĜA��jA��^A��RA��A�n�A�A���A��uA�l�A�I�A�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                Aˉ7Aˉ7AˋDAˋDAˋDAˉ7A�~�A�x�A�~�A�~�A˃A�|�A�^5A�M�A��A���Aʺ^AʬAʧ�Aʧ�Aʩ�Aʧ�Aʥ�Aʥ�Aʣ�Aʝ�Aʙ�Aʗ�Aʗ�Aʏ\A�z�A�bNA�VA�O�A�M�A�;dA�/A�/A�+A� �A��A�oA��#A�\)A�33A�/A�(�A�"�A��A�ZA�E�A�?}A��!A���A���A�jA�O�A���A�;dA�bA���A��#A��jA�E�A���A��RA�~�A�;dA�ĜA��A���A��A���A�K�A���A���A�+A��FA���A�A���A�A�A���A��A���A�z�A�=qA�1A�bNA�`BA��
A���A��A��A��A��A��#A�=qA���A�z�A�ZA�A�A�-A�  A���A�1'A�
=A�1A��A�~�A�"�A�A��A�VA�VA�1A���A�XA�%A�\)A��TA�VA��`A�bA|-Az �AyO�Ay33Ax�\Awt�As�ApbAnE�AlA�Aj�Ah��Agx�Ad�uAb�RA^��A\^5A[AXffAWhsAT�AQ&�AK�AJn�AH{AD��AC�ACK�ABM�A@��A>��A;XA9;dA8�+A7t�A6bA4�RA3�7A1x�A.z�A-�TA-�mA-�mA-��A,�A,-A*�HA*bA)��A)A(��A'��A'A&�+A&�A%33A$5?A#�hA#;dA#VA"�yA"n�A!|�A M�A��A�HA�AhsAVAM�A��A|�AVA�AdZA�A��A�A-A��A�A�;AdZA�A��A�wA�wA�PA+A
��A
=A
��A
��A
��A
jA
{A�DA�A`BA%A�uAjAA�A  A�A=qAp�AAn�A�#A�wA�A?}A�A�A ��A VA -@��;@��@��@��P@��@�J@�Ĝ@��@�^5@�z�@��+@��@�/@�&�@��/@�A�@�o@�-@�%@�\@�  @�t�@�C�@��@�p�@��/@���@���@��m@��@���@�J@�ƨ@ݑh@�  @��@�ff@�p�@��/@���@֗�@�`B@�S�@��@��#@�x�@�ȴ@�@�`B@�r�@˕�@�K�@��@ɡ�@�1@�o@�5?@�?}@�z�@ÍP@��h@���@��@�bN@�  @��P@�^5@�@�p�@�X@�z�@���@�ƨ@���@�dZ@�@��y@�=q@��`@�I�@��;@��@��+@���@��;@�|�@�C�@�^5@�`B@��D@�A�@� �@�z�@��j@�Ĝ@��u@�(�@�1@��m@�\)@�ff@�{@��#@���@���@�bN@���@��@��R@���@�5?@�I�@���@��+@�ff@�5?@�@��@�G�@�V@���@��D@�I�@�1'@�b@�9X@���@�S�@�33@��@�"�@�+@�33@�+@�+@�+@�@��y@�@��@���@�ȴ@��\@�ff@�V@�-@��-@��@���@��#@�p�@��@�Ĝ@���@��9@�bN@�A�@�A�@�Z@�r�@���@��@��P@�+@��\@��+@��\@��+@�=q@�$�@�{@��#@���@�Ĝ@���@�"�@�n�@�{@���@���@�x�@�O�@�&�@���@��9@��@�bN@�Z@�Q�@�I�@� �@�ƨ@��@�t�@�t�@�t�@�l�@�\)@�o@�~�@�5?@�5?@�5?@�$�@��@���@��@��@���@��#@���@���@�p�@���@��@��@�v�@���@���@��7@�x�@��@�hs@�?}@���@��/@�Ĝ@��u@�z�@�j@�Z@�I�@�(�@��@�1@���@��m@�ƨ@��F@���@���@�|�@�"�@�n�@���@���@�/@��/@���@��u@��D@�z�@�bN@�I�@�1@��m@��F@�\)@�"�@�@��H@���@�^5@�E�@�$�@��@��#@���@�hs@�?}@�&�@�V@���@��j@�z�@�b@��
@���@�K�@�33@��@�E�@��@��-@��h@�p�@�&�@���@�Ĝ@��9@��@��D@�Z@�1@��;@��w@��P@�C�@��@��+@�M�@�-@�@��^@�hs@�?}@��@��`@���@���@��@K�@~��@~�@~ȴ@}��@|Z@{��@{S�@z�H@z~�@z�@y��@y�7@y&�@y%@xĜ@w��@v�y@v�@v��@vV@v5?@v5?@v$�@up�@t�@t(�@sƨ@s��@s"�@r�!@rn�@r^5@r=q@r�@rJ@q��@q�^@q7L@q&�@q%@p��@p��@n��@n@l�/@l9X@l�@k��@kƨ@kdZ@j��@j=q@i�@iX@h��@g�;@g�@fv�@e��@e?}@d�/@d�j@d��@d��@dz�@cƨ@co@b�!@b�\@b�\@b~�@b^5@bM�@b-@a�@ax�@a%@`Ĝ@`��@`�@`  @_�@^ȴ@^v�@^@]V@\�j@\z�@\Z@\I�@\9X@\9X@\�@[��@[�F@[�@[C�@Z��@Zn�@Z�@Y��@Y��@YX@Y7L@Y7L@Y&�@Y�@Y%@X��@X�u@X1'@W��@W�P@WK�@W�@V��@V��@V��@V��@V��@V�@V@UO�@U�@T�@TI�@T1@S�F@St�@SC�@R�@QX@PĜ@P1'@O�@O�;@O��@O�w@O�@O��@O|�@O|�@O
=@N$�@M`B@M�@L��@L��@K��@Ko@J�!@I�@I��@I�7@IX@HĜ@G�@G
=@F�+@Fff@F5?@F@E�@EV@Dz�@C�m@C��@Ct�@C"�@B��@B�!@B��@Bn�@B=q@B-@BJ@A��@A�7@Ahs@Ahs@AX@@��@@bN@@1'@?�;@?�P@?K�@>��@>5?@=?}@=V@<�/@<�@<�D@<Z@<1@;��@;C�@:��@:-@9��@9��@97L@8��@8�u@8�@8r�@8Q�@8 �@7��@7K�@6��@6@5��@5@5�-@5`B@49X@3��@3�F@3t�@3o@3@2�@2�H@2��@2��@2�\@2~�@2^5@2M�@2=q@2�@1�#@1G�@0��@0��@0r�@0bN@0A�@/��@/|�@/l�@/\)@/\)@/�@.�@.��@.{@-`B@-�@,��@,I�@+t�@+C�@+33@+33@+33@+o@*n�@)��@)�#@)��@)�^@)�^@)��@)��@)��@)hs@)7L@)%@(��@(bN@'�@'|�@&��@&5?@%�@%��@%��@%��@%O�@%?}@%?}@%?}@%�@$��@$�@$��@$z�@$z�@$j@$j@$j@$j@$j@$9X@$�@#��@#�@#t�@#"�@"�H@"��@"~�@"n�@"n�@"M�@"�@!�7@!&�@!&�@!�@ ��@ ��@ �@  �@�@;d@��@�@ȴ@ȴ@ȴ@��@�+@�+@�+@ff@�@��@O�@��@j@Z@�@1@ƨ@ƨ@ƨ@ƨ@�
@��@33@@��@n�@=q@�@x�@&�@Ĝ@  @�P@\)@K�@;d@��@�y@�@�@�@ȴ@�R@v�@V@E�@5?@{@�@@�-@�h@?}@�@V@��@�@�/@��@9X@�F@dZ@C�@@-@J@�@��@�7@�@�`@��@Ĝ@��@Ĝ@Ĝ@Ĝ@Ĝ@�9@Q�@1'@  @��@|�@l�@l�@\)@l�@l�@l�@\)@K�@;d@;d@+@�@�R@��@ff@$�@@�-@��@�/@��@��@�D@j@I�@9X@(�@��@�F@dZ@"�@
^5@	�^@	X@	G�@	G�@	7L@	7L@	7L@	%@�`@bN@1'@b@  @�@�;@��G�O�A˅Aˇ+Aˇ+AˍPAˉ7AˍPAˉ7Aˉ7Aˇ+AˍPAˇ+Aˏ\AˋDAˏ\Aˇ+AˍPAˉ7Aˏ\AˋDAˍPAˋDAˏ\A�z�A�|�AˁAˁA�x�A�t�A�~�AˁA�z�A�~�A�z�A˅A�z�Aˉ7A�z�Aˉ7AˁA�~�A�t�A�t�A˅A˃A�S�A�G�A�I�A�O�A�O�A�O�A�Q�A�O�A�I�A�33A��A��A�9XA�oA��A��yA��
A���A���A�A���Aʺ^AʼjAʼjAʶFAʸRAʮAʲ-Aʩ�AʮAʧ�AʬAʥ�Aʩ�Aʥ�Aʩ�Aʣ�AʬAʣ�Aʩ�Aʥ�AʬAʥ�AʬAʧ�AʮAʩ�AʬAʧ�Aʩ�Aʥ�Aʩ�Aʥ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʣ�Aʩ�Aʥ�Aʧ�Aʣ�Aʥ�Aʡ�Aʣ�Aʣ�Aʝ�Aʙ�Aʙ�Aʙ�Aʛ�Aʗ�Aʙ�Aʗ�Aʙ�Aʗ�Aʙ�Aʙ�Aʗ�Aʗ�Aʗ�Aʙ�Aʗ�Aʙ�Aʗ�Aʗ�Aʕ�Aʗ�AʓuAʓuAʑhAʓuAʍPAʏ\Aʉ7AʋDAʁAʃA�p�A�r�A�ffA�n�A�bNA�jA�`BA�dZA�ZA�XA�VA�VA�XA�S�A�XA�O�A�VA�O�A�S�A�K�A�O�A�I�A�O�A�K�A�M�A�M�A�K�A�Q�A�G�A�G�A�A�A�9XA�33A�5?A�/A�33A�-A�33A�-A�1'A�-A�1'A�-A�/A�/A�/A�/A�-A�/A�-A�/A�(�A�(�A� �A�$�A��A�"�A��A� �A� �A��A� �A��A��A�{A��A�bA��A�oA�oA�oA�bA�oA�
=A�%A���A���AɋDAɉ7AɁA�|�A�p�A�A�A�9XA�33A�9XA�1'A�5?A�1'A�5?A�33A�/A�1'A�-A�1'A�+A�1'A�(�A�-A�(�A�+A�+A�(�A�(�A�$�A�+A�"�A�(�A�"�A� �A� �A��A��A�VA�A��A��/A���AȮAț�AȍPAȇ+A�n�A�ffA�VAǝ�A�&�A���A�G�A��AžwAţ�AőhAŁA�jA�S�A�G�A�
=A�^5A�C�A�A�VA�C�A��;A��PA�G�A�  A��
A��FA�p�A�E�A�(�A�{A���A���A��!A�t�A�r�A�hsA�l�A�hsA�hsA�jA�dZA�bNA�S�A�K�A�A�A�?}A�7LA�1'A�VA��A��A��-A��!A���A��A�5?A�bA���A��A�bNA�+A�oA��!A��DA�ZA��A��\A�7LA���A��DA��+A�E�A��HA��A�\)A�"�A��TA���A�ȴA��RA���A��7A�x�A�t�A�`BA�M�A�E�A�E�A�A�A�7LA�5?A�7LA��A��A��A��A�JA�A�  A���A���A���A��mA��A��HA��/A��A�ȴA���A�ĜA�ȴA���A��RA��-A��A��A���A���A���A���A��hA��A��DA�x�A�x�A�~�A�x�A�|�A�r�A�r�A�t�A�ZA�XA�O�A�Q�A�K�A�9XA�1'A�1'A�+A� �A�"�A��A��A�1A���A��yA��;A�ȴA��9A��!A���A��7A�r�A�`BA�S�A�;dA�=qA�7LA�(�A�(�A��A��A�JA�A���A��mA��TA���A���A���A���A���A��uA��PA��hA�|�A�p�A�hsA�\)A�E�A�(�A��A���A��yA��#A���A��jA���A��PA�r�A�G�A�Q�A�=qA�-A�9XA�5?A�5?A���A�K�A��RA��7A�z�A�/A���A��9A�-A���A�dZA�C�A�5?A� �A�JA��mA�ƨA��!A��uA�ffA�5?A�A��A��TA���A��RA��-A���A���A��hA��A�r�A�hsA�^5A�`BA�S�A�I�A�9XA� �A�bA���A��A��HA��HA��A��HA���A���A��9A���A���A�~�A�XA�$�A��HA��;A��#A���A���A��RA���A��A�XA�Q�A�;dA�"�A�JA��A��A��A���A���A��DA�I�A�{A���A��yA�ĜA��PA�`BA�(�A��;A�ƨA��FA�jA�S�A�9XA� �A��A���A��jA�dZA��9A��PA��A���A�  A���A��A���A��A��A��A��A��`A��
A��jA��^A��-A��A���A���A���A���A���A���A���A��uA��PA��A��+A�z�A�z�A�z�A�v�A�v�A�jA�bNA�bNA�ZA�O�A�S�A�M�A�I�A�I�A�A�A�1A��A�\)A�O�A�9XA�oA�A��yA�ȴA��FA�A���A��A��\A�r�A�x�A�dZA�VA�+A�  A�A��-A��PA�t�A�l�A�dZA�(�A�A���A��A��A��/A���A���A���A�ĜA�ĜA��^A��^A��RA��!A��A���A��uA��A�n�A�K�A�C�A�(�A�VA���A��`A���A���A��\A�z�A�`BA�;dA�$�A��A�A�~�A�S�A�JA���A���A��A��`A��`A��HA��#A��A�A�Q�A�"�A�  A���A��A��`A��#A���A��-A��7A�VA�bA���A�S�A�A�A�+A�jA�hsA���A���A�"�A�I�A�$�A�{A��A��!A���A��uA��hA��\A��uA��PA��PA��PA�|�A�v�A�t�A�x�A�t�A�t�A�l�A�hsA�l�A�jA�`BA�K�A�M�A�C�A�I�A�I�A�G�A�E�A�C�A�;dA�E�A�7LA�1'A�1'A�33A�+A�-A�-A�&�A�$�A�"�A��A�{A�
=A���A��A���A���A��A��/A���A�ĜA��jA��^A��RA��A�n�A�A���A��uA�l�A�I�A�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B��B��B��B�B�B�B�B��B�B�HB�B�B�mB�B��B�ZB�&B�B��B��B��B��B�B�,B�`B�`B�`B�2B�yB�B�cB�5B�B��B�GB�|B�B�B��B�`B�xB�B�BBB�B�B&LB6FB/�B8�B-wB.�B1'B3�B7�B=qBB�BNpB<B49B.�B,B'RB"�B!bB(�B 'B7B�B&B&�B�B	7B�B��B��B��B�WB�5B҉B�-B� B��B��B�-B��B�bB�B��B��B�xBy>BpBc�B?�B=B�B�B�B�B\BB��B�oB�B��B�mB��B��B�B�iBh>BbNB`vBZBW�BO�BIBC�B=<B7B�B.B�BYBB
�	B
�WB
ӏB
�B
��B
��B
��B
��B
�\B
��B
o�B
`�B
TaB
I�B
@�B
1�B
'�B
�B
oB	�rB	��B	�`B	�B	��B	�B	�BB	��B	��B	�dB	�XB	�B	��B	�6B	�B	�RB	��B	��B	�'B	�!B	�hB	�IB	�B	��B	�B	��B	��B	��B	��B	��B	�FB	��B	�:B	��B	�B	�rB	��B	��B	�1B	�B	�GB	��B	��B	{�B	zDB	|B	y>B	x�B	x8B	z�B	x8B	{B	v�B	u%B	tTB	pB	o�B	cTB	`�B	NB	M�B	MB	M6B	Z�B	e�B	gB	lWB	rGB	tB	��B	�MB	��B	��B	��B	�B	�SB	��B	�;B	.B	~]B	��B	��B	�MB	��B	� B	{JB	zxB	zxB	zB	y�B	y	B	}�B	��B	��B	��B	�MB	�B	�B	��B	�fB	�=B	��B	��B	�fB	�lB	��B	��B	�"B	��B	��B	��B	�(B	�(B	�uB	��B	��B	�B	�uB	��B	�hB	�.B	��B	��B	� B	��B	��B	��B	cB	�4B	}�B	~(B	}�B	}�B	�B	��B	��B	�B	�B	�lB	�B	�4B	��B	�PB	��B	��B	��B	�"B	�bB	��B	�{B	�SB	�1B	��B	��B	�_B	�RB	�RB	��B	��B	��B	�'B	��B	�-B	��B	��B	�?B	ɆB	ΥB	��B	רB	�yB	�dB	� B	�TB	�DB	��B	��B	�B	��B	�B	�DB	�B	��B	��B	�8B	��B
B
1B

	B
�B
�B
�B
�B
�B
uB
�B
B
�B
#�B
%B
&�B
,�B
.�B
.IB
/�B
:�B
<6B
:�B
:�B
:�B
<jB
<�B
=�B
>�B
?}B
?�B
?�B
@B
A�B
DgB
N<B
XB
Y�B
[#B
]�B
_;B
_�B
`BB
a�B
b�B
cTB
dZB
h
B
lWB
q�B
v`B
w�B
wfB
w�B
w�B
x�B
x�B
|�B
��B
�_B
��B
��B
��B
��B
�xB
�xB
��B
��B
��B
��B
�hB
�hB
�:B
�hB
��B
�B
�:B
�uB
�B
��B
�:B
��B
��B
�uB
�B
��B
��B
�_B
��B
�B
��B
��B
��B
�B
��B
�'B
��B
��B
��B
�4B
�@B
�FB
�zB
��B
��B
��B
��B
�$B
��B
�=B
��B
�B
�=B
�=B
�B
��B
�B
�B
�B
��B
�=B
�B
�[B
�?B
��B
��B
��B
��B
��B
�B
�qB
��B
�BB
�}B
��B
�OB
� B
�UB
��B
��B
��B
�'B
�[B
�[B
B
��B
�-B
ÖB
ÖB
ÖB
ÖB
ĜB
�mB
��B
�EB
ʌB
�0B
�6B
�6B
�jB
�jB
�jB
��B
�<B
ΥB
�B
��B
�HB
�HB
бB
��B
� B
ѷB
�TB
��B
�&B
��B
��B
�2B
�gB
՛B
�B
�B
��B
خB
�yB
خB
�B
��B
�B
چB
ںB
��B
چB
��B
�WB
�WB
�WB
�WB
�#B
�WB
��B
��B
�/B
ݘB
��B
�5B
��B
�B
ߤB
ߤB
�B
��B
�|B
�B
��B
�B
� B
��B
��B
��B
��B
�B
��B
�2B
�mB
�
B
�
B
�B
�B
��B
�B
�KB
��B
�B
�B
�B
�)B
�)B
�B
��B
��B
��B
�B
��B
�B
��B
�AB
�AB
�B
�|B
�GB
�|B
�|B
�B
�GB
�GB
�B
�B
��B
�B
�B
�|B
�B
��B
��B
�`B
�`B
�`B
��B
�fB
�B
�lB
�lB
�>B
�>B
�xB
�B
��B
��B
��B
�"B
��B
��B
�"B
��B
�(B
�]B
�(B
��B
�(B
��B
�(B
�(B
��B
��B
��B
�.B
�.B
�.B
�.B
��B 4B
��B  B  B�BBoBoBoBoB;B;BoBoB;BoBBuBB�B�BB�BBBBBBB�BBMB�B�B�B�B�B�BMBMB�B�B�B�B�B�B�B�B�B	7B	B	�B
�B
rB
�B
�B
�B
rB
�B
rB
	BB�B~B~B�B~BVB"BVB�B(B(B�B�B B�B�B�BhB�BoB�BuB�B�B�BB�B�B�B�B�B�B�B�BB�B�B�BBBBSBBBSBYB�B�B+B_B+B_B�B1BeB�B7BkB�B=B�B�B�B�BBCBxB�B~B�B�B�BIB~B�B�B�B!BVBVB�BVB�B�B�B�B�B�B�B�B�B \B \B �B �B �B �B �B!bB!-B!-B!-B!bB!�B!�B!�B"�B"�B"�B#�B$�B$tB$tB$@B$tB$@B%zB%�B%�B%�B%�B%�B&B%�B%�B&B&B&LB&�B&�B'�B'�B'�B(�B(�B(�B)*B)*B)_B)�B)�B)�B)�B*0B*0B*0B*0B*eB*eB*eB*eB*eB*0B*eB*eB+6B+B+B+�B+kB+�B,B+�B+�B+�B+�B-B-B,�B-B,�B-B-CB-�B.B.}B.�B.�B.�B.�B.�B.�B.�B.�B.}B.�B.�B.�B/�B/�B/�B/�B0UB0!B0�B0�B0�B0UB0!B0�B0�B0�B1[B1'B1[B1�B2-B2aB33B4B49B49B49B4nB4�B4�B4�B4�B4�B4�B4�B5B5B5B5?B5tB5tB5�B5�B5�B5�B5�B6B5�B5�B5�B6FB6�B7LB7�B7LB7�B8�B8�B8�B8�B9$B9�B9�B:*B:*B9�B:*B9�B9�B9�B9�B:�B:�B:�B;dB;dB;�B;�B;�B;�B;�B;�B;�B;�B;�B;�B;�B<�B<jB<�B=B=<B=qB=�B>�B>�B>�B?HB?B?�B?�B?�B?}B?�B@OB@�B@�BB�BB�BCaBC-BC-BC-BC-BC-BCaBCaBD3BD3BD3BDgBDgBD�BD�BD�B�B�B�B�vB�B�B�TB�B� B�B�TB�B��B��B�TB�B� B��B�NB�HB�B��B��B�|B��B�B�&B�B��B��B�B��B�B�dB��B��B�ZB�B�B��B��B�vB��B�|B�
B��B�B�B�B�TB�B�&B�B�B�5B��B�QB�2B�AB��B�B�
B��B��B��B��B��B�ZB�`B�B��B� B�B�B��B��B��B�B��B� B�`B� B�B�TB�B�TB��B�B��B��B��B�ZB�2B��B�2B�B�B�B��B�B��B�B�2B�B��B�B��B�B�2B�B��B�B��B�ZB�`B�&B�ZB�B�ZB�2B�fB�,B�`B�B�B�`B�B�B�`B�`B��B�B��B�B��B�`B��B�B�,B��B��B��B��B�B��B�8B�B�mB�,B�B�B��B�DB�WB�B�B�KB��B�"B��B��B�/B�B�B��B�]B�iB�B�B�/B�B��B��B� B�oB�iB�5B�;B�/B�vB��B�vB��B�MB�B�B�B�B�B�B�B�B��B�B�B�B�|B�GB�MB�GB�B�B��B��B�ZB�MB�ZB��B�%B�B�B�%B��B�fB��B�2B�%B��B��B��B�`B��B��B��B�fB�`B��BB�PB�B�VB��B 4B�B�B�B�B�BBBB�B�B�B�BBSBBSB�B�BBuB�BuBGB;BBoB�B�B�B�BGB_B1B�B�B�B�B�B�B�B#nB$B;�B6�B5?B<jBB�B;0B33B-�B,=B,=B,�B*�B+�B/�BN�BA�B?B7BA B-B-wB+�B/�B)�B+�B2�B,B,B*eB+�B/�B-�B4B/�B2aB0�B1'B1�B0�B2aB0�B33B5?B4B3�B5?B33B9�B:^B7�B;0B5B6FB?}B:�B:^BFtBS&BH�B?HB:^B?B>�B;dB<jBO�BaBJXB4nB5?BG�BC�B=�B6FB<6B5�B4B2�B2-B2�B5�B1�B/�B0�B1[B.IB,�B-wB-�B.IB+�B1[B+6B.�B,qB-B.�B+kB+B*�B+kB0UB(�B*�B+�B)*B(�B'�B)*B'B)�B'RB'B'RB&�B'B%zB$@B%zB �B%�B!bB'�B"4B �B#B �B$tB�B!�B,=B �B!bBOB!bB!�B!bB�B�B!�B�B#nB \B#:B$B'�B'RB)�B.}B+B.�B,qB,qB)_B%�B$�B \B"4B"4B�B �B�B�B~B�B=B�B1B�BxBxB�B	BB�B	BqB�B�BYB�B�B�B�BSB$BB�B=B�B~BYB#�B�B7B#�B�B5�B:�B7�B*�B)_B+kB,B,qB9�B&�BVB_B�BYBYB�B�BB�B�B+B\B�BBJBB	lB�B�B%B�B+BB�BMB�B�B%B�BBGB�BB��BB�VB��BB��B��B�PB��B�B	lBB�B�fB��B��B�>B�B�JB��B�B�B��B��B�B�B�]B�B�B�B��B�5B��B��B�B��B�yB�B�8B�;B�B�B�]B�/B�)B�B�B�9B�B�,B��B�KB��B�mB��BĜB�aB�-B�aB�[B��B��B�EB�[B�'B��B��B�'B��B��B��B�wB��B��B�}B��B��B�HB��B��B��B�OB��B�}B��B�qB�qB��B��B�6B��B��B��B�)B՛B��B��B��B��B��B��B��B�B�eB��B�dB�XB�IB�$B��B��B�@B��B��B��B�LB�B��B��B�'B��B�	B�B�eB�B�7B��B��B�B��B�B��B�$B��B�$B�MB�1B�$B�$B��B�@B�MB�{B�\B� B�SB�@B��B�(B��B�	B��B��B�=B��B��B��B��BzBy�BzxBv�Bv`Bv`Bt�Bv`B��BpBuZBjKBk�Bl"Bf�BlWBh>BjBc�Bn�Bf2B]dBW
BS&Bm]Bi�BC-B9XBF?B/�B 'BIB+�BqB�B1B�B�B�B�BBSB�B�BSB�B_B�B1BB+BMB�B�B�B�B�B�B{B:B�B�B�B�B�BoBBhB�B�B.BB B\B�BPB4B�B�B
�B BVB�B�B
�B	�B�B�B1B�BBVBMB�]B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2021122420413920211224204139IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021122423313820211224233138QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021122423313820211224233138QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225820220906072258IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                