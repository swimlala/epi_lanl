CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-19T04:45:59Z creation; 2021-03-26T17:01:02Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20210219044559  20210326170212  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               B   BAA  AOAO7836_008777_066                 7836_008777_066                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�_���gw@�_���gw11  @�_���r@�_���r@;���a=@;���a=�d��s���d��s��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@=p�@}p�@��\@�G�@�\AG�A��A\)A+�A@��A`��A�  A��A�  A�Q�A�Q�A�Q�A�\)A�\)B   B(�B�B�B�
B((�B0(�B8(�B@(�BH  BO�
BX(�B`  Bh(�Bp(�Bw�B�
B�  B��B�  B�{B�  B�  B�  B�  B�{B��B�  B�=qB�  B�B��
B��B��B�  B�{B�  B��B�  B�{B�{B�  B��B��B�{B�{B��B�  C   C��C  C��C  C
  C��C  C��C�C�C��C  C  C  C
=C   C"  C$  C&  C(  C*
=C,
=C.  C0
=C1��C3��C6  C7��C9��C;�C=��C@
=CB{CD  CF  CH  CJ  CL
=CN
=CP  CQ��CS�CU��CX  CZ
=C\{C^  C_�Cb  Cd{Cf
=Ch
=Cj{Cl
=Cm��Co��Cq��Cs��Cv  Cx  Cz  C|  C~  C��C���C�  C�C���C���C���C���C�
=C�
=C�
=C�C�  C�C�  C���C�
=C���C���C�C�C���C�  C�C�C�  C���C���C�  C�C���C�  C���C���C���C�  C�C�  C���C�  C�C�C�  C���C�C�  C�C�C�C�  C�C�  C�  C�  C�C�  C�C���C���C���C�  C�  C���C���C�
=C�C�C�  C���C�  C�  C�C�
=C�C���C�  C�C���C���C�C���C�  C�C�C�  C�  C�C�  C���C���C���C�C�  C���C�  C�C�
=C�C���C���C���C��C���C�  C�  C�  C���C�C�
=C�  C�C�C���C���C�  C���C�C�
=C�  C�
=C�
=C���C���C�
=C�C�  C�  C���D D ��D�D��D�qD}qD  D}qD  D� D�D� D  D� D�qD��DD�D	  D	� D
  D
� D
�qD� DD��D  D� D  D� D  D� D�qD��D  D}qD�qD��D  D}qD  D��D  D� DD��D�qD}qD  D��D  D� D�qD}qD  D� D  D� D�D��DD�D�D��D   D � D!�D!��D!�qD"xRD#  D#��D$  D$�D%D%�D&�D&��D&�qD'� D(�D(� D)  D)��D*�D*}qD*��D+z�D+�RD,xRD,��D-}qD.  D.� D/�D/� D0�D0� D1�D1��D2D2�D3D3}qD3�qD4� D4�qD5}qD6�D6��D7  D7� D7�qD8� D9�D9��D:�D:z�D:��D;xRD;��D<� D=  D=}qD=��D>}qD?  D?}qD@  D@��DA�DA� DB�DB�DCDC�DD�DD}qDE  DE� DE��DF� DG�DG}qDG�qDH}qDH�qDI}qDJ  DJ�DK�DK}qDK�qDL� DM�DM� DN  DN� DN��DO� DP�DP� DQ  DQ}qDQ��DR��DS�DS}qDS��DT��DU�DU��DV  DV}qDWDW��DW�qDX� DY  DY� DZ�DZ��D[  D[}qD[�qD\� D]D]��D^�D^}qD_  D_� D_�qD`}qD`�qDa}qDb  Db�DcDc��DdDd��De�De� Df  Df��Dg�Dg}qDh  Dh��Di  Di��DjDj� Dj�qDk� Dl�Dl� Dl�qDm� Dm�qDn}qDo�Do��Dp  Dp�DqDq�DrDr��Ds�Ds��Dt�Dt��DuDu�Dv  Dv� DwDw�Dx�Dx��Dx��Dyz�Dz  Dz��D{�D{��D|�D|� D|�qD}}qD~�D~��D~�qD� D�HD�>�D�� D��HD�HD�@ D�� D�� D���D�AHD�� D�� D���D�<)D�}qD�� D��D�AHD�� D��HD�HD�AHD���D���D�  D�>�D�~�D��qD���D�>�D�~�D���D�  D�@ D�� D��HD���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D��HD�D�  D�=qD�~�D��HD�HD�AHD��HD��qD�HD�@ D�}qD��qD���D�AHD�� D�� D�HD�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�~�D�� D�HD�AHD�~�D���D�HD�AHD���D�� D��qD�>�D�� D�� D�  D�>�D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D�� D���D�@ D��HD��HD�  D�@ D�� D��HD��D�AHD�� D��HD�HD�@ D��HD��HD�HD�@ D�~�D���D�HD�@ D�~�D���D���D�@ D�~�D��HD�  D�=qD�}qD�� D�HD�>�D�|)D���D�  D�=qD�}qD��qD�  D�B�D��HD�� D���D�=qD�}qD��qD�  D�>�D�~�D�� D��D�B�D�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�~�D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD���D�=qD�� D��HD�HD�AHD�� D�� D�HD�AHD�� D�D��D�AHD��HD�� D���D�@ D���D�D�  D�=qD�~�D��HD��D�AHD�~�D���D���D�>�D��HD���D��qD�@ D�� D��qD�  D�AHD��HD��HD�  D�@ D�� D���D��qD�@ D�� D���D�  D�AHD��HD���D��qD�=qD�}qD��HD��D�B�D�� D�� D�  D�@ D��HD�D�  D�@ D���D�D��D�AHD�� D��HD�  D�>�D�}qD��qD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�  D�AHD�D�D��D�@ D�~�D�� D�HD�AHD�~�D�� D�HD�AHDł�D��HD�  D�AHD�~�Dƾ�D�  D�>�Dǀ D�D�HD�>�DȀ D�� D�  D�B�DɁHD�� D�HD�@ Dʀ D�� D���D�>�Dˀ D��HD�  D�@ D̀ D̾�D���D�@ D́HD�D�HD�@ D΀ D�� D�HD�@ DρHD��HD�  D�AHD�~�Dо�D��qD�>�Dр DѾ�D�  D�B�DҀ D�� D�HD�@ DӁHD��HD���D�AHDԀ DԽqD�  D�B�DՂ�D�� D���D�>�Dր D��HD�  D�=qD�~�D�� D���D�>�D؀ D�D�HD�>�D�~�D�� D�  D�>�DځHD��HD�HD�@ D�~�D۾�D�  D�AHD܁HD�D��D�@ D�~�Dݾ�D���D�AHD�~�D��HD�HD�AHD߂�D��HD�  D�@ D�}qD�qD�  D�@ D� D�� D���D�@ D�HD��HD�  D�AHD�HD㾸D�  D�>�D� D��HD���D�=qD�}qD徸D�HD�@ D� D澸D���D�AHD�HD��HD�  D�AHD�HD�� D�HD�@ D邏D�D��D�B�DꂏD���D�HD�>�D� D뾸D��qD�=qD�~�D��HD�HD�@ D�}qD���D��D�AHD�~�D�� D���D�=qD�~�D�� D���D�@ D��HD��HD�  D�@ D�HD��HD��D�@ D�}qD�� D�  D�@ D� D�� D��D�@ D�~�D���D�  D�@ D�~�D���D��qD�=qD�~�D���D�  D�>�D�}qD�� D�  D�<)D�~�D��HD��qD�=qD�� D���D���D�>�D���>���?.{?W
=?�z�?�Q�?��?��H@\)@(��@0��@G�@Y��@h��@z�H@�ff@�{@���@�G�@���@�z�@��H@���@�{@�(�@�\@�@�@��HAz�A
=A{A��A�A=qA!G�A%�A*=qA/\)A333A:=qA<��AC�
AG�AMp�AQ�AW�A\��AaG�Ag
=Aj�HAr�\Atz�A|��A\)A��HA��A�\)A��HA�(�A��A���A�z�A�ffA�G�A��
A�ffA���A��A�{A��A��HA��A�  A��A���A��RA���A��A�ffA�G�A\A�ffAǮA�33A���A�\)Aҏ\A��
A׮A�G�A�z�A�
=A��A�z�A�{A�G�A�A�A���A��HA�A�  A��A�p�A�
=B ��B=qB
=B��B��B
=BQ�B	�B
�\B�
B��BffB\)BQ�B{B�HBQ�BBffBQ�B��B�\B\)B��B{B
=B ��B!��B"�RB$z�B%p�B&�\B'�
B(��B*{B+�B,z�B-B/33B0  B1p�B2�RB3�B5G�B6ffB733B8��B9�B;
=B<Q�B=B>�\B?�
BAG�BB{BC\)BD��BE�BF�HBHz�BI��BJffBL  BMp�BN=qBO\)BP��BQ�BR�HBTz�BU�BV�RBX  BY��BZffB[�B]G�B]�B_\)B`��BaBb�HBd��Bep�BfffBg�
BiG�Bj{Bk�Bm�BmBo
=Bp��BqBr�RBt  Bu��BvffBw�Bx��Bz{Bz�HB|Q�B}�B~�HB�B��RB�p�B��B�Q�B�
=B�B�=qB���B�p�B�=qB��RB��B�  B���B�
=B���B�Q�B�
=B��B�  B���B�p�B��
B���B�\)B�B�Q�B��B��B�(�B���B�p�B�=qB���B��B�B�z�B�
=B�B�ffB�
=B��B�{B��RB�\)B��B�z�B�G�B��B���B�
=B���B�=qB���B���B�=qB���B���B�(�B���B�33B�B�z�B�33B��B��\B�
=B��B�(�B���B���B�Q�B��HB�p�B��B�z�B�G�B�  B��\B�\)B��B�z�B��B���B�Q�B��HB��B�  B���B�p�B�  B��RB�\)B�{B��RB�G�B�  B�z�B��HB��B�Q�B��B��B�{B���B�\)B�(�B��HB�p�B��B�z�B�
=B�B�z�B��B��
B�ffB��HB�p�B�  Bʣ�B�\)B�{B���B�p�B�  BΏ\B�33B�B�Q�B���BѮB�Q�B���BӮB�=qB��HBՙ�B�Q�BָRB�\)B��B؏\B��B�B�ffB�
=B�B�z�B�
=BݮB�ffB��B߮B�Q�B���B�p�B�{B��B�G�B��B�z�B��B�B�z�B�33B��B�ffB�
=B�B�(�B���B뙚B�Q�B���B홚B�=qB���B�p�B�  B��\B��B��
B�\B�G�B�  B��RB�33B��
B�Q�B�
=B��
B���B��B��B�=qB��HB���B�=qB���B��B�ffB�
=B���C (�C ffC �RC
=CQ�C��C��C=qC�C�
CG�C��C��C=qC�C�
C(�Cz�C��C�Cp�CC
=CQ�C��C�CG�C��C�C	(�C	�C	��C
�C
ffC
�RC
=CQ�C��C�C33Cz�C��C33Cz�C�
C33C�C�
C(�Cz�C��C�Cp�CC{CffC�RC{CffC�RC
=C\)C�C  C=qC�\C�
C{Cp�CC�Cp�C��C�HC�C\)C�\CC  C=qCz�C�C�HC{C33CffC�\CC�HC
=C=qCp�C�C�HC�CG�Cp�C�\C�RC�C�C\)C�\CC�HC
=C(�C\)C�\C�
C��C{C=qCz�C�RC�
C�C(�CffC�\C��C�
C �C G�C \)C �\C ��C ��C!
=C!G�C!�C!�C!C!�HC"{C"Q�C"�C"�RC"�
C"��C#(�C#\)C#��C#�RC#��C${C$G�C$\)C$��C$��C$�C%
=C%Q�C%z�C%�\C%C&
=C&33C&G�C&z�C&C&�C'  C'=qC'z�C'�\C'��C({C(33C(G�C(�\C(��C(��C){C)\)C)�\C)��C)�HC*(�C*=qC*p�C*�RC*�C*��C+=qC+�C+��C+C,  C,G�C,p�C,�C,�
C-  C-�C-p�C-��C-C.{C.(�C.\)C.�C.C.��C/=qC/\)C/�\C/�
C/�C0=qC0\)C0��C0�HC0�C133C1p�C1��C1C2{C2(�C2p�C2�C2C3
=C3Q�C3\)C3�C3�HC4  C4Q�C4z�C4��C4�C5{C5G�C5�C5��C5�
C6(�C6G�C6z�C6C6�
C7�C7Q�C7p�C7��C7�HC8�C8ffC8z�C8�
C8�HC9=qC9\)C9�\C9�
C9�C:G�C:Q�C:��C:��C;  C;G�C;\)C;�C;�HC<  C<Q�C<ffC<C<�
C=�C=\)C=z�C=�
C=��C>(�C>z�C>�C>�
C?  C?=qC?z�C?��C?�HC@{C@G�C@�C@C@�CA(�CAffCA��CA��CB{CB(�CB�CB��CB�CC�CC\)CC�CC��CD  CD=qCDp�CD��CD��CE{CEffCEz�CE�
CE��CF=qCFp�CF��CF�CG  CG\)CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                              ?�  ?��H@=p�@}p�@��\@�G�@�\AG�A��A\)A+�A@��A`��A�  A��A�  A�Q�A�Q�A�Q�A�\)A�\)B   B(�B�B�B�
B((�B0(�B8(�B@(�BH  BO�
BX(�B`  Bh(�Bp(�Bw�B�
B�  B��B�  B�{B�  B�  B�  B�  B�{B��B�  B�=qB�  B�B��
B��B��B�  B�{B�  B��B�  B�{B�{B�  B��B��B�{B�{B��B�  C   C��C  C��C  C
  C��C  C��C�C�C��C  C  C  C
=C   C"  C$  C&  C(  C*
=C,
=C.  C0
=C1��C3��C6  C7��C9��C;�C=��C@
=CB{CD  CF  CH  CJ  CL
=CN
=CP  CQ��CS�CU��CX  CZ
=C\{C^  C_�Cb  Cd{Cf
=Ch
=Cj{Cl
=Cm��Co��Cq��Cs��Cv  Cx  Cz  C|  C~  C��C���C�  C�C���C���C���C���C�
=C�
=C�
=C�C�  C�C�  C���C�
=C���C���C�C�C���C�  C�C�C�  C���C���C�  C�C���C�  C���C���C���C�  C�C�  C���C�  C�C�C�  C���C�C�  C�C�C�C�  C�C�  C�  C�  C�C�  C�C���C���C���C�  C�  C���C���C�
=C�C�C�  C���C�  C�  C�C�
=C�C���C�  C�C���C���C�C���C�  C�C�C�  C�  C�C�  C���C���C���C�C�  C���C�  C�C�
=C�C���C���C���C��C���C�  C�  C�  C���C�C�
=C�  C�C�C���C���C�  C���C�C�
=C�  C�
=C�
=C���C���C�
=C�C�  C�  C���D D ��D�D��D�qD}qD  D}qD  D� D�D� D  D� D�qD��DD�D	  D	� D
  D
� D
�qD� DD��D  D� D  D� D  D� D�qD��D  D}qD�qD��D  D}qD  D��D  D� DD��D�qD}qD  D��D  D� D�qD}qD  D� D  D� D�D��DD�D�D��D   D � D!�D!��D!�qD"xRD#  D#��D$  D$�D%D%�D&�D&��D&�qD'� D(�D(� D)  D)��D*�D*}qD*��D+z�D+�RD,xRD,��D-}qD.  D.� D/�D/� D0�D0� D1�D1��D2D2�D3D3}qD3�qD4� D4�qD5}qD6�D6��D7  D7� D7�qD8� D9�D9��D:�D:z�D:��D;xRD;��D<� D=  D=}qD=��D>}qD?  D?}qD@  D@��DA�DA� DB�DB�DCDC�DD�DD}qDE  DE� DE��DF� DG�DG}qDG�qDH}qDH�qDI}qDJ  DJ�DK�DK}qDK�qDL� DM�DM� DN  DN� DN��DO� DP�DP� DQ  DQ}qDQ��DR��DS�DS}qDS��DT��DU�DU��DV  DV}qDWDW��DW�qDX� DY  DY� DZ�DZ��D[  D[}qD[�qD\� D]D]��D^�D^}qD_  D_� D_�qD`}qD`�qDa}qDb  Db�DcDc��DdDd��De�De� Df  Df��Dg�Dg}qDh  Dh��Di  Di��DjDj� Dj�qDk� Dl�Dl� Dl�qDm� Dm�qDn}qDo�Do��Dp  Dp�DqDq�DrDr��Ds�Ds��Dt�Dt��DuDu�Dv  Dv� DwDw�Dx�Dx��Dx��Dyz�Dz  Dz��D{�D{��D|�D|� D|�qD}}qD~�D~��D~�qD� D�HD�>�D�� D��HD�HD�@ D�� D�� D���D�AHD�� D�� D���D�<)D�}qD�� D��D�AHD�� D��HD�HD�AHD���D���D�  D�>�D�~�D��qD���D�>�D�~�D���D�  D�@ D�� D��HD���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D��HD�D�  D�=qD�~�D��HD�HD�AHD��HD��qD�HD�@ D�}qD��qD���D�AHD�� D�� D�HD�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�~�D�� D�HD�AHD�~�D���D�HD�AHD���D�� D��qD�>�D�� D�� D�  D�>�D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D�� D���D�@ D��HD��HD�  D�@ D�� D��HD��D�AHD�� D��HD�HD�@ D��HD��HD�HD�@ D�~�D���D�HD�@ D�~�D���D���D�@ D�~�D��HD�  D�=qD�}qD�� D�HD�>�D�|)D���D�  D�=qD�}qD��qD�  D�B�D��HD�� D���D�=qD�}qD��qD�  D�>�D�~�D�� D��D�B�D�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�~�D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD���D�=qD�� D��HD�HD�AHD�� D�� D�HD�AHD�� D�D��D�AHD��HD�� D���D�@ D���D�D�  D�=qD�~�D��HD��D�AHD�~�D���D���D�>�D��HD���D��qD�@ D�� D��qD�  D�AHD��HD��HD�  D�@ D�� D���D��qD�@ D�� D���D�  D�AHD��HD���D��qD�=qD�}qD��HD��D�B�D�� D�� D�  D�@ D��HD�D�  D�@ D���D�D��D�AHD�� D��HD�  D�>�D�}qD��qD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�  D�AHD�D�D��D�@ D�~�D�� D�HD�AHD�~�D�� D�HD�AHDł�D��HD�  D�AHD�~�Dƾ�D�  D�>�Dǀ D�D�HD�>�DȀ D�� D�  D�B�DɁHD�� D�HD�@ Dʀ D�� D���D�>�Dˀ D��HD�  D�@ D̀ D̾�D���D�@ D́HD�D�HD�@ D΀ D�� D�HD�@ DρHD��HD�  D�AHD�~�Dо�D��qD�>�Dр DѾ�D�  D�B�DҀ D�� D�HD�@ DӁHD��HD���D�AHDԀ DԽqD�  D�B�DՂ�D�� D���D�>�Dր D��HD�  D�=qD�~�D�� D���D�>�D؀ D�D�HD�>�D�~�D�� D�  D�>�DځHD��HD�HD�@ D�~�D۾�D�  D�AHD܁HD�D��D�@ D�~�Dݾ�D���D�AHD�~�D��HD�HD�AHD߂�D��HD�  D�@ D�}qD�qD�  D�@ D� D�� D���D�@ D�HD��HD�  D�AHD�HD㾸D�  D�>�D� D��HD���D�=qD�}qD徸D�HD�@ D� D澸D���D�AHD�HD��HD�  D�AHD�HD�� D�HD�@ D邏D�D��D�B�DꂏD���D�HD�>�D� D뾸D��qD�=qD�~�D��HD�HD�@ D�}qD���D��D�AHD�~�D�� D���D�=qD�~�D�� D���D�@ D��HD��HD�  D�@ D�HD��HD��D�@ D�}qD�� D�  D�@ D� D�� D��D�@ D�~�D���D�  D�@ D�~�D���D��qD�=qD�~�D���D�  D�>�D�}qD�� D�  D�<)D�~�D��HD��qD�=qD�� D���D���D�>�G�O�>���?.{?W
=?�z�?�Q�?��?��H@\)@(��@0��@G�@Y��@h��@z�H@�ff@�{@���@�G�@���@�z�@��H@���@�{@�(�@�\@�@�@��HAz�A
=A{A��A�A=qA!G�A%�A*=qA/\)A333A:=qA<��AC�
AG�AMp�AQ�AW�A\��AaG�Ag
=Aj�HAr�\Atz�A|��A\)A��HA��A�\)A��HA�(�A��A���A�z�A�ffA�G�A��
A�ffA���A��A�{A��A��HA��A�  A��A���A��RA���A��A�ffA�G�A\A�ffAǮA�33A���A�\)Aҏ\A��
A׮A�G�A�z�A�
=A��A�z�A�{A�G�A�A�A���A��HA�A�  A��A�p�A�
=B ��B=qB
=B��B��B
=BQ�B	�B
�\B�
B��BffB\)BQ�B{B�HBQ�BBffBQ�B��B�\B\)B��B{B
=B ��B!��B"�RB$z�B%p�B&�\B'�
B(��B*{B+�B,z�B-B/33B0  B1p�B2�RB3�B5G�B6ffB733B8��B9�B;
=B<Q�B=B>�\B?�
BAG�BB{BC\)BD��BE�BF�HBHz�BI��BJffBL  BMp�BN=qBO\)BP��BQ�BR�HBTz�BU�BV�RBX  BY��BZffB[�B]G�B]�B_\)B`��BaBb�HBd��Bep�BfffBg�
BiG�Bj{Bk�Bm�BmBo
=Bp��BqBr�RBt  Bu��BvffBw�Bx��Bz{Bz�HB|Q�B}�B~�HB�B��RB�p�B��B�Q�B�
=B�B�=qB���B�p�B�=qB��RB��B�  B���B�
=B���B�Q�B�
=B��B�  B���B�p�B��
B���B�\)B�B�Q�B��B��B�(�B���B�p�B�=qB���B��B�B�z�B�
=B�B�ffB�
=B��B�{B��RB�\)B��B�z�B�G�B��B���B�
=B���B�=qB���B���B�=qB���B���B�(�B���B�33B�B�z�B�33B��B��\B�
=B��B�(�B���B���B�Q�B��HB�p�B��B�z�B�G�B�  B��\B�\)B��B�z�B��B���B�Q�B��HB��B�  B���B�p�B�  B��RB�\)B�{B��RB�G�B�  B�z�B��HB��B�Q�B��B��B�{B���B�\)B�(�B��HB�p�B��B�z�B�
=B�B�z�B��B��
B�ffB��HB�p�B�  Bʣ�B�\)B�{B���B�p�B�  BΏ\B�33B�B�Q�B���BѮB�Q�B���BӮB�=qB��HBՙ�B�Q�BָRB�\)B��B؏\B��B�B�ffB�
=B�B�z�B�
=BݮB�ffB��B߮B�Q�B���B�p�B�{B��B�G�B��B�z�B��B�B�z�B�33B��B�ffB�
=B�B�(�B���B뙚B�Q�B���B홚B�=qB���B�p�B�  B��\B��B��
B�\B�G�B�  B��RB�33B��
B�Q�B�
=B��
B���B��B��B�=qB��HB���B�=qB���B��B�ffB�
=B���C (�C ffC �RC
=CQ�C��C��C=qC�C�
CG�C��C��C=qC�C�
C(�Cz�C��C�Cp�CC
=CQ�C��C�CG�C��C�C	(�C	�C	��C
�C
ffC
�RC
=CQ�C��C�C33Cz�C��C33Cz�C�
C33C�C�
C(�Cz�C��C�Cp�CC{CffC�RC{CffC�RC
=C\)C�C  C=qC�\C�
C{Cp�CC�Cp�C��C�HC�C\)C�\CC  C=qCz�C�C�HC{C33CffC�\CC�HC
=C=qCp�C�C�HC�CG�Cp�C�\C�RC�C�C\)C�\CC�HC
=C(�C\)C�\C�
C��C{C=qCz�C�RC�
C�C(�CffC�\C��C�
C �C G�C \)C �\C ��C ��C!
=C!G�C!�C!�C!C!�HC"{C"Q�C"�C"�RC"�
C"��C#(�C#\)C#��C#�RC#��C${C$G�C$\)C$��C$��C$�C%
=C%Q�C%z�C%�\C%C&
=C&33C&G�C&z�C&C&�C'  C'=qC'z�C'�\C'��C({C(33C(G�C(�\C(��C(��C){C)\)C)�\C)��C)�HC*(�C*=qC*p�C*�RC*�C*��C+=qC+�C+��C+C,  C,G�C,p�C,�C,�
C-  C-�C-p�C-��C-C.{C.(�C.\)C.�C.C.��C/=qC/\)C/�\C/�
C/�C0=qC0\)C0��C0�HC0�C133C1p�C1��C1C2{C2(�C2p�C2�C2C3
=C3Q�C3\)C3�C3�HC4  C4Q�C4z�C4��C4�C5{C5G�C5�C5��C5�
C6(�C6G�C6z�C6C6�
C7�C7Q�C7p�C7��C7�HC8�C8ffC8z�C8�
C8�HC9=qC9\)C9�\C9�
C9�C:G�C:Q�C:��C:��C;  C;G�C;\)C;�C;�HC<  C<Q�C<ffC<C<�
C=�C=\)C=z�C=�
C=��C>(�C>z�C>�C>�
C?  C?=qC?z�C?��C?�HC@{C@G�C@�C@C@�CA(�CAffCA��CA��CB{CB(�CB�CB��CB�CC�CC\)CC�CC��CD  CD=qCDp�CD��CD��CE{CEffCEz�CE�
CE��CF=qCFp�CF��CF�CG  CG\)CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                              @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A��uA��uA��uA��hA��hA��\A��DA��\A��uA��PA��+A��7A��7A��DA��DA��7A��7A��DA��7A��7A��7A��A�|�A�r�A�jA�C�A���A�x�A�A�A���A�7LA��wA�bNA� �A��`A���A�E�A��A�ȴA��!A��DA�9XA���A��A�K�A��A��wA�9XA���A�jA�K�A�A�A��FA�"�A��TA�$�A���A�S�A�oA��wA���A��A�l�A��A�C�A�{A�jA���A���A���A��A��DA��jA��\A�Q�A�A��uA��A�z�A���A��uA�`BA��#A��A���A�=qA�ĜA�bNA���A��#A�
=A��!A�A�oA~r�A|~�A{dZAy��AxJAv�!Avn�AvM�Aul�AsVAn�\AmVAl�/Al�DAkO�Ai\)Ag�FAg;dAf�\Ad��Ab�jAa�#Aat�Aa�A`�jA]��A[x�AXz�AV��AU�mAT��ASt�ARJAQS�AO�FAN��AM�7AK�^AK/AJ��AI��AG|�AFE�AEoAC�ABȴAB9XAB�AA��AA�TAAAAC�A@ZA@  A?��A>�RA<��A;�
A;O�A: �A8bNA7��A7
=A6�RA6$�A4JA3t�A3G�A2jA1�A0�9A/�;A.�`A-hsA,��A,�A,  A*�A)��A)�A(�/A(��A(~�A(Q�A(-A'��A%��A#�7A"��A �`A  �A��A��A�A�jA(�A|�A�Ap�Ar�A�;A�AAQ�AG�A�jA��AVA9XA��A$�AO�A��A��A�A�uA�AAK�A
~�A	��A��A�jA�uA1A��A�;A��A�HA1AhsAffA  A��Ap�A\)AO�A;d@��;@�b@�@��F@�-@���@�G�@��@�&�@���@�dZ@�33@��@�
=@�@�n�@�h@�j@�\)@��#@��@���@��@���@�z�@�Q�@�9X@��@���@��m@�ƨ@�ƨ@���@���@���@߶F@�C�@�C�@�"�@��@���@���@އ+@�x�@ܴ9@��@�  @���@թ�@Ձ@�/@��m@�O�@�C�@�V@��`@���@�o@ʗ�@�-@ɡ�@��@�1'@��@�E�@�hs@öF@�~�@��@��`@� �@��w@�t�@�v�@�5?@�@�r�@�C�@��H@��T@���@��@�r�@��P@���@��7@��@�^5@��@�?}@�z�@��P@�~�@��T@�`B@�z�@�  @��@�+@��@��@��@���@��@�hs@��j@�b@�1@��@�K�@�n�@���@��@�1'@�o@�ff@�M�@�-@�G�@�o@�E�@���@��h@�`B@�G�@��@���@��9@�bN@�1'@���@�"�@���@�=q@��@���@��^@���@��7@�O�@��`@���@�Q�@�1@���@��P@��@���@�n�@�5?@��T@���@�G�@���@�Ĝ@�r�@�b@���@�+@�V@���@��^@��7@�`B@�%@��`@��/@���@��@�I�@�(�@�1@��;@��F@���@���@�l�@��@��@���@�@�&�@��`@�z�@��m@�dZ@�
=@���@�M�@���@��-@��h@��@�p�@�`B@�&�@��/@��@�j@�1@+@~�y@~v�@~$�@~{@}�@|��@|�D@|I�@{ƨ@{S�@{@z��@zM�@y��@y�7@y�@xĜ@x�@xA�@x  @w��@w��@w|�@w+@v��@v�@vȴ@v��@vv�@v$�@u�-@up�@u`B@u?}@t�@s�m@s�@s"�@r��@rn�@r=q@q��@q��@qG�@p��@pĜ@p��@pr�@p  @o��@o�w@o�@nff@mO�@l�@l�j@l��@l��@lz�@lz�@lj@lZ@l9X@k��@j�!@i�#@iX@h��@h��@h��@h�u@hr�@hbN@g�w@g|�@g;d@g|�@gK�@g�@g�@g
=@f�@fȴ@f�R@f��@f��@f�+@f5?@f{@e�T@e�@d1@cƨ@c�F@c��@c��@c��@cdZ@b�H@b��@b�!@b�\@bn�@b=q@bJ@a�@aX@`�`@`�9@`bN@`1'@`b@_�;@_�@_�P@_�P@_l�@_+@^��@^��@^V@]@]�@\��@\�@\��@\�j@\j@[��@[ƨ@[��@[dZ@Z�@Z��@Z��@Z��@Z��@Zn�@ZM�@Y�^@Yhs@Y%@X��@XbN@W��@W��@V��@V�+@V@UV@T�D@T�D@T�D@Tz�@T1@SS�@So@R-@Q�7@Qhs@Q7L@Q%@P��@P�9@PA�@O|�@N�y@N�R@Nv�@N$�@M��@M��@M��@M?}@L��@L��@L�@LZ@K�m@K@J=q@I��@I�7@H�@G�w@G;d@G
=@F�y@F��@Fv�@Fff@FV@F5?@F$�@E��@D�@D�j@Dj@D(�@C�m@C��@C�@Ct�@CdZ@CS�@C"�@C@B�@B��@B��@B�\@B~�@B�@A�#@Ax�@@��@@��@@  @?��@?K�@?;d@?+@>�y@>��@>V@>$�@>@=�@=�-@=��@=�h@=`B@<�/@<�D@<9X@<9X@<�@;ƨ@;�F@;��@;t�@:�@:��@:^5@9�#@9��@9x�@9X@9G�@9&�@8�`@8�u@8bN@7�@7�w@7��@7+@6�+@5�@5�T@5@5�-@5�-@5p�@5O�@5�@4�@4��@4�@4z�@4I�@4(�@3�F@3��@3��@3�@3t�@3dZ@3dZ@3dZ@3C�@2�!@2=q@1��@1��@1x�@1hs@1X@1%@0�`@0Ĝ@0�u@0Q�@/�@/��@/�@/l�@/+@.��@.�R@.��@.�+@.5?@.@-�T@-��@-�-@-�@-O�@-`B@-`B@,��@,�@,I�@,�@,�@+��@+ƨ@+�F@+dZ@*�@*�!@*��@*�\@*~�@*^5@*-@)��@)hs@)X@)G�@(Ĝ@(r�@(1'@'��@'�P@'
=@&ȴ@&�R@&$�@%�@%O�@%�@$��@$�@$j@$�@#�F@#t�@#S�@#C�@#33@#o@"��@"�!@"��@"M�@!�#@!��@!x�@!hs@!X@!%@ �9@ r�@ r�@ bN@ A�@�;@�@�P@+@�@
=@�R@v�@�@�-@�h@p�@p�@`B@O�@?}@�@��@��@��@�D@I�@��@ƨ@�F@�F@��@��@�@S�@��@��@�!@�!@M�@J@�#@��@��@hs@�@�@b@�@l�@�@
=@��@�@��@E�@E�@$�@{@{@{@��@`B@��@z�@9X@��@�
@ƨ@�F@dZ@@�!@~�@�@�^@�7@hs@7L@%@�`@Ĝ@�9@��@�@Q�@ �@�@�w@\)@�@�R@��@�+@ff@V@5?@{@{@@�@�-@��@��@��@�h@�h@p�@`B@�@�/@�@Z@Z@I�@9X@9X@9X@�m@t�@o@@
�@
�H@
�H@
�!@
~�@
n�@
n�@
M�@
=q@
-@
�@
J@	�#@	��@	��@	�^@	7L@Ĝ@r�@ �@  @�;@�w@�@l�@K�@K�@K�@K�@;d@�@
=@�y@ȴ@ȴ@�R@��@5?@p�@/@/@�@V@V@V@V@��@��@�@�@�/@�j@9X@��@�m@ƨ@��@�@dZ@S�@o@�@�@�@�H@��@��@�!@��@n�@=q@-@J@�@�@��@��@�7@7L@%@ �`A��PA��hA���A���A���A���A���A��uA���A���A���A���A���A���A���A���A���A���A��uA���A��uA���A���A���A���A���A���A��uA���A��uA���A��uA���A��hA��uA���A��uA���A��hA���A��hA��uA��uA���A���A��uA���A��\A��uA��PA��\A��PA��uA��hA��hA��uA��hA��hA��PA��\A��PA��PA��PA��DA��PA��7A��PA��DA��PA��7A��\A��PA��uA��hA��hA��hA��hA��uA��hA���A��hA��uA��\A��hA��hA��PA��7A��+A��DA��A��+A��DA��+A��7A��+A��7A��7A��+A��7A��DA��+A��DA��+A��7A��7A��+A��DA��+A��7A��DA��7A��PA��DA��DA��DA��7A��DA��PA��7A��DA��DA��7A��PA��7A��DA��+A��7A��+A��+A��DA��+A��DA��7A��7A��PA��DA��7A��PA��DA��7A��DA��DA��7A��PA��7A��+A��DA��+A��7A��DA��+A��7A��PA��7A��7A��7A��+A��+A��7A��7A��A��+A��+A��A��+A��A��A��A��A��A�z�A�z�A�x�A�t�A�t�A�v�A�t�A�p�A�r�A�n�A�jA�jA�jA�jA�jA�hsA�ffA�dZA�S�A�E�A�A�A�A�A�33A�/A�(�A�{A���A��A��A���A���A��uA�~�A�v�A�l�A�ffA�^5A�dZA�`BA�^5A�C�A�+A��A���A���A��A��
A���A�A��-A��uA�n�A�VA�;dA�{A���A��TA��HA��A���A��wA��9A���A��A�t�A�p�A�l�A�\)A�Q�A�O�A�M�A�=qA�&�A��A�bA�JA�%A���A��A��A��`A���A�ĜA��wA��!A���A���A��hA��PA�x�A�jA�^5A�Q�A�;dA�+A� �A��A�
=A���A��A��TA��A���A���A���A���A�ȴA�ƨA���A��jA��FA��-A��-A���A���A���A���A���A��uA��DA��A�n�A�ffA�Q�A�C�A�7LA� �A�%A���A��`A���A�ȴA���A��RA��9A��A��DA�x�A�p�A�ffA�\)A�S�A�S�A�M�A�G�A�A�A�=qA�9XA�/A�&�A�oA�JA���A��A��A��;A�ĜA��-A���A���A�z�A�hsA�O�A�(�A��A�oA�bA�  A���A��-A��A���A���A��PA��+A�jA�S�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�K�A�I�A�I�A�I�A�G�A�C�A�9XA�/A��A�ĜA��9A��A���A���A��A�I�A�(�A��A�
=A��A��A��A��A��A��;A��
A���A���A�K�A��A�  A��`A���A���A��jA��A���A��uA��hA��+A�t�A�ZA�XA�O�A�9XA�1'A�(�A��A��A��A���A��#A���A���A��jA��-A��!A��-A��A��A���A���A���A���A��uA��\A��DA��+A�~�A�z�A�z�A�v�A�r�A�n�A�n�A�hsA�bNA�C�A�(�A���A��A��/A��wA�~�A� �A��A�33A��yA���A�dZA�K�A�33A�"�A�{A�  A��HA���A��wA�A���A�%A��A��mA��#A���A�ƨA���A��+A�`BA�S�A�K�A�9XA�{A���A��DA�33A��A�A��RA���A�~�A�v�A�dZA�M�A�+A��A��jA��!A��uA�+A��A��A�1A�%A�A��yA��#A���A�A��9A��A���A���A���A���A���A���A���A���A��uA��hA��PA��DA��A�|�A�r�A�n�A�bNA�ZA�XA�XA�S�A�Q�A�M�A�;dA�&�A��A��A��A��A�{A�VA�%A�  A��A��`A��A���A���A�A��9A���A���A��\A��A�x�A�p�A�hsA�S�A�7LA� �A�%A���A���A���A��A��HA��RA��7A�O�A�(�A�1A��mA���A��jA���A��A�dZA�VA�&�A���A��\A�XA�G�A�/A�
=A���A��A��^A��\A�ffA�A�A��A��A�JA���A��;A�ȴA���A�~�A�Q�A�$�A�A��mA���A�ĜA���A��A�p�A�S�A�=qA�-A�{A��A��`A��TA��;A��#A��A��#A��A��
A���A���A���A�A��!A��PA�jA�7LA��A��A�l�A�C�A�-A�&�A��A��A�bA�
=A�A��A���A��FA��!A���A��hA�~�A�hsA�"�A��A��A��TA��;A���A���A���A�ȴA�ĜA�ĜA�A��jA��^A��RA��-A��-A��A���A��A�dZA�C�A�(�A��A���A��A�dZA�?}A��A�A��A��A��DA�jA�M�A�/A�
=A���A��;A���A��FA��A��!A���A���A��A�hsA�C�A��A���A��A���A��A��-A�O�A�A��A��9A��DA�r�A�jA�dZA�bNA�ZA�VA�A�A�5?A�(�A�"�A�bA��/A��DA�x�A�K�A�E�A�7LA�+A� �A�oA�  A�FA�hAp�AO�A�A~�`A~�A~�A}A}�7A}\)A}"�A|��A|�A|z�A|Q�A|I�A|5?A|$�A|{A|bA|  A{��A{��A{dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                              A���A���A���A���A���A���A���A���A���A��uA��uA��uA��hA��hA��\A��DA��\A��uA��PA��+A��7A��7A��DA��DA��7A��7A��DA��7A��7A��7A��A�|�A�r�A�jA�C�A���A�x�A�A�A���A�7LA��wA�bNA� �A��`A���A�E�A��A�ȴA��!A��DA�9XA���A��A�K�A��A��wA�9XA���A�jA�K�A�A�A��FA�"�A��TA�$�A���A�S�A�oA��wA���A��A�l�A��A�C�A�{A�jA���A���A���A��A��DA��jA��\A�Q�A�A��uA��A�z�A���A��uA�`BA��#A��A���A�=qA�ĜA�bNA���A��#A�
=A��!A�A�oA~r�A|~�A{dZAy��AxJAv�!Avn�AvM�Aul�AsVAn�\AmVAl�/Al�DAkO�Ai\)Ag�FAg;dAf�\Ad��Ab�jAa�#Aat�Aa�A`�jA]��A[x�AXz�AV��AU�mAT��ASt�ARJAQS�AO�FAN��AM�7AK�^AK/AJ��AI��AG|�AFE�AEoAC�ABȴAB9XAB�AA��AA�TAAAAC�A@ZA@  A?��A>�RA<��A;�
A;O�A: �A8bNA7��A7
=A6�RA6$�A4JA3t�A3G�A2jA1�A0�9A/�;A.�`A-hsA,��A,�A,  A*�A)��A)�A(�/A(��A(~�A(Q�A(-A'��A%��A#�7A"��A �`A  �A��A��A�A�jA(�A|�A�Ap�Ar�A�;A�AAQ�AG�A�jA��AVA9XA��A$�AO�A��A��A�A�uA�AAK�A
~�A	��A��A�jA�uA1A��A�;A��A�HA1AhsAffA  A��Ap�A\)AO�A;d@��;@�b@�@��F@�-@���@�G�@��@�&�@���@�dZ@�33@��@�
=@�@�n�@�h@�j@�\)@��#@��@���@��@���@�z�@�Q�@�9X@��@���@��m@�ƨ@�ƨ@���@���@���@߶F@�C�@�C�@�"�@��@���@���@އ+@�x�@ܴ9@��@�  @���@թ�@Ձ@�/@��m@�O�@�C�@�V@��`@���@�o@ʗ�@�-@ɡ�@��@�1'@��@�E�@�hs@öF@�~�@��@��`@� �@��w@�t�@�v�@�5?@�@�r�@�C�@��H@��T@���@��@�r�@��P@���@��7@��@�^5@��@�?}@�z�@��P@�~�@��T@�`B@�z�@�  @��@�+@��@��@��@���@��@�hs@��j@�b@�1@��@�K�@�n�@���@��@�1'@�o@�ff@�M�@�-@�G�@�o@�E�@���@��h@�`B@�G�@��@���@��9@�bN@�1'@���@�"�@���@�=q@��@���@��^@���@��7@�O�@��`@���@�Q�@�1@���@��P@��@���@�n�@�5?@��T@���@�G�@���@�Ĝ@�r�@�b@���@�+@�V@���@��^@��7@�`B@�%@��`@��/@���@��@�I�@�(�@�1@��;@��F@���@���@�l�@��@��@���@�@�&�@��`@�z�@��m@�dZ@�
=@���@�M�@���@��-@��h@��@�p�@�`B@�&�@��/@��@�j@�1@+@~�y@~v�@~$�@~{@}�@|��@|�D@|I�@{ƨ@{S�@{@z��@zM�@y��@y�7@y�@xĜ@x�@xA�@x  @w��@w��@w|�@w+@v��@v�@vȴ@v��@vv�@v$�@u�-@up�@u`B@u?}@t�@s�m@s�@s"�@r��@rn�@r=q@q��@q��@qG�@p��@pĜ@p��@pr�@p  @o��@o�w@o�@nff@mO�@l�@l�j@l��@l��@lz�@lz�@lj@lZ@l9X@k��@j�!@i�#@iX@h��@h��@h��@h�u@hr�@hbN@g�w@g|�@g;d@g|�@gK�@g�@g�@g
=@f�@fȴ@f�R@f��@f��@f�+@f5?@f{@e�T@e�@d1@cƨ@c�F@c��@c��@c��@cdZ@b�H@b��@b�!@b�\@bn�@b=q@bJ@a�@aX@`�`@`�9@`bN@`1'@`b@_�;@_�@_�P@_�P@_l�@_+@^��@^��@^V@]@]�@\��@\�@\��@\�j@\j@[��@[ƨ@[��@[dZ@Z�@Z��@Z��@Z��@Z��@Zn�@ZM�@Y�^@Yhs@Y%@X��@XbN@W��@W��@V��@V�+@V@UV@T�D@T�D@T�D@Tz�@T1@SS�@So@R-@Q�7@Qhs@Q7L@Q%@P��@P�9@PA�@O|�@N�y@N�R@Nv�@N$�@M��@M��@M��@M?}@L��@L��@L�@LZ@K�m@K@J=q@I��@I�7@H�@G�w@G;d@G
=@F�y@F��@Fv�@Fff@FV@F5?@F$�@E��@D�@D�j@Dj@D(�@C�m@C��@C�@Ct�@CdZ@CS�@C"�@C@B�@B��@B��@B�\@B~�@B�@A�#@Ax�@@��@@��@@  @?��@?K�@?;d@?+@>�y@>��@>V@>$�@>@=�@=�-@=��@=�h@=`B@<�/@<�D@<9X@<9X@<�@;ƨ@;�F@;��@;t�@:�@:��@:^5@9�#@9��@9x�@9X@9G�@9&�@8�`@8�u@8bN@7�@7�w@7��@7+@6�+@5�@5�T@5@5�-@5�-@5p�@5O�@5�@4�@4��@4�@4z�@4I�@4(�@3�F@3��@3��@3�@3t�@3dZ@3dZ@3dZ@3C�@2�!@2=q@1��@1��@1x�@1hs@1X@1%@0�`@0Ĝ@0�u@0Q�@/�@/��@/�@/l�@/+@.��@.�R@.��@.�+@.5?@.@-�T@-��@-�-@-�@-O�@-`B@-`B@,��@,�@,I�@,�@,�@+��@+ƨ@+�F@+dZ@*�@*�!@*��@*�\@*~�@*^5@*-@)��@)hs@)X@)G�@(Ĝ@(r�@(1'@'��@'�P@'
=@&ȴ@&�R@&$�@%�@%O�@%�@$��@$�@$j@$�@#�F@#t�@#S�@#C�@#33@#o@"��@"�!@"��@"M�@!�#@!��@!x�@!hs@!X@!%@ �9@ r�@ r�@ bN@ A�@�;@�@�P@+@�@
=@�R@v�@�@�-@�h@p�@p�@`B@O�@?}@�@��@��@��@�D@I�@��@ƨ@�F@�F@��@��@�@S�@��@��@�!@�!@M�@J@�#@��@��@hs@�@�@b@�@l�@�@
=@��@�@��@E�@E�@$�@{@{@{@��@`B@��@z�@9X@��@�
@ƨ@�F@dZ@@�!@~�@�@�^@�7@hs@7L@%@�`@Ĝ@�9@��@�@Q�@ �@�@�w@\)@�@�R@��@�+@ff@V@5?@{@{@@�@�-@��@��@��@�h@�h@p�@`B@�@�/@�@Z@Z@I�@9X@9X@9X@�m@t�@o@@
�@
�H@
�H@
�!@
~�@
n�@
n�@
M�@
=q@
-@
�@
J@	�#@	��@	��@	�^@	7L@Ĝ@r�@ �@  @�;@�w@�@l�@K�@K�@K�@K�@;d@�@
=@�y@ȴ@ȴ@�R@��@5?@p�@/@/@�@V@V@V@V@��@��@�@�@�/@�j@9X@��@�m@ƨ@��@�@dZ@S�@o@�@�@�@�H@��@��@�!@��@n�@=q@-@J@�@�@��@��@�7@7L@%G�O�A��PA��hA���A���A���A���A���A��uA���A���A���A���A���A���A���A���A���A���A��uA���A��uA���A���A���A���A���A���A��uA���A��uA���A��uA���A��hA��uA���A��uA���A��hA���A��hA��uA��uA���A���A��uA���A��\A��uA��PA��\A��PA��uA��hA��hA��uA��hA��hA��PA��\A��PA��PA��PA��DA��PA��7A��PA��DA��PA��7A��\A��PA��uA��hA��hA��hA��hA��uA��hA���A��hA��uA��\A��hA��hA��PA��7A��+A��DA��A��+A��DA��+A��7A��+A��7A��7A��+A��7A��DA��+A��DA��+A��7A��7A��+A��DA��+A��7A��DA��7A��PA��DA��DA��DA��7A��DA��PA��7A��DA��DA��7A��PA��7A��DA��+A��7A��+A��+A��DA��+A��DA��7A��7A��PA��DA��7A��PA��DA��7A��DA��DA��7A��PA��7A��+A��DA��+A��7A��DA��+A��7A��PA��7A��7A��7A��+A��+A��7A��7A��A��+A��+A��A��+A��A��A��A��A��A�z�A�z�A�x�A�t�A�t�A�v�A�t�A�p�A�r�A�n�A�jA�jA�jA�jA�jA�hsA�ffA�dZA�S�A�E�A�A�A�A�A�33A�/A�(�A�{A���A��A��A���A���A��uA�~�A�v�A�l�A�ffA�^5A�dZA�`BA�^5A�C�A�+A��A���A���A��A��
A���A�A��-A��uA�n�A�VA�;dA�{A���A��TA��HA��A���A��wA��9A���A��A�t�A�p�A�l�A�\)A�Q�A�O�A�M�A�=qA�&�A��A�bA�JA�%A���A��A��A��`A���A�ĜA��wA��!A���A���A��hA��PA�x�A�jA�^5A�Q�A�;dA�+A� �A��A�
=A���A��A��TA��A���A���A���A���A�ȴA�ƨA���A��jA��FA��-A��-A���A���A���A���A���A��uA��DA��A�n�A�ffA�Q�A�C�A�7LA� �A�%A���A��`A���A�ȴA���A��RA��9A��A��DA�x�A�p�A�ffA�\)A�S�A�S�A�M�A�G�A�A�A�=qA�9XA�/A�&�A�oA�JA���A��A��A��;A�ĜA��-A���A���A�z�A�hsA�O�A�(�A��A�oA�bA�  A���A��-A��A���A���A��PA��+A�jA�S�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�K�A�I�A�I�A�I�A�G�A�C�A�9XA�/A��A�ĜA��9A��A���A���A��A�I�A�(�A��A�
=A��A��A��A��A��A��;A��
A���A���A�K�A��A�  A��`A���A���A��jA��A���A��uA��hA��+A�t�A�ZA�XA�O�A�9XA�1'A�(�A��A��A��A���A��#A���A���A��jA��-A��!A��-A��A��A���A���A���A���A��uA��\A��DA��+A�~�A�z�A�z�A�v�A�r�A�n�A�n�A�hsA�bNA�C�A�(�A���A��A��/A��wA�~�A� �A��A�33A��yA���A�dZA�K�A�33A�"�A�{A�  A��HA���A��wA�A���A�%A��A��mA��#A���A�ƨA���A��+A�`BA�S�A�K�A�9XA�{A���A��DA�33A��A�A��RA���A�~�A�v�A�dZA�M�A�+A��A��jA��!A��uA�+A��A��A�1A�%A�A��yA��#A���A�A��9A��A���A���A���A���A���A���A���A���A��uA��hA��PA��DA��A�|�A�r�A�n�A�bNA�ZA�XA�XA�S�A�Q�A�M�A�;dA�&�A��A��A��A��A�{A�VA�%A�  A��A��`A��A���A���A�A��9A���A���A��\A��A�x�A�p�A�hsA�S�A�7LA� �A�%A���A���A���A��A��HA��RA��7A�O�A�(�A�1A��mA���A��jA���A��A�dZA�VA�&�A���A��\A�XA�G�A�/A�
=A���A��A��^A��\A�ffA�A�A��A��A�JA���A��;A�ȴA���A�~�A�Q�A�$�A�A��mA���A�ĜA���A��A�p�A�S�A�=qA�-A�{A��A��`A��TA��;A��#A��A��#A��A��
A���A���A���A�A��!A��PA�jA�7LA��A��A�l�A�C�A�-A�&�A��A��A�bA�
=A�A��A���A��FA��!A���A��hA�~�A�hsA�"�A��A��A��TA��;A���A���A���A�ȴA�ĜA�ĜA�A��jA��^A��RA��-A��-A��A���A��A�dZA�C�A�(�A��A���A��A�dZA�?}A��A�A��A��A��DA�jA�M�A�/A�
=A���A��;A���A��FA��A��!A���A���A��A�hsA�C�A��A���A��A���A��A��-A�O�A�A��A��9A��DA�r�A�jA�dZA�bNA�ZA�VA�A�A�5?A�(�A�"�A�bA��/A��DA�x�A�K�A�E�A�7LA�+A� �A�oA�  A�FA�hAp�AO�A�A~�`A~�A~�A}A}�7A}\)A}"�A|��A|�A|z�A|Q�A|I�A|5?A|$�A|{A|bA|  A{��A{��A{dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�B�MB�{B�GB�GB��B�GB��B��B�uB�AB�B��B�B��B�B�iB��B��B��B�4B�4B�B.B~�B~�B~�B}�B|�B{Bx�Bu%Bm]Ba�B\�BS�BIRB<jB49B0�B-CB)*B!�B�B�BhB�B�B��B��B��B��B�B�B�EB�vB�#BȀB�}B�?B�kB�B��B�$B��B�B�7B��B�iBw2BkBN�BEmB7�B,�B�BbB{B��B��B�B�BBخB��B�OB��B�_B�_Bz�Bv+Bb�BZ�BOBI�B8B$B�B��B��BרB�pB�B��B��B��B�B�%B��B|�Bl"BT�BB'B<jB9XB2aB+�B~B�B�B_B
�fB
�/B
�WB
�B
�NB
��B
�tB
�dB
��B
��B
�-B
�B
��B
�B
�lB
{�B
|PB
oiB
hsB
g�B
c B
b�B
T�B
JXB
DgB
;�B
6zB
5tB
3�B
3�B
0�B
1[B
+kB
)_B
'RB
&�B
�B
B
�B
�B
MB	��B	�]B	�B	�%B	��B	��B	�"B	�KB	��B	�B	�TB	�TB	�/B	خB	�2B	уB	�<B	ʌB	�-B	��B	��B	�qB	��B	��B	�zB	�'B	�B	��B	�qB	��B	��B	�MB	��B	��B	��B	�DB	�B	��B	��B	��B	�B	~�B	|B	|�B	{B	v�B	sB	m�B	jB	h
B	bB	`�B	\�B	^�B	Z�B	V�B	S[B	O�B	PHB	OBB	K�B	I�B	G�B	G�B	F�B	B�B	@�B	C�B	>B	@B	<�B	<B	:�B	9�B	8RB	6FB	2-B	6�B	3hB	-CB	/�B	'�B	)�B	+B	%FB	%zB	#�B	#B	"�B	!�B	!�B	 'B	!-B	 \B	�B	�B	�B	�B	OB	OB	xB	xB	CB	B	CB	B	CB	CB	�B	�B	qB	qB	B	B	�B	qB		B	=B	�B	kB	�B	7B		B	�B	�B	1B	_B	�B	7B	eB	�B	B	qB	=B	�B	B	B	�B	�B	B	B	qB	�B	B	OB	�B	�B	�B	B	B	�B	�B	OB	!�B	 �B	 �B	VB	'�B	&LB	'�B	)*B	*eB	*�B	.�B	0�B	;0B	A B	A�B	C�B	F�B	HKB	I�B	K�B	MB	M�B	M�B	M�B	M�B	MjB	MjB	OBB	Q�B	T�B	WsB	V�B	VmB	YB	\�B	_;B	bB	d�B	i�B	k�B	kQB	j�B	p�B	{�B	~�B	� B	�uB	��B	�B	�MB	��B	�%B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�$B	��B	��B	�CB	�~B	��B	��B	�4B	��B	�B	��B	�eB	�B	�B	�!B	��B	��B	��B	��B	��B	�dB	��B	�B	ɺB	��B	̘B	͟B	бB	��B	�NB	ӏB	��B	�gB	�mB	רB	�B	�#B	�WB	��B	��B	�pB	��B	�B	�mB	�B	��B	��B	�2B	�B	�VB
 �B
�B
�B
	lB

�B
B
�B
~B
VB
�B
�B
�B
�B
kB
�B
B
�B
!-B
#nB
%B
&�B
'RB
)_B
*�B
,=B
.B
0!B
1'B
2�B
4nB
5�B
5�B
6�B
7�B
8�B
9�B
:�B
;�B
<�B
=<B
=qB
>wB
?HB
A�B
C-B
DgB
D�B
F�B
G�B
K�B
M�B
OvB
P�B
S&B
T,B
VB
W�B
ZQB
[�B
\]B
]dB
^jB
aB
a�B
a�B
d&B
g�B
l�B
n/B
n�B
o5B
oiB
o�B
pB
pB
p;B
poB
s�B
x8B
{B
|�B
~(B
~�B
� B
�B
��B
�AB
��B
�YB
�7B
��B
�xB
�JB
�~B
�~B
��B
��B
�VB
�(B
��B
��B
��B
� B
�4B
�uB
��B
��B
�B
�$B
�YB
��B
��B
��B
��B
�xB
��B
�~B
�OB
�!B
��B
�hB
��B
�B
�LB
�B
�RB
�$B
��B
��B
��B
�_B
�eB
�6B
��B
�B
�}B
��B
�'B
��B
�[B
�-B
��B
��B
�FB
��B
��B
�XB
��B
��B
��B
�^B
��B
��B
��B
�qB
��B
�B
��B
�OB
�B
��B
B
�aB
��B
�zB
��B
�KB
�KB
�RB
��B
��B
�HB
уB
уB
� B
� B
ҽB
�[B
ӏB
�gB
�?B
خB
�KB
�QB
ںB
ںB
�#B
��B
��B
��B
�dB
��B
��B
�B
�TB
�B
�B
�sB
�B
�B
�KB
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
�B
�B
�vB
�B
�|B
�|B
�B
�B
�MB
�B
�B
�B
�B
�TB
�TB
��B
�8B
��B
��B
�xB
�B
��B
�VB
�VB
�VB
��B
�]B
�.B
��B
��B
��B iB iB iB;BBuBGB�BGB�B�B�BMBSBBYB_B�B�B�B�B1B�B	lB	lB
�B
rB
�B�B�B�B�B(B(B\B�B.B�B B4BhB�B�BBBBB@B@B@B@BBuBFBBB�B�B�B�B�B�B�B_B�BeB�B�B7B�B	B=B=BqBBxBxBxB�BBB�B�B�BOB!B!B!B \B �B �B!�B"�B#:B#nB$@B$�B$�B%FB&LB&�B&�B&�B'�B'�B'�B($B(�B)*B(�B(�B)�B)�B)_B)�B)�B*0B*�B+6B,=B,qB,�B,qB,�B,�B-wB-wB-wB-�B.�B.�B/�B/�B/�B0�B0�B1[B1'B1'B1[B1�B2aB2aB2�B2�B2�B33B3�B4nB4nB4�B5B5B5B4�B5B5?B5�B6FB6FB6zB6�B8B8RB8�B8RB8RB8�B8�B8�B9�B9�B9�B9�B:^B:�B;dB;�B;�B<B<6B<�B=B<�B>B>B>wB>�B?B?HB?HB?HB?}B?�B@�BA BB�BC�BC�BD�BD�BE9BE9BEBEBE�BE�BFtBFtBF�BGzBG�BG�BHKBH�BIRBI�BI�BI�BI�BJ#BJXBJXBJ�BK�BK�BK�BK�BL0BLdBLdBL�BL�BL�BL�BMBM6BM6BM6BM6BM6BMjBMjBMjBM�BNBN�BOBN�BOBOBN�BN�BO�BPHBP�BP�BP�BP�BP�BQBQBQNBQNBQ�BQ�BQ�BQ�BQ�BRTBQ�BQNBQNBR�BR�BS[BS�BS�BS�BS�BS�BT,BTaBTaBTaBT,BTaBT�BTaBT�BT�BT�BT�BT�BU2BU�BUgBU2BU�BU�BU�BU�BVBVBV9BV9BVBV9BVmBW
BW?BWsBW�BWsBWsBW�BW�BXBXyBX�BX�BYBYBYBYBYKBYBYBYBYBYBYBZBY�BY�BZ�BZ�B[WB�MB��B��B��B��B�B�B��B��B�B��B��B��B�B�B��B��B��B�B��B��B��B�{B�B��B�{B��B�B��B��B�B�{B�B��B�uB�uB�B�B��B�B�GB��B�B�AB�AB�B��B�B�oB��B�B�B�B��B�AB��B�B��B��B�iB��B��B�B��B��B�{B�;B�B��B�AB�iB�uB�B��B��B�B��B�oB�uB�iB�AB�B��B�B�B�oB�B�iB�B�B�iB�B�B� B�iB�;B� B�AB�;B�iB�AB�B�B��B��B�B� B�;B�iB�B�;BcB�B��B�B��B��B�B�B.B�B�;B.B��BcB� B�B�iB�iBcB��B~�BcB�4B.B~(B�B~�BcB�B~�B~]B�B}�B.B�B}�B�B~�B}�B�B}�B}�B.B�B.B~�B.B}�B}�B�B}�B}�B~�B~�B|�B}�B}�B{�B|�B}�B|�Bz�B|�B|PBz�B{B|PBzxBz�B{�BxByrBzxBwfBw�By>Bv�B|PBv�Bv�BqvBs�BsBs�BqAB�SBhsBd�Bc�Bd�BhsB_�Ba�Bb�B^jB]�B]�BZB\)B\�BbNBa�BRTBT�BS�BX�BO�BR�BQ�BOvBO�BJ�BJ#BK)BB'B@�B=B<6B<jB<jB8�B=qB>BB6FB3�B49B8�B3�B0!B1[B5�B3�B0�B,�B.}B.�B/OB.B-�B+6B-wB-B)�B*0B'�B)�B(�B'B*�B%�B#B!�B&�B�B!B�BVB�B7B_B�B�B$B�B�BFB@BoBBBB�B�B�BVB�BPBVBBJB~B�B
rB�B_BYB�BGB �BoB��B��B�B�B�DB��B�TB��B�B�TB�B�vB�vB�|B�B�AB��B��B�iB��B�"B�iB�B�B�"B�B�2B��B�,B�&B�HB�B�TB�B��B֡BںB�BٴBӏB��BҽBӏBѷB�BϫB�)B˒B�#B�RBɺB�)B��BɆB�B˒BɺBȀBȀB��BȴB��B��B��B��B��B�0B��B�-B�[B��B�B�hB�wB�wB�B��B�CB��B�B��B�B��B�OB��B�kB��B��B�YB�$B��B��B��B�B�1B��B��B�1B��B��B�YB��B�@B��B�B��B��B�"B�\B��B��B�lB�7B��B�	B�+B��B��B�YB��B�B�B�MB��B�GB�uB�oB}"B~�B|B}�B{Bv�BqABq�BoiBwfB{BrGBx�BZ�Bh
BW�BRTBR�BNBNBNBJ�BEmBC�B@�BLdB\�B7LB:*B=�B9�B5�B?}B7B7�B+6B)�B/�B0�B-�BB,BeB!�BVB$B�B B BB B{BBSB	7BFB�B�DB�;B�AB�B�B�B�B�vB��B�cB�QB��B��B��B��B��B��B�B�B�QB��B�B��B�B�B�B��B��B�mB�B�,B��B�B�B�fB�B�ZB��BߤB��B�|B�BߤBߤB��B�pB��B�QBںB��B�]B�KB�?BרB��B՛B�9B�
BچB֡B�TB�<B�B�jB˒B�}B͟B�BB�}B��B�9B�B�[B�}B�B�BB��B��B��B��B�RB�LB��B�}B��B��B��B�_B��B��B�-B��B��B�7B��B��B��B��B�xB��B��B��B��B��B�B�B�DB�fB��B�B��B��B��B�B|�B|�B}VB|�BzDByrBy	Bx�Bx8Bw�Bx�BxlB{JBv�B|By�Bx�BqABncBh�Bd�Be�Bc�Bd�BbBa|Bc�BdZBa�B[�B^5B_�BY�B_�BlWB[�BTaBR�BR�BU2BP�BP�BPBN�BNBM6BNBMBJ�BK)BI�BI�BJ�BO�BI�BJXBF�BJ�B@�BA�BGzB:�B5B7�B8�B3�B2�B/�B,B,B*�B$�B&�B#�B�BVBqBB�BxBBIB�B�B�BVB �B,�B�B_B��B��B��B�B�B�WB�B�B�B�"B�B�B�B��B�oB�2BیB�B�9B��B�
B�EB�sB��B��B҉B�B��B��B�TB�BѷB��B�EB�B��B��B�B�B�dB��B��B�$B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021021904455920210219044559IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030103010220210301030102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030103010220210301030102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164520210325101645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                