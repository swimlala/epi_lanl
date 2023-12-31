CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-09-21T16:08:10Z creation; 2023-04-26T19:14:28Z DMQC;      
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20190921160810  20230426191428  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               >   >AA  AOAO7316_008644_062                 7316_008644_062                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��i��@��i��11  @��i���'@��i���'@)�W}�W@)�W}�W�cҷ��a�cҷ��a11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@:�H@�  @�G�@�p�@�  AG�A��A   A,(�A?\)A`  A�Q�A�Q�A�  A�  A�Q�A�Q�A�Q�A�  B   B  B  B  B   B(  B0  B7�
B?�BG�
BO�
BX(�B`(�Bh  Bo�
BxQ�B�B��B�  B�  B��B�  B�{B�  B�  B�  B�{B�  B�  B�{B�  B��B��
B��B��B��B�{B�{B��B��B��B��B��B�  B�  B�  B�{B�  C   C��C��C��C��C	��C��C��C��C��C��C  C
=C  C��C��C 
=C"{C$
=C&  C(  C*  C,  C-��C/��C2  C4  C6
=C8  C:  C<
=C>  C?��CB  CD  CE��CH  CJ{CL{CN
=CP  CR  CT  CU��CW��CZ  C\
=C^
=C`  Ca��Cc��Ce�Ch  Cj  Ck��Cn
=Co��Cq��Ct  Cv  Cw�Cy��C|  C}��C�  C�
=C�C�C�  C�  C�  C���C�C�  C���C�C�C�  C���C�C�  C���C�  C��C���C�C�C�C�  C�  C�C�C�
=C�  C�  C�C�  C�C�C�  C�C�C�C�  C�
=C�C�  C�  C�C�C�  C���C�C�  C�  C�
=C�  C���C���C���C�C�  C���C�  C�  C���C���C�  C�C�C�  C���C���C�  C�C�
=C�C���C�  C���C��C���C���C�  C�C�  C���C�  C�C�  C�  C���C���C�  C���C���C�  C�
=C�
=C�
=C�
=C�C�C�  C�
=C�  C�  C���C���C�C�  C�  C�  C���C�C�C�C�  C�  C�  C�C�C���C���C�C�C�C�  C���C��C���C�D D ��D�D� D  D� D  D��D�qD� D  D��D�D��D�D}qD��Dz�D�qD	z�D
  D
� D
�qD� D�qD� D�D� D�qD}qD�qDz�D��Dz�D��Dz�D��D}qD�qDz�D�qDz�D��D}qD�qD��D�D}qD�qD� D  D� D�D�DD��DD� D�qD� D�D}qD  D� D�qD }qD!  D!� D"  D"��D#  D#}qD$  D$��D%�D%��D&�D&��D'�D'� D(  D(��D)  D)}qD)��D*� D+�D+��D,�D,� D,�qD-� D.  D.� D/  D/}qD/�qD0� D1�D1� D2  D2��D3  D3}qD4  D4� D5  D5��D6�D6}qD6�qD7}qD7�qD8� D9  D9� D:  D:� D:�qD;� D<�D<� D<�qD=� D=�qD>}qD>�qD?z�D?�qD@� DA  DA� DA�qDB}qDC  DC� DD  DD� DE  DE}qDF  DF}qDF��DG� DH�DH� DI  DI� DJ�DJ��DJ�qDK� DL  DL}qDL�qDMz�DM�qDN��DO  DO}qDO�qDP}qDP��DQ}qDR  DR� DS  DS� DT  DT��DU�DU� DV  DV� DW�DW��DXDX��DY  DY��DZ�DZ��D[  D[��D\�D\� D\�qD]}qD^  D^��D_  D_��D`�D`�Da  Da}qDb�Db��DcDc�Dd�Dd��De  De��DfDf��Dg  Dg}qDh�Dh�Dh�qDi}qDj�Dj� Dk  Dk� Dl�Dl��Dm  Dm}qDn  Dn� Do�Do�Dp�Dp}qDp�qDqz�Dq��Dr}qDs�Ds� Dt  Dt}qDt�qDu��DvDv��Dw�Dw� Dw�qDx��Dy�Dy��DzDz� Dz��D{z�D{�qD|}qD|�qD}}qD}�qD~}qD�D��D�  D�@ D��HD���D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�~�D���D�  D�@ D�� D���D�HD�AHD�� D�� D��qD�=qD�}qD��qD�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�B�D��HD�� D�HD�AHD�� D��HD��D�AHD�~�D�� D�  D�>�D��HD�D�HD�@ D�~�D��qD���D�@ D���D��HD���D�=qD�� D�� D�HD�AHD�~�D���D���D�=qD�� D��HD�HD�B�D�� D���D���D�@ D�~�D�� D�HD�AHD�~�D���D��qD�@ D���D�� D�HD�AHD��HD�� D�  D�@ D�}qD���D���D�@ D�� D���D�  D�@ D�}qD���D�HD�@ D�~�D�� D�HD�AHD�~�D�� D��D�B�D�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�@ D�}qD�� D�  D�=qD�~�D�� D���D�=qD�~�D��HD�  D�@ D��HD�D�HD�@ D��HD�D�HD�AHD�� D�� D�  D�AHD���D��HD�  D�>�D�~�D���D��qD�@ D��HD��HD�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�~�D�� D�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�B�D���D�D��D�AHD�~�D�� D�  D�@ D��HD�� D��qD�>�D�� D��HD�HD�B�D��HD�� D�HD�@ D�~�D�� D�HD�AHD��HD���D�  D�AHD�� D���D���D�@ D���D�� D���D�AHD���D�� D���D�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D�  D�AHD�� D�� D�  D�>�D�~�D¾�D��qD�=qD�~�D�� D�  D�@ DĀ D��HD�HD�@ Dŀ Dž�D���D�>�Dƀ D�� D���D�>�Dǀ D�� D���D�@ DȁHD�� D�  D�@ D�~�Dɾ�D�HD�AHDʁHD�� D�  D�@ D�~�D�� D�  D�>�D́HD�D�HD�>�D�~�D;�D�  D�>�D�~�D��HD�HD�>�D�~�D�� D���D�>�DЀ Dо�D���D�@ DсHD��HD�  D�@ D�~�DҾ�D�  D�AHDӁHD�� D�  D�@ DԀ D�� D�HD�@ D�~�DսqD���D�@ Dր D־�D���D�>�D�~�D��HD�  D�@ D؀ Dؾ�D���D�>�D�~�D�� D�HD�@ Dڀ D��HD�HD�>�Dۀ D��HD�HD�B�D܀ D�� D�HD�AHD݂�D��HD�HD�@ Dހ D޾�D��qD�@ D߁HD�� D�  D�@ D�~�D�� D�HD�AHD�HD��HD�HD�AHD� D�� D���D�>�D� D��HD�  D�AHD�HD�� D���D�@ D傏D��HD�HD�@ D�~�D�qD���D�>�D�~�D�qD���D�>�D�~�D辸D���D�@ D�~�D��HD�HD�AHD�~�D�� D���D�>�D�~�D�� D�HD�AHD� D쾸D�HD�AHD�HD�� D���D�>�D� D��HD�HD�>�D�~�DﾸD��qD�@ D�� D�� D�HD�AHD� D�� D�  D�AHD�HD�� D���D�>�D�~�D�D�  D�AHD�HD�� D�  D�AHD�� D��qD�  D�AHD��HD��HD��D�AHD�~�D�� D��D�B�D�� D���D�HD�B�D��HD��HD���D�:�>��
?\)?W
=?��R?���@   @
=@.{@J=q@aG�@xQ�@�=q@�Q�@��
@���@�p�@���@ٙ�@�ff@�AG�A�A�RAffA(�A"�\A*=qA1�A8Q�A>�RAG
=AMp�AS33AZ=qAb�\Ah��An�RAtz�Az�HA���A�33A�A���A��HA�p�A��A���A��A�A�
=A�G�A�33A���A�
=A�G�A��HA�z�A��RA���A�=qA�z�A�ffA�  A���A��
A�A�\)A�G�A�33A�z�A��RA���A\A�(�A�ffA�  Aə�A��
A��A�
=A�G�A�33A�z�A�ffA���A�=qA�(�A�ffA�Q�AᙚA��
A�{A�A陚A��
A�A�\)A�A��
A�p�A�\)A���A��
A�p�A�\)B ��B�B�RB�B��B�B�RB�B��B	B
�RB�
B��B��B�\B�B��B��B�\B�
B��B��B�RB�
B��B��B�RB�
B��BB�HB�B ��B!�B"�RB#�B$��B%�B&�RB'�B(��B)��B*�\B+�B,��B-p�B.�RB/�B0Q�B1G�B2ffB3\)B4  B4��B6{B6�HB7�B8��B9p�B:{B;
=B<  B<��B=p�B>ffB?\)B@(�B@��BA�BB�HBC�BDQ�BEG�BFffBG
=BG�
BH��BI�BJ�\BK\)BLz�BMp�BN{BO
=BP(�BP��BQBR�HBT  BU�BU�BV�HBX(�BYG�BZ=qB[\)B\��B]B^�RB_�B`��Bb{Bc\)BdQ�BeG�BfffBg�Bh��Bj{Bk33Bl(�Bmp�Bn�RBp  Bq�Br=qBs\)Bt��Bv{Bw33BxQ�ByG�Bz{Bz�HB{�B|z�B}�B}��B}B~{B~ffB~�\B~�\B~�RB~�\B~�\B~�\B~�RB~�HB~�HB~�RB~�RB~�RB~�RB~�RB~�HB
=B33B
=B~�HB
=B\)B�B�B�B�B�
B�{B�(�B�Q�B�ffB�z�B���B��RB��HB�
=B�G�B�p�B���B�B��B�{B�Q�B��\B���B�
=B�G�B�\)B��B��B�(�B�z�B���B��HB�
=B�\)B��B��B�(�B�Q�B��\B��HB�33B��B��
B�{B�ffB���B��HB��B�\)B�B�{B�ffB���B�
=B�G�B���B��B�=qB���B�
=B�\)B���B��
B�(�B�z�B��HB�33B���B��B�Q�B���B���B�G�B��B�  B�Q�B���B�33B���B�  B�Q�B��RB��B��B��B�ffB���B�33B���B�  B�Q�B���B��B�p�B��B�ffB��RB��B�p�B��
B�(�B��\B���B�\)B�B�(�B�z�B���B�33B��B��
B�(�B��\B���B�\)B�B�{B�z�B���B�33B��B��
B�(�B��\B��HB�33B���B�  B�Q�B���B��B��B��B�Q�B���B���B�G�B��B�{B�Q�B���B�
=B�p�B��
B�(�B�z�B��HB�33B���B�  B�Q�B���B�
=B�\)B��B�{B�ffB��RB��B��B��B�Q�B��RB��B��B�  B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�z�B��HB�33B���B�{B�ffB���B�G�B��B�{B�z�B��HB�G�B��B�(�B�z�B���B�\)B��
B�=qB��RB��B���B�{B\B�
=B�p�B��B�ffB��HB�G�B�B�=qBƸRB�33BǙ�B�{Bȣ�B�
=BɅB�  B�z�B�
=B�p�B�  B�z�B���B�p�B��B�ffB��HB�\)B��
B�ffB���B�\)B��
B�ffB��HB�\)B��
B�ffB��HB�p�B��B�z�B���B�p�B��B�z�B���BمB�{Bڏ\B�
=Bۙ�B�(�Bܣ�B��Bݙ�B�(�Bޣ�B�33B߮B�=qB�RB�33B�B�=qB�RB�G�B�B�=qB���B�G�B�B�Q�B��HB�p�B��B�z�B�
=B陚B�{B��B�33B�B�(�B��B�33B��B�(�B��B�33B�B�=qB�RB�G�B��
B�ffB���B�p�B�{B���B�33B��B�=qB���B�G�B��
B�Q�B���B�\)B��
B�ffB��HB��B�  B�z�B�
=B���B�(�B��RB�G�B�C (�C ffC �C �HC(�CffC��C�C33Cz�CC
=CG�C�\C�
C�C\)C��C�HC(�CffC�C�C33Cz�CC  CG�C�\C�
C�CffC�C��C	33C	p�C	�RC	��C
=qC
�C
C
=C=qC�\C��C
=CQ�C��C�
C�C\)C��C��C=qC�C��C
=CQ�C��C�HC�C\)C��C�HC�CffC�C��C=qC�CC  CG�C�C��C{C\)C�C��C33Cp�C�RC  CG�C�\C�HC33Cz�CC  CG�C�\C�C33C�CC{C\)C�C��C=qC�CC�CffC�RC{CffC�C��C=qC�C�
C33C�C�
C �C ffC �C!  C!Q�C!��C!��C"33C"z�C"��C#�C#p�C#C$
=C$Q�C$��C$��C%G�C%��C%��C&=qC&�C&�
C'(�C'�C'�
C(33C(z�C(��C)�C)ffC)C*{C*p�C*��C+�C+ffC+�C,
=C,ffC,C-{C-ffC-�RC.  C.Q�C.�C/{C/\)C/�C0  C0\)C0C1{C1ffC1�RC2
=C2ffC2��C3�C3p�C3C4{C4p�C4��C5(�C5p�C5C6{C6p�C6��C7(�C7�C7�HC833C8z�C8�
C933C9�\C9�C:=qC:�\C:�C;G�C;��C;�C<=qC<��C=  C=Q�C=�C=��C>G�C>�C?
=C?\)C?�RC@  C@\)C@CA�CAffCA�RCB{CBz�CB��CC(�CCp�CC��CD(�CD�CD�HCE(�CE�CE�CF=qCF��CF�HCG=qCG�\CG��CHQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 ?u?��H@:�H@�  @�G�@�p�@�  AG�A��A   A,(�A?\)A`  A�Q�A�Q�A�  A�  A�Q�A�Q�A�Q�A�  B   B  B  B  B   B(  B0  B7�
B?�BG�
BO�
BX(�B`(�Bh  Bo�
BxQ�B�B��B�  B�  B��B�  B�{B�  B�  B�  B�{B�  B�  B�{B�  B��B��
B��B��B��B�{B�{B��B��B��B��B��B�  B�  B�  B�{B�  C   C��C��C��C��C	��C��C��C��C��C��C  C
=C  C��C��C 
=C"{C$
=C&  C(  C*  C,  C-��C/��C2  C4  C6
=C8  C:  C<
=C>  C?��CB  CD  CE��CH  CJ{CL{CN
=CP  CR  CT  CU��CW��CZ  C\
=C^
=C`  Ca��Cc��Ce�Ch  Cj  Ck��Cn
=Co��Cq��Ct  Cv  Cw�Cy��C|  C}��C�  C�
=C�C�C�  C�  C�  C���C�C�  C���C�C�C�  C���C�C�  C���C�  C��C���C�C�C�C�  C�  C�C�C�
=C�  C�  C�C�  C�C�C�  C�C�C�C�  C�
=C�C�  C�  C�C�C�  C���C�C�  C�  C�
=C�  C���C���C���C�C�  C���C�  C�  C���C���C�  C�C�C�  C���C���C�  C�C�
=C�C���C�  C���C��C���C���C�  C�C�  C���C�  C�C�  C�  C���C���C�  C���C���C�  C�
=C�
=C�
=C�
=C�C�C�  C�
=C�  C�  C���C���C�C�  C�  C�  C���C�C�C�C�  C�  C�  C�C�C���C���C�C�C�C�  C���C��C���C�D D ��D�D� D  D� D  D��D�qD� D  D��D�D��D�D}qD��Dz�D�qD	z�D
  D
� D
�qD� D�qD� D�D� D�qD}qD�qDz�D��Dz�D��Dz�D��D}qD�qDz�D�qDz�D��D}qD�qD��D�D}qD�qD� D  D� D�D�DD��DD� D�qD� D�D}qD  D� D�qD }qD!  D!� D"  D"��D#  D#}qD$  D$��D%�D%��D&�D&��D'�D'� D(  D(��D)  D)}qD)��D*� D+�D+��D,�D,� D,�qD-� D.  D.� D/  D/}qD/�qD0� D1�D1� D2  D2��D3  D3}qD4  D4� D5  D5��D6�D6}qD6�qD7}qD7�qD8� D9  D9� D:  D:� D:�qD;� D<�D<� D<�qD=� D=�qD>}qD>�qD?z�D?�qD@� DA  DA� DA�qDB}qDC  DC� DD  DD� DE  DE}qDF  DF}qDF��DG� DH�DH� DI  DI� DJ�DJ��DJ�qDK� DL  DL}qDL�qDMz�DM�qDN��DO  DO}qDO�qDP}qDP��DQ}qDR  DR� DS  DS� DT  DT��DU�DU� DV  DV� DW�DW��DXDX��DY  DY��DZ�DZ��D[  D[��D\�D\� D\�qD]}qD^  D^��D_  D_��D`�D`�Da  Da}qDb�Db��DcDc�Dd�Dd��De  De��DfDf��Dg  Dg}qDh�Dh�Dh�qDi}qDj�Dj� Dk  Dk� Dl�Dl��Dm  Dm}qDn  Dn� Do�Do�Dp�Dp}qDp�qDqz�Dq��Dr}qDs�Ds� Dt  Dt}qDt�qDu��DvDv��Dw�Dw� Dw�qDx��Dy�Dy��DzDz� Dz��D{z�D{�qD|}qD|�qD}}qD}�qD~}qD�D��D�  D�@ D��HD���D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�~�D���D�  D�@ D�� D���D�HD�AHD�� D�� D��qD�=qD�}qD��qD�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�B�D��HD�� D�HD�AHD�� D��HD��D�AHD�~�D�� D�  D�>�D��HD�D�HD�@ D�~�D��qD���D�@ D���D��HD���D�=qD�� D�� D�HD�AHD�~�D���D���D�=qD�� D��HD�HD�B�D�� D���D���D�@ D�~�D�� D�HD�AHD�~�D���D��qD�@ D���D�� D�HD�AHD��HD�� D�  D�@ D�}qD���D���D�@ D�� D���D�  D�@ D�}qD���D�HD�@ D�~�D�� D�HD�AHD�~�D�� D��D�B�D�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�@ D�}qD�� D�  D�=qD�~�D�� D���D�=qD�~�D��HD�  D�@ D��HD�D�HD�@ D��HD�D�HD�AHD�� D�� D�  D�AHD���D��HD�  D�>�D�~�D���D��qD�@ D��HD��HD�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D�  D�>�D�~�D���D���D�>�D�~�D��qD��qD�>�D�~�D�� D�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�B�D���D�D��D�AHD�~�D�� D�  D�@ D��HD�� D��qD�>�D�� D��HD�HD�B�D��HD�� D�HD�@ D�~�D�� D�HD�AHD��HD���D�  D�AHD�� D���D���D�@ D���D�� D���D�AHD���D�� D���D�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D�  D�AHD�� D�� D�  D�>�D�~�D¾�D��qD�=qD�~�D�� D�  D�@ DĀ D��HD�HD�@ Dŀ Dž�D���D�>�Dƀ D�� D���D�>�Dǀ D�� D���D�@ DȁHD�� D�  D�@ D�~�Dɾ�D�HD�AHDʁHD�� D�  D�@ D�~�D�� D�  D�>�D́HD�D�HD�>�D�~�D;�D�  D�>�D�~�D��HD�HD�>�D�~�D�� D���D�>�DЀ Dо�D���D�@ DсHD��HD�  D�@ D�~�DҾ�D�  D�AHDӁHD�� D�  D�@ DԀ D�� D�HD�@ D�~�DսqD���D�@ Dր D־�D���D�>�D�~�D��HD�  D�@ D؀ Dؾ�D���D�>�D�~�D�� D�HD�@ Dڀ D��HD�HD�>�Dۀ D��HD�HD�B�D܀ D�� D�HD�AHD݂�D��HD�HD�@ Dހ D޾�D��qD�@ D߁HD�� D�  D�@ D�~�D�� D�HD�AHD�HD��HD�HD�AHD� D�� D���D�>�D� D��HD�  D�AHD�HD�� D���D�@ D傏D��HD�HD�@ D�~�D�qD���D�>�D�~�D�qD���D�>�D�~�D辸D���D�@ D�~�D��HD�HD�AHD�~�D�� D���D�>�D�~�D�� D�HD�AHD� D쾸D�HD�AHD�HD�� D���D�>�D� D��HD�HD�>�D�~�DﾸD��qD�@ D�� D�� D�HD�AHD� D�� D�  D�AHD�HD�� D���D�>�D�~�D�D�  D�AHD�HD�� D�  D�AHD�� D��qD�  D�AHD��HD��HD��D�AHD�~�D�� D��D�B�D�� D���D�HD�B�D��HD��HD���G�O�>��
?\)?W
=?��R?���@   @
=@.{@J=q@aG�@xQ�@�=q@�Q�@��
@���@�p�@���@ٙ�@�ff@�AG�A�A�RAffA(�A"�\A*=qA1�A8Q�A>�RAG
=AMp�AS33AZ=qAb�\Ah��An�RAtz�Az�HA���A�33A�A���A��HA�p�A��A���A��A�A�
=A�G�A�33A���A�
=A�G�A��HA�z�A��RA���A�=qA�z�A�ffA�  A���A��
A�A�\)A�G�A�33A�z�A��RA���A\A�(�A�ffA�  Aə�A��
A��A�
=A�G�A�33A�z�A�ffA���A�=qA�(�A�ffA�Q�AᙚA��
A�{A�A陚A��
A�A�\)A�A��
A�p�A�\)A���A��
A�p�A�\)B ��B�B�RB�B��B�B�RB�B��B	B
�RB�
B��B��B�\B�B��B��B�\B�
B��B��B�RB�
B��B��B�RB�
B��BB�HB�B ��B!�B"�RB#�B$��B%�B&�RB'�B(��B)��B*�\B+�B,��B-p�B.�RB/�B0Q�B1G�B2ffB3\)B4  B4��B6{B6�HB7�B8��B9p�B:{B;
=B<  B<��B=p�B>ffB?\)B@(�B@��BA�BB�HBC�BDQ�BEG�BFffBG
=BG�
BH��BI�BJ�\BK\)BLz�BMp�BN{BO
=BP(�BP��BQBR�HBT  BU�BU�BV�HBX(�BYG�BZ=qB[\)B\��B]B^�RB_�B`��Bb{Bc\)BdQ�BeG�BfffBg�Bh��Bj{Bk33Bl(�Bmp�Bn�RBp  Bq�Br=qBs\)Bt��Bv{Bw33BxQ�ByG�Bz{Bz�HB{�B|z�B}�B}��B}B~{B~ffB~�\B~�\B~�RB~�\B~�\B~�\B~�RB~�HB~�HB~�RB~�RB~�RB~�RB~�RB~�HB
=B33B
=B~�HB
=B\)B�B�B�B�B�
B�{B�(�B�Q�B�ffB�z�B���B��RB��HB�
=B�G�B�p�B���B�B��B�{B�Q�B��\B���B�
=B�G�B�\)B��B��B�(�B�z�B���B��HB�
=B�\)B��B��B�(�B�Q�B��\B��HB�33B��B��
B�{B�ffB���B��HB��B�\)B�B�{B�ffB���B�
=B�G�B���B��B�=qB���B�
=B�\)B���B��
B�(�B�z�B��HB�33B���B��B�Q�B���B���B�G�B��B�  B�Q�B���B�33B���B�  B�Q�B��RB��B��B��B�ffB���B�33B���B�  B�Q�B���B��B�p�B��B�ffB��RB��B�p�B��
B�(�B��\B���B�\)B�B�(�B�z�B���B�33B��B��
B�(�B��\B���B�\)B�B�{B�z�B���B�33B��B��
B�(�B��\B��HB�33B���B�  B�Q�B���B��B��B��B�Q�B���B���B�G�B��B�{B�Q�B���B�
=B�p�B��
B�(�B�z�B��HB�33B���B�  B�Q�B���B�
=B�\)B��B�{B�ffB��RB��B��B��B�Q�B��RB��B��B�  B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�z�B��HB�33B���B�{B�ffB���B�G�B��B�{B�z�B��HB�G�B��B�(�B�z�B���B�\)B��
B�=qB��RB��B���B�{B\B�
=B�p�B��B�ffB��HB�G�B�B�=qBƸRB�33BǙ�B�{Bȣ�B�
=BɅB�  B�z�B�
=B�p�B�  B�z�B���B�p�B��B�ffB��HB�\)B��
B�ffB���B�\)B��
B�ffB��HB�\)B��
B�ffB��HB�p�B��B�z�B���B�p�B��B�z�B���BمB�{Bڏ\B�
=Bۙ�B�(�Bܣ�B��Bݙ�B�(�Bޣ�B�33B߮B�=qB�RB�33B�B�=qB�RB�G�B�B�=qB���B�G�B�B�Q�B��HB�p�B��B�z�B�
=B陚B�{B��B�33B�B�(�B��B�33B��B�(�B��B�33B�B�=qB�RB�G�B��
B�ffB���B�p�B�{B���B�33B��B�=qB���B�G�B��
B�Q�B���B�\)B��
B�ffB��HB��B�  B�z�B�
=B���B�(�B��RB�G�B�C (�C ffC �C �HC(�CffC��C�C33Cz�CC
=CG�C�\C�
C�C\)C��C�HC(�CffC�C�C33Cz�CC  CG�C�\C�
C�CffC�C��C	33C	p�C	�RC	��C
=qC
�C
C
=C=qC�\C��C
=CQ�C��C�
C�C\)C��C��C=qC�C��C
=CQ�C��C�HC�C\)C��C�HC�CffC�C��C=qC�CC  CG�C�C��C{C\)C�C��C33Cp�C�RC  CG�C�\C�HC33Cz�CC  CG�C�\C�C33C�CC{C\)C�C��C=qC�CC�CffC�RC{CffC�C��C=qC�C�
C33C�C�
C �C ffC �C!  C!Q�C!��C!��C"33C"z�C"��C#�C#p�C#C$
=C$Q�C$��C$��C%G�C%��C%��C&=qC&�C&�
C'(�C'�C'�
C(33C(z�C(��C)�C)ffC)C*{C*p�C*��C+�C+ffC+�C,
=C,ffC,C-{C-ffC-�RC.  C.Q�C.�C/{C/\)C/�C0  C0\)C0C1{C1ffC1�RC2
=C2ffC2��C3�C3p�C3C4{C4p�C4��C5(�C5p�C5C6{C6p�C6��C7(�C7�C7�HC833C8z�C8�
C933C9�\C9�C:=qC:�\C:�C;G�C;��C;�C<=qC<��C=  C=Q�C=�C=��C>G�C>�C?
=C?\)C?�RC@  C@\)C@CA�CAffCA�RCB{CBz�CB��CC(�CCp�CC��CD(�CD�CD�HCE(�CE�CE�CF=qCF��CF�HCG=qCG�\CG��CHQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��/A���A��TA��mA��A��A��A�(�A�I�A�O�A�\A蛦A蟾A��A��A�!A�A��#A��A��`A���A�^A�!A��A藍A�A�l�A�VA���A� �A��A�ZAӲ-A�VA�&�A�ĜA�A�S�A�I�A�|�A���A�dZA�S�A���A�S�A��`A���A��A��A�;dA�\)A���A��7A�VA��RA��A��A�E�A�/A�hsA��`A���A��A~ȴAy+AsG�Ap�An  Al  Aj�uAh-Ae"�Ac��Ab�9Aa/A^��A\��AYXAU�7AShsAR(�AR�`AQ�AL��AHI�ADQ�AA7LA>��A=�-A<ffA;G�A:��A9�#A8�uA7p�A6�!A5\)A41'A3�^A3dZA3
=A2�yA2��A2��A2�`A2��A3%A3oA2�A05?A/A/;dA-��A,bA)�A(��A(��A)O�A)`BA(�DA(v�A'��A&I�A%dZA$��A#��A#&�A#+A#33A#�hA#��A#��A#�^A#�A#XA#"�A"�jA"I�A"bA!�A"  A!��A!�PA!7LA!VA �A ��A (�AS�A?}AC�A33A�/A�AAt�AS�AO�A33A%AĜAn�A-A�#A�TA�A��AVA�A�RA^5AA�
A��AK�A�A�HA�\A�
A�A`BA�A�/A��A=qA��A��A\)A�Az�A��A��AƨA��AG�AoA�jAr�AhsA�yAA��A{A�^A��A�A�PA�7At�A\)AK�A"�A�A�A��A��Av�A1A+A^5A�
AhsA;dA"�A�A
�RA
�A	G�A�HAȴA�jA�\Ar�AVA$�A�#A��Ax�AdZAK�A�`A~�A�AA��A�!A�+A�mA�AO�A�9AA�A�#A��A��A�PA\)A"�A �`A n�A M�A -A �@���@���@���@��@�^5@�%@� �@��F@�\)@�;d@�+@�o@���@�ȴ@���@�A�@�  @��@��F@��@�@���@�?}@�j@�ƨ@�o@�+@�J@��@��@�1'@@홚@�hs@�@�K�@�$�@��@�@�@�u@�I�@��@�K�@��@�$�@�@���@��#@�D@�S�@�5?@���@�1@߶F@߅@�dZ@�\)@�o@�{@�hs@���@� �@�o@�~�@ى7@أ�@��;@ו�@�33@��H@���@֟�@���@���@�  @���@�+@��y@ҸR@��@��@Ѳ-@��`@�bN@� �@��@�  @ϥ�@�;d@��y@��H@�V@�{@Ͳ-@�`B@�/@���@�Ĝ@�9X@�|�@ʸR@�n�@�v�@�~�@�E�@���@ɡ�@�hs@�7L@�%@���@ǥ�@Ƈ+@��@ũ�@�x�@���@��@�o@�@���@���@�(�@�t�@�
=@�v�@�{@��^@�X@�j@�1@�t�@��H@���@�-@��@��@��`@��@�9X@�ƨ@�t�@�-@���@��7@�x�@�G�@���@�A�@� �@���@�\)@��@�ȴ@��+@�-@�$�@��-@�O�@�%@��@�bN@�9X@� �@�  @�|�@�C�@�
=@��#@�%@��j@��D@��u@��u@�1'@��;@�|�@�C�@���@�5?@��-@�%@���@��D@�Z@�b@���@�l�@�C�@�+@��R@��+@�n�@�=q@�$�@��@��7@�%@�bN@�ƨ@�o@��H@���@�E�@�{@��#@�G�@�7L@�&�@��D@�bN@�I�@�1'@�  @���@�K�@�;d@�+@�+@�+@���@�ȴ@�ff@�@��7@�`B@�?}@�Ĝ@��u@�r�@�A�@��;@���@���@�t�@�K�@�ȴ@��!@�n�@�@���@�&�@�z�@�  @��;@��P@�l�@�S�@�33@��@���@�~�@�=q@���@�X@�/@�V@��u@�ƨ@��F@���@�t�@�K�@�;d@��@��y@���@�v�@�{@�@��@��@��@��^@�x�@�hs@��j@��
@���@�C�@�ȴ@�E�@��^@���@�9X@�b@��
@��@��@���@��R@���@�n�@�5?@�J@���@��h@�p�@�X@�&�@���@��u@��w@�t�@�C�@��y@�v�@��@���@�&�@�z�@��
@�ƨ@�o@�ȴ@��\@�E�@�@�@���@��7@�hs@�G�@�&�@��@��@��@�(�@�@�P@K�@;d@~�y@~�R@~ff@~@}�-@}�h@}�@}`B@|�@|�D@|I�@|1@{ƨ@{dZ@{o@z�H@z~�@z=q@y��@yhs@yG�@y%@xĜ@x�u@x �@w|�@v��@v5?@v@u�@u@uO�@t�D@t1@s��@s�@st�@s"�@r��@r��@rM�@rJ@q�^@q7L@p��@p�u@pbN@pb@o��@o�P@ol�@n��@nȴ@nv�@n5?@n@m@m�@l��@lz�@k�m@kC�@ko@j�H@j��@j�!@jn�@i��@iX@i&�@i%@h��@h�u@hQ�@h  @g�;@g�P@f�+@e�@ep�@d��@d�@d(�@c�
@c�F@c��@ct�@cS�@c@bM�@a��@a&�@`�`@`��@`r�@_��@_�P@_|�@_K�@_�@^v�@^$�@^{@]��@]p�@]`B@]O�@\�@\�D@\I�@\�@[��@[dZ@[o@Z�!@Z�\@Z~�@Z~�@Zn�@Y�#@Yhs@Y&�@X��@X��@XQ�@W�;@W|�@V�y@Vff@V@U`B@UV@T�@T�@TZ@T�@S��@S"�@S@R��@R��@R�@Q��@QX@P�`@P��@P�@P1'@O�@N��@N�R@N��@N5?@M@M?}@L�@L�j@Lz�@Lj@LZ@LI�@K�
@KdZ@J�@J~�@J�@I��@IG�@I%@H�`@H�u@G��@G�@Fȴ@F��@F��@F�+@F$�@E�T@E?}@D�@DZ@D1@C�
@C"�@B�@B��@B~�@A��@AG�@@��@@�u@@Q�@@ �@@  @?�@?�@?�@>ȴ@>��@>E�@>5?@>@=@=�@=O�@<�/@<I�@;��@;t�@;S�@:�H@:~�@9��@9�7@9X@8��@8b@7|�@7+@6��@6ȴ@6E�@5�@5@5`B@4��@3�m@3ƨ@3�F@3��@3��@3��@333@2��@2n�@2M�@2-@1��@1�#@1��@17L@0��@0Ĝ@0�@01'@/\)@.ff@.5?@.5?@.$�@.{@-�@-�@-�h@-�h@-`B@-O�@-?}@-/@-V@,��@,��@,�@,�@,�@,��@,1@+�@+@*~�@*J@)��@)&�@(��@(Q�@(A�@(1'@( �@'�@'�@'�P@&��@&E�@%�@%�-@%��@%p�@%?}@$��@$��@$Z@$�@#�m@#��@#t�@"�@"��@"n�@!�@!hs@!G�@!7L@!�@!%@ ��@ ��@ �`@ ��@ Q�@�@;d@��@��@�+@V@{@�-@p�@/@�/@j@1@�m@ƨ@33@o@��@n�@�@��@&�@��@r�@Q�@ �@��@|�@+@�y@��@��@��@��@��@�y@ȴ@��@ff@V@{@p�@O�@?}@�@�@�@1@��@�m@�
@�
@�
@��@t�@dZ@dZ@dZ@C�@"�@�H@�!@�\@�@�#@�7@7L@&�@�@�u@r�@ �@�@�w@�@��@�P@�P@�P@|�@l�@
=@�y@ȴ@�R@ff@ff@E�@$�@{@�@�@�T@�@?}@�@��@��@��@��A���A���A���A���A��`A��#A���A��A���A���A��`A��A��mA��/A��A� �A�{A�{A��A�$�A��A��A�&�A�1'A�K�A�C�A�I�A�l�A�7A虚A蛦A虚A虚A蛦A蟾A蝲A蛦A蝲A��A蟾A蝲A��A��A蝲A��A蟾A蝲A��A蟾A蟾A��A��A�A�A�A��A��A�A��A��A�!A�A��A�9A�RA�FA�RA�wA�jA�wA�A�ĜA�ĜA�ȴA�ƨA�ƨA���A���A���A��/A��HA��HA��`A��yA��yA��mA��A��A��yA��A��A��A��A��A��A��mA��mA��TA��/A��
A��#A��/A��A���A���A���A�ȴA�A�ĜA�A�wA�^A�jA�jA�9A�FA�9A�A�!A�-A�9A�A�A�!A�!A�A�!A�FA�A��A��A��A��A蛦A蝲A蝲A蕁A蕁A藍A�\A�PA�\A�+A�A�A�A�z�A�z�A�|�A�t�A�t�A�v�A�p�A�jA�hsA�dZA�\)A�\)A�^5A�\)A�VA�VA�VA�Q�A�M�A�O�A�M�A�E�A�C�A�?}A�9XA�-A��A��A�A�x�A�wA�7LA�ffA�1A��#A��A�\)A�7LA��
A�A�=qA��A�%A��TA�^A�!A�A�+A��;A�Q�A��A�A�A־wA�Q�AԬA�K�A���A�r�A�l�A�^5A�A�A�-A��A�ffA�$�AѓuA�hsA�v�A�VA�|�A�^5AɼjA�1'A�K�Aġ�A�JA��mA��A�Q�A��
A���A��RA�n�A��
A�^5A��A��DA�+A�A��`A��#A��A�  A�=qA�^5A���A���A��!A��-A���A��FA���A���A��A�/A�I�A�\)A�ffA�hsA�dZA�XA�E�A�/A�oA��A�ĜA��+A�&�A���A��A��A��TA�K�A���A���A��9A�~�A�;dA�ȴA��A�"�A�
=A�oA�(�A�A�A�Q�A��A�"�A���A�\)A�jA�I�A���A���A�t�A�S�A�(�A��A���A��A���A�
=A�{A�{A��A�1'A��jA��/A��/A��^A���A�z�A�hsA�S�A� �A�ƨA���A�n�A��A��FA�r�A�(�A��A��9A�n�A�(�A��/A��^A��hA�v�A�n�A�p�A�|�A�bNA���A�`BA��`A�z�A�
=A��7A���A�E�A��HA�^5A��FA��#A��A�hsA�XA�7LA�&�A� �A��A��A��A�oA�JA���A��HA�ȴA��wA��9A��-A��-A��!A��A���A���A���A���A��+A�v�A�XA�oA���A��uA�v�A�^5A�Q�A�=qA�5?A�7LA��A���A���A�bNA��HA��7A�5?A��^A�JA��hA�jA�^5A�ZA�O�A�I�A�I�A�E�A�?}A�;dA�5?A�-A�"�A��A�VA�A���A���A��A��HA��;A��A���A��9A�hsA�t�A���A�ZA�K�A�JA�O�A�5?A�$�A�~�A�|�A�|�A�r�A�ZA�9XA�oA��FA�v�A�O�A�ZA�S�A�O�A�G�A�?}A�&�A�(�A�+A��A�bA�VA�JA�1A��A��A���A��7A�ZA�=qA��A���A�M�A�bA�t�A�bA��A��^A�z�A�9XA�A���A���A�z�A�S�A�+A�%A��`A�ȴA���A��7A�n�A�Q�A�9XA�/A�+A� �A�VA���A��jA��7A�I�A�$�A��yA��RA���A�p�A�G�A�$�A��mA��A�p�A�l�A�jA�ffA�Q�A���A�l�A�  A��9A�C�A��A���A��jA��A��A���A���A���A�hsA�=qA�%A���A�M�A�+A��A���A���A��-A���A��hA�t�A�7LA���A��jA�n�A�;dA��RA�~�A�^5A�A��A�+A�~�A��A�$�A�hsA�$�A��AdZA~�A~�A}�TA|�/A{�7Az��Az~�Az�AyVAxbNAw"�Au�PAt�9At{As��Ar��Ar��ArffArI�Ar1Aq�TAq��AqdZAp��ApZAo��AoO�An�An�Ann�An�Am�FAm;dAl�HAlĜAl�Alv�Al1'Ak�;Ak�^AkhsAj��Aj��Aj�!Aj��Aj�+Ajz�Ajv�Ajn�AjVAi�TAiC�Ah~�Ag�
Ag�AfZAe�PAel�AeS�Ae33Ae"�Ae�Ad�Ad��Ad��AdQ�Ad �Ad{Ac��Ac�FAc�PAcx�Acl�Ac&�Ab�Ab��Abr�AbM�Ab9XAb�Aa�Aa�^Aat�Aa�A`��A`9XA_�A_��A_XA_&�A^��A^�+A^9XA]�A]�wA]t�A]7LA\�/A\^5A\�A[ƨA[p�A[�AZȴAZbNAYt�AX��AW�AW?}AVjAV1AU�AU��AU|�AU
=AT�AT�9ATz�ATJASƨAS�-AS�7ARZAQ��AQ�AQ�mAQ�AR{ARA�AR^5AR�ARȴAR�AR�AR��AR��AR��AR��ARVAR  AQS�AP��APVAPbAO��AO�ANr�AM�;AK��AJ1'AIp�AI\)AI�AHȴAHM�AG�wAGG�AF��AE��AD��AD �AC��ACƨAC`BAB��AA��AA�AA/A@��A@��A@E�A?�
A?t�A?�A>��A>$�A=��A=�A=��A=��A=��A=��A=�A=S�A=
=A<�jA<=qA;�#A;��A;�A;hsA;K�A;?}A;;dA;+A;oA;
=A;%A:��A:�+A:bNA:VA:-A9��A9��A9��A9|�A97LA8��A8ĜA8r�A8VA8�A7�#A7��A7x�A7`BA7G�A7/A7�A6��A6��A6��A6r�A6^5A6=qA6�A5��A5"�A4�DA4VA41'A45?A49XA49XA41'A4A3�;A3�wA3�FA3��A3��A3��A3�A3l�A3`BA3XA3K�A3?}A3&�A3VA3A2��A2��A2��A2��A2�A2�`A2�A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2�A2�HA2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 A���A��/A���A��TA��mA��A��A��A�(�A�I�A�O�A�\A蛦A蟾A��A��A�!A�A��#A��A��`A���A�^A�!A��A藍A�A�l�A�VA���A� �A��A�ZAӲ-A�VA�&�A�ĜA�A�S�A�I�A�|�A���A�dZA�S�A���A�S�A��`A���A��A��A�;dA�\)A���A��7A�VA��RA��A��A�E�A�/A�hsA��`A���A��A~ȴAy+AsG�Ap�An  Al  Aj�uAh-Ae"�Ac��Ab�9Aa/A^��A\��AYXAU�7AShsAR(�AR�`AQ�AL��AHI�ADQ�AA7LA>��A=�-A<ffA;G�A:��A9�#A8�uA7p�A6�!A5\)A41'A3�^A3dZA3
=A2�yA2��A2��A2�`A2��A3%A3oA2�A05?A/A/;dA-��A,bA)�A(��A(��A)O�A)`BA(�DA(v�A'��A&I�A%dZA$��A#��A#&�A#+A#33A#�hA#��A#��A#�^A#�A#XA#"�A"�jA"I�A"bA!�A"  A!��A!�PA!7LA!VA �A ��A (�AS�A?}AC�A33A�/A�AAt�AS�AO�A33A%AĜAn�A-A�#A�TA�A��AVA�A�RA^5AA�
A��AK�A�A�HA�\A�
A�A`BA�A�/A��A=qA��A��A\)A�Az�A��A��AƨA��AG�AoA�jAr�AhsA�yAA��A{A�^A��A�A�PA�7At�A\)AK�A"�A�A�A��A��Av�A1A+A^5A�
AhsA;dA"�A�A
�RA
�A	G�A�HAȴA�jA�\Ar�AVA$�A�#A��Ax�AdZAK�A�`A~�A�AA��A�!A�+A�mA�AO�A�9AA�A�#A��A��A�PA\)A"�A �`A n�A M�A -A �@���@���@���@��@�^5@�%@� �@��F@�\)@�;d@�+@�o@���@�ȴ@���@�A�@�  @��@��F@��@�@���@�?}@�j@�ƨ@�o@�+@�J@��@��@�1'@@홚@�hs@�@�K�@�$�@��@�@�@�u@�I�@��@�K�@��@�$�@�@���@��#@�D@�S�@�5?@���@�1@߶F@߅@�dZ@�\)@�o@�{@�hs@���@� �@�o@�~�@ى7@أ�@��;@ו�@�33@��H@���@֟�@���@���@�  @���@�+@��y@ҸR@��@��@Ѳ-@��`@�bN@� �@��@�  @ϥ�@�;d@��y@��H@�V@�{@Ͳ-@�`B@�/@���@�Ĝ@�9X@�|�@ʸR@�n�@�v�@�~�@�E�@���@ɡ�@�hs@�7L@�%@���@ǥ�@Ƈ+@��@ũ�@�x�@���@��@�o@�@���@���@�(�@�t�@�
=@�v�@�{@��^@�X@�j@�1@�t�@��H@���@�-@��@��@��`@��@�9X@�ƨ@�t�@�-@���@��7@�x�@�G�@���@�A�@� �@���@�\)@��@�ȴ@��+@�-@�$�@��-@�O�@�%@��@�bN@�9X@� �@�  @�|�@�C�@�
=@��#@�%@��j@��D@��u@��u@�1'@��;@�|�@�C�@���@�5?@��-@�%@���@��D@�Z@�b@���@�l�@�C�@�+@��R@��+@�n�@�=q@�$�@��@��7@�%@�bN@�ƨ@�o@��H@���@�E�@�{@��#@�G�@�7L@�&�@��D@�bN@�I�@�1'@�  @���@�K�@�;d@�+@�+@�+@���@�ȴ@�ff@�@��7@�`B@�?}@�Ĝ@��u@�r�@�A�@��;@���@���@�t�@�K�@�ȴ@��!@�n�@�@���@�&�@�z�@�  @��;@��P@�l�@�S�@�33@��@���@�~�@�=q@���@�X@�/@�V@��u@�ƨ@��F@���@�t�@�K�@�;d@��@��y@���@�v�@�{@�@��@��@��@��^@�x�@�hs@��j@��
@���@�C�@�ȴ@�E�@��^@���@�9X@�b@��
@��@��@���@��R@���@�n�@�5?@�J@���@��h@�p�@�X@�&�@���@��u@��w@�t�@�C�@��y@�v�@��@���@�&�@�z�@��
@�ƨ@�o@�ȴ@��\@�E�@�@�@���@��7@�hs@�G�@�&�@��@��@��@�(�@�@�P@K�@;d@~�y@~�R@~ff@~@}�-@}�h@}�@}`B@|�@|�D@|I�@|1@{ƨ@{dZ@{o@z�H@z~�@z=q@y��@yhs@yG�@y%@xĜ@x�u@x �@w|�@v��@v5?@v@u�@u@uO�@t�D@t1@s��@s�@st�@s"�@r��@r��@rM�@rJ@q�^@q7L@p��@p�u@pbN@pb@o��@o�P@ol�@n��@nȴ@nv�@n5?@n@m@m�@l��@lz�@k�m@kC�@ko@j�H@j��@j�!@jn�@i��@iX@i&�@i%@h��@h�u@hQ�@h  @g�;@g�P@f�+@e�@ep�@d��@d�@d(�@c�
@c�F@c��@ct�@cS�@c@bM�@a��@a&�@`�`@`��@`r�@_��@_�P@_|�@_K�@_�@^v�@^$�@^{@]��@]p�@]`B@]O�@\�@\�D@\I�@\�@[��@[dZ@[o@Z�!@Z�\@Z~�@Z~�@Zn�@Y�#@Yhs@Y&�@X��@X��@XQ�@W�;@W|�@V�y@Vff@V@U`B@UV@T�@T�@TZ@T�@S��@S"�@S@R��@R��@R�@Q��@QX@P�`@P��@P�@P1'@O�@N��@N�R@N��@N5?@M@M?}@L�@L�j@Lz�@Lj@LZ@LI�@K�
@KdZ@J�@J~�@J�@I��@IG�@I%@H�`@H�u@G��@G�@Fȴ@F��@F��@F�+@F$�@E�T@E?}@D�@DZ@D1@C�
@C"�@B�@B��@B~�@A��@AG�@@��@@�u@@Q�@@ �@@  @?�@?�@?�@>ȴ@>��@>E�@>5?@>@=@=�@=O�@<�/@<I�@;��@;t�@;S�@:�H@:~�@9��@9�7@9X@8��@8b@7|�@7+@6��@6ȴ@6E�@5�@5@5`B@4��@3�m@3ƨ@3�F@3��@3��@3��@333@2��@2n�@2M�@2-@1��@1�#@1��@17L@0��@0Ĝ@0�@01'@/\)@.ff@.5?@.5?@.$�@.{@-�@-�@-�h@-�h@-`B@-O�@-?}@-/@-V@,��@,��@,�@,�@,�@,��@,1@+�@+@*~�@*J@)��@)&�@(��@(Q�@(A�@(1'@( �@'�@'�@'�P@&��@&E�@%�@%�-@%��@%p�@%?}@$��@$��@$Z@$�@#�m@#��@#t�@"�@"��@"n�@!�@!hs@!G�@!7L@!�@!%@ ��@ ��@ �`@ ��@ Q�@�@;d@��@��@�+@V@{@�-@p�@/@�/@j@1@�m@ƨ@33@o@��@n�@�@��@&�@��@r�@Q�@ �@��@|�@+@�y@��@��@��@��@��@�y@ȴ@��@ff@V@{@p�@O�@?}@�@�@�@1@��@�m@�
@�
@�
@��@t�@dZ@dZ@dZ@C�@"�@�H@�!@�\@�@�#@�7@7L@&�@�@�u@r�@ �@�@�w@�@��@�P@�P@�P@|�@l�@
=@�y@ȴ@�R@ff@ff@E�@$�@{@�@�@�T@�@?}@�@��@��@��G�O�A���A���A���A���A��`A��#A���A��A���A���A��`A��A��mA��/A��A� �A�{A�{A��A�$�A��A��A�&�A�1'A�K�A�C�A�I�A�l�A�7A虚A蛦A虚A虚A蛦A蟾A蝲A蛦A蝲A��A蟾A蝲A��A��A蝲A��A蟾A蝲A��A蟾A蟾A��A��A�A�A�A��A��A�A��A��A�!A�A��A�9A�RA�FA�RA�wA�jA�wA�A�ĜA�ĜA�ȴA�ƨA�ƨA���A���A���A��/A��HA��HA��`A��yA��yA��mA��A��A��yA��A��A��A��A��A��A��mA��mA��TA��/A��
A��#A��/A��A���A���A���A�ȴA�A�ĜA�A�wA�^A�jA�jA�9A�FA�9A�A�!A�-A�9A�A�A�!A�!A�A�!A�FA�A��A��A��A��A蛦A蝲A蝲A蕁A蕁A藍A�\A�PA�\A�+A�A�A�A�z�A�z�A�|�A�t�A�t�A�v�A�p�A�jA�hsA�dZA�\)A�\)A�^5A�\)A�VA�VA�VA�Q�A�M�A�O�A�M�A�E�A�C�A�?}A�9XA�-A��A��A�A�x�A�wA�7LA�ffA�1A��#A��A�\)A�7LA��
A�A�=qA��A�%A��TA�^A�!A�A�+A��;A�Q�A��A�A�A־wA�Q�AԬA�K�A���A�r�A�l�A�^5A�A�A�-A��A�ffA�$�AѓuA�hsA�v�A�VA�|�A�^5AɼjA�1'A�K�Aġ�A�JA��mA��A�Q�A��
A���A��RA�n�A��
A�^5A��A��DA�+A�A��`A��#A��A�  A�=qA�^5A���A���A��!A��-A���A��FA���A���A��A�/A�I�A�\)A�ffA�hsA�dZA�XA�E�A�/A�oA��A�ĜA��+A�&�A���A��A��A��TA�K�A���A���A��9A�~�A�;dA�ȴA��A�"�A�
=A�oA�(�A�A�A�Q�A��A�"�A���A�\)A�jA�I�A���A���A�t�A�S�A�(�A��A���A��A���A�
=A�{A�{A��A�1'A��jA��/A��/A��^A���A�z�A�hsA�S�A� �A�ƨA���A�n�A��A��FA�r�A�(�A��A��9A�n�A�(�A��/A��^A��hA�v�A�n�A�p�A�|�A�bNA���A�`BA��`A�z�A�
=A��7A���A�E�A��HA�^5A��FA��#A��A�hsA�XA�7LA�&�A� �A��A��A��A�oA�JA���A��HA�ȴA��wA��9A��-A��-A��!A��A���A���A���A���A��+A�v�A�XA�oA���A��uA�v�A�^5A�Q�A�=qA�5?A�7LA��A���A���A�bNA��HA��7A�5?A��^A�JA��hA�jA�^5A�ZA�O�A�I�A�I�A�E�A�?}A�;dA�5?A�-A�"�A��A�VA�A���A���A��A��HA��;A��A���A��9A�hsA�t�A���A�ZA�K�A�JA�O�A�5?A�$�A�~�A�|�A�|�A�r�A�ZA�9XA�oA��FA�v�A�O�A�ZA�S�A�O�A�G�A�?}A�&�A�(�A�+A��A�bA�VA�JA�1A��A��A���A��7A�ZA�=qA��A���A�M�A�bA�t�A�bA��A��^A�z�A�9XA�A���A���A�z�A�S�A�+A�%A��`A�ȴA���A��7A�n�A�Q�A�9XA�/A�+A� �A�VA���A��jA��7A�I�A�$�A��yA��RA���A�p�A�G�A�$�A��mA��A�p�A�l�A�jA�ffA�Q�A���A�l�A�  A��9A�C�A��A���A��jA��A��A���A���A���A�hsA�=qA�%A���A�M�A�+A��A���A���A��-A���A��hA�t�A�7LA���A��jA�n�A�;dA��RA�~�A�^5A�A��A�+A�~�A��A�$�A�hsA�$�A��AdZA~�A~�A}�TA|�/A{�7Az��Az~�Az�AyVAxbNAw"�Au�PAt�9At{As��Ar��Ar��ArffArI�Ar1Aq�TAq��AqdZAp��ApZAo��AoO�An�An�Ann�An�Am�FAm;dAl�HAlĜAl�Alv�Al1'Ak�;Ak�^AkhsAj��Aj��Aj�!Aj��Aj�+Ajz�Ajv�Ajn�AjVAi�TAiC�Ah~�Ag�
Ag�AfZAe�PAel�AeS�Ae33Ae"�Ae�Ad�Ad��Ad��AdQ�Ad �Ad{Ac��Ac�FAc�PAcx�Acl�Ac&�Ab�Ab��Abr�AbM�Ab9XAb�Aa�Aa�^Aat�Aa�A`��A`9XA_�A_��A_XA_&�A^��A^�+A^9XA]�A]�wA]t�A]7LA\�/A\^5A\�A[ƨA[p�A[�AZȴAZbNAYt�AX��AW�AW?}AVjAV1AU�AU��AU|�AU
=AT�AT�9ATz�ATJASƨAS�-AS�7ARZAQ��AQ�AQ�mAQ�AR{ARA�AR^5AR�ARȴAR�AR�AR��AR��AR��AR��ARVAR  AQS�AP��APVAPbAO��AO�ANr�AM�;AK��AJ1'AIp�AI\)AI�AHȴAHM�AG�wAGG�AF��AE��AD��AD �AC��ACƨAC`BAB��AA��AA�AA/A@��A@��A@E�A?�
A?t�A?�A>��A>$�A=��A=�A=��A=��A=��A=��A=�A=S�A=
=A<�jA<=qA;�#A;��A;�A;hsA;K�A;?}A;;dA;+A;oA;
=A;%A:��A:�+A:bNA:VA:-A9��A9��A9��A9|�A97LA8��A8ĜA8r�A8VA8�A7�#A7��A7x�A7`BA7G�A7/A7�A6��A6��A6��A6r�A6^5A6=qA6�A5��A5"�A4�DA4VA41'A45?A49XA49XA41'A4A3�;A3�wA3�FA3��A3��A3��A3�A3l�A3`BA3XA3K�A3?}A3&�A3VA3A2��A2��A2��A2��A2�A2�`A2�A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2��A2�A2�HA2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�6B�B��B��B�}B�EBȴB�6B�pB�HB�/B�B{BMBBeB�B($BF�Bq�B��B�IB��B��B��B��BɺB��B�B�B	!�B	F�B	�OB	��B	�:B	��B	�'B	��B
@B
m]B
�B�B"�B!bB"�B&�B2aBY�B\�BU�B \B�B iB
�B
ҽB
�OB
��B
�kB
��B
��B
v�B
]�B
O�B
6FB
�B	��B	�UB	�aB	�bB	�B	�7B	�B	ncB	g8B	_;B	V�B	GB	8�B	0�B	$�B	3hB	��B	��B	�AB
B	�dB	��B	��B	��B	�FB	��B	��B	��B	�B	�zB	��B	��B	�B	�qB	��B	��B	��B	��B	��B	�lB

	B
B
#B
I�B
P�B
FtB
@B
;�B
:�B
1�B
1�B
/�B
/B
CaB
I�B
YB
V9B
iB
S�B
N�B
Q�B
I�B
HB
T,B
VmB
f�B
|�B
�B
�@B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�4B
�tB
�FB
�LB
�B
�B
��B
��B
��B
�CB
��B
�B
�B
�qB
�qB
��B
�eB
�VB
��B
�OB
�B
�B
��B
�	B
��B
�	B
�eB
��B
��B
��B
��B
�uB
�B
�B
��B
��B
��B
�JB
�JB
�	B
�lB
��B
�B
~(B
cB
��B
��B
~�B
z�B
v�B
s�B
k�B
n�B
rGB
m�B
l"B
qAB
q�B
r|B
tB
tTB
s�B
s�B
sMB
rB
q�B
q�B
p�B
p;B
oiB
m�B
iDB
h�B
f�B
d�B
d&B
b�B
b�B
`BB
X�B
U�B
S�B
S�B
S�B
S[B
S&B
S�B
S[B
S�B
S&B
S[B
S�B
V9B
VB
U2B
T�B
R�B
N�B
N<B
M6B
JXB
I�B
J#B
GzB
GEB
EmB
EB
EB
D�B
C�B
D�B
A�B
A�B
AUB
@�B
@OB
?�B
>�B
>B
@OB
>wB
=B
;dB
:�B
:*B
:*B
9�B
9$B
8�B
:^B
8B
6zB
6B
6B
5�B
5B
5tB
4�B
2aB
2�B
1[B
1[B
0!B
0!B
.�B
0�B
.}B
+�B
*�B
+kB
*�B
(�B
)�B
'�B
'�B
'�B
(XB
(�B
(�B
)�B
'�B
&�B
&�B
%B
%B
!B
CB
$B
�B
�B
�B
�B
uB
uB
{B
�B
�B
oB
hB
bB
"B
VB
�B
�B
VB
�B
�B
4B
�B
"B
xB
B

�B

	B
	�B
	7B
1B
1B
�B
1B
�B
�B
�B
�B
�B
�B

	B

	B

�B
B
PB
B
�B
�B
�B
B
B

�B
�B
JB
�B
�B
B
�B
�B
�B
�B
�B

rB

=B
	lB
	�B
	�B

	B
�B
�B
�B
fB
fB
�B
1B
1B
�B
�B
�B
�B
�B
�B
�B
_B
�B
1B
�B
�B
�B
fB
�B
�B
	lB
	B
�B
�B
�B
	�B
	�B
	�B

	B
DB
xB
B
B
B
xB
PB
�B
�B
�B
�B
�B
�B
oB
B
@B
uB
�B
�B
�B
�B
�B
$B
_B
�B
�B
�B
1B
�B
B
kB
7B
kB
kB
�B
	B
�B
�B
�B
IB
B
�B
IB
B
IB
~B
�B
�B
!B
�B
!B
VB
�B
�B
 'B
 �B
 'B
 \B
!bB
!-B
!-B
!-B
!�B
"�B
#B
#B
#B
#:B
#�B
$tB
$tB
%B
&B
&�B
&LB
&�B
&�B
&LB
&LB
&�B
'�B
'B
'B
&�B
'B
&�B
&�B
'�B
(�B
)*B
)�B
+B
+kB
+B
+�B
+kB
+�B
+�B
+�B
,qB
,=B
,�B
-wB
-B
-B
,�B
.}B
.IB
-�B
.IB
.IB
.IB
.}B
.IB
.�B
.}B
/OB
/�B
/�B
/OB
/OB
/B
/�B
/B
.�B
0�B
1'B
0�B
1'B
1�B
1�B
0�B
1�B
1'B
0�B
1�B
2�B
4B
3�B
3�B
3�B
3�B
2�B
2�B
2�B
33B
2�B
33B
3hB
33B
4�B
5�B
5tB
5�B
5tB
6B
6�B
7B
8RB
:*B
9�B
9�B
=B
<jB
=B
=�B
>�B
?B
?B
?HB
?�B
?�B
@B
@OB
AUB
A�B
B�B
B�B
B�B
B�B
B�B
CaB
CaB
C�B
D3B
DgB
D�B
D�B
EB
E�B
EmB
FB
E�B
FB
FtB
F�B
F�B
GB
GB
G�B
G�B
G�B
G�B
HB
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J#B
K^B
K^B
K�B
K�B
K�B
K�B
L0B
L0B
LdB
LdB
L�B
MB
MB
M6B
MB
M�B
M�B
M�B
M�B
NB
NB
N<B
N<B
N<B
NpB
OBB
N�B
OB
O�B
O�B
PB
O�B
O�B
O�B
PB
QB
P�B
P�B
P�B
P�B
QNB
QB
QNB
P�B
QNB
RTB
R�B
R�B
R�B
R�B
S�B
S[B
S�B
S&B
S[B
S[B
S[B
TaB
TaB
T�B
T�B
T�B
UgB
VB
UgB
UgB
U�B
V9B
W
B
W
B
W
B
WsB
W�B
WsB
W?B
W�B
XB
XyB
XyB
XyB
YB
YB
YB
YKB
YKB
YB
YB
Y�B
ZQB
ZB
ZQB
ZB
Z�B
Z�B
[#B
[�B
[WB
[�B
\]B
\)B
\)B
\�B
\�B
]dB
^jB
^jB
^jB
^�B
^jB
_pB
_�B
_�B
`BB
`BB
`B
`vB
aB
a|B
a�B
a�B
bNB
b�B
c B
c B
cTB
c�B
c�B
cTB
cTB
d&B
d&B
dZB
d�B
d�B
e`B
e,B
e`B
e`B
e�B
f2B
f�B
gB
gB
f�B
gB
gB
g�B
h
B
h>B
h�B
h�B
iB
iyB
i�B
i�B
i�B
jB
j�B
j�B
j�B
kB
kQB
kB
kB
kB
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
l�B
l�B
m�B
m�B
ncB
n/B
n/B
m�B
n/B
m�B
m�B
n/B
o B
o5B
oiB
o�B
o�B
o�B
o�B
o�B
pB
poB
qAB
qAB
qAB
qAB
qAB
qAB
q�B
q�B
rB
rB
rGB
rGB
rGB
r|B
r�B
sB
r�B
sB
sMB
t�B
u%B
u%B
u%B
u%B
uZB
u�B
u�B
u�B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
uZB
u%B
uZB
u�B
u�B
u�B
v`B
v`B
v`B
v�B
wfB
wfB
wfB
wfB
wfB
w�B
w�B
w�B
x�B
x�B
x�B
y	B
x�B
y>B
y>B
yrB
y�B
zB
y�B
zDB
zDB
zDB
z�B
{B
{B
{�B
{�B
|B
|B
|PB
|B
|B
|B
|B
|PB
|�B
|�B
}�B
~]B
~(B
}�B
~(B
~]B
~�B
~�B
~�B
cB
�B
�B
�B
�4B
�iB
�4B
�4B
�4B
� B
� B
��B
�iB
��B
� B
�iB
��B
��B
�;B
��B
�;B
�;B
�;B
�oB
�B
�B
��B
�B
��B
�oB
��B
��B
��B
��B
�AB
�B
�{B
�{B
�MB
��B
��B
�B
��B
��B
�B
��B
��B
��B
��B
��B
�%B
�%B
�%B
��B
�%B
��B
��B
��B
�+B
�fB
��B
�1B
�B
�7B
��B
��B
�7B
�7B
�7B
��B
��B
��B
�B
�DB
��B
�xB
��B
�DB
��B
��B
�JB
�B
�~B
�PB
��B
��B
�"B
�"B
��B
�PB��B��B��B��B��B��B�OB��B��B��B�6B��B��B�aB�0B�B�BƨBȀB�vB�B��BʌB��B�`B�"B�HB�B��B:BB@BMBBuB�BBB�B�BSBMB�B�B�B�B�B�BBYB{B7B�B�B�B�BkB1B�B	BB=BCBBB�B"4B 'B$�B$@B%B'B+kB,qB/B1�B3�B5�B;�BB[BL0BQ�BWsBY�B^Be`Bg�Bm]BtTBv�BzxB�B��B�+B��B��B��B�	B�VB�nB�FB��B�B�}B�!B��B��B��B��B�[B��B�B�3B�3B�tB��B�B�LB�B�FB��B��B�B�LB��B�XB��B��B��B�3B�aB�3B��B��B�EB��B�RB�KB�EBɆB�RB�KBɺB��B�RBɺB�^B�#B��B�)B�XB�RB��B�0B��B��B��B�^BɆB��B�XBȴB�zBȀBȴB�B��BȴB�B�zB��BȴB�#B˒B�B	�B	B	�B	&�B	 �B	$@B	(�B	1'B	.B	=�B	@�B	D�B	EB	E9B	IB	K)B	G�B	E9B	Q�B	�B	��B	�pB	��B	��B	�KB	�hB	��B	�B	�LB	�:B	��B	�B	�~B	�=B	�FB	�FB	��B	�=B	�B	��B	��B	��B	��B	ɺB	�EB	�9B	��B	��B	��B	��B	�qB	�tB	�\B	�SB	��B	�XB	��B	�-B	�9B	�B	�*B	�=B	�tB	�6B	�nB	��B	�qB	��B	��B	�bB	��B	��B	��B	��B	��B	�hB	�B	�\B	��B	��B	�=B	��B	�_B	�+B	��B	��B	�SB	�eB	�\B	�B	�0B	��B	��B	��B	�BB	�?B	��B	�[B	��B	��B	��B	ΥB	��B	�5B	�8B	�B	��B
(B
SB
@B
	B
1[B
.�B
.�B
D�B
?}B
@�B
C�B
J#B
GzB
LdB
K)B
IRB
U�B
V�B
T�B
YB
V�B
o�B
��B
�B
��B
бB
�9B
�,B
�mB
��B
��B
�B
��BfBJB
rB�BBB.BFB�BPB(B~BDB
�BB�B"�B#�B!�B!-B"�B$�B+kBB�B�B%�B0UB�B�BIB \B 'B \B�B�B�B�B �B#nB%zB%zB#nB#nB"hB!bB!�B"hB"�B#�B$tB%�B'B'B)�B,�B'�B$@B#:B%�B$�B(�B&�B/�B7B<�B:�BD�B>�B@�BF�BM�B\�Bd�B[�B]�B\�B_pB_�B_pB_�B^�B^jB_;B]dB\�B\�B\�BZ�BYKBX�BXEBVmBTaBR�BQ�BS�BV�B�{BlWBA�B:�BE�BE9BW?B^5B�B�BGB�B+B	7BxB�B�BB 4B;B�BSBBBAB 4BoB{BoBB iB�B�B
��B
�`B
��B
�B
� B
�"B
�	B
��B 4B
�B
�9B
�B
�EB
�KB
� B
ѷB
��B
ȀB
�zB
�zB
�B
B
��B
��B
��B
�0B
��B
�B
��B
��B
��B
�?B
�9B
�$B
�'B
��B
�=B
�3B
��B
��B
��B
��B
�B
��B
��B
�'B
�_B
��B
��B
��B
�XB
��B
�7B
�FB
��B
�SB
�fB
�YB
��B
��B
w2B
v�B
t�B
uZB
o�B
sB
u%B
h
B
aHB
^jB
_;B
[�B
Z�B
V9B
V9B
V�B
V9B
VmB
R�B
L�B
JXB
S[B
8�B
8�B
B�B
5B
1�B
7�B
)�B
C�B
 �B
�B
	B
�B
�B	�B	�B
	�B	�ZB	�B	�pB	�B	�ZB	ٴB	�B	֡B	�^B	��B	��B	ȀB	��B	�RB	��B	�LB	��B	��B	��B	�XB	�-B	�}B	��B	�B	�nB	��B	�B	��B	��B	�=B	��B	�YB	��B	�B	�MB	��B	��B	��B	��B	�B	�xB	�B	�B	��B	�YB	�_B	��B	�PB	��B	��B	�4B	��B	~]B	p;B	pB	o B	m)B	l�B	o�B	l"B	oiB	l�B	iyB	e`B	gmB	e�B	b�B	aHB	`�B	c B	c�B	]�B	\�B	ZQB	X�B	YB	W�B	ZB	WsB	[�B	XyB	TaB	M�B	P}B	K^B	J�B	CaB	E9B	B�B	?�B	:*B	>B	;�B	;dB	<6B	4�B	2�B	2�B	-B	1[B	.�B	:*B	2-B	+�B	/B	(XB	 \B	�B	%B	.}B	'B	"�B	#:B	)�B	&�B	%zB	�B	'RB	Q�B	S�B	\�B	n�B	{�B	��B	�{B	��B	�B	�*B	�B	�B	�[B	�'B	� B	��B	خB	��B	�B	��B	�B	��B
�B
�B
xB
	�B
 \B
�B	�iB	��B	��B	�HB	��B	�dB	��B	�}B	یB	�jB	��B	�XB	��B	�jB	��B	�B	�B	��B	��B	�zB	��B	�wB	��B	��B	�B	�^B	��B	��B	�XB	�9B	�nB	��B	��B	��B	�[B	��B	�#B	�?B	��B	��B	�B	�dB	�B	�^B	��B	��B	��B	��B	��B	�KB	�$B	�B	��B	�RB	��B	�LB	��B	��B	�?B	�B	�FB	��B	��B	��B	��B	��B	��B	��B	�OB	��B	�wB	��B	�B	��B	�eB	�eB	�_B	�}B	��B	�CB	�CB	�B	��B	��B	��B	�B	�aB	��B	�$B	�RB	�*B	��B	�B	��B	��B	�BB	�B	��B	B	��B	ǮB	ȴB	��B	��B	��B	�pB	�B	�[B	�QB	��B	�B	�B	��B	�,B	�B	�sB	�B	�B	�lB	��B
�B
B
B
�B
B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 B� B�B�nB��B��B�mB�lB�{B��B߷BܮBeB?B?B�B=BdB'�BF�Bq�B�5B��B��B�6B�]B�VB�OB˘B��B�B	0B	nUB	��B	�B	�;B	��B	��B	�UB
$@B
t9ByB%B8�B%VB&�B0NBD B^�BiBxwB,�B
�B
QBB
�B
��B
�LB
��B
� B
�B
��B
f�B
aYB
L�B
VB	�NB	��B	��B	�B	�XB	�kB	��B	rlB	k�B	d�B	^�B	N�B	D'B	=B	+�B	8B	�=B	�[B	��B
1B	�B	љB	��B	�3B	��B	�MB	��B	�;B	�}B	�WB	��B	��B	��B	�MB	��B	��B	�\B	�QB	�B	�*B
	�B
B
#[B
MB
V�B
HOB
BOB
@tB
AKB
9EB
6<B
/�B
-BB
CfB
LUB
Y\B
X�B
nHB
V�B
Q�B
T�B
KjB
HBB
S�B
U~B
f�B
|LB
��B
��B
�jB
�iB
�B
�)B
��B
�B
��B
��B
��B
��B
�@B
�B
��B
�#B
��B
��B
�qB
��B
�`B
��B
�NB
��B
�uB
��B
��B
�[B
�B
�VB
�oB
�tB
��B
��B
�B
��B
��B
��B
�PB
��B
��B
��B
�B
�1B
��B
�B
�B
��B
�B
��B
��B
��B
�~B
�B
��B
�eB
�B
��B
�B
��B
}�B
�B
�~B
��B
�B
|/B
z�B
u�B
k�B
p:B
tB
n�B
l�B
q�B
q�B
r�B
tqB
t�B
s�B
tB
s�B
ryB
rB
r(B
q�B
q�B
r�B
p�B
k9B
jQB
gJB
e'B
doB
dLB
d�B
c%B
ZB
U�B
T=B
T�B
S�B
S�B
S�B
UB
TFB
TB
SB
S�B
U@B
W�B
WgB
V�B
WLB
S�B
OvB
PjB
N�B
KSB
L.B
K�B
H�B
HB
E�B
EGB
E�B
EzB
D�B
F,B
B�B
BB
A�B
A&B
@�B
@CB
?B
@/B
B�B
@B
=�B
<	B
;=B
:TB
:\B
9�B
9�B
:qB
<�B
8�B
6�B
6�B
6�B
6�B
5�B
7�B
6\B
3�B
3�B
2bB
2BB
0�B
1CB
0�B
3|B
0QB
,bB
,EB
-�B
,�B
*�B
*�B
(B
'�B
(XB
)zB
)~B
)�B
*�B
'�B
'	B
&�B
'rB
'>B
!WB
"B
LB
�B
DB
B
�B
'B
AB
�B
B
/B
UB
�B
*B
�B
�B
%B
=B
�B
B
+B
{B
B
�B
B
4B
*B

�B

�B
	�B
�B
	�B
�B
�B
�B
	B
xB
�B
�B
4B
B

�B
�B
�B
�B
�B
'B
�B
B
tB
�B

�B
�B
�B
JB
_B
�B
KB
NB
�B
�B
�B
�B

�B
	�B

�B
�B
�B
	�B

QB

vB
	�B
	�B
	pB
	CB
�B
MB
mB
vB
[B
�B
	
B
&B
IB
�B
�B
wB
}B
�B
	@B
�B

B

yB
	1B
�B
	HB
	�B

�B
	�B

B
.B
�B
B
�B
�B
>B
XB
B
�B
lB
�B
�B
�B
'B
^B
�B
�B
�B
oB
QB
B
�B
�B
�B
 B
OB
HB
�B
WB
�B
@B
�B
�B
�B
�B
-B
�B
�B
B
�B
�B
MB
BB
�B
�B
$B
�B
�B
 B
 gB
 (B
�B
 B
 "B
 @B
!>B
 �B
 aB
!yB
!�B
!dB
!gB
!�B
"�B
#\B
#*B
#(B
#B
#IB
$
B
$�B
%8B
%�B
&�B
&�B
&�B
'�B
'B
&�B
&�B
'lB
'�B
':B
'dB
'PB
(B
'%B
'vB
(�B
)TB
*0B
+B
+�B
+�B
+�B
,B
+�B
+�B
+�B
,IB
,�B
,�B
-�B
.B
-iB
-bB
-�B
/�B
.zB
.)B
.�B
.�B
.uB
.�B
.�B
/B
/B
0B
/�B
/�B
/eB
/]B
/�B
0 B
/aB
0NB
2�B
1�B
1�B
2B
2�B
3B
2PB
2�B
1�B
1oB
2�B
3�B
4�B
4	B
4B
4%B
4B
3[B
3?B
3|B
3yB
3=B
3�B
3�B
4B
6!B
6oB
5�B
6bB
6bB
7B
7�B
8B
9�B
;[B
:DB
;B
=�B
<�B
=�B
>_B
?-B
?XB
?KB
?�B
?�B
?�B
@�B
@�B
A�B
B�B
C)B
C%B
C:B
CB
CIB
C�B
C�B
D.B
D�B
D�B
D�B
D�B
EwB
F4B
E�B
FNB
FB
FoB
F�B
GB
GB
G[B
G�B
H%B
G�B
G�B
H)B
HPB
HYB
I#B
IDB
J;B
J#B
JB
I�B
J3B
J�B
K�B
K�B
K�B
K�B
K�B
LZB
LZB
L�B
L�B
L�B
MB
MIB
MaB
MpB
MXB
N
B
M�B
M�B
NB
N@B
N\B
NB
NrB
N�B
OB
O�B
O5B
O�B
PyB
PB
PDB
O�B
PB
P.B
P�B
Q�B
QB
QB
P�B
Q,B
Q�B
QlB
QyB
QKB
R?B
R�B
SB
R�B
SMB
StB
S�B
S�B
S�B
SZB
S�B
S�B
TB
UB
T�B
UBB
T�B
U	B
VB
VEB
UB
U�B
U�B
V�B
WZB
W$B
WRB
W�B
W�B
W�B
W�B
X=B
XVB
X�B
X�B
Y	B
Y�B
YwB
Y�B
Y`B
YRB
Y7B
Y�B
ZYB
Z�B
ZRB
Z~B
Z�B
[,B
Z�B
[�B
\B
[�B
\^B
\�B
\QB
\sB
\�B
]EB
]�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
`IB
`�B
`mB
`iB
`�B
a�B
a�B
a�B
bWB
b�B
c8B
cuB
cUB
c�B
c�B
c�B
cqB
c�B
d�B
d�B
d�B
d�B
evB
e�B
erB
e�B
e�B
fXB
f�B
g!B
g'B
gB
f�B
gfB
gVB
h>B
hfB
h�B
h�B
iB
i�B
i�B
i�B
jB
jnB
j�B
kWB
k,B
k,B
kPB
ktB
k6B
kgB
k�B
lB
k�B
lLB
l:B
l�B
l�B
l�B
l�B
m6B
m�B
m�B
nBB
n�B
n�B
n�B
n�B
nuB
n B
nfB
oB
o�B
o�B
o�B
o�B
p B
p$B
pB
p>B
p�B
qAB
qfB
qXB
qcB
qFB
qNB
q�B
rB
r;B
r9B
r;B
rzB
rnB
r�B
r�B
s(B
sPB
s-B
sxB
t"B
ukB
uZB
u,B
u9B
u;B
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
u�B
u�B
ubB
uYB
vB
vJB
vJB
v|B
v�B
v�B
v�B
wKB
w�B
w{B
wzB
w}B
w�B
w�B
w�B
xuB
y7B
y)B
yB
y B
y
B
ysB
y�B
y�B
z B
zQB
zB
z�B
z�B
z�B
{6B
{SB
{�B
|2B
|B
|1B
|@B
|cB
|/B
|"B
|4B
|aB
|�B
}%B
}�B
~~B
~sB
~@B
~+B
~qB
~�B
	B
B
TB
�B
�B
�B
�B
��B
��B
�}B
��B
��B
�_B
��B
��B
��B
��B
�9B
��B
��B
��B
�yB
��B
�?B
�?B
�?B
�rB
�B
�/B
��B
�<B
��B
��B
�BB
�B
��B
��B
��B
��B
��B
��B
�_B
��B
��B
�!B
��B
��B
�/B
��B
��B
��B
�B
��B
�XB
�PB
��B
�B
�}B
��B
��B
��B
��B
��B
�B
�fB
�4B
�JB
��B
��B
�:B
�<B
�LB
��B
�B
��B
�2B
�[B
�+B
�|B
�B
�hB
��B
��B
�OB
�2B
��B
��B
��B
��B
�$B
�#B
��G�O�B��B��B��B��B��B��B�OB��B��B��B�6B��B��B�aB�0B�B�BƨBȀB�vB�B��BʌB��B�`B�"B�HB�B��B:BB@BMBBuB�BBB�B�BSBMB�B�B�B�B�B�BBYB{B7B�B�B�B�BkB1B�B	BB=BCBBB�B"4B 'B$�B$@B%B'B+kB,qB/B1�B3�B5�B;�BB[BL0BQ�BWsBY�B^Be`Bg�Bm]BtTBv�BzxB�B��B�+B��B��B��B�	B�VB�nB�FB��B�B�}B�!B��B��B��B��B�[B��B�B�3B�3B�tB��B�B�LB�B�FB��B��B�B�LB��B�XB��B��B��B�3B�aB�3B��B��B�EB��B�RB�KB�EBɆB�RB�KBɺB��B�RBɺB�^B�#B��B�)B�XB�RB��B�0B��B��B��B�^BɆB��B�XBȴB�zBȀBȴB�B��BȴB�B�zB��BȴB�#B˒B�B	�B	B	�B	&�B	 �B	$@B	(�B	1'B	.B	=�B	@�B	D�B	EB	E9B	IB	K)B	G�B	E9B	Q�B	�B	��B	�pB	��B	��B	�KB	�hB	��B	�B	�LB	�:B	��B	�B	�~B	�=B	�FB	�FB	��B	�=B	�B	��B	��B	��B	��B	ɺB	�EB	�9B	��B	��B	��B	��B	�qB	�tB	�\B	�SB	��B	�XB	��B	�-B	�9B	�B	�*B	�=B	�tB	�6B	�nB	��B	�qB	��B	��B	�bB	��B	��B	��B	��B	��B	�hB	�B	�\B	��B	��B	�=B	��B	�_B	�+B	��B	��B	�SB	�eB	�\B	�B	�0B	��B	��B	��B	�BB	�?B	��B	�[B	��B	��B	��B	ΥB	��B	�5B	�8B	�B	��B
(B
SB
@B
	B
1[B
.�B
.�B
D�B
?}B
@�B
C�B
J#B
GzB
LdB
K)B
IRB
U�B
V�B
T�B
YB
V�B
o�B
��B
�B
��B
бB
�9B
�,B
�mB
��B
��B
�B
��BfBJB
rB�BBB.BFB�BPB(B~BDB
�BB�B"�B#�B!�B!-B"�B$�B+kBB�B�B%�B0UB�B�BIB \B 'B \B�B�B�B�B �B#nB%zB%zB#nB#nB"hB!bB!�B"hB"�B#�B$tB%�B'B'B)�B,�B'�B$@B#:B%�B$�B(�B&�B/�B7B<�B:�BD�B>�B@�BF�BM�B\�Bd�B[�B]�B\�B_pB_�B_pB_�B^�B^jB_;B]dB\�B\�B\�BZ�BYKBX�BXEBVmBTaBR�BQ�BS�BV�B�{BlWBA�B:�BE�BE9BW?B^5B�B�BGB�B+B	7BxB�B�BB 4B;B�BSBBBAB 4BoB{BoBB iB�B�B
��B
�`B
��B
�B
� B
�"B
�	B
��B 4B
�B
�9B
�B
�EB
�KB
� B
ѷB
��B
ȀB
�zB
�zB
�B
B
��B
��B
��B
�0B
��B
�B
��B
��B
��B
�?B
�9B
�$B
�'B
��B
�=B
�3B
��B
��B
��B
��B
�B
��B
��B
�'B
�_B
��B
��B
��B
�XB
��B
�7B
�FB
��B
�SB
�fB
�YB
��B
��B
w2B
v�B
t�B
uZB
o�B
sB
u%B
h
B
aHB
^jB
_;B
[�B
Z�B
V9B
V9B
V�B
V9B
VmB
R�B
L�B
JXB
S[B
8�B
8�B
B�B
5B
1�B
7�B
)�B
C�B
 �B
�B
	B
�B
�B	�B	�B
	�B	�ZB	�B	�pB	�B	�ZB	ٴB	�B	֡B	�^B	��B	��B	ȀB	��B	�RB	��B	�LB	��B	��B	��B	�XB	�-B	�}B	��B	�B	�nB	��B	�B	��B	��B	�=B	��B	�YB	��B	�B	�MB	��B	��B	��B	��B	�B	�xB	�B	�B	��B	�YB	�_B	��B	�PB	��B	��B	�4B	��B	~]B	p;B	pB	o B	m)B	l�B	o�B	l"B	oiB	l�B	iyB	e`B	gmB	e�B	b�B	aHB	`�B	c B	c�B	]�B	\�B	ZQB	X�B	YB	W�B	ZB	WsB	[�B	XyB	TaB	M�B	P}B	K^B	J�B	CaB	E9B	B�B	?�B	:*B	>B	;�B	;dB	<6B	4�B	2�B	2�B	-B	1[B	.�B	:*B	2-B	+�B	/B	(XB	 \B	�B	%B	.}B	'B	"�B	#:B	)�B	&�B	%zB	�B	'RB	Q�B	S�B	\�B	n�B	{�B	��B	�{B	��B	�B	�*B	�B	�B	�[B	�'B	� B	��B	خB	��B	�B	��B	�B	��B
�B
�B
xB
	�B
 \B
�B	�iB	��B	��B	�HB	��B	�dB	��B	�}B	یB	�jB	��B	�XB	��B	�jB	��B	�B	�B	��B	��B	�zB	��B	�wB	��B	��B	�B	�^B	��B	��B	�XB	�9B	�nB	��B	��B	��B	�[B	��B	�#B	�?B	��B	��B	�B	�dB	�B	�^B	��B	��B	��B	��B	��B	�KB	�$B	�B	��B	�RB	��B	�LB	��B	��B	�?B	�B	�FB	��B	��B	��B	��B	��B	��B	��B	�OB	��B	�wB	��B	�B	��B	�eB	�eB	�_B	�}B	��B	�CB	�CB	�B	��B	��B	��B	�B	�aB	��B	�$B	�RB	�*B	��B	�B	��B	��B	�BB	�B	��B	B	��B	ǮB	ȴB	��B	��B	��B	�pB	�B	�[B	�QB	��B	�B	�B	��B	�,B	�B	�sB	�B	�B	�lB	��B
�B
B
B
�B
B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<yC?=��=<<�4�=1�=��<☣=g� <���<#�
<�e<��<�<#�
<#�
<1ܰ<�=�<#�
<VE�=��<\�J<#�
<6)�<x-�<#�
<#�
<$�<#�
<j<J�=<4/�<-�c<��<��<�[�<��}<#�
<1T<#�
<#�
<#�
<1W�<#�
<#�
<#�
<#�
<#�
<H2<X��<#�
<#�
<#�
<#�
<u�<�_h<lO�<A�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019092116081020190921160810IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019100117004020191001170040QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019100117004020191001170040QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573020200109065730IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                