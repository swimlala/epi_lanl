CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-07-24T23:57:14Z creation; 2023-04-26T19:14:30Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200724235714  20230426191430  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               ]   ]AA  AOAO7316_008644_093                 7316_008644_093                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�+;�2�P@�+;�2�P11  @�+;ݗ�+@�+;ݗ�+@(�q���@(�q����c�yR�4��c�yR�4�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@B�\@�G�@�  @�  @�  @��RA  A\)A+�A@  A^�RA~�RA�  A�Q�A�  A��AϮA�\)A�\)B   B  B  B  B   B(  B/�B7�B@  BH  BO�
BW�
B`  Bh  Bp  Bx  B�  B�  B�  B�{B�  B��B�  B��B�  B�  B��B�  B��B��B�{B�{B�  B��B�  B�{B�{B�(�B�(�B�Q�B�  B��B��B��
B��B�  B�{B�  B��C  C
=C  C  C

=C
=C  C��C��C
=C
=C  C  C��C
=C 
=C"  C#��C&  C(
=C*  C,  C.  C/��C2  C4  C5��C7��C9��C<  C>
=C?��CA�CC��CF  CH  CJ  CL{CN{CP  CQ��CT  CV{CX{CZ  C[�C]�C_��Cb  Cd  Cf
=Ch  Cj  Ck��Cm��Co��Cq��Ct  Cv  Cw��Cz  C|  C~  C�  C���C�  C�  C�  C�C�C�C�C�  C���C�  C�  C���C�C�C�  C�  C�  C���C���C���C���C�  C�
=C�
=C�\C�
=C�C�  C���C�C�C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�C�  C���C���C�  C�C�
=C�C�  C�  C���C���C�  C�C�C�C�C���C���C�  C���C�  C�C�  C���C�  C�C�  C���C�C�  C���C�  C�
=C���C���C�C�C�C�
=C�
=C�C���C�  C�  C�  C���C�  C�C�
=C�C�  C���C���C���C���C�  C�  C�  C���C���C�  C�  C�C�C�C�  C���C���C�  C�
=C�
=C���C��C���C�C�  C���C���C���C���C�  C�  C�  D �D �D  Dz�D�qD� D�D}qD�RD}qD  D��D�qD� D�D� DD� D��D	� D
�D
�D  Dz�D�qD� D�qDz�D�qD��DD��D  D�D�D}qD�D}qD  D��D�qD}qD�qD� D�D� D  D}qD  D� D  D}qD�qD� D  D� D  D� D�qD� D�D��D  D}qD�qD � D!�D!��D"  D"��D#D#��D$�D$��D%�D%� D%�qD&}qD'  D'��D(  D(}qD(�qD)� D*�D*� D*�qD+}qD,  D,��D-  D-� D.  D.� D/  D/}qD/�qD0}qD0�qD1� D2  D2}qD3  D3��D4  D4}qD5  D5��D6  D6��D7  D7}qD7�qD8� D9�D9� D9�qD:� D;�D;� D<  D<� D=�D=� D>  D>��D?  D?z�D@  D@� D@�qDA� DB  DB� DC�DC� DD  DD��DE�DE� DE�qDF� DF�qDG}qDH  DH� DI  DI��DJ  DJ}qDJ��DK}qDL  DL��DMDM��DM�qDN� DO  DO� DP  DP}qDP�qDQ}qDR  DR��DS�DS� DT�DT}qDU  DU� DV  DV� DW�DW�DX  DX}qDY  DY� DZ  DZ�D[D[��D[�qD\}qD]�D]� D^  D^��D_  D_}qD`  D`� D`�qDa��Db�Db��Dc�Dc��Dd  Dd� De  De}qDf  Df}qDg  Dg��Dh�Dh� Di  Di� Dj  Dj}qDk  Dk� Dl  Dl��Dm�Dm� Dn�Dn��Do  Do}qDo�qDp� Dq  Dq��Dr  Dr}qDs  Ds� Ds�qDt� Du�Du� Du�qDv}qDw�Dw��Dx  Dxz�Dx��Dyz�Dz  Dz�D{D{��D|�D|�D}D}�D~D~� D~�qD}qD�  D�>�D�}qD���D�HD�@ D�� D�D�HD�@ D�� D���D���D�>�D�� D���D�  D�AHD�� D�� D��D�AHD�� D�� D���D�AHD�� D���D�HD�AHD���D�D�HD�@ D�� D��HD�HD�B�D���D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD�� D���D��qD�>�D�� D��HD�HD�>�D�}qD��qD���D�@ D��HD��HD�  D�>�D��HD�D��D�AHD��HD�� D���D�>�D��HD�D��D�>�D�~�D��HD�HD�AHD�� D���D���D�>�D�~�D��qD�  D�@ D�}qD��qD�  D�AHD��HD�� D���D�>�D�}qD��qD�  D�B�D���D��HD���D�=qD�}qD���D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD���D�  D�B�D���D��HD�  D�AHD�~�D��qD��qD�>�D�� D�D�  D�>�D�~�D�� D���D�=qD�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�D�HD�AHD��HD��HD�HD�AHD��HD���D��qD�@ D��HD�� D�HD�>�D�� D�� D���D�@ D��HD�D�HD�>�D�� D��HD���D�@ D��HD���D��qD�>�D�� D��qD���D�AHD�~�D��HD��D�@ D��HD�D�HD�@ D��HD�D�HD�@ D�~�D�� D��D�AHD�� D��HD�  D�@ D��HD�� D��qD�>�D�� D���D��qD�>�D�~�D�� D�  D�>�D�� D��HD���D�>�D�� D��HD�HD�>�D�~�D���D�  D�AHD�~�D�� D�  D�@ D�� D��HD�HD�@ D�}qD���D�HD�B�D�� D��HD��D�@ D�� D��HD�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D�� D�  D�=qD D��HD�HD�@ DÀ D��HD�HD�@ DĀ D��HD�  D�>�DŁHD��HD�  D�AHDƀ D�� D�HD�@ Dǀ D�� D�  D�@ DȁHD��HD�  D�@ DɁHD�� D���D�@ Dʀ Dʾ�D���D�@ Dˀ D�� D���D�>�D̀ D�� D�  D�@ D�~�D;�D�HD�AHD΀ D��HD�  D�>�D�~�D�� D���D�>�DЁHD��HD���D�@ Dр D��HD�HD�AHDҀ D�� D�  D�AHD�~�DӾ�D���D�=qD�~�D�� D�  D�AHDՀ Dվ�D�  D�@ D�~�D�� D�  D�AHDׁHD�� D��qD�>�D؀ D�� D�  D�>�Dـ D�� D�  D�@ DځHD��HD�  D�>�D�~�D�� D�HD�@ D܀ D�� D�  D�AHD݂�D��HD�  D�@ Dނ�D��HD�  D�@ D߀ D߾�D���D�>�D�~�D�� D�  D�=qD�~�D��HD�HD�@ D�~�D⾸D�  D�@ D�~�D㾸D�  D�AHD� D�� D�HD�AHD�HD��HD�  D�>�D�~�D澸D�  D�@ D�~�D羸D�HD�B�D�HD�� D���D�>�D�~�D龸D���D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D�}qD쾸D�  D�@ D�HD�� D���D�AHD� D�� D��D�AHD�~�DﾸD�HD�AHD�~�D�D�HD�AHD�HD��HD�  D�@ D�~�D�� D�  D�@ D� D�� D���D�=qD� D�D���D�=qD�~�D���D�  D�AHD�~�D�� D���D�@ D��HD��HD�  D�@ D�~�D�� D�  D�@ D�~�D���D�  D�AHD��3?��?L��?�z�?��
?Ǯ?��@�\@z�@#�
@8Q�@G�@W
=@k�@z�H@��
@�{@�
=@��R@��@���@���@�G�@˅@�z�@�p�@�ff@��@���AG�AffA�A  A�A��Ap�A"�\A(Q�A,��A1G�A5A;�A?\)AC�
AJ=qAN�RAR�\AW�A]p�A`��AeAk�Ap  Au�Az�HA~�RA���A�z�A�
=A�G�A��
A�ffA���A��HA�A�  A��\A�p�A��A���A���A�\)A���A�(�A�
=A�G�A��A�ffA���A�33A�A���A\A�p�A�Q�A�=qA���AϮA�=qA�z�A�
=A��A�(�A޸RA�G�A��
A�ffA���A�A�{A�Q�A��HA�A�Q�A�=qA�p�B (�B�BffB�
B�BffB�
B��B
{B\)B��B�B33B��B�B33BQ�BB33B(�Bp�B
=B(�BG�B�HB   B!�B"�RB$  B$��B&=qB'�B)�B*=qB+�B,��B.=qB/\)B0��B2{B333B4z�B5B733B8Q�B9p�B;
=B<(�B=p�B>�HB@  BA�BB�\BD  BE�BF=qBG�BI�BJffBK�BL��BN=qBO\)BP��BR{BS\)BTz�BU�BW\)BXz�BY��B[
=B\z�B]��B_
=B`Q�Ba��Bb�HBd(�Be��Bf�RBh  Bip�Bj�\Bk�
BmG�Bn�\Bo�Bp��Br=qBs�Bt��Bv{Bw�Bx��ByB{
=B|z�B}p�B~�RB�{B���B�33B��
B�z�B�
=B��B�Q�B���B��B�(�B���B�\)B�  B��RB�\)B��B�z�B�33B��
B�ffB�
=B�B�ffB���B��B�ffB���B��B�=qB���B���B�{B���B���B�=qB���B��B�=qB��HB�p�B�{B��RB�p�B�(�B��RB�\)B�{B��HB��B�{B���B��B�=qB��HB��B�Q�B��B�B�ffB��B�  B��RB�\)B�{B���B��B�ffB��B��
B��\B�p�B�(�B���B���B�Q�B��B�  B��RB�p�B�(�B��HB�B��\B�33B��B��RB��B�Q�B�
=B�B�z�B�33B�  B���B��B�=qB��HB�Bď\B�\)B�{B���BǅB�Q�B�33B�  BʸRB˅B�=qB�
=B��BΣ�B�\)B�{B���Bљ�B�ffB��B��
B�z�B�33B��B֣�B�\)B��
B�ffB�
=Bٙ�B�{B�z�B��HB��B�p�B�B�  B�=qB�ffB�z�B���B���B�
=B�33B�p�BݮB��
B��
B�{B�Q�Bޏ\B޸RB��HB���B��B�\)B߅B�B��B�{B�=qB�ffB��B��HB�
=B�33B�G�B�p�B�B��B�{B�=qB�ffB�z�B�RB���B��B�\)B�p�B㙚B�B��B�(�B�ffB�\B�RB��HB���B��B�\)B�B�B�  B�(�B�Q�B�z�B�RB���B�33B�\)B�B�B��
B�{B�Q�B�\B���B���B�33B�\)B�B�B�{B�Q�B�\B���B���B�33B�p�B�B�{B�ffB��B���B�G�B�B�B�  B�ffB�RB�
=B�\)B�B�{B�ffB��B���B�G�B�B��
B�=qB�\B���B�G�B�B�  B�Q�B���B���B�G�B��B�  B�ffB���B�33B���B��B�Q�B���B���B�G�B��B�  B�ffB���B�33B���B�  B�Q�B��RB�
=B�\)B��B�(�B�z�B���B�\)B�C {C Q�C z�C �C �
C{C=qCffC��C�
C
=C=qCp�C�C�HC�CQ�C�CC��C�CQ�C�C�RC�C(�CffC��C��C
=CG�C�C�RC��C33Cp�C��C�HC{CG�C�C�RC�C	�C	Q�C	�\C	��C

=C
=qC
z�C
�RC
��C(�CffC��C��C
=C=qCz�C�C�C�C\)C��C�
C{CQ�C�\C��C
=CG�C�CC
=CG�C�CC  C=qCz�C�C�C�CffC��C�
C{CG�C�CC  C=qCz�C�RC��C(�CffC��C�C�C\)C�\C��C
=C=qCz�C�RC��C33Cp�C�C�C�C\)C��C�HC{CQ�C�\C��C  C=qCz�C�RC��C(�Cp�C�C�HC�CffC��C�HC(�CffC��C�HC(�CffC��C�HC (�C \)C ��C �
C!�C!\)C!��C!�
C"{C"Q�C"�\C"��C#  C#=qC#z�C#�RC#��C$33C$p�C$��C$�C%�C%\)C%��C%�
C&�C&\)C&��C&�HC'�C'ffC'��C'�HC(�C(\)C(��C(�HC)�C)\)C)��C)�HC*{C*\)C*��C*�HC+�C+ffC+�C+�C,33C,z�C,�RC,��C-=qC-z�C-�RC-��C.=qC.�C.��C/
=C/Q�C/��C/�
C0�C0ffC0��C0�C133C1ffC1�C1��C2=qC2�C2�
C3�C3p�C3�C3�C433C4z�C4��C5�C5ffC5�C6  C6=qC6�C6��C7�C7p�C7C8
=C8\)C8��C8�HC9(�C9z�C9�
C:�C:p�C:C;
=C;Q�C;��C;�C<=qC<�\C<�HC=(�C=p�C=�RC>  C>\)C>��C?  C?G�C?��C?�HC@33C@z�C@�
CA33CA�CA�HCB33CB�CB��CC(�CC�CC�HCD=qCD�\CD�HCE33CE�CE�HCF=qCF��CG  CGQ�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  ?u?��H@B�\@�G�@�  @�  @�  @��RA  A\)A+�A@  A^�RA~�RA�  A�Q�A�  A��AϮA�\)A�\)B   B  B  B  B   B(  B/�B7�B@  BH  BO�
BW�
B`  Bh  Bp  Bx  B�  B�  B�  B�{B�  B��B�  B��B�  B�  B��B�  B��B��B�{B�{B�  B��B�  B�{B�{B�(�B�(�B�Q�B�  B��B��B��
B��B�  B�{B�  B��C  C
=C  C  C

=C
=C  C��C��C
=C
=C  C  C��C
=C 
=C"  C#��C&  C(
=C*  C,  C.  C/��C2  C4  C5��C7��C9��C<  C>
=C?��CA�CC��CF  CH  CJ  CL{CN{CP  CQ��CT  CV{CX{CZ  C[�C]�C_��Cb  Cd  Cf
=Ch  Cj  Ck��Cm��Co��Cq��Ct  Cv  Cw��Cz  C|  C~  C�  C���C�  C�  C�  C�C�C�C�C�  C���C�  C�  C���C�C�C�  C�  C�  C���C���C���C���C�  C�
=C�
=C�\C�
=C�C�  C���C�C�C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�C�  C���C���C�  C�C�
=C�C�  C�  C���C���C�  C�C�C�C�C���C���C�  C���C�  C�C�  C���C�  C�C�  C���C�C�  C���C�  C�
=C���C���C�C�C�C�
=C�
=C�C���C�  C�  C�  C���C�  C�C�
=C�C�  C���C���C���C���C�  C�  C�  C���C���C�  C�  C�C�C�C�  C���C���C�  C�
=C�
=C���C��C���C�C�  C���C���C���C���C�  C�  C�  D �D �D  Dz�D�qD� D�D}qD�RD}qD  D��D�qD� D�D� DD� D��D	� D
�D
�D  Dz�D�qD� D�qDz�D�qD��DD��D  D�D�D}qD�D}qD  D��D�qD}qD�qD� D�D� D  D}qD  D� D  D}qD�qD� D  D� D  D� D�qD� D�D��D  D}qD�qD � D!�D!��D"  D"��D#D#��D$�D$��D%�D%� D%�qD&}qD'  D'��D(  D(}qD(�qD)� D*�D*� D*�qD+}qD,  D,��D-  D-� D.  D.� D/  D/}qD/�qD0}qD0�qD1� D2  D2}qD3  D3��D4  D4}qD5  D5��D6  D6��D7  D7}qD7�qD8� D9�D9� D9�qD:� D;�D;� D<  D<� D=�D=� D>  D>��D?  D?z�D@  D@� D@�qDA� DB  DB� DC�DC� DD  DD��DE�DE� DE�qDF� DF�qDG}qDH  DH� DI  DI��DJ  DJ}qDJ��DK}qDL  DL��DMDM��DM�qDN� DO  DO� DP  DP}qDP�qDQ}qDR  DR��DS�DS� DT�DT}qDU  DU� DV  DV� DW�DW�DX  DX}qDY  DY� DZ  DZ�D[D[��D[�qD\}qD]�D]� D^  D^��D_  D_}qD`  D`� D`�qDa��Db�Db��Dc�Dc��Dd  Dd� De  De}qDf  Df}qDg  Dg��Dh�Dh� Di  Di� Dj  Dj}qDk  Dk� Dl  Dl��Dm�Dm� Dn�Dn��Do  Do}qDo�qDp� Dq  Dq��Dr  Dr}qDs  Ds� Ds�qDt� Du�Du� Du�qDv}qDw�Dw��Dx  Dxz�Dx��Dyz�Dz  Dz�D{D{��D|�D|�D}D}�D~D~� D~�qD}qD�  D�>�D�}qD���D�HD�@ D�� D�D�HD�@ D�� D���D���D�>�D�� D���D�  D�AHD�� D�� D��D�AHD�� D�� D���D�AHD�� D���D�HD�AHD���D�D�HD�@ D�� D��HD�HD�B�D���D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD�� D���D��qD�>�D�� D��HD�HD�>�D�}qD��qD���D�@ D��HD��HD�  D�>�D��HD�D��D�AHD��HD�� D���D�>�D��HD�D��D�>�D�~�D��HD�HD�AHD�� D���D���D�>�D�~�D��qD�  D�@ D�}qD��qD�  D�AHD��HD�� D���D�>�D�}qD��qD�  D�B�D���D��HD���D�=qD�}qD���D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD���D�  D�B�D���D��HD�  D�AHD�~�D��qD��qD�>�D�� D�D�  D�>�D�~�D�� D���D�=qD�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�D�HD�AHD��HD��HD�HD�AHD��HD���D��qD�@ D��HD�� D�HD�>�D�� D�� D���D�@ D��HD�D�HD�>�D�� D��HD���D�@ D��HD���D��qD�>�D�� D��qD���D�AHD�~�D��HD��D�@ D��HD�D�HD�@ D��HD�D�HD�@ D�~�D�� D��D�AHD�� D��HD�  D�@ D��HD�� D��qD�>�D�� D���D��qD�>�D�~�D�� D�  D�>�D�� D��HD���D�>�D�� D��HD�HD�>�D�~�D���D�  D�AHD�~�D�� D�  D�@ D�� D��HD�HD�@ D�}qD���D�HD�B�D�� D��HD��D�@ D�� D��HD�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D�� D�  D�=qD D��HD�HD�@ DÀ D��HD�HD�@ DĀ D��HD�  D�>�DŁHD��HD�  D�AHDƀ D�� D�HD�@ Dǀ D�� D�  D�@ DȁHD��HD�  D�@ DɁHD�� D���D�@ Dʀ Dʾ�D���D�@ Dˀ D�� D���D�>�D̀ D�� D�  D�@ D�~�D;�D�HD�AHD΀ D��HD�  D�>�D�~�D�� D���D�>�DЁHD��HD���D�@ Dр D��HD�HD�AHDҀ D�� D�  D�AHD�~�DӾ�D���D�=qD�~�D�� D�  D�AHDՀ Dվ�D�  D�@ D�~�D�� D�  D�AHDׁHD�� D��qD�>�D؀ D�� D�  D�>�Dـ D�� D�  D�@ DځHD��HD�  D�>�D�~�D�� D�HD�@ D܀ D�� D�  D�AHD݂�D��HD�  D�@ Dނ�D��HD�  D�@ D߀ D߾�D���D�>�D�~�D�� D�  D�=qD�~�D��HD�HD�@ D�~�D⾸D�  D�@ D�~�D㾸D�  D�AHD� D�� D�HD�AHD�HD��HD�  D�>�D�~�D澸D�  D�@ D�~�D羸D�HD�B�D�HD�� D���D�>�D�~�D龸D���D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D�}qD쾸D�  D�@ D�HD�� D���D�AHD� D�� D��D�AHD�~�DﾸD�HD�AHD�~�D�D�HD�AHD�HD��HD�  D�@ D�~�D�� D�  D�@ D� D�� D���D�=qD� D�D���D�=qD�~�D���D�  D�AHD�~�D�� D���D�@ D��HD��HD�  D�@ D�~�D�� D�  D�@ D�~�D���D�  D�AHG�O�?��?L��?�z�?��
?Ǯ?��@�\@z�@#�
@8Q�@G�@W
=@k�@z�H@��
@�{@�
=@��R@��@���@���@�G�@˅@�z�@�p�@�ff@��@���AG�AffA�A  A�A��Ap�A"�\A(Q�A,��A1G�A5A;�A?\)AC�
AJ=qAN�RAR�\AW�A]p�A`��AeAk�Ap  Au�Az�HA~�RA���A�z�A�
=A�G�A��
A�ffA���A��HA�A�  A��\A�p�A��A���A���A�\)A���A�(�A�
=A�G�A��A�ffA���A�33A�A���A\A�p�A�Q�A�=qA���AϮA�=qA�z�A�
=A��A�(�A޸RA�G�A��
A�ffA���A�A�{A�Q�A��HA�A�Q�A�=qA�p�B (�B�BffB�
B�BffB�
B��B
{B\)B��B�B33B��B�B33BQ�BB33B(�Bp�B
=B(�BG�B�HB   B!�B"�RB$  B$��B&=qB'�B)�B*=qB+�B,��B.=qB/\)B0��B2{B333B4z�B5B733B8Q�B9p�B;
=B<(�B=p�B>�HB@  BA�BB�\BD  BE�BF=qBG�BI�BJffBK�BL��BN=qBO\)BP��BR{BS\)BTz�BU�BW\)BXz�BY��B[
=B\z�B]��B_
=B`Q�Ba��Bb�HBd(�Be��Bf�RBh  Bip�Bj�\Bk�
BmG�Bn�\Bo�Bp��Br=qBs�Bt��Bv{Bw�Bx��ByB{
=B|z�B}p�B~�RB�{B���B�33B��
B�z�B�
=B��B�Q�B���B��B�(�B���B�\)B�  B��RB�\)B��B�z�B�33B��
B�ffB�
=B�B�ffB���B��B�ffB���B��B�=qB���B���B�{B���B���B�=qB���B��B�=qB��HB�p�B�{B��RB�p�B�(�B��RB�\)B�{B��HB��B�{B���B��B�=qB��HB��B�Q�B��B�B�ffB��B�  B��RB�\)B�{B���B��B�ffB��B��
B��\B�p�B�(�B���B���B�Q�B��B�  B��RB�p�B�(�B��HB�B��\B�33B��B��RB��B�Q�B�
=B�B�z�B�33B�  B���B��B�=qB��HB�Bď\B�\)B�{B���BǅB�Q�B�33B�  BʸRB˅B�=qB�
=B��BΣ�B�\)B�{B���Bљ�B�ffB��B��
B�z�B�33B��B֣�B�\)B��
B�ffB�
=Bٙ�B�{B�z�B��HB��B�p�B�B�  B�=qB�ffB�z�B���B���B�
=B�33B�p�BݮB��
B��
B�{B�Q�Bޏ\B޸RB��HB���B��B�\)B߅B�B��B�{B�=qB�ffB��B��HB�
=B�33B�G�B�p�B�B��B�{B�=qB�ffB�z�B�RB���B��B�\)B�p�B㙚B�B��B�(�B�ffB�\B�RB��HB���B��B�\)B�B�B�  B�(�B�Q�B�z�B�RB���B�33B�\)B�B�B��
B�{B�Q�B�\B���B���B�33B�\)B�B�B�{B�Q�B�\B���B���B�33B�p�B�B�{B�ffB��B���B�G�B�B�B�  B�ffB�RB�
=B�\)B�B�{B�ffB��B���B�G�B�B��
B�=qB�\B���B�G�B�B�  B�Q�B���B���B�G�B��B�  B�ffB���B�33B���B��B�Q�B���B���B�G�B��B�  B�ffB���B�33B���B�  B�Q�B��RB�
=B�\)B��B�(�B�z�B���B�\)B�C {C Q�C z�C �C �
C{C=qCffC��C�
C
=C=qCp�C�C�HC�CQ�C�CC��C�CQ�C�C�RC�C(�CffC��C��C
=CG�C�C�RC��C33Cp�C��C�HC{CG�C�C�RC�C	�C	Q�C	�\C	��C

=C
=qC
z�C
�RC
��C(�CffC��C��C
=C=qCz�C�C�C�C\)C��C�
C{CQ�C�\C��C
=CG�C�CC
=CG�C�CC  C=qCz�C�C�C�CffC��C�
C{CG�C�CC  C=qCz�C�RC��C(�CffC��C�C�C\)C�\C��C
=C=qCz�C�RC��C33Cp�C�C�C�C\)C��C�HC{CQ�C�\C��C  C=qCz�C�RC��C(�Cp�C�C�HC�CffC��C�HC(�CffC��C�HC(�CffC��C�HC (�C \)C ��C �
C!�C!\)C!��C!�
C"{C"Q�C"�\C"��C#  C#=qC#z�C#�RC#��C$33C$p�C$��C$�C%�C%\)C%��C%�
C&�C&\)C&��C&�HC'�C'ffC'��C'�HC(�C(\)C(��C(�HC)�C)\)C)��C)�HC*{C*\)C*��C*�HC+�C+ffC+�C+�C,33C,z�C,�RC,��C-=qC-z�C-�RC-��C.=qC.�C.��C/
=C/Q�C/��C/�
C0�C0ffC0��C0�C133C1ffC1�C1��C2=qC2�C2�
C3�C3p�C3�C3�C433C4z�C4��C5�C5ffC5�C6  C6=qC6�C6��C7�C7p�C7C8
=C8\)C8��C8�HC9(�C9z�C9�
C:�C:p�C:C;
=C;Q�C;��C;�C<=qC<�\C<�HC=(�C=p�C=�RC>  C>\)C>��C?  C?G�C?��C?�HC@33C@z�C@�
CA33CA�CA�HCB33CB�CB��CC(�CC�CC�HCD=qCD�\CD�HCE33CE�CE�HCF=qCF��CG  CGQ�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�ȴAؾwAز-AخAإ�A؅A�jA�`BA�XA�?}A�$�A�VA�%A�  A���A���A���A���A���A���A���A���A�1A�bA�/A�v�A���A�5?AمA��A�VA�p�AڑhAڅA�
=A���A؅A�VA�~�A��A�{A�JA��
AˮAɇ+A�jA��A�A�A�ffA�A�1'A��A��\A��!A�^5A���A�$�A�-A���A���A�t�A�5?A��DA��A�XA�7LA��HA�VA���A�p�A�9XA���A�x�A��A��hA�C�A�K�A�n�A�ffA� �A��-A}�#Au��Aq�mAn �Ah-A`~�AZ-AXVAVz�AUl�AS�#AO�-AM33AL��AL��AL�DALv�AL9XAJI�AF5?A?��A=x�A;&�A7+A5\)A3�A3A1��A,�uA(�A'%A%�#A%`BA$z�A$1A#ƨA"A�A �\AhsA��Ar�Ar�AffA=qA��AjAM�A  A�;A��AĜA��A��A-A��A�7A�AffA��A��AbNA��A�#A=qA1'A7LA��A�HA�AXA�A��A��A=qA��A�
AhsA��A�A��A�^A`BA�AVA�AXA�A+A%AffAƨA`BAK�A�A�uAbNA �A�mA�A"�A��Ar�A��AȴA�A��A=qAVAXA+A1'AO�A?}A
��A�A
ffA	\)A��A��A�A��A�A�AbNA~�A�\Av�A1'A(�Al�AbNA(�AƨAƨA�^A�TAbA\)AȴAp�AhsA ��A 1'@���@��@��@�@���@�=q@�@��@�l�@�ff@���@�X@��`@�Z@��
@�|�@�K�@�+@�"�@��@�ff@���@��h@�x�@�9X@�D@��m@�+@�\@���@�@�/@��@�K�@@���@�h@��@�@�(�@��;@�l�@�ȴ@�5?@�V@蛦@�1'@��;@�ƨ@�P@��H@�v�@���@�7@��@��;@�R@�V@�@�@�r�@���@߅@߶F@��H@�M�@�x�@�%@��/@�9X@۝�@�K�@���@�E�@�@�X@�V@��`@�A�@�K�@��y@և+@���@�`B@�/@�&�@���@�9X@��
@�\)@�@�v�@�5?@��@ѡ�@��@мj@Ѓ@�ƨ@���@�v�@�=q@��@́@̛�@�bN@��@�$�@�%@ț�@�Ĝ@�I�@��;@ǥ�@���@�"�@�@�~�@�E�@�J@�p�@�r�@�  @å�@��@�n�@��T@��@��/@�Z@�1'@��@��P@��@���@�{@���@���@���@� �@��@�ȴ@�n�@�^5@���@�/@�Ĝ@��D@�b@���@�S�@�
=@��+@��T@���@�hs@���@��@� �@��P@�K�@�
=@�E�@�J@�@�%@�1'@�l�@�33@��H@���@�n�@�J@��#@�`B@��@�Ĝ@�bN@��;@�K�@��y@���@���@�^5@��#@�p�@�&�@�Ĝ@�9X@��@�33@��R@�n�@�@�x�@�V@�j@�9X@���@�o@���@�n�@�E�@��@���@���@�@��h@��/@�b@�l�@�
=@��+@�=q@�-@���@�x�@�7L@���@�I�@���@�@��R@���@�n�@�E�@�J@��@��@��u@�Q�@�ƨ@�S�@��@�V@��@���@�`B@�?}@�7L@��@���@��@�I�@��@��m@�l�@�K�@�+@��y@���@�n�@�5?@���@�X@��@���@�r�@�A�@�  @���@�l�@�C�@��H@��+@�v�@�E�@�J@�x�@�&�@���@�r�@�Q�@�b@��;@�o@���@�^5@�$�@���@�O�@��@���@��/@��j@��u@��u@��@�bN@�A�@�1@��;@�t�@��@�ȴ@�ȴ@���@��R@���@���@�^5@�J@�@��7@�/@��`@���@��D@�r�@�bN@�(�@�\)@�@���@��R@��+@�E�@�=q@�$�@��-@�`B@�X@�&�@���@���@�r�@�Z@�9X@�w@~��@~�R@~$�@}��@}/@|�@|j@|�@{�@{33@zn�@z=q@y��@y��@y�^@y�7@x��@xQ�@x �@w�@w�@w\)@w�@v�y@vȴ@vv�@u��@t�j@tZ@t9X@t(�@s�
@so@r��@r�!@r�\@r~�@r-@q�@q�#@q�^@q�7@qhs@q&�@p�u@pbN@pA�@p �@o��@o+@n�+@n$�@m�@m��@m�-@m�@mO�@m/@mV@lZ@l1@k��@k33@j��@j��@j~�@j^5@j=q@jJ@i��@i��@i�7@i7L@h��@h�@hr�@hA�@g�;@gl�@g�@f��@fV@e�@e��@ep�@eV@dz�@d9X@c�F@c"�@bJ@a�#@a&�@`�`@`1'@_�P@^�@^��@^V@]�T@]�h@]�h@]p�@]O�@]V@\�/@\�D@\�@[��@Z��@Zn�@Z^5@Z^5@Y��@YG�@Y7L@Y%@XĜ@XA�@W��@W��@Wl�@W;d@V��@V�+@U�@U��@U?}@T�j@T9X@T�@S��@S�F@S"�@R^5@Q��@Qx�@QX@Q7L@P��@P��@P��@Pr�@PA�@P1'@Pb@O�@O��@O;d@N�y@N��@N@M��@M��@M`B@L�@Lj@Kƨ@K33@J�\@Jn�@JM�@J=q@J-@I��@I�#@Ix�@I7L@I%@H��@HbN@H  @G�P@GK�@G�@F��@F��@Fv�@F5?@E�h@EO�@E�@Dz�@C�
@C�F@C�@CC�@B�@B��@B~�@BM�@B-@A��@AX@AG�@@��@@�@@b@?�P@?l�@?K�@>�@>v�@>E�@>{@=�@<��@<j@<j@<j@<Z@;�
@;��@;S�@:�@:��@:=q@9�#@9X@8�`@8�9@8��@8�u@8�@8bN@8Q�@81'@8  @7�w@7|�@7l�@7l�@6�y@6�+@5�@5�h@5p�@5`B@5?}@4��@49X@3�F@3C�@3@2��@2M�@2�@2J@1��@1�@1��@1G�@1�@1%@1%@0�u@/�P@/+@.�y@.�@.ȴ@.��@.E�@-/@-V@,�/@,�j@,��@,Z@+��@+t�@+C�@+"�@+"�@+o@+@*��@*n�@*-@)��@)7L@(�`@(r�@(A�@(A�@(1'@(  @'��@'�P@'l�@'K�@'K�@'K�@&�y@&�+@&5?@%�@%��@%��@%�h@%`B@%?}@%V@%�@%V@$�D@$1@#S�@"��@"��@"~�@"M�@!�@!�^@!x�@!G�@ ��@ Ĝ@ �@   @��@�@�@�@
=@��@5?@�@��@��@O�@?}@�/@��@�j@�D@z�@��@��@t�@o@33@o@n�@M�@M�@-@�@��@X@%@�`@Ĝ@�@Q�@1'@b@  @  @�;@��@|�@K�@;d@;d@�@��@��@�+@V@5?@{@@��@��@��@/@V@�/@�/@��@�j@��@(�@1@��@��@�m@�F@dZ@dZ@@��@��@~�@n�@n�@^5@M�@-@�@J@J@J@�@�^@��@��@�7@hs@X@&�@�`@�9@��@�u@bN@�;@�w@��@l�@�@�y@�@�@��@�+@ff@v�@V@E�@E�@5?@@�T@@�-@�h@p�@?}@��@�/@�j@��@�D@�@1@1@�
@ƨ@��@��A�A���A��
A���A�ȴA���A���A���A���A���A�ƨA�ƨA�ĜAز-A�ƨA�ȴAؼjAة�AجAش9AخAإ�AخAا�A؟�Aإ�Aء�A؁A�v�A؃A�ffA�jA�jA�hsA�`BA�^5A�\)A�VA�XA�ZA�E�A�/A�5?A�7LA�-A�1'A�-A�+A�$�A��A��A��A��A�{A�JA�JA�JA�%A�%A�
=A�%A�A�%A�1A�  A�%A�A���A���A���A���A���A���A���A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A��A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�%A�1A�%A�1A�VA�VA�
=A�VA�VA�JA�bA��A��A�{A�"�A�/A�5?A�;dA�O�A�bNA�dZA�v�A؅A؉7AؓuAجAؼjA�ƨA���A��mA��A�A�{A�9XA�;dA�;dA�E�A�ZA�bNA�r�AكAٗ�Aٛ�A٩�A�ĜA��HA��A���A�
=A�&�A�O�A�VA�VA�S�A�XA�^5A�`BA�`BA�t�A�v�A�t�A�v�A�|�AڅAڍPAڛ�Aڣ�AړuAڙ�Aڛ�AړuAډ7AڃA�z�A�^5A��A�A�A�A�A�A���A��A��A��mA�ĜAٙ�A�VA��A��Aش9Aؗ�A�7LAן�A׋DA� �A�bNA�5?A���Aգ�AՇ+A�A�A��A�bNA�=qA�(�A�1A�t�A�(�A�A���AҶFA�-A�M�A��;A�(�A���A�K�A��#A�dZA�$�A�K�A���A�Q�A���A��
A��A���A���A˩�A��
A��TA��A���A�Aˣ�A�^5AʾwA�^5A���A� �A�ƨA�G�A��AǶFA�p�A��A��
AƟ�A�^5A�&�A�ȴA�v�A�/A�A��yA�ȴAĉ7A�bNA�?}A�  A��A��yA��;A���AÙ�A�x�A�jA�1'A��yA�ȴA©�ADAA�x�A�jA�VA�5?A�%A���A���A��A��A���A�E�A���A�%A���A�C�A�
=A��A��
A�ȴA���A��\A��A�p�A�bNA�^5A�ZA�O�A�5?A�bA���A���A�z�A�Q�A�9XA�VA��A� �A�&�A�-A�1'A�9XA�7LA�9XA�7LA�1'A�/A�-A�+A�-A�+A�+A�$�A�&�A�$�A� �A��A��A�{A�A���A��A��A��A��yA��HA��;A���A�ȴA�A��!A���A��uA��7A�hsA�C�A� �A�1A���A��#A���A�ȴA�ȴA��wA��wA��jA��jA��!A���A���A���A���A���A��DA�|�A�x�A�t�A�l�A�hsA�`BA�\)A�VA�K�A�E�A�A�A�E�A�C�A�7LA��A��A���A��!A��DA�n�A�S�A�$�A��mA��RA���A�~�A�M�A��A���A��A��A��A�?}A��A�r�A�7LA�bA���A��A�5?A���A�9XA���A��A��A�?}A��A��wA��7A�dZA�bNA�S�A��A�v�A�A�A�%A���A���A�l�A�E�A�(�A�1A��TA��A�$�A��TA���A�r�A�G�A� �A���A���A�ƨA���A�E�A��mA�ffA��#A��hA�dZA�VA�ĜA��A�z�A�-A���A�M�A��A���A��TA��wA��A��A��A�A�p�A� �A���A��wA��A���A���A��A�r�A��A��FA��DA�ffA�O�A�=qA���A�+A���A�x�A�oA��HA��A�
=A�
=A��A���A��FA��\A�t�A�G�A�$�A�%A�ȴA�~�A�I�A��TA���A�E�A���A�VA���A��DA��A��-A�^5A�1'A�oA��/A���A�1'A��A��\A�^5A��A���A��A�A�A��
A�x�A�S�A�?}A��A��+A�oA��A�`BA�bA���A�/A��-A�VA���A�ffA�JA�ƨA�~�A�=qA���A���A��+A�r�A�=qA�%A��!A�VA���A���A�A�A���A���A�VA�G�A���A�9XA��-A�33A���A��A�1'A��yA���A�VA�%A��\A�  A���A�XA�"�A��`A�?}A��`A��A�K�A��A�ffA��A��A�=qA��!A�  A���A��RA��FA��FA��9A��-A��\A�hsA�G�A�  AA~5?A}K�A|��A|�A{7LAwp�Av��Av��Av�RAvr�Av  At�`As�-AsoArȴArM�Ar �Aq��Aq��AqG�Ap�/ApM�Ao�Aox�An�AnffAm��Al�`Ak��Ak\)Aj�Ai�
Ai�wAi�Ai/Ag7LAe;dAdA�AdJAc|�AbA�Aa&�A`n�A_33A^E�A];dA[t�AZ�HAZ�jAZ1'AY�TAY�-AYt�AYXAYVAX�/AXȴAX�DAX1AW�
AW��AWx�AW33AW
=AV��AVv�AV-AU�AU�wAU�PAU|�AU|�AUx�AUl�AU\)AUS�AUG�AUC�AU"�AT��ATZAR�/AR�\AR  AQ+APv�APZAPbAO�wAO?}AM�wAMt�AMK�AM7LAM&�AM"�AM�AM
=AL�ALȴAL��AL�9AL�AL�AL��AL��AL��AL��AL��AL��AL�uALv�ALn�ALn�AL�AL�jAL�9AL�\AL�ALz�ALv�ALr�ALr�ALr�ALv�ALv�ALv�ALv�AL-AK�
AKƨAK��AK�AJI�AI�FAIS�AH��AH=qAG�AGVAE�AD-AC&�AA�wA@��A?��A?+A>�A>bA>bA>A=�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  A���A���A�ȴAؾwAز-AخAإ�A؅A�jA�`BA�XA�?}A�$�A�VA�%A�  A���A���A���A���A���A���A���A���A�1A�bA�/A�v�A���A�5?AمA��A�VA�p�AڑhAڅA�
=A���A؅A�VA�~�A��A�{A�JA��
AˮAɇ+A�jA��A�A�A�ffA�A�1'A��A��\A��!A�^5A���A�$�A�-A���A���A�t�A�5?A��DA��A�XA�7LA��HA�VA���A�p�A�9XA���A�x�A��A��hA�C�A�K�A�n�A�ffA� �A��-A}�#Au��Aq�mAn �Ah-A`~�AZ-AXVAVz�AUl�AS�#AO�-AM33AL��AL��AL�DALv�AL9XAJI�AF5?A?��A=x�A;&�A7+A5\)A3�A3A1��A,�uA(�A'%A%�#A%`BA$z�A$1A#ƨA"A�A �\AhsA��Ar�Ar�AffA=qA��AjAM�A  A�;A��AĜA��A��A-A��A�7A�AffA��A��AbNA��A�#A=qA1'A7LA��A�HA�AXA�A��A��A=qA��A�
AhsA��A�A��A�^A`BA�AVA�AXA�A+A%AffAƨA`BAK�A�A�uAbNA �A�mA�A"�A��Ar�A��AȴA�A��A=qAVAXA+A1'AO�A?}A
��A�A
ffA	\)A��A��A�A��A�A�AbNA~�A�\Av�A1'A(�Al�AbNA(�AƨAƨA�^A�TAbA\)AȴAp�AhsA ��A 1'@���@��@��@�@���@�=q@�@��@�l�@�ff@���@�X@��`@�Z@��
@�|�@�K�@�+@�"�@��@�ff@���@��h@�x�@�9X@�D@��m@�+@�\@���@�@�/@��@�K�@@���@�h@��@�@�(�@��;@�l�@�ȴ@�5?@�V@蛦@�1'@��;@�ƨ@�P@��H@�v�@���@�7@��@��;@�R@�V@�@�@�r�@���@߅@߶F@��H@�M�@�x�@�%@��/@�9X@۝�@�K�@���@�E�@�@�X@�V@��`@�A�@�K�@��y@և+@���@�`B@�/@�&�@���@�9X@��
@�\)@�@�v�@�5?@��@ѡ�@��@мj@Ѓ@�ƨ@���@�v�@�=q@��@́@̛�@�bN@��@�$�@�%@ț�@�Ĝ@�I�@��;@ǥ�@���@�"�@�@�~�@�E�@�J@�p�@�r�@�  @å�@��@�n�@��T@��@��/@�Z@�1'@��@��P@��@���@�{@���@���@���@� �@��@�ȴ@�n�@�^5@���@�/@�Ĝ@��D@�b@���@�S�@�
=@��+@��T@���@�hs@���@��@� �@��P@�K�@�
=@�E�@�J@�@�%@�1'@�l�@�33@��H@���@�n�@�J@��#@�`B@��@�Ĝ@�bN@��;@�K�@��y@���@���@�^5@��#@�p�@�&�@�Ĝ@�9X@��@�33@��R@�n�@�@�x�@�V@�j@�9X@���@�o@���@�n�@�E�@��@���@���@�@��h@��/@�b@�l�@�
=@��+@�=q@�-@���@�x�@�7L@���@�I�@���@�@��R@���@�n�@�E�@�J@��@��@��u@�Q�@�ƨ@�S�@��@�V@��@���@�`B@�?}@�7L@��@���@��@�I�@��@��m@�l�@�K�@�+@��y@���@�n�@�5?@���@�X@��@���@�r�@�A�@�  @���@�l�@�C�@��H@��+@�v�@�E�@�J@�x�@�&�@���@�r�@�Q�@�b@��;@�o@���@�^5@�$�@���@�O�@��@���@��/@��j@��u@��u@��@�bN@�A�@�1@��;@�t�@��@�ȴ@�ȴ@���@��R@���@���@�^5@�J@�@��7@�/@��`@���@��D@�r�@�bN@�(�@�\)@�@���@��R@��+@�E�@�=q@�$�@��-@�`B@�X@�&�@���@���@�r�@�Z@�9X@�w@~��@~�R@~$�@}��@}/@|�@|j@|�@{�@{33@zn�@z=q@y��@y��@y�^@y�7@x��@xQ�@x �@w�@w�@w\)@w�@v�y@vȴ@vv�@u��@t�j@tZ@t9X@t(�@s�
@so@r��@r�!@r�\@r~�@r-@q�@q�#@q�^@q�7@qhs@q&�@p�u@pbN@pA�@p �@o��@o+@n�+@n$�@m�@m��@m�-@m�@mO�@m/@mV@lZ@l1@k��@k33@j��@j��@j~�@j^5@j=q@jJ@i��@i��@i�7@i7L@h��@h�@hr�@hA�@g�;@gl�@g�@f��@fV@e�@e��@ep�@eV@dz�@d9X@c�F@c"�@bJ@a�#@a&�@`�`@`1'@_�P@^�@^��@^V@]�T@]�h@]�h@]p�@]O�@]V@\�/@\�D@\�@[��@Z��@Zn�@Z^5@Z^5@Y��@YG�@Y7L@Y%@XĜ@XA�@W��@W��@Wl�@W;d@V��@V�+@U�@U��@U?}@T�j@T9X@T�@S��@S�F@S"�@R^5@Q��@Qx�@QX@Q7L@P��@P��@P��@Pr�@PA�@P1'@Pb@O�@O��@O;d@N�y@N��@N@M��@M��@M`B@L�@Lj@Kƨ@K33@J�\@Jn�@JM�@J=q@J-@I��@I�#@Ix�@I7L@I%@H��@HbN@H  @G�P@GK�@G�@F��@F��@Fv�@F5?@E�h@EO�@E�@Dz�@C�
@C�F@C�@CC�@B�@B��@B~�@BM�@B-@A��@AX@AG�@@��@@�@@b@?�P@?l�@?K�@>�@>v�@>E�@>{@=�@<��@<j@<j@<j@<Z@;�
@;��@;S�@:�@:��@:=q@9�#@9X@8�`@8�9@8��@8�u@8�@8bN@8Q�@81'@8  @7�w@7|�@7l�@7l�@6�y@6�+@5�@5�h@5p�@5`B@5?}@4��@49X@3�F@3C�@3@2��@2M�@2�@2J@1��@1�@1��@1G�@1�@1%@1%@0�u@/�P@/+@.�y@.�@.ȴ@.��@.E�@-/@-V@,�/@,�j@,��@,Z@+��@+t�@+C�@+"�@+"�@+o@+@*��@*n�@*-@)��@)7L@(�`@(r�@(A�@(A�@(1'@(  @'��@'�P@'l�@'K�@'K�@'K�@&�y@&�+@&5?@%�@%��@%��@%�h@%`B@%?}@%V@%�@%V@$�D@$1@#S�@"��@"��@"~�@"M�@!�@!�^@!x�@!G�@ ��@ Ĝ@ �@   @��@�@�@�@
=@��@5?@�@��@��@O�@?}@�/@��@�j@�D@z�@��@��@t�@o@33@o@n�@M�@M�@-@�@��@X@%@�`@Ĝ@�@Q�@1'@b@  @  @�;@��@|�@K�@;d@;d@�@��@��@�+@V@5?@{@@��@��@��@/@V@�/@�/@��@�j@��@(�@1@��@��@�m@�F@dZ@dZ@@��@��@~�@n�@n�@^5@M�@-@�@J@J@J@�@�^@��@��@�7@hs@X@&�@�`@�9@��@�u@bN@�;@�w@��@l�@�@�y@�@�@��@�+@ff@v�@V@E�@E�@5?@@�T@@�-@�h@p�@?}@��@�/@�j@��@�D@�@1@1@�
@ƨ@��G�O�A�A���A��
A���A�ȴA���A���A���A���A���A�ƨA�ƨA�ĜAز-A�ƨA�ȴAؼjAة�AجAش9AخAإ�AخAا�A؟�Aإ�Aء�A؁A�v�A؃A�ffA�jA�jA�hsA�`BA�^5A�\)A�VA�XA�ZA�E�A�/A�5?A�7LA�-A�1'A�-A�+A�$�A��A��A��A��A�{A�JA�JA�JA�%A�%A�
=A�%A�A�%A�1A�  A�%A�A���A���A���A���A���A���A���A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A��A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�%A�1A�%A�1A�VA�VA�
=A�VA�VA�JA�bA��A��A�{A�"�A�/A�5?A�;dA�O�A�bNA�dZA�v�A؅A؉7AؓuAجAؼjA�ƨA���A��mA��A�A�{A�9XA�;dA�;dA�E�A�ZA�bNA�r�AكAٗ�Aٛ�A٩�A�ĜA��HA��A���A�
=A�&�A�O�A�VA�VA�S�A�XA�^5A�`BA�`BA�t�A�v�A�t�A�v�A�|�AڅAڍPAڛ�Aڣ�AړuAڙ�Aڛ�AړuAډ7AڃA�z�A�^5A��A�A�A�A�A�A���A��A��A��mA�ĜAٙ�A�VA��A��Aش9Aؗ�A�7LAן�A׋DA� �A�bNA�5?A���Aգ�AՇ+A�A�A��A�bNA�=qA�(�A�1A�t�A�(�A�A���AҶFA�-A�M�A��;A�(�A���A�K�A��#A�dZA�$�A�K�A���A�Q�A���A��
A��A���A���A˩�A��
A��TA��A���A�Aˣ�A�^5AʾwA�^5A���A� �A�ƨA�G�A��AǶFA�p�A��A��
AƟ�A�^5A�&�A�ȴA�v�A�/A�A��yA�ȴAĉ7A�bNA�?}A�  A��A��yA��;A���AÙ�A�x�A�jA�1'A��yA�ȴA©�ADAA�x�A�jA�VA�5?A�%A���A���A��A��A���A�E�A���A�%A���A�C�A�
=A��A��
A�ȴA���A��\A��A�p�A�bNA�^5A�ZA�O�A�5?A�bA���A���A�z�A�Q�A�9XA�VA��A� �A�&�A�-A�1'A�9XA�7LA�9XA�7LA�1'A�/A�-A�+A�-A�+A�+A�$�A�&�A�$�A� �A��A��A�{A�A���A��A��A��A��yA��HA��;A���A�ȴA�A��!A���A��uA��7A�hsA�C�A� �A�1A���A��#A���A�ȴA�ȴA��wA��wA��jA��jA��!A���A���A���A���A���A��DA�|�A�x�A�t�A�l�A�hsA�`BA�\)A�VA�K�A�E�A�A�A�E�A�C�A�7LA��A��A���A��!A��DA�n�A�S�A�$�A��mA��RA���A�~�A�M�A��A���A��A��A��A�?}A��A�r�A�7LA�bA���A��A�5?A���A�9XA���A��A��A�?}A��A��wA��7A�dZA�bNA�S�A��A�v�A�A�A�%A���A���A�l�A�E�A�(�A�1A��TA��A�$�A��TA���A�r�A�G�A� �A���A���A�ƨA���A�E�A��mA�ffA��#A��hA�dZA�VA�ĜA��A�z�A�-A���A�M�A��A���A��TA��wA��A��A��A�A�p�A� �A���A��wA��A���A���A��A�r�A��A��FA��DA�ffA�O�A�=qA���A�+A���A�x�A�oA��HA��A�
=A�
=A��A���A��FA��\A�t�A�G�A�$�A�%A�ȴA�~�A�I�A��TA���A�E�A���A�VA���A��DA��A��-A�^5A�1'A�oA��/A���A�1'A��A��\A�^5A��A���A��A�A�A��
A�x�A�S�A�?}A��A��+A�oA��A�`BA�bA���A�/A��-A�VA���A�ffA�JA�ƨA�~�A�=qA���A���A��+A�r�A�=qA�%A��!A�VA���A���A�A�A���A���A�VA�G�A���A�9XA��-A�33A���A��A�1'A��yA���A�VA�%A��\A�  A���A�XA�"�A��`A�?}A��`A��A�K�A��A�ffA��A��A�=qA��!A�  A���A��RA��FA��FA��9A��-A��\A�hsA�G�A�  AA~5?A}K�A|��A|�A{7LAwp�Av��Av��Av�RAvr�Av  At�`As�-AsoArȴArM�Ar �Aq��Aq��AqG�Ap�/ApM�Ao�Aox�An�AnffAm��Al�`Ak��Ak\)Aj�Ai�
Ai�wAi�Ai/Ag7LAe;dAdA�AdJAc|�AbA�Aa&�A`n�A_33A^E�A];dA[t�AZ�HAZ�jAZ1'AY�TAY�-AYt�AYXAYVAX�/AXȴAX�DAX1AW�
AW��AWx�AW33AW
=AV��AVv�AV-AU�AU�wAU�PAU|�AU|�AUx�AUl�AU\)AUS�AUG�AUC�AU"�AT��ATZAR�/AR�\AR  AQ+APv�APZAPbAO�wAO?}AM�wAMt�AMK�AM7LAM&�AM"�AM�AM
=AL�ALȴAL��AL�9AL�AL�AL��AL��AL��AL��AL��AL��AL�uALv�ALn�ALn�AL�AL�jAL�9AL�\AL�ALz�ALv�ALr�ALr�ALr�ALv�ALv�ALv�ALv�AL-AK�
AKƨAK��AK�AJI�AI�FAIS�AH��AH=qAG�AGVAE�AD-AC&�AA�wA@��A?��A?+A>�A>bA>bA>A=�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	l�B	l�B	lWB	kQB	m]B	k�B	k�B	k�B	k�B	kB	jB	k�B	lWB	l�B	k�B	kQB	j�B	l"B	m)B	m]B	m�B	m�B	m]B	poB	tTB	v�B	~(B	��B	�6B	�^B	�`B
�B
0UB
9XB
C�B
H�B
?B
<B
�B	ݘB	�B	�4B	^B	�B	*0B	r�B	��B	�hB	͟B
�B
-wB
4�B
6B
>B
J#B
R�B
Q�B
M�B
F�B
W�B
��B
��B
�dB
��B0�Bu�B��B��B��B��B�B�-B�UB�B��Bv+BY�B1[BDB
��B
��B
z�B
2�B	�B	�5B	ΥB	��B	�~B	kB	O�B	'RB	B	�B	�B	�B	$B	.�B	G�B	RTB	c�B	� B	��B	�9B	��B	��B	dZB	S�B	?}B	<6B	H�B	bNB	d�B	g�B	?HB	+B	#�B	 �B	�B	�B	�B	+B	�B	$�B	/�B	:�B	C�B	J#B	PB	U2B	XB	ZQB	`B	g�B	zxB	�lB	��B	��B	��B	�B	�'B	�$B	��B	�/B	��B	�cB	�
B	�sB	�B	��B	��B	�`B
�B
B	��B	��B	��B	�VB
B
�B
B
�B
.B
�B
MB
!-B
!B
�B
1B
�B
$B
_B
B
#nB
"�B
!bB
�B
#�B
$�B
%FB
(�B
+6B
+kB
+�B
+B
)_B
,qB
6B
9�B
8B
9�B
6FB
IRB
MjB
OBB
J#B
<�B
=qB
<jB
A B
B[B
=<B
2-B
9�B
6zB
1'B
/�B
2�B
0�B
5tB
9�B
8B
7�B
8�B
9�B
/�B
.IB
,�B
3hB
5tB
8B
?�B
=qB
<jB
/B
/�B
/�B
0UB
+B
+B
.B
1�B
4�B
3�B
1�B
1�B
,�B
)�B
&�B
$�B
'B
'B
'B
'B
%B
%�B
$tB
)�B
'B
%zB
$B
)�B
%zB
%�B
)_B
(�B
$�B
'�B
'�B
)�B
(�B
(XB
'�B
&�B
%zB
%B
%B
$@B
#�B
#�B
#�B
#nB
"�B
!�B
!�B
!-B
 'B
 �B
 �B
 \B
!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
(B
B
�B
:B
�B
hB
�B
�B
.B
�B
.B
�B
�B
�B
4B
�B
�B
\B
�B
VB
PB
PB
~B
�B
�B
�B
�B
DB

�B
�B
B
�B
�B
B
DB

�B

�B

=B
�B
bB
bB
�B
\B
�B
bB
DB
�B
1B
B
~B

=B

	B
	B
	lB
�B
�B
�B
�B
B
~B
xB

rB

�B

�B
B

�B

rB
	�B
B
�B
�B
�B
�B
B
�B
�B
PB
~B
�B
�B
~B
�B
�B
�B
�B
�B
VB
�B
�B
B
PB
�B
�B
�B
�B
\B
�B
�B
\B
(B
�B
(B
\B
bB
 B
hB
4B
hB
4B
hB
�B
�B
oB
:B
oB
oB
B
@B
�B
uB
uB
FB
B
{B
FB
�B
�B
�B
�B
SB
B
�B
$B
�B
�B
+B
eB
�B
	B
�B
	B
	B
	B
=B
	B
	B
B
xB
�B
B
CB
�B
�B
�B
B
CB
B
�B
B
OB
�B
�B
�B
�B
VB
 \B
 \B
 \B
 \B
!bB
!-B
!�B
"hB
"�B
"�B
#B
#B
#:B
%�B
'B
($B
(�B
(�B
)_B
)_B
)�B
*0B
*0B
+B
+�B
,�B
-�B
.IB
.�B
/�B
/�B
/OB
/�B
/�B
0!B
0!B
0�B
1�B
1�B
2-B
2�B
33B
33B
3�B
3�B
3�B
3�B
3hB
5?B
5?B
5?B
4�B
6B
6zB
6FB
6�B
6�B
7B
7�B
7�B
7�B
8B
8B
8�B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8RB
8RB
8�B
9XB
9�B
9�B
:^B
:�B
:^B
:�B
:�B
:�B
:�B
=B
<�B
<�B
<�B
=B
=B
=B
=<B
=�B
>BB
>wB
?�B
?}B
@�B
?�B
?�B
?�B
@OB
@B
?�B
@�B
@�B
AUB
A�B
A�B
B�B
CaB
CaB
C�B
C�B
D3B
D3B
C�B
D3B
EmB
EB
E�B
EmB
E�B
FB
FB
FB
E�B
FtB
GB
GzB
GEB
GzB
GB
G�B
HB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
IRB
IRB
IRB
I�B
J�B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
MjB
M�B
MjB
MjB
M�B
M�B
M�B
M�B
NB
M�B
N<B
NpB
NpB
NB
NpB
N�B
N�B
N�B
OvB
OBB
O�B
O�B
O�B
P}B
PB
PHB
O�B
P�B
QB
QB
Q�B
QNB
R B
R�B
R�B
S&B
S&B
S[B
S&B
S[B
S�B
S�B
T,B
S�B
T,B
TaB
T�B
T�B
TaB
T,B
T,B
U�B
U2B
T�B
UgB
UgB
VB
U�B
VB
VB
VB
V9B
V9B
V�B
W
B
W?B
WsB
XEB
W�B
W�B
W�B
X�B
YKB
Y�B
Y�B
Y�B
YB
Y�B
Y�B
Y�B
ZB
Y�B
ZB
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
[�B
[WB
[�B
[�B
\)B
\]B
]/B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^5B
^5B
^B
^5B
^�B
_B
_;B
_;B
_;B
_�B
_;B
_�B
_pB
`vB
`B
`BB
aHB
aHB
a|B
aHB
a�B
bNB
bNB
bB
bNB
bNB
b�B
b�B
b�B
cTB
cTB
c�B
d&B
c�B
d&B
d�B
d�B
d�B
d�B
f2B
e�B
e�B
e�B
e�B
e`B
f2B
e�B
f2B
ffB
f�B
gmB
g�B
h>B
hsB
hsB
h>B
h>B
h>B
hsB
h>B
h�B
h�B
h�B
iDB
iB
iB
iyB
i�B
jB
jB
jB
jKB
jB
j�B
kB
kQB
k�B
k�B
k�B
l"B
lWB
lWB
lWB
lWB
l�B
l�B
l�B
ncB
o B
o5B
o B
oiB
o�B
o�B
pB
p;B
p�B
q�B
q�B
q�B
q�B
rB
r|B
sB
sB
sMB
s�B
s�B
s�B
sMB
s�B
s�B
tB
t�B
t�B
t�B
u�B
u�B
v+B
v+B
v`B
v`B
v�B
v�B
v`B
v+B
v+B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
xlB
xlB
xlB
xlB
x�B
x�B
x�B
x8B
wfB
wfB
wfB
wfB
w�B
wfB
w�B
xB
xlB
x8B
x�B
x�B
y	B
y�B
zDB
zDB
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}�B
~�B
~]B
.B
�B
.B
~�B
~(B
~�B
�B
cB
cB
.B
cB
.B
~�B
~]B
~]B
~�B
~�B
cB
�B
� B
�4B
�4B
�iB
�iB
�iB
��B
��B
��B
��B
��B
�4B
�iB
�iB
�B
�;B
�oB
�oB
��B
��B
��B
�B
��B
�B
�{B
�MB
�B
��B
��B
��B
��B
��B
��B
��B
�SB
�%B
�YB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
��B
�fB
�fB
��B
��B
�fB
�B
�lB
�7B
�7B
��B
�rB
�	B
��B
��B
��B
�xB
��B
�DB
�JB
�~B
��B
��B
�B
��B
��B
�~B
�JB
��B
��B
��B
�~B
�B
�B
�~B
��B
��B
�~B
�JB
��B
�B	m�B	h�B	k�B	o5B	k�B	k�B	m)B	l�B	jKB	m]B	lWB	k�B	j�B	k�B	i�B	jB	o B	m�B	l�B	h>B	m�B	l�B	jB	k�B	o B	j�B	m�B	r|B	e�B	lWB	m�B	l�B	hsB	i�B	n/B	iDB	h�B	j�B	k�B	jB	k�B	m�B	jB	kQB	lWB	k�B	kB	kB	m�B	lWB	k�B	m�B	l�B	kQB	m�B	lWB	j�B	k�B	l"B	j�B	k�B	l�B	j�B	kB	m]B	j�B	k�B	kQB	k�B	j�B	l�B	k�B	jB	j�B	lWB	j�B	i�B	k�B	k�B	k�B	l�B	m�B	k�B	l�B	m�B	l�B	lWB	m�B	m�B	lWB	m)B	m�B	m�B	l�B	l�B	n�B	m)B	l�B	ncB	ncB	m)B	m)B	m�B	n/B	l�B	n/B	n�B	m)B	m�B	o�B	m�B	ncB	p;B	o5B	n/B	o5B	qvB	qB	oiB	p�B	r�B	rB	q�B	tTB	r�B	uZB	v`B	u�B	u�B	v`B	v�B	w2B	v�B	y>B	z�B	{�B	{B	�B	��B	�SB	��B	��B	�B	�B	��B	��B	�xB	��B	�=B	�B	��B	�FB	��B	��B	�9B	�&B	�TB	�B	��B	�5B	��B	�B	�DB	�B	�;B	�lB	�rB
	7B
DB
�B
�B
/�B
0�B
1'B
33B
33B
4nB
5?B
6B
8�B
:�B
;�B
<6B
;�B
<B
C-B
@�B
J�B
K)B
HB
H�B
I�B
I�B
H�B
FB
H�B
F�B
;dB
:�B
=<B
?}B
=<B
=�B
>B
;�B
:�B
=<B
:�B
2�B
2-B
-wB
!�B
$B
VB
�B	�B
�B	��B	ԕB	՛B	�RB	�6B	��B	��B	��B	�+B	�VB	�hB	�B	~]B	zDB	�\B	��B	��B	��B	l"B	rGB	N<B	F�B	4nB	*�B	4B	?HB	�B	�B	"hB	hB	�B	(XB	1'B	.}B	;dB	_�B	c�B	k�B	u�B	zxB	�7B	�SB	��B	~�B	�	B	�.B	�7B	��B	�xB	�zB	�0B	��B	��B	�3B	��B	�2B	�vB	�/B	��B
@B
�B
(�B
(�B
)�B
3hB
-B
-�B
/�B
/�B
1'B
.IB
5?B
>wB
8�B
1'B
1�B
7�B
9�B
<�B
;0B
<jB
=<B
B�B
>wB
A�B
C-B
F�B
H�B
U2B
a�B
R�B
T,B
NB
NpB
N�B
N�B
T�B
WsB
OB
O�B
NpB
NpB
K�B
M6B
L�B
L�B
I�B
HB
C�B
GzB
C�B
G�B
N�B
T�B
^�B
_;B
b�B
g�B
sMB
|B
��B
��B
��B
�B
�B
�B
�B
�@B
��B
��B
��B
�:B
��B
��B
�@B
�B
��B
��B
�CB
�B
��B
��B
�!B
��B
�B
��B
�qB
�9B
��B
�^B
ӏB
�B
��B
�B
��B
�B
� B
�B
�vB
�B
�BB
��B
�]B
��B
��B
ݘB
�dB
�WB
�#B
�]B
یB
ޞB
�]B
�dB
�B
ܒB
�dB
��B
ݘB
��B
ޞB
�;B
�B
�2B
�DB
�B
��B
��B
��B  B  B�B�B_BYB�B
rBhB�B�B$B�B=�BB-wB)�B&�B#�B%�B?�BF�BS&BF�BP�BOvBYB]/BoiBsBtTBxlBz�B}VB�B��B�1B�rB�B��B�B��B��B�GB�PB�+B��B��B��B�B��B�@B��B��B�"B�+B��B��B��B�+B�@B�hB�B��B��B��B�B�_B��B�(B��B��B��B�YB��B�tB�IB��B��B�B��B��B��B��B�@B��B��B��B��B��B��B�wB�KB�OB��B��B�wB�RB�B��B��BÖB��B�-B��B��BB��B��B�HB�}B��B��B��B�B�wB��B�CB�=B�B��B�B��B��B��B�bB�uB��B�SB|�B��B|BwfBm�Bu%Bm�B]�B[�B^5Bo5B]�BQBQBS�BPBCaBB�BI�B0UB,�B*�BB!�B�BxB
	B+B+B
=B+B�B
��B;B
��B
��B
�B
�B
�vB
�5B
�B
�gB
ӏB
�B
��B
�?B
��B
��B
��B
�OB
�	B
��B
��B
�iB
w�B
l"B
k�B
uZB
?}B
R�B
V�B
>�B
5?B
�B
!�B
�B
	B
	lB	��B	��B	�8B	�B	�B	�ZB	�]B	�yB	�>B	�QB	��B	��B	�B	�B	��B	��B
N�B	��B	�3B	��B	�}B	ȀB	�B	��B	�UB	�IB	��B	�zB	��B	�:B	�-B	��B	�kB	��B	�.B	�hB	��B	��B	��B	�iB	|B	y�B	hsB	^�B	]dB	m)B	�AB	{B	W?B	S�B	W�B	a�B	OvB	J�B	R�B	F�B	?B	S[B	&LB	#nB	/�B	�B	#:B	�B	�B	!�B	qB	�B	 �B	 �B	B	�B	�B	�B	B	�B	�B	MB	FB	�B	�B	�B	�B	"B	�B	�B	�B	
=B		B	�B	_B	�B	�B	�B	MB	!-B	�B	
rB		lB	�B	�B	>B	#nB	%�B	(�B	+�B	0�B	9$B	=B	B'B	A�B	GzB	I�B	K�B	K�B	L�B	N�B	PHB	S[B	S[B	S�B	S�B	VB	XB	YB	\�B	\�B	s�B	{�B	|�B	~�B	cB	� B	� B	��B	�AB	��B	��B	��B	��B	�-B	�UB	�B	�^B	�[B	�aB	�<B	��B	�LB	��B	�zB	��B	�eB	��B	�qB	��B	��B	�YB	k�B	dZB	a�B	a|B	e,G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  B	l�B	l�B	l�B	k�B	m�B	lAB	l�B	l�B	lLB	k�B	j�B	l(B	l�B	l�B	k�B	k�B	j�B	l2B	mJB	mcB	m�B	m�B	m\B	pGB	t#B	v%B	|�B	�B	�AB	ɏB	�(B
�B
/�B
8�B
D8B
J�B
A%B
C1B
)�B	��B	�mB	�
B	m�B	'@B	,�B	~xB	��B	��B	ԢB
�B
0�B
9B
8�B
A�B
UoB
W_B
S�B
QB
J$B
XcB
��B
�
B
�?B�BJ�B��B�B�B� B�YB�:B��B�CB�8B��B��Bn�B@�BB�B
ȥB
��B
H�B	�B
�B	ܭB	�'B	�?B	��B	d
B	.<B	"9B	zB	�B	@B	~B	0LB	HB	R�B	dyB	��B	�B	�}B	��B	�SB	mB	`9B	FB	A~B	L1B	gB	u�B	s�B	FWB	/TB	%�B	#�B	!%B	DB	�B	-B	�B	&�B	0�B	:�B	C�B	J�B	Q�B	Y�B	X�B	[mB	`�B	iB	}JB	�lB	�eB	�tB	�^B	��B	��B	��B	�B	�GB	��B	�EB	��B	�aB	��B	��B	� B	��B
�B
'B	�kB	�TB	�GB	��B
�B
�B
�B
~B
lB
�B
?B
"QB
!B
�B
B
B
�B
QB
�B
%�B
%B
"�B
 1B
%B
%�B
&B
)�B
,B
,�B
-@B
,�B
)�B
+]B
6%B
:WB
8�B
:jB
3�B
H+B
NBB
R�B
MB
=!B
>�B
;�B
C�B
FB
?�B
2�B
="B
8�B
2DB
0 B
2�B
0sB
5HB
:B
9B
7�B
;2B
=B
0�B
/�B
,�B
3~B
4�B
7�B
BB
?�B
@�B
/�B
2:B
1�B
1�B
,	B
+YB
.B
2�B
5nB
4�B
3B
4�B
.�B
+(B
'NB
%�B
(B
(B
'�B
'xB
%ZB
%�B
$�B
+)B
(iB
%�B
$|B
+�B
%&B
'!B
*�B
)�B
%�B
(8B
(�B
*�B
+WB
)�B
(�B
'�B
&sB
&B
%�B
$�B
$�B
%B
$�B
%pB
#�B
"�B
"_B
!iB
 �B
!�B
!kB
!LB
 B
�B
�B
�B
�B
,B
�B
�B
�B
�B
	B
B
�B
�B
�B
�B
	B
�B
�B
sB
~B
�B
�B
-B
�B
	B
�B
%B
VB
]B
kB
�B
�B
ZB
3B
�B
�B
�B
�B
8B
�B
�B
�B
�B
�B
DB
B
6B
4B
�B
KB
UB
�B
�B
B
\B
�B
B
�B
@B

�B
,B
�B
	�B
iB
�B
rB
�B
�B
[B
7B
�B
�B
�B
�B
B
gB

 B
�B
uB
`B
hB
�B
�B
B
7B
�B
pB
�B
1B
�B
B
B
QB
B
�B
=B
yB
�B
B
~B
AB
B
�B
bB
kB
�B
�B
�B
�B
tB
�B
�B
�B
hB
�B
�B
�B
�B
!B
B
�B
�B
�B
7B
oB
#B
�B
$B
�B
�B
FB
�B
B
B
�B
.B
"B
�B
�B
�B
�B
B
�B
iB
B
�B
�B
qB
)B
_B
PB
gB
]B
�B
eB
�B
�B
yB
B
�B
B
�B
B
�B
 B
B
�B
3B
�B
3B
B
�B
nB
 iB
!wB
!
B
 �B
!kB
"AB
!�B
"�B
#*B
#1B
#-B
#FB
# B
#tB
%�B
'�B
(�B
(�B
)gB
*@B
)�B
)�B
*�B
*�B
+vB
,$B
-�B
.B
.�B
/[B
02B
0!B
/�B
0rB
0QB
0�B
0�B
1�B
1�B
2aB
2�B
3�B
3�B
3�B
4�B
3�B
4B
4'B
4�B
6'B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
8fB
8�B
8�B
9'B
9�B
9�B
8�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:B
:pB
:�B
:�B
:�B
;0B
:�B
;B
<�B
=�B
=B
<�B
=B
=�B
=$B
=JB
>B
>rB
>aB
>�B
@B
@,B
@�B
@"B
@3B
@hB
AB
@kB
@xB
AB
A1B
A�B
A�B
BB
CRB
C�B
DB
D6B
DB
DdB
DLB
DAB
D�B
E�B
E@B
E�B
E�B
F)B
FQB
F@B
F4B
FB
GEB
G�B
G�B
GnB
G�B
GmB
HaB
HZB
G�B
HB
G�B
H8B
H�B
H�B
H�B
H�B
IB
I B
IrB
I�B
IzB
I}B
I�B
J`B
K�B
K�B
K�B
K�B
K�B
L0B
K�B
LB
K�B
L�B
M&B
M6B
MvB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N/B
N)B
N�B
N�B
N�B
NAB
N�B
OB
O3B
OZB
O�B
O�B
O�B
PB
PGB
QB
PbB
P�B
P�B
Q�B
QdB
Q�B
Q�B
R B
R�B
ScB
S<B
SpB
S�B
S�B
S1B
S�B
S�B
S�B
TbB
TQB
T�B
T�B
U�B
U B
TwB
T?B
T�B
V(B
UNB
U6B
U�B
U�B
VqB
U�B
V9B
V9B
VMB
V�B
V�B
W1B
WqB
W�B
W�B
XiB
XB
W�B
XzB
YxB
ZB
ZB
Y�B
Y�B
Y�B
ZB
ZB
ZB
ZNB
ZB
Z?B
ZB
ZBB
Z�B
Z�B
Z�B
[yB
[�B
[�B
[�B
\5B
\�B
]B
]�B
^hB
]�B
]�B
]�B
]�B
]�B
]�B
^.B
^xB
^kB
^?B
^�B
_B
_zB
_�B
_uB
_�B
_�B
_fB
_�B
`B
`�B
`RB
`�B
a�B
aqB
a�B
a�B
bB
b�B
btB
bMB
bzB
b�B
cWB
c
B
c*B
c�B
c�B
doB
dNB
dB
d�B
eTB
d�B
e:B
e�B
f�B
e�B
e�B
e�B
e�B
e�B
fwB
fFB
f�B
f�B
g-B
g�B
h&B
h�B
h�B
h�B
hSB
hTB
haB
h�B
hgB
h�B
h�B
iB
iWB
i B
i�B
i�B
jAB
jwB
j�B
j�B
jyB
j�B
kHB
k�B
k�B
k�B
lB
l@B
lVB
llB
llB
lpB
l�B
l�B
m&B
mB
npB
o}B
p-B
ojB
o�B
o�B
o�B
p?B
p�B
q�B
q�B
q�B
rB
rB
rcB
s3B
sAB
sMB
soB
s�B
s�B
s�B
s�B
t>B
t8B
t�B
u-B
uB
uaB
u�B
u�B
vBB
v_B
v�B
v�B
v�B
v�B
vfB
v8B
v�B
v�B
v�B
w?B
w#B
v�B
wB
xB
x�B
x�B
xeB
x�B
yWB
y`B
yUB
x�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xVB
x�B
x�B
yUB
y?B
y�B
zB
zEB
z.B
|TB
|�B
|�B
|�B
|�B
}	B
|�B
}GB
}B
~4B
~�B
~B
�B
�B
eB
 B
~B
~�B
�`B
�B
jB
VB
�B
sB
B
~�B
~�B
~�B
=B
�B
�B
�#B
�FB
�;B
��B
��B
��B
�B
��B
��B
��B
�B
�@B
��B
��B
�*B
�^B
��B
��B
��B
��B
�FB
�1B
�B
�B
��B
�aB
�EB
��B
��B
��B
��B
��B
��B
��B
�cB
��B
��B
�'B
��B
��B
��B
��B
�B
�B
�B
�
B
�.B
��B
�B
��B
��B
��B
��B
�B
��B
��B
�B
��B
�yB
��B
�B
��B
�)B
��B
�nB
��B
�B
��B
�B
��B
��B
��B
�pB
�B
�XB
�LB
��B
�B
�B
�;B
��B
��B
��B
��B
��B
�B
��B
��B
�5B
��B
��B
��B
�B
��B
�zB
��G�O�B	m�B	h�B	k�B	o5B	k�B	k�B	m)B	l�B	jKB	m]B	lWB	k�B	j�B	k�B	i�B	jB	o B	m�B	l�B	h>B	m�B	l�B	jB	k�B	o B	j�B	m�B	r|B	e�B	lWB	m�B	l�B	hsB	i�B	n/B	iDB	h�B	j�B	k�B	jB	k�B	m�B	jB	kQB	lWB	k�B	kB	kB	m�B	lWB	k�B	m�B	l�B	kQB	m�B	lWB	j�B	k�B	l"B	j�B	k�B	l�B	j�B	kB	m]B	j�B	k�B	kQB	k�B	j�B	l�B	k�B	jB	j�B	lWB	j�B	i�B	k�B	k�B	k�B	l�B	m�B	k�B	l�B	m�B	l�B	lWB	m�B	m�B	lWB	m)B	m�B	m�B	l�B	l�B	n�B	m)B	l�B	ncB	ncB	m)B	m)B	m�B	n/B	l�B	n/B	n�B	m)B	m�B	o�B	m�B	ncB	p;B	o5B	n/B	o5B	qvB	qB	oiB	p�B	r�B	rB	q�B	tTB	r�B	uZB	v`B	u�B	u�B	v`B	v�B	w2B	v�B	y>B	z�B	{�B	{B	�B	��B	�SB	��B	��B	�B	�B	��B	��B	�xB	��B	�=B	�B	��B	�FB	��B	��B	�9B	�&B	�TB	�B	��B	�5B	��B	�B	�DB	�B	�;B	�lB	�rB
	7B
DB
�B
�B
/�B
0�B
1'B
33B
33B
4nB
5?B
6B
8�B
:�B
;�B
<6B
;�B
<B
C-B
@�B
J�B
K)B
HB
H�B
I�B
I�B
H�B
FB
H�B
F�B
;dB
:�B
=<B
?}B
=<B
=�B
>B
;�B
:�B
=<B
:�B
2�B
2-B
-wB
!�B
$B
VB
�B	�B
�B	��B	ԕB	՛B	�RB	�6B	��B	��B	��B	�+B	�VB	�hB	�B	~]B	zDB	�\B	��B	��B	��B	l"B	rGB	N<B	F�B	4nB	*�B	4B	?HB	�B	�B	"hB	hB	�B	(XB	1'B	.}B	;dB	_�B	c�B	k�B	u�B	zxB	�7B	�SB	��B	~�B	�	B	�.B	�7B	��B	�xB	�zB	�0B	��B	��B	�3B	��B	�2B	�vB	�/B	��B
@B
�B
(�B
(�B
)�B
3hB
-B
-�B
/�B
/�B
1'B
.IB
5?B
>wB
8�B
1'B
1�B
7�B
9�B
<�B
;0B
<jB
=<B
B�B
>wB
A�B
C-B
F�B
H�B
U2B
a�B
R�B
T,B
NB
NpB
N�B
N�B
T�B
WsB
OB
O�B
NpB
NpB
K�B
M6B
L�B
L�B
I�B
HB
C�B
GzB
C�B
G�B
N�B
T�B
^�B
_;B
b�B
g�B
sMB
|B
��B
��B
��B
�B
�B
�B
�B
�@B
��B
��B
��B
�:B
��B
��B
�@B
�B
��B
��B
�CB
�B
��B
��B
�!B
��B
�B
��B
�qB
�9B
��B
�^B
ӏB
�B
��B
�B
��B
�B
� B
�B
�vB
�B
�BB
��B
�]B
��B
��B
ݘB
�dB
�WB
�#B
�]B
یB
ޞB
�]B
�dB
�B
ܒB
�dB
��B
ݘB
��B
ޞB
�;B
�B
�2B
�DB
�B
��B
��B
��B  B  B�B�B_BYB�B
rBhB�B�B$B�B=�BB-wB)�B&�B#�B%�B?�BF�BS&BF�BP�BOvBYB]/BoiBsBtTBxlBz�B}VB�B��B�1B�rB�B��B�B��B��B�GB�PB�+B��B��B��B�B��B�@B��B��B�"B�+B��B��B��B�+B�@B�hB�B��B��B��B�B�_B��B�(B��B��B��B�YB��B�tB�IB��B��B�B��B��B��B��B�@B��B��B��B��B��B��B�wB�KB�OB��B��B�wB�RB�B��B��BÖB��B�-B��B��BB��B��B�HB�}B��B��B��B�B�wB��B�CB�=B�B��B�B��B��B��B�bB�uB��B�SB|�B��B|BwfBm�Bu%Bm�B]�B[�B^5Bo5B]�BQBQBS�BPBCaBB�BI�B0UB,�B*�BB!�B�BxB
	B+B+B
=B+B�B
��B;B
��B
��B
�B
�B
�vB
�5B
�B
�gB
ӏB
�B
��B
�?B
��B
��B
��B
�OB
�	B
��B
��B
�iB
w�B
l"B
k�B
uZB
?}B
R�B
V�B
>�B
5?B
�B
!�B
�B
	B
	lB	��B	��B	�8B	�B	�B	�ZB	�]B	�yB	�>B	�QB	��B	��B	�B	�B	��B	��B
N�B	��B	�3B	��B	�}B	ȀB	�B	��B	�UB	�IB	��B	�zB	��B	�:B	�-B	��B	�kB	��B	�.B	�hB	��B	��B	��B	�iB	|B	y�B	hsB	^�B	]dB	m)B	�AB	{B	W?B	S�B	W�B	a�B	OvB	J�B	R�B	F�B	?B	S[B	&LB	#nB	/�B	�B	#:B	�B	�B	!�B	qB	�B	 �B	 �B	B	�B	�B	�B	B	�B	�B	MB	FB	�B	�B	�B	�B	"B	�B	�B	�B	
=B		B	�B	_B	�B	�B	�B	MB	!-B	�B	
rB		lB	�B	�B	>B	#nB	%�B	(�B	+�B	0�B	9$B	=B	B'B	A�B	GzB	I�B	K�B	K�B	L�B	N�B	PHB	S[B	S[B	S�B	S�B	VB	XB	YB	\�B	\�B	s�B	{�B	|�B	~�B	cB	� B	� B	��B	�AB	��B	��B	��B	��B	�-B	�UB	�B	�^B	�[B	�aB	�<B	��B	�LB	��B	�zB	��B	�eB	��B	�qB	��B	��B	�YB	k�B	dZB	a�B	a|B	e,G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D�*<0~�<1ng<��D<�L<#�
<#�
<C��<I7�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<@9<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��5<��<�y�<{�#<��H<q��<BD�<kW�<#�
<h��<��3<�/<�a�<� �<{��<��e<�tX<�1�<���<��2<Yz�<��+<i�<c��<��}<ȕ<��<#�
<#�
<#�
<#�
<`��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<]�0<���<#�
<#�
<T�*<#�
<#�
<#�
<#�
<���<L/<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020072423571420200724235714IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020080400005020200804000050QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020080400005020200804000050QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020082411445520200824114455IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                