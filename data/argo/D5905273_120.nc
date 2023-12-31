CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-13T23:18:33Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210413231833  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               x   xAA  AOAO7314_008642_120                 7314_008642_120                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�l��A�@�l��A�11  @�l���"�@�l���"�@1��0�8@1��0�8�b�rGE8��b�rGE8�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�\)?��H@=p�@�  @�G�@�G�@�  A   A  A\)A+�A?\)A`  A\)A�\)A��A��A�\)AϮA�  A�  A��B  B  B  B   B(  B0(�B8(�B@(�BH  BO�
BX  B`  Bh(�Bp(�Bw�B�
B��B�(�B�  B��B��B��B�  B�{B�  B�  B�{B�  B�{B�  B��B�  B��B�  B�  B�  B��
B��
B��
B�  B�{B�  B�{B�  B��B�  B�  B��C�C  C
=C  C
  C  C  C
=C  C  C
=C  C  C  C��C   C"  C$  C&  C(
=C*  C+��C.  C0  C2
=C4{C6  C7��C9��C<  C>  C?��CB  CD
=CF
=CH  CI��CL  CN
=CP{CR
=CT  CV  CX
=CZ
=C\  C^  C`
=Ca��Cd  Cf
=Ch  Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz  C|  C~  C��C�  C�C�
=C���C���C�C�C�C�  C���C���C�  C�  C���C���C���C�  C�  C�  C�C�C�C�  C���C�C�C�C�  C�  C�C���C���C�C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�C�
=C�C�  C���C���C�  C�  C�C�C�C�  C�  C�C�C�C�C�  C���C�  C�C�C�  C���C�  C���C���C�  C���C���C���C���C���C���C�C�C�  C���C���C���C���C�  C�  C���C���C�  C���C���C�  C�C�C�
=C�C���C���C�  C���C���C�  C�C�C�C�C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD  D� D  D� D�qDz�D�qD��D  D}qD�qD� D	�D	� D
  D
��D�D��D  D}qD  D�D  D��DD��D  D}qD��Dz�D��D� DD��D�D� D�D��D  D� DD�D  D}qD�qD� D  D}qD��D� D�D� D�qD��D  D}qD  D� D   D }qD �qD!��D"�D"}qD#  D#��D$�D$�D%  D%� D&  D&��D'D'�D(�D(}qD)  D)� D)�qD*z�D*�RD+}qD,�D,� D-�D-�D.  D.� D.�qD/��D0�D0�D1�D1� D1�qD2� D3�D3� D3�qD4� D5D5��D6  D6z�D7  D7��D8�D8��D9  D9}qD9�qD:z�D:�qD;��D<  D<}qD=  D=��D>  D>��D?�D?��D@  D@}qDA  DA� DB  DB��DC�DC� DD  DD}qDE  DE� DE��DF� DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM  DM� DN�DN��DO  DO}qDP  DP� DQ  DQ��DR�DR�DS�DS��DT  DT}qDU  DU� DV  DV� DW  DW� DX�DX��DY  DY}qDZ  DZ� D[  D[}qD\  D\� D]�D]��D^�D^��D_  D_��D`�D`� D`�qDaz�Da�qDb��Dc�Dc� Dd  Dd��De  De� Df�Df}qDg  Dg��Dh  Dh� Di  Di� Dj  Dj��Dk  Dk� Dk�qDl}qDm  Dm��Dn  Dn}qDn�qDo��DpDp� Dp��Dq� DrDr� Dr��Dsz�Ds�qDt� Du  Du}qDv  Dv� Dv�qDw}qDw�qDx� Dx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD}  D}}qD}��D~}qD  D��D�  D�>�D�� D�� D���D�>�D�~�D��HD�HD�>�D�~�D���D���D�AHD��HD���D���D�@ D��HD��HD�  D�@ D��HD�� D�HD�AHD�� D���D�  D�AHD�~�D���D���D�>�D�� D�� D�HD�>�D�� D�� D��qD�=qD�~�D�� D�  D�@ D��HD���D�  D�AHD�~�D�� D�HD�AHD�� D���D��qD�>�D��HD��HD�  D�@ D�~�D��qD��qD�>�D�� D�� D�  D�@ D�~�D���D��qD�>�D�� D�� D���D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD�� D���D��qD�@ D��HD���D��qD�@ D��HD�� D��qD�>�D��HD�D�  D�>�D�}qD���D�  D�>�D�� D��HD�  D�=qD�~�D�� D���D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D���D���D�>�D�~�D�� D���D�>�D�� D�� D���D�>�D�� D�� D�HD�@ D�~�D�� D���D�=qD�~�D�� D�  D�@ D��HD��HD���D�>�D�� D�� D�HD�>�D��HD�D�  D�AHD���D�� D���D�AHD���D��HD�  D�>�D�}qD�� D�HD�@ D�� D���D���D�AHD���D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D���D�  D�AHD�� D���D�  D�@ D��HD�D��D�@ D�}qD��qD���D�>�D�~�D���D�  D�AHD���D�� D��qD�=qD�~�D�� D�  D�AHD��HD�� D���D�@ D��HD�D�  D�=qD�~�D�� D�HD�AHD���D��HD�  D�AHD��HD�� D���D�@ D��HD��HD�  D�>�D�� D�� D�HD�AHD��HD��HD���D�>�D�� D��HD�HD�@ D�� D��HD��D�AHDHD��HD���D�>�D�~�D�� D�  D�@ DāHD��HD�HD�@ D�~�D�� D�  D�@ D�~�Dƾ�D�  D�AHDǁHD��HD���D�=qD�~�D�� D���D�>�Dɀ D�� D�  D�@ DʁHD��HD�HD�AHDˁHD˾�D�  D�@ D�}qD̾�D���D�@ D̀ D��HD�HD�@ D�~�Dξ�D�  D�@ D�~�D�� D���D�>�DЀ D�� D�  D�>�DсHD�D�  D�=qDҀ D�� D���D�>�DӀ D�� D�  D�>�D�~�D�� D�HD�B�DՁHD�� D���D�>�Dր D��HD�HD�>�D�~�D׾�D���D�@ D؀ D��HD�HD�AHDـ D��HD�HD�AHDځHD��HD�HD�AHDہHD�� D���D�@ D܁HD�� D���D�>�D�}qD�� D�HD�@ Dހ D޾�D�  D�@ D߀ D�� D��qD�>�D�~�DྸD�  D�AHD�HD�� D���D�@ D�HD�� D���D�=qD� D��HD�  D�>�D�}qD侸D�HD�@ D�~�D徸D���D�@ D� D�qD�  D�AHD�HD��HD�  D�>�D�~�D�� D�HD�AHD� D龸D�  D�AHD�HD�� D�HD�B�D낏D��HD���D�=qD� D쾸D���D�>�D�~�D�� D�  D�@ D� D�� D�HD�AHD� D�� D���D�AHD��HD�� D�  D�AHD�HD�D�  D�@ D� D�� D�HD�AHD� D�� D�HD�@ D�}qD���D�HD�B�D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD��HD��HD���D�>�D�~�D���D�  D�L�?L��?��?���?\?�G�@   @\)@!G�@.{@@  @Q�@aG�@s33@��\@��@�33@�(�@��@�{@�Q�@�G�@�=q@�z�@�p�@�ff@��@��HA�\A
=A(�AG�AA�HA\)A$z�A(��A-p�A1�A7
=A;�A@��AEAJ�HAO\)ATz�AY��A^�RAc�
Ah��Amp�As33Aw�A|��A���A��A�p�A�  A�=qA�z�A�
=A��A�z�A�
=A�G�A��
A�{A���A�33A�A�Q�A��\A��A��A�=qA�z�A�
=A�G�A��A�{A���A�33A�p�A�  A��HA�p�AϮAҏ\A���A�\)A��A�(�A޸RA�G�A�A�{A�Q�A��HA�p�A�A�=qA��A�\)A��A�z�A�
=B ��B{B33B��B�B33BQ�B	��B
�RB�
B��B=qB�B��B=qB�B��B{B\)B��B�B33BQ�B��B�RB   B!G�B"ffB#�B$��B&{B'\)B(��B)�B+\)B,z�B-B.�HB0  B1G�B2ffB3�B4��B6=qB7�B8��B:{B;\)B<z�B=B>�HB@(�BAG�BB�\BC�
BE�BF�\BG�BI�BJ=qBK�BL��BN{BO
=BPQ�BQ��BR�HBT(�BUp�BV�RBX  BYG�BZ�\B[�B\��B^{B_�B`��BaBc
=BdQ�Be��Bf�HBh  Bip�Bj�\Bl  Bm�Bn�\Bo�
Bq�BrffBs�Bt��Bv{Bw\)Bx��By�B{33B|��B}�B33B�=qB���B�p�B�{B��RB�\)B�  B��\B�33B��
B�z�B�33B��
B�z�B�
=B��B�=qB��HB��B�(�B���B�p�B�{B���B�\)B��B��\B�33B��
B�z�B�
=B��B�=qB���B�p�B�  B���B�G�B��B��\B�33B��
B�Q�B���B��B�{B���B�G�B��B��\B�33B��
B�Q�B���B��B�(�B��RB�\)B��B���B�G�B��B��\B��B��B�=qB���B���B�=qB��HB��B�=qB��HB��B�=qB��HB��B�{B���B��B�=qB���B���B�=qB���B���B�=qB���B���B�Q�B�
=B��B�ffB��B�B�ffB�
=B��B�ffB�33B��
B���B�G�B��B\B�G�B��Bģ�B�\)B�{B���BǅB�(�B���Bə�B�Q�B���B˙�B�ffB��B��
BΣ�B�G�B��BУ�B�G�B�  BҸRBӅB�=qB�
=BծB�ffB�
=B��
B؏\B�\)B�{BڸRB�p�B�(�B�
=B��
Bޏ\B�G�B�  B�RB�p�B�(�B���B�B�z�B��B��
B�z�B�G�B�  B��B�\)B�{B��B�33B�B�=qB�RB�G�B��B�{B�ffB�RB�
=B�\)B�B�  B�=qB��\B���B�
=B�\)B�B�{B�ffB��B��HB�33B�B��B�=qB�\B���B��B�\)B�B�{B�z�B���B���B�G�B��B��B�=qB���B���B�33B��B�B�(�B��\B��RB�
=B�G�B���B��B�Q�B��RB�
=B�G�B��B��B�Q�B���B���B�33B�p�B��
C �C =qC ffC �C �RC �HC{C33CQ�C�C�RC�C{C=qCffC�\C�RC�C�CQ�Cp�C��CC��C�CQ�Cz�C��CC��C(�C\)C�C��C��C��C(�C\)C�C�C�
C��C(�C\)C�\C�C�
C��C(�C\)C�\C�RC�
C	  C	(�C	\)C	�C	�RC	�
C
  C
(�C
ffC
�\C
C
�HC  C33C\)C��CC�HC
=C=qCffC��C��C  C�CG�Cz�C�C�HC  C(�CQ�C�\C�RC�HC  C33CffC��C�RC�HC
=C=qCp�C��CC�HC{CG�Cp�C��C�RC�C{CG�CffC��C�RC�C{CG�CffC�C�RC�HC
=C33CQ�Cz�C�C�HC  C�CG�Cz�C�C��C��C{CG�Cz�C��CC�HC{C=qCffC�C�RC�HC�C33C\)C�C�RC�HC
=C33CG�Cz�C�C�
C��C�CQ�C�C��C��C  C(�CQ�Cp�C��C�
C  C(�CG�Cp�C��C�HC  C�CQ�C�C�RC�
C  C33CffC�\C�RC�C�CG�Cp�C�\C��C   C (�C =qC ffC ��C �
C ��C!�C!Q�C!�C!�RC!��C"  C"33C"\)C"�C"�C"�HC#
=C#=qC#\)C#�\C#C#�C${C$=qC$p�C$�C$�
C%  C%(�C%\)C%�\C%�RC%�HC&{C&\)C&z�C&��C&�HC'{C'=qC'ffC'�\C'��C(  C(�C(\)C(��C(�RC(�C)�C)\)C)�C)�C)�HC*�C*Q�C*z�C*��C*�C+�C+G�C+z�C+�RC+��C,�C,Q�C,�\C,��C,�C-33C-p�C-��C-��C.
=C.G�C.p�C.�C.�C/(�C/Q�C/�\C/��C0  C0(�C0ffC0��C0�HC1{C1Q�C1�\C1��C1��C233C2p�C2�RC2�C3�C3\)C3��C3��C4
=C4Q�C4�\C4C4��C5=qC5z�C5�C5��C633C6p�C6��C6�C733C7\)C7�\C7��C8{C8Q�C8�C8��C9
=C9=qC9z�C9C9��C:(�C:ffC:�RC:�C;�C;ffC;�C;�HC<{C<\)C<��C<��C={C=\)C=�\C=��C>{C>Q�C>�C>��C?{C?G�C?�C?��C@
=C@G�C@�C@�
CA  CA=qCA�\CACA��CB=qCB�CB�RCC  CC=qCCp�CC�CD  CD33CDffCD�RCE  CE(�CEffCE�RCE��CF33CFz�CF�RCF�CG=qCG�CG�RCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          ?�\)?��H@=p�@�  @�G�@�G�@�  A   A  A\)A+�A?\)A`  A\)A�\)A��A��A�\)AϮA�  A�  A��B  B  B  B   B(  B0(�B8(�B@(�BH  BO�
BX  B`  Bh(�Bp(�Bw�B�
B��B�(�B�  B��B��B��B�  B�{B�  B�  B�{B�  B�{B�  B��B�  B��B�  B�  B�  B��
B��
B��
B�  B�{B�  B�{B�  B��B�  B�  B��C�C  C
=C  C
  C  C  C
=C  C  C
=C  C  C  C��C   C"  C$  C&  C(
=C*  C+��C.  C0  C2
=C4{C6  C7��C9��C<  C>  C?��CB  CD
=CF
=CH  CI��CL  CN
=CP{CR
=CT  CV  CX
=CZ
=C\  C^  C`
=Ca��Cd  Cf
=Ch  Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz  C|  C~  C��C�  C�C�
=C���C���C�C�C�C�  C���C���C�  C�  C���C���C���C�  C�  C�  C�C�C�C�  C���C�C�C�C�  C�  C�C���C���C�C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�C�
=C�C�  C���C���C�  C�  C�C�C�C�  C�  C�C�C�C�C�  C���C�  C�C�C�  C���C�  C���C���C�  C���C���C���C���C���C���C�C�C�  C���C���C���C���C�  C�  C���C���C�  C���C���C�  C�C�C�
=C�C���C���C�  C���C���C�  C�C�C�C�C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C���C���C���C���C���C���C���C���C���D }qD �qD}qD�qD}qD  D� D  D� D�qDz�D�qD��D  D}qD�qD� D	�D	� D
  D
��D�D��D  D}qD  D�D  D��DD��D  D}qD��Dz�D��D� DD��D�D� D�D��D  D� DD�D  D}qD�qD� D  D}qD��D� D�D� D�qD��D  D}qD  D� D   D }qD �qD!��D"�D"}qD#  D#��D$�D$�D%  D%� D&  D&��D'D'�D(�D(}qD)  D)� D)�qD*z�D*�RD+}qD,�D,� D-�D-�D.  D.� D.�qD/��D0�D0�D1�D1� D1�qD2� D3�D3� D3�qD4� D5D5��D6  D6z�D7  D7��D8�D8��D9  D9}qD9�qD:z�D:�qD;��D<  D<}qD=  D=��D>  D>��D?�D?��D@  D@}qDA  DA� DB  DB��DC�DC� DD  DD}qDE  DE� DE��DF� DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM  DM� DN�DN��DO  DO}qDP  DP� DQ  DQ��DR�DR�DS�DS��DT  DT}qDU  DU� DV  DV� DW  DW� DX�DX��DY  DY}qDZ  DZ� D[  D[}qD\  D\� D]�D]��D^�D^��D_  D_��D`�D`� D`�qDaz�Da�qDb��Dc�Dc� Dd  Dd��De  De� Df�Df}qDg  Dg��Dh  Dh� Di  Di� Dj  Dj��Dk  Dk� Dk�qDl}qDm  Dm��Dn  Dn}qDn�qDo��DpDp� Dp��Dq� DrDr� Dr��Dsz�Ds�qDt� Du  Du}qDv  Dv� Dv�qDw}qDw�qDx� Dx�qDy}qDy�qDz}qDz�qD{}qD{�qD|}qD}  D}}qD}��D~}qD  D��D�  D�>�D�� D�� D���D�>�D�~�D��HD�HD�>�D�~�D���D���D�AHD��HD���D���D�@ D��HD��HD�  D�@ D��HD�� D�HD�AHD�� D���D�  D�AHD�~�D���D���D�>�D�� D�� D�HD�>�D�� D�� D��qD�=qD�~�D�� D�  D�@ D��HD���D�  D�AHD�~�D�� D�HD�AHD�� D���D��qD�>�D��HD��HD�  D�@ D�~�D��qD��qD�>�D�� D�� D�  D�@ D�~�D���D��qD�>�D�� D�� D���D�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD�� D���D��qD�@ D��HD���D��qD�@ D��HD�� D��qD�>�D��HD�D�  D�>�D�}qD���D�  D�>�D�� D��HD�  D�=qD�~�D�� D���D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D���D���D�>�D�~�D�� D���D�>�D�� D�� D���D�>�D�� D�� D�HD�@ D�~�D�� D���D�=qD�~�D�� D�  D�@ D��HD��HD���D�>�D�� D�� D�HD�>�D��HD�D�  D�AHD���D�� D���D�AHD���D��HD�  D�>�D�}qD�� D�HD�@ D�� D���D���D�AHD���D��HD�HD�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�>�D�~�D���D�  D�AHD�� D���D�  D�@ D��HD�D��D�@ D�}qD��qD���D�>�D�~�D���D�  D�AHD���D�� D��qD�=qD�~�D�� D�  D�AHD��HD�� D���D�@ D��HD�D�  D�=qD�~�D�� D�HD�AHD���D��HD�  D�AHD��HD�� D���D�@ D��HD��HD�  D�>�D�� D�� D�HD�AHD��HD��HD���D�>�D�� D��HD�HD�@ D�� D��HD��D�AHDHD��HD���D�>�D�~�D�� D�  D�@ DāHD��HD�HD�@ D�~�D�� D�  D�@ D�~�Dƾ�D�  D�AHDǁHD��HD���D�=qD�~�D�� D���D�>�Dɀ D�� D�  D�@ DʁHD��HD�HD�AHDˁHD˾�D�  D�@ D�}qD̾�D���D�@ D̀ D��HD�HD�@ D�~�Dξ�D�  D�@ D�~�D�� D���D�>�DЀ D�� D�  D�>�DсHD�D�  D�=qDҀ D�� D���D�>�DӀ D�� D�  D�>�D�~�D�� D�HD�B�DՁHD�� D���D�>�Dր D��HD�HD�>�D�~�D׾�D���D�@ D؀ D��HD�HD�AHDـ D��HD�HD�AHDځHD��HD�HD�AHDہHD�� D���D�@ D܁HD�� D���D�>�D�}qD�� D�HD�@ Dހ D޾�D�  D�@ D߀ D�� D��qD�>�D�~�DྸD�  D�AHD�HD�� D���D�@ D�HD�� D���D�=qD� D��HD�  D�>�D�}qD侸D�HD�@ D�~�D徸D���D�@ D� D�qD�  D�AHD�HD��HD�  D�>�D�~�D�� D�HD�AHD� D龸D�  D�AHD�HD�� D�HD�B�D낏D��HD���D�=qD� D쾸D���D�>�D�~�D�� D�  D�@ D� D�� D�HD�AHD� D�� D���D�AHD��HD�� D�  D�AHD�HD�D�  D�@ D� D�� D�HD�AHD� D�� D�HD�@ D�}qD���D�HD�B�D�� D��HD�HD�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD��HD��HD���D�>�D�~�D���D�  G�O�?L��?��?���?\?�G�@   @\)@!G�@.{@@  @Q�@aG�@s33@��\@��@�33@�(�@��@�{@�Q�@�G�@�=q@�z�@�p�@�ff@��@��HA�\A
=A(�AG�AA�HA\)A$z�A(��A-p�A1�A7
=A;�A@��AEAJ�HAO\)ATz�AY��A^�RAc�
Ah��Amp�As33Aw�A|��A���A��A�p�A�  A�=qA�z�A�
=A��A�z�A�
=A�G�A��
A�{A���A�33A�A�Q�A��\A��A��A�=qA�z�A�
=A�G�A��A�{A���A�33A�p�A�  A��HA�p�AϮAҏ\A���A�\)A��A�(�A޸RA�G�A�A�{A�Q�A��HA�p�A�A�=qA��A�\)A��A�z�A�
=B ��B{B33B��B�B33BQ�B	��B
�RB�
B��B=qB�B��B=qB�B��B{B\)B��B�B33BQ�B��B�RB   B!G�B"ffB#�B$��B&{B'\)B(��B)�B+\)B,z�B-B.�HB0  B1G�B2ffB3�B4��B6=qB7�B8��B:{B;\)B<z�B=B>�HB@(�BAG�BB�\BC�
BE�BF�\BG�BI�BJ=qBK�BL��BN{BO
=BPQ�BQ��BR�HBT(�BUp�BV�RBX  BYG�BZ�\B[�B\��B^{B_�B`��BaBc
=BdQ�Be��Bf�HBh  Bip�Bj�\Bl  Bm�Bn�\Bo�
Bq�BrffBs�Bt��Bv{Bw\)Bx��By�B{33B|��B}�B33B�=qB���B�p�B�{B��RB�\)B�  B��\B�33B��
B�z�B�33B��
B�z�B�
=B��B�=qB��HB��B�(�B���B�p�B�{B���B�\)B��B��\B�33B��
B�z�B�
=B��B�=qB���B�p�B�  B���B�G�B��B��\B�33B��
B�Q�B���B��B�{B���B�G�B��B��\B�33B��
B�Q�B���B��B�(�B��RB�\)B��B���B�G�B��B��\B��B��B�=qB���B���B�=qB��HB��B�=qB��HB��B�=qB��HB��B�{B���B��B�=qB���B���B�=qB���B���B�=qB���B���B�Q�B�
=B��B�ffB��B�B�ffB�
=B��B�ffB�33B��
B���B�G�B��B\B�G�B��Bģ�B�\)B�{B���BǅB�(�B���Bə�B�Q�B���B˙�B�ffB��B��
BΣ�B�G�B��BУ�B�G�B�  BҸRBӅB�=qB�
=BծB�ffB�
=B��
B؏\B�\)B�{BڸRB�p�B�(�B�
=B��
Bޏ\B�G�B�  B�RB�p�B�(�B���B�B�z�B��B��
B�z�B�G�B�  B��B�\)B�{B��B�33B�B�=qB�RB�G�B��B�{B�ffB�RB�
=B�\)B�B�  B�=qB��\B���B�
=B�\)B�B�{B�ffB��B��HB�33B�B��B�=qB�\B���B��B�\)B�B�{B�z�B���B���B�G�B��B��B�=qB���B���B�33B��B�B�(�B��\B��RB�
=B�G�B���B��B�Q�B��RB�
=B�G�B��B��B�Q�B���B���B�33B�p�B��
C �C =qC ffC �C �RC �HC{C33CQ�C�C�RC�C{C=qCffC�\C�RC�C�CQ�Cp�C��CC��C�CQ�Cz�C��CC��C(�C\)C�C��C��C��C(�C\)C�C�C�
C��C(�C\)C�\C�C�
C��C(�C\)C�\C�RC�
C	  C	(�C	\)C	�C	�RC	�
C
  C
(�C
ffC
�\C
C
�HC  C33C\)C��CC�HC
=C=qCffC��C��C  C�CG�Cz�C�C�HC  C(�CQ�C�\C�RC�HC  C33CffC��C�RC�HC
=C=qCp�C��CC�HC{CG�Cp�C��C�RC�C{CG�CffC��C�RC�C{CG�CffC�C�RC�HC
=C33CQ�Cz�C�C�HC  C�CG�Cz�C�C��C��C{CG�Cz�C��CC�HC{C=qCffC�C�RC�HC�C33C\)C�C�RC�HC
=C33CG�Cz�C�C�
C��C�CQ�C�C��C��C  C(�CQ�Cp�C��C�
C  C(�CG�Cp�C��C�HC  C�CQ�C�C�RC�
C  C33CffC�\C�RC�C�CG�Cp�C�\C��C   C (�C =qC ffC ��C �
C ��C!�C!Q�C!�C!�RC!��C"  C"33C"\)C"�C"�C"�HC#
=C#=qC#\)C#�\C#C#�C${C$=qC$p�C$�C$�
C%  C%(�C%\)C%�\C%�RC%�HC&{C&\)C&z�C&��C&�HC'{C'=qC'ffC'�\C'��C(  C(�C(\)C(��C(�RC(�C)�C)\)C)�C)�C)�HC*�C*Q�C*z�C*��C*�C+�C+G�C+z�C+�RC+��C,�C,Q�C,�\C,��C,�C-33C-p�C-��C-��C.
=C.G�C.p�C.�C.�C/(�C/Q�C/�\C/��C0  C0(�C0ffC0��C0�HC1{C1Q�C1�\C1��C1��C233C2p�C2�RC2�C3�C3\)C3��C3��C4
=C4Q�C4�\C4C4��C5=qC5z�C5�C5��C633C6p�C6��C6�C733C7\)C7�\C7��C8{C8Q�C8�C8��C9
=C9=qC9z�C9C9��C:(�C:ffC:�RC:�C;�C;ffC;�C;�HC<{C<\)C<��C<��C={C=\)C=�\C=��C>{C>Q�C>�C>��C?{C?G�C?�C?��C@
=C@G�C@�C@�
CA  CA=qCA�\CACA��CB=qCB�CB�RCC  CC=qCCp�CC�CD  CD33CDffCD�RCE  CE(�CEffCE�RCE��CF33CFz�CF�RCF�CG=qCG�CG�RCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A²-A§�APA�hsA��A���A�hsA�XA�=qA� �A�oA�VA�A���A���A���A��A��A��A��A��A��A��mA��yA��mA��mA��yA��A��A��A��A��A��A��A��A��A��yA��TA��`A��HA��;A��#A���A���A��jA�dZA��A�ƨA�-A���A��A��A�oA�K�A���A�9XA��+A��
A�A�I�A�bNA�  A�A���A�I�A���A���A��#A���A���A���A�t�A���A��A�1'A���A�+A���A�t�A�K�A���A���A��#A�G�A�G�A���A���A�bA�p�A���A��A�  A�9XA�K�A��;A��RA��#A��A���A��-A�XA�JA�{A��A|ZAy�;AuXAtJAq��An�`Am�FAl��AkS�Ah=qAfr�Ae�Ad=qAbA�A`��A]
=AY��AY\)AW�TAU|�AQ7LAM7LAKS�AJn�AG�AD�RAC�;AB=qA@=qA;�A;t�A;��A;hsA8�9A4��A3�mA1/A0�\A0��A0��A-��A,n�A,jA,n�A,  A,(�A-G�A-�A.�!A.�jA.��A/+A/;dA/A-��A,��A,1A-A-7LA,  A+��A+l�A+%A+S�A-A.9XA.  A-"�A+�A'+A&�9A&M�A%dZA'dZA&bNA%7LA#ƨA$jA#�TA"�A!�TA!�wA"��A"n�A"�A!`BA ĜA�^AdZA(�A��A��A�wAv�A�FA �A��AA�A��A�A�DA=qA�mA+A��A�wAv�A��A&�AjA
=A
�/A
�+A
�DA	�A	33A	%A  AXA�AjA�AC�A Q�@���@�G�@��9@�l�@���@�@�@���@�%@���@���@�E�@�{@�p�@���@�@�v�@�n�@��+@�X@�\)@�"�@�$�@��@��D@�I�@�;d@��/@�Z@���@�7@���@�%@�Z@�33@�!@�t�@�@�!@��@�A�@�|�@�@�J@�7L@��@�33@�~�@��@��D@�33@�5?@�@��@��@���@��;@ۅ@�l�@ڸR@��@��@׾w@֧�@��@���@�9X@���@�l�@�v�@Ѳ-@љ�@��/@�1'@ϝ�@��@�`B@̛�@�bN@�(�@�S�@�+@��y@��@�E�@ɲ-@�V@�A�@��;@�l�@���@Ɵ�@�ff@��@Ų-@őh@�`B@���@�bN@Õ�@��@§�@�J@��#@���@���@��7@�X@���@�1'@��F@��P@�33@�
=@��@���@�E�@�J@�`B@���@��D@�(�@��m@�ƨ@�l�@��\@��-@��@���@��@�r�@�%@���@��-@��7@�/@���@��j@�\)@��@��@��R@�v�@��-@��@�bN@��w@���@��7@���@�A�@���@��y@�=q@���@���@��@�O�@��@�V@��@�%@��7@�`B@�?}@�?}@�`B@��@��@�j@�9X@�(�@���@���@��@�dZ@�+@���@���@�ff@�E�@�hs@�%@���@��@�z�@�  @��@�+@��H@��\@�^5@���@�@���@�A�@�b@��;@��w@���@�t�@�K�@�+@��@�ȴ@���@��!@�ff@�{@��T@���@�X@���@��D@�b@�ƨ@�t�@�C�@�o@���@���@�$�@��#@��7@��@��@�9X@�  @��;@�dZ@�ȴ@��+@�ff@�M�@��@���@�7L@���@�Z@��m@��F@���@�S�@���@��R@�~�@�M�@�J@��-@���@�`B@�/@��`@��j@�r�@�b@��F@�\)@�+@�o@�
=@�
=@�
=@�@��y@��R@�V@��@���@�`B@�?}@�&�@���@���@�(�@���@��F@���@�\)@�o@��y@���@�ff@�V@��@���@��h@�hs@�O�@�7L@��@���@�Z@�1'@�  @��@�33@�ȴ@���@�~�@�M�@�$�@��@��@�7L@���@��j@��@��u@�I�@� �@��
@�|�@�K�@�o@���@��y@���@�ff@�E�@��@�@���@���@�`B@��@��@�Ĝ@��@��D@�Z@�1'@���@���@���@�C�@��@���@��\@�~�@�^5@�5?@��@��h@�O�@��@�%@���@��`@���@�r�@�A�@�@�P@+@~��@~�@~ff@~E�@}�@}��@}�-@|��@|z�@|I�@|1@{ƨ@{ƨ@{ƨ@{��@{@z�H@z��@zJ@y��@x�9@x �@w��@w�P@v�R@v5?@u��@u@u�h@t��@t�D@t9X@sƨ@so@r^5@r�@q��@q��@q�7@p��@pr�@o��@o�@o|�@n�y@nv�@n5?@m��@m?}@l��@l��@ko@j�@j�!@j^5@j�@i�@i��@i��@i�7@i7L@h�`@hĜ@h��@h�@hbN@hb@gl�@f�@fff@f@e��@e@e��@ep�@dz�@c�m@cƨ@b��@a��@a�#@a�^@ahs@a7L@`Ĝ@`A�@_��@_��@_|�@_K�@_;d@^�@]�@]�@\�@\j@\1@[��@[C�@Z�@Z-@Y�#@Y7L@X�@XQ�@XQ�@X �@W�@W+@W
=@V�y@Vȴ@V�+@V$�@U��@U�-@U�@U/@U�@T��@T�/@T�@Tz�@TI�@S�m@S�@SC�@S"�@S@R�@R�!@R�@Q�7@Q�@P��@P�9@P�u@P�u@PQ�@O��@O��@O��@O|�@N�y@N$�@M��@M��@M`B@MV@MV@L��@L�@LZ@L9X@L(�@K��@K�@K"�@K@J�@J�H@J��@Jn�@J-@I�@I��@Ix�@I&�@H�`@H��@H�u@Hr�@H �@Gl�@F�@Fv�@FV@F$�@F@E��@D��@D�D@D(�@C��@C�
@C�@CS�@C33@B�@B�\@B-@Ahs@@r�@@b@@ �@?�;@?�P@?;d@>��@>{@=�h@=O�@=V@<Z@;�m@;t�@:�@:��@:��@:~�@:~�@:n�@:M�@9��@9hs@9%@8��@8�9@8bN@7��@7K�@7
=@6��@65?@5��@5�h@5p�@5O�@5�@4��@4I�@3��@2�@2��@2n�@2=q@1�#@1x�@1%@0�9@0r�@0A�@0 �@/��@/�@/\)@/+@.��@.ȴ@.v�@.E�@.$�@.$�@.@-@-��@-p�@-?}@-/@-�@,��@,��@,�@,��@,�@,��@,�D@,j@,j@,I�@,9X@,(�@,(�@+�@+C�@+"�@+o@*�H@*�!@*n�@*-@)��@)��@)��@)��@)hs@)G�@(��@(bN@(1'@'�@'l�@'+@&��@&ȴ@&��@&v�@%�@%�-@%�-@%��@%p�@$��@$��@$�D@$�D@$z�@$Z@#�F@#dZ@#C�@"�H@"��@"n�@"^5@"M�@"-@"�@"-@"J@!�#@!�^@!hs@ �`@ bN@ 1'@   @�;@�w@�P@;d@�@$�@�@V@V@V@�@z�@Z@�
@��@��@��@�@dZ@S�@33@o@@�H@n�@-@J@��@�^@x�@G�@%@��@ �@�w@�P@;d@�@�@��@ȴ@�R@�R@��@ff@V@5?@$�@@�T@��@@��@��@p�@O�@V@�@�@Z@I�@9X@9X@��@�F@�F@��@dZ@33@@�H@�H@��@��@�\@=q@��@��@x�@hs@hs@�@�9@�9@Q�@A�@A�@ �@��@l�@;d@
=@�y@ȴ@��@��@�+A²-A¸RA°!A²-A©�A¥�A¥�A\AhA�AA�n�A�r�A�^5A�hsA�%A��A��RA��!A��!A���A��7A�l�A�jA�ffA�\)A�ZA�XA�S�A�I�A�7LA�1'A�(�A�"�A�{A�oA�oA�oA�{A�{A�VA�1A�1A�%A�A�A�A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��mA��yA��yA��yA��A��yA��mA��mA��mA��mA��`A��`A��mA��`A��mA��mA��mA��mA��mA��yA��yA��yA��yA��yA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��mA��`A��HA��HA��TA��TA��TA��`A��TA��`A��`A��mA��`A��`A��`A��TA��HA��;A��;A��/A��;A��;A��;A��;A��HA��;A��;A��;A��;A��/A��#A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ĜA�A���A��jA��jA��RA���A��+A�z�A�l�A�ffA�M�A�7LA�7LA��A��;A��-A�dZA�I�A�-A�A��A���A���A���A���A�v�A�\)A�+A��A�bA�JA�A��A��;A���A��^A��\A�ffA�1'A�bA�VA�1A�  A���A���A��A��A��A��mA��A��A�%A��/A���A���A��A�\)A�G�A�5?A�+A�(�A�(�A��A���A��`A�A��!A��DA�ZA�$�A�A��#A��A���A��\A��A�v�A�ZA�I�A�5?A��A��\A�=qA�+A�+A�$�A�A���A���A�ffA�`BA�\)A�Q�A�JA�ƨA��uA�|�A�M�A�-A���A�v�A�JA���A��TA��PA�M�A�7LA��A���A��#A��uA�33A��HA��-A��\A�jA�JA���A�n�A�O�A�9XA�(�A���A���A�A���A�~�A�A�A��A��A��/A���A�~�A�M�A�5?A��A��A��^A�p�A�33A��A���A���A�l�A�O�A�E�A�E�A�G�A�A�A�1'A�(�A��A�$�A��A��A�A��A��TA���A��-A���A�v�A�O�A�/A��A�{A�VA�JA���A���A���A��A��mA��/A��A���A���A���A��wA��FA��-A���A���A��DA��+A�l�A�XA�Q�A�G�A�I�A�A�A�7LA�(�A��A�1A��HA��hA�(�A��A��FA���A���A���A��uA��+A�|�A�l�A�`BA�VA�E�A�=qA�7LA�/A�$�A��A�JA�A��A���A���A���A�ffA�VA�E�A�33A�"�A�{A�A���A��A��TA��#A���A��-A��7A�z�A�bNA�G�A�;dA�/A�-A�33A�-A��A��A�bA�A���A���A��A��A��yA��yA��A��!A�t�A�33A�JA���A��
A�ĜA��7A�7LA�
=A���A���A��yA��;A���A�ȴA�ĜA��jA��A���A���A���A���A��hA��DA��A��A�v�A�bNA�E�A�A���A���A��+A�I�A�{A���A��yA��
A�ƨA��-A���A���A�~�A�jA�Q�A�?}A�7LA�+A��A�  A��yA���A��!A���A�XA�7LA�(�A���A��9A��A�l�A�1'A���A�ĜA��uA�M�A���A��#A�A��\A�hsA�VA�M�A�G�A�?}A�5?A�&�A��A�oA�JA�A���A��A��A��A��^A���A�jA�"�A��`A��RA���A��uA��A�jA�9XA���A���A���A���A���A���A���A���A��hA�ffA�G�A�-A�A���A�ȴA��A��A�"�A���A��DA�x�A�hsA�bNA�^5A�XA�Q�A�K�A�M�A�O�A�M�A�9XA�/A�$�A�A��`A���A��^A��!A���A���A���A��hA��7A�v�A�ZA�-A��A���A�jA�O�A�-A��A�jA���A�p�A�G�A��A�A���A���A��A���A���A���A���A���A���A��uA��DA�~�A��7A�t�A�r�A�n�A�l�A�K�A��A��9A���A�|�A�33A�  A���A���A��A�S�A��A��TA���A�bNA�+A���A��HA��!A��+A�VA��A��A���A��jA���A���A��PA�v�A�bNA�M�A�=qA�$�A�
=A���A��A��jA���A�`BA�$�A��-A�G�A���A���A�z�A�$�A�%A��yA��A��A�bNA�C�A�/A��A�1A�A��A��#A��-A���A�p�A�K�A�/A��A�
=A��;A��FA���A��A�S�A�I�A�33A� �A�JA��A���A���A�x�A�A�A�A��TA��wA���A�~�A�VA�-A�JA���A��uA�n�A�A�A�"�A�oA���A���A�r�A�"�A�dZA���A�5?A���A���A���A��A�O�A�/A�
=A��yA���A��RA���A�r�A�^5A�A�A�-A�JA��A~�yA}�A}�A}dZA|��A|��A|�A{�A{%Az��Az��Az�Az-Ay�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          A²-A§�APA�hsA��A���A�hsA�XA�=qA� �A�oA�VA�A���A���A���A��A��A��A��A��A��A��mA��yA��mA��mA��yA��A��A��A��A��A��A��A��A��A��yA��TA��`A��HA��;A��#A���A���A��jA�dZA��A�ƨA�-A���A��A��A�oA�K�A���A�9XA��+A��
A�A�I�A�bNA�  A�A���A�I�A���A���A��#A���A���A���A�t�A���A��A�1'A���A�+A���A�t�A�K�A���A���A��#A�G�A�G�A���A���A�bA�p�A���A��A�  A�9XA�K�A��;A��RA��#A��A���A��-A�XA�JA�{A��A|ZAy�;AuXAtJAq��An�`Am�FAl��AkS�Ah=qAfr�Ae�Ad=qAbA�A`��A]
=AY��AY\)AW�TAU|�AQ7LAM7LAKS�AJn�AG�AD�RAC�;AB=qA@=qA;�A;t�A;��A;hsA8�9A4��A3�mA1/A0�\A0��A0��A-��A,n�A,jA,n�A,  A,(�A-G�A-�A.�!A.�jA.��A/+A/;dA/A-��A,��A,1A-A-7LA,  A+��A+l�A+%A+S�A-A.9XA.  A-"�A+�A'+A&�9A&M�A%dZA'dZA&bNA%7LA#ƨA$jA#�TA"�A!�TA!�wA"��A"n�A"�A!`BA ĜA�^AdZA(�A��A��A�wAv�A�FA �A��AA�A��A�A�DA=qA�mA+A��A�wAv�A��A&�AjA
=A
�/A
�+A
�DA	�A	33A	%A  AXA�AjA�AC�A Q�@���@�G�@��9@�l�@���@�@�@���@�%@���@���@�E�@�{@�p�@���@�@�v�@�n�@��+@�X@�\)@�"�@�$�@��@��D@�I�@�;d@��/@�Z@���@�7@���@�%@�Z@�33@�!@�t�@�@�!@��@�A�@�|�@�@�J@�7L@��@�33@�~�@��@��D@�33@�5?@�@��@��@���@��;@ۅ@�l�@ڸR@��@��@׾w@֧�@��@���@�9X@���@�l�@�v�@Ѳ-@љ�@��/@�1'@ϝ�@��@�`B@̛�@�bN@�(�@�S�@�+@��y@��@�E�@ɲ-@�V@�A�@��;@�l�@���@Ɵ�@�ff@��@Ų-@őh@�`B@���@�bN@Õ�@��@§�@�J@��#@���@���@��7@�X@���@�1'@��F@��P@�33@�
=@��@���@�E�@�J@�`B@���@��D@�(�@��m@�ƨ@�l�@��\@��-@��@���@��@�r�@�%@���@��-@��7@�/@���@��j@�\)@��@��@��R@�v�@��-@��@�bN@��w@���@��7@���@�A�@���@��y@�=q@���@���@��@�O�@��@�V@��@�%@��7@�`B@�?}@�?}@�`B@��@��@�j@�9X@�(�@���@���@��@�dZ@�+@���@���@�ff@�E�@�hs@�%@���@��@�z�@�  @��@�+@��H@��\@�^5@���@�@���@�A�@�b@��;@��w@���@�t�@�K�@�+@��@�ȴ@���@��!@�ff@�{@��T@���@�X@���@��D@�b@�ƨ@�t�@�C�@�o@���@���@�$�@��#@��7@��@��@�9X@�  @��;@�dZ@�ȴ@��+@�ff@�M�@��@���@�7L@���@�Z@��m@��F@���@�S�@���@��R@�~�@�M�@�J@��-@���@�`B@�/@��`@��j@�r�@�b@��F@�\)@�+@�o@�
=@�
=@�
=@�@��y@��R@�V@��@���@�`B@�?}@�&�@���@���@�(�@���@��F@���@�\)@�o@��y@���@�ff@�V@��@���@��h@�hs@�O�@�7L@��@���@�Z@�1'@�  @��@�33@�ȴ@���@�~�@�M�@�$�@��@��@�7L@���@��j@��@��u@�I�@� �@��
@�|�@�K�@�o@���@��y@���@�ff@�E�@��@�@���@���@�`B@��@��@�Ĝ@��@��D@�Z@�1'@���@���@���@�C�@��@���@��\@�~�@�^5@�5?@��@��h@�O�@��@�%@���@��`@���@�r�@�A�@�@�P@+@~��@~�@~ff@~E�@}�@}��@}�-@|��@|z�@|I�@|1@{ƨ@{ƨ@{ƨ@{��@{@z�H@z��@zJ@y��@x�9@x �@w��@w�P@v�R@v5?@u��@u@u�h@t��@t�D@t9X@sƨ@so@r^5@r�@q��@q��@q�7@p��@pr�@o��@o�@o|�@n�y@nv�@n5?@m��@m?}@l��@l��@ko@j�@j�!@j^5@j�@i�@i��@i��@i�7@i7L@h�`@hĜ@h��@h�@hbN@hb@gl�@f�@fff@f@e��@e@e��@ep�@dz�@c�m@cƨ@b��@a��@a�#@a�^@ahs@a7L@`Ĝ@`A�@_��@_��@_|�@_K�@_;d@^�@]�@]�@\�@\j@\1@[��@[C�@Z�@Z-@Y�#@Y7L@X�@XQ�@XQ�@X �@W�@W+@W
=@V�y@Vȴ@V�+@V$�@U��@U�-@U�@U/@U�@T��@T�/@T�@Tz�@TI�@S�m@S�@SC�@S"�@S@R�@R�!@R�@Q�7@Q�@P��@P�9@P�u@P�u@PQ�@O��@O��@O��@O|�@N�y@N$�@M��@M��@M`B@MV@MV@L��@L�@LZ@L9X@L(�@K��@K�@K"�@K@J�@J�H@J��@Jn�@J-@I�@I��@Ix�@I&�@H�`@H��@H�u@Hr�@H �@Gl�@F�@Fv�@FV@F$�@F@E��@D��@D�D@D(�@C��@C�
@C�@CS�@C33@B�@B�\@B-@Ahs@@r�@@b@@ �@?�;@?�P@?;d@>��@>{@=�h@=O�@=V@<Z@;�m@;t�@:�@:��@:��@:~�@:~�@:n�@:M�@9��@9hs@9%@8��@8�9@8bN@7��@7K�@7
=@6��@65?@5��@5�h@5p�@5O�@5�@4��@4I�@3��@2�@2��@2n�@2=q@1�#@1x�@1%@0�9@0r�@0A�@0 �@/��@/�@/\)@/+@.��@.ȴ@.v�@.E�@.$�@.$�@.@-@-��@-p�@-?}@-/@-�@,��@,��@,�@,��@,�@,��@,�D@,j@,j@,I�@,9X@,(�@,(�@+�@+C�@+"�@+o@*�H@*�!@*n�@*-@)��@)��@)��@)��@)hs@)G�@(��@(bN@(1'@'�@'l�@'+@&��@&ȴ@&��@&v�@%�@%�-@%�-@%��@%p�@$��@$��@$�D@$�D@$z�@$Z@#�F@#dZ@#C�@"�H@"��@"n�@"^5@"M�@"-@"�@"-@"J@!�#@!�^@!hs@ �`@ bN@ 1'@   @�;@�w@�P@;d@�@$�@�@V@V@V@�@z�@Z@�
@��@��@��@�@dZ@S�@33@o@@�H@n�@-@J@��@�^@x�@G�@%@��@ �@�w@�P@;d@�@�@��@ȴ@�R@�R@��@ff@V@5?@$�@@�T@��@@��@��@p�@O�@V@�@�@Z@I�@9X@9X@��@�F@�F@��@dZ@33@@�H@�H@��@��@�\@=q@��@��@x�@hs@hs@�@�9@�9@Q�@A�@A�@ �@��@l�@;d@
=@�y@ȴ@��@��G�O�A²-A¸RA°!A²-A©�A¥�A¥�A\AhA�AA�n�A�r�A�^5A�hsA�%A��A��RA��!A��!A���A��7A�l�A�jA�ffA�\)A�ZA�XA�S�A�I�A�7LA�1'A�(�A�"�A�{A�oA�oA�oA�{A�{A�VA�1A�1A�%A�A�A�A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��mA��yA��yA��yA��A��yA��mA��mA��mA��mA��`A��`A��mA��`A��mA��mA��mA��mA��mA��yA��yA��yA��yA��yA��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��mA��mA��mA��mA��`A��HA��HA��TA��TA��TA��`A��TA��`A��`A��mA��`A��`A��`A��TA��HA��;A��;A��/A��;A��;A��;A��;A��HA��;A��;A��;A��;A��/A��#A��
A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA�ĜA�A���A��jA��jA��RA���A��+A�z�A�l�A�ffA�M�A�7LA�7LA��A��;A��-A�dZA�I�A�-A�A��A���A���A���A���A�v�A�\)A�+A��A�bA�JA�A��A��;A���A��^A��\A�ffA�1'A�bA�VA�1A�  A���A���A��A��A��A��mA��A��A�%A��/A���A���A��A�\)A�G�A�5?A�+A�(�A�(�A��A���A��`A�A��!A��DA�ZA�$�A�A��#A��A���A��\A��A�v�A�ZA�I�A�5?A��A��\A�=qA�+A�+A�$�A�A���A���A�ffA�`BA�\)A�Q�A�JA�ƨA��uA�|�A�M�A�-A���A�v�A�JA���A��TA��PA�M�A�7LA��A���A��#A��uA�33A��HA��-A��\A�jA�JA���A�n�A�O�A�9XA�(�A���A���A�A���A�~�A�A�A��A��A��/A���A�~�A�M�A�5?A��A��A��^A�p�A�33A��A���A���A�l�A�O�A�E�A�E�A�G�A�A�A�1'A�(�A��A�$�A��A��A�A��A��TA���A��-A���A�v�A�O�A�/A��A�{A�VA�JA���A���A���A��A��mA��/A��A���A���A���A��wA��FA��-A���A���A��DA��+A�l�A�XA�Q�A�G�A�I�A�A�A�7LA�(�A��A�1A��HA��hA�(�A��A��FA���A���A���A��uA��+A�|�A�l�A�`BA�VA�E�A�=qA�7LA�/A�$�A��A�JA�A��A���A���A���A�ffA�VA�E�A�33A�"�A�{A�A���A��A��TA��#A���A��-A��7A�z�A�bNA�G�A�;dA�/A�-A�33A�-A��A��A�bA�A���A���A��A��A��yA��yA��A��!A�t�A�33A�JA���A��
A�ĜA��7A�7LA�
=A���A���A��yA��;A���A�ȴA�ĜA��jA��A���A���A���A���A��hA��DA��A��A�v�A�bNA�E�A�A���A���A��+A�I�A�{A���A��yA��
A�ƨA��-A���A���A�~�A�jA�Q�A�?}A�7LA�+A��A�  A��yA���A��!A���A�XA�7LA�(�A���A��9A��A�l�A�1'A���A�ĜA��uA�M�A���A��#A�A��\A�hsA�VA�M�A�G�A�?}A�5?A�&�A��A�oA�JA�A���A��A��A��A��^A���A�jA�"�A��`A��RA���A��uA��A�jA�9XA���A���A���A���A���A���A���A���A��hA�ffA�G�A�-A�A���A�ȴA��A��A�"�A���A��DA�x�A�hsA�bNA�^5A�XA�Q�A�K�A�M�A�O�A�M�A�9XA�/A�$�A�A��`A���A��^A��!A���A���A���A��hA��7A�v�A�ZA�-A��A���A�jA�O�A�-A��A�jA���A�p�A�G�A��A�A���A���A��A���A���A���A���A���A���A��uA��DA�~�A��7A�t�A�r�A�n�A�l�A�K�A��A��9A���A�|�A�33A�  A���A���A��A�S�A��A��TA���A�bNA�+A���A��HA��!A��+A�VA��A��A���A��jA���A���A��PA�v�A�bNA�M�A�=qA�$�A�
=A���A��A��jA���A�`BA�$�A��-A�G�A���A���A�z�A�$�A�%A��yA��A��A�bNA�C�A�/A��A�1A�A��A��#A��-A���A�p�A�K�A�/A��A�
=A��;A��FA���A��A�S�A�I�A�33A� �A�JA��A���A���A�x�A�A�A�A��TA��wA���A�~�A�VA�-A�JA���A��uA�n�A�A�A�"�A�oA���A���A�r�A�"�A�dZA���A�5?A���A���A���A��A�O�A�/A�
=A��yA���A��RA���A�r�A�^5A�A�A�-A�JA��A~�yA}�A}�A}dZA|��A|��A|�A{�A{%Az��Az��Az�Az-Ay�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                          ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	s�B	t�B	s�B	r|B	w2B	pB	ncB	m�B	o B	n/B	k�B	j�B	jB	kB	jB	j�B	kB	jB	kB	kQB	jKB	jB	k�B	jB	k�B	kB	kB	l�B	l�B	l�B	m)B	m)B	m�B	m�B	k�B	l�B	k�B	k�B	jB	j�B	iDB	iDB	h�B	h>B	j�B	�%B	�
B
tTB
��B
͟B
��BB(�B9$B>�BN�BXyBgmBr|B~�B�\B��B�IB�B�6B��B�#BҽBɺB��B�B�/B�sBϫB�EB�B�NBܒB��B�)B��B��B��B��B��B��B��Br�BK�B,�B�B
�iB
خB
�RB
�qB
�OB
��B
}�B
k�B
[#B
?}B
&�B
AB	��B	��B	ѷB	ʌB	��B	�RB	��B	�B	� B	�7B	{B	m)B	d�B	^jB	S�B	GB	?�B	'RB	"4B	�B	�B	 iB�MB��B�B�;BѷB�0B��BȴB�[B͟B�BB�;B�BбB��B�zBŢB��B̘B�EB�[B�B�B�`B��B	B	 \B	6�B	OB	n/B	��B	�PB	�B	�FB	��B	��B	��B	��B	�3B	��B	˒B	�B	�B

	B
0�B
5tB
0�B
-wB
GB	�B	�xB	��B
_B
�B
B
�B
qB
�B
{B
B

rB
&�B
&B
$�B
 �B
�B
OB
�B
�B
�B
�B
.B

	B
SB
 4B	�AB	��B	ߤB	�QB	�
B	��B	�TB	��B	�B	��B	ɺB	�B	��B	�3B	�OB	��B	ŢB	�6B	��B	ĜB	��B	�<B	��B	�xB	��B	�LB	�@B	�1B	}VB	|B	z�B	yrB	{B	yrB	�4B	��B	��B	��B	��B	��B	��B	��B	��B	�4B	�+B	��B	��B	��B	��B	�!B	�VB	��B	�B	��B	�-B	��B	�B	�:B	��B	��B	�-B	��B	�'B	��B	��B	��B	�UB	�wB	��B	�B	�LB	��B	��B	��B	�B	�B	�LB	�RB	�zB	��B	��B	��B	��B	�0B	��B	�jB	�B	��B	��B	�HB	�qB	��B	��B	�RB	�6B	��B	��B	��B	�^B	��B	��B	�6B	�B	��B	��B	��B	�RB	��B	�B	�0B	�B	�6B	�B	��B	�aB	��B	��B	��B	�tB	�B	��B	�RB	�B	�RB	ɆB	ʌB	��B	�jB	�0B	�B	�pB	�pB	�BB	�B	�B	�HB	�B	ӏB	��B	��B	�[B	��B	��B	҉B	��B	�[B	�9B	�B	�mB	�?B	֡B	�B	�mB	�QB	ٴB	�yB	�B	ںB	ܒB	�BB	�B	�B	�B	�KB	��B	��B	�B	�mB	�B	�fB	�,B	��B	�B	��B	�pB	�)B	�KB	�mB	�mB	�,B	ԕB	�aB	�[B	ԕB	�sB	�sB	�?B	�sB	خB	�yB	ޞB	�B	��B	�`B	�B	�cB	�B	�;B	�;B	�B	�B	�vB	�vB	�B	��B	��B	�B	�|B	�|B	�B	��B	�B	�TB	�B	�%B	��B	��B	��B	��B	�ZB	�+B	��B	��B	�fB	�B	��B	��B	�>B	��B	�DB	�DB	��B	��B	��B	��B	��B	��B	�]B	��B	�.B	��B
 �B
oB
�B
AB
AB
�B
�B
�B
�B
�B
B
�B
B
�B
�B
�B
YB
+B
+B
�B
�B
+B
1B
�B
fB
fB
	lB
	7B
	�B

rB

�B

�B

rB

�B
xB
DB
B
xB
xB
�B
�B
B
B
�B
"B
VB
"B
"B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
uB
@B
FB
FB
�B
MB
�B
�B
B
�B
�B
�B
�B
�B
�B
YB
_B
+B
_B
�B
�B
7B
B
kB
�B
�B
	B
CB
�B
IB
�B
�B
�B
�B
�B
VB
 'B
 'B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
#:B
#:B
#B
#:B
#B
#B
#nB
#:B
#�B
$B
$�B
&B
&�B
'RB
'B
'RB
'RB
'�B
'�B
(�B
)*B
)_B
)_B
)_B
)_B
)�B
)�B
*eB
+B
+B
+6B
+6B
+kB
+�B
+�B
,=B
,B
,=B
-B
,�B
,�B
.IB
.}B
.IB
.IB
.�B
/OB
/B
/OB
/OB
/�B
0�B
1'B
0�B
1'B
0�B
0�B
1'B
1'B
1'B
1�B
1[B
1'B
1�B
2aB
2�B
2�B
2�B
2�B
33B
3�B
3�B
5B
5B
5B
6FB
6�B
6�B
7B
7�B
8B
9�B
:�B
:�B
:�B
:�B
:�B
;dB
;dB
;dB
;dB
;�B
;�B
;�B
;�B
;�B
;�B
<B
<B
<�B
<jB
=B
=B
=B
<�B
<�B
=�B
=qB
=�B
?HB
?HB
?B
?HB
?}B
?}B
@B
@OB
@�B
@�B
@�B
@�B
@�B
A B
B'B
B�B
CaB
C�B
C�B
D3B
DgB
D�B
E9B
E9B
FB
F?B
E�B
E�B
FB
F�B
GEB
GEB
GzB
G�B
HB
HKB
H�B
H�B
IB
IB
IB
IRB
IB
IB
IRB
I�B
I�B
J�B
J�B
J�B
K)B
K)B
K�B
L�B
M�B
NB
N<B
NpB
N<B
NpB
N�B
OvB
OB
N�B
OB
PHB
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
R B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
T�B
U2B
UgB
U�B
U�B
U�B
U�B
U�B
VB
V�B
W
B
W?B
W?B
W?B
W
B
W
B
XyB
XB
XEB
X�B
YB
YKB
YKB
YKB
YB
Y�B
ZB
Z�B
Z�B
Z�B
[WB
[WB
[�B
[�B
[WB
[�B
[WB
[#B
Z�B
[WB
[WB
[�B
\]B
\]B
\]B
\�B
\�B
\]B
\�B
]�B
]�B
^5B
^5B
^jB
^jB
_�B
_�B
_�B
`BB
`vB
`�B
`�B
`�B
`�B
`�B
aB
a�B
a|B
bB
bB
bNB
b�B
b�B
c B
c�B
c�B
c�B
d&B
d&B
d�B
d�B
e,B
e,B
e,B
e�B
e�B
f2B
ffB
f�B
f�B
gB
gB
g8B
g8B
g8B
gmB
gmB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
h
B
h
B
h>B
iB
iB
iB
iyB
iB
iyB
iDB
iyB
jB
jB
jB
jKB
j�B
j�B
kQB
kQB
kQB
kQB
kQB
kQB
k�B
k�B
k�B
l�B
m�B
m]B
m]B
m]B
m]B
m�B
n/B
m�B
m�B
n/B
ncB
oiB
n�B
o5B
oiB
o�B
o�B
o�B
o�B
pB
p;B
poB
p;B
pB
o�B
poB
p�B
q�B
q�B
rGB
r|B
r|B
r�B
sMB
s�B
t�B
t�B
u�B
uZB
uZB
u�B
v+B
v+B
v�B
v`B
v`B
v�B
v�B
w2B
wfB
w�B
w�B
xB
xB
x�B
y	B
x�B
x�B
y>B
y>B
yrB
y�B
y�B
zB
zxB
zxB
z�B
z�B
z�B
z�B
{JB
{�B
{�B
{�B
|B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
~(B
~�B
~�B
~�B
~�B
~�B
.B
cB
cB
cB
�B
�B
�B
�B
�B
� B
�4B
��B
��B
�;B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�GB
�B
�MB
��B
��B
��B
�B
�SB
��B
�SB	u�B	rGB	tB	s�B	v`B	rGB	t�B	r�B	r�B	sB	xB	v�B	n/B	sB	sB	��B	t�B	�iB	lWB	r|B	m�B	s�B	q�B	m�B	l�B	m�B	m]B	m]B	o B	m�B	q�B	n/B	m�B	n�B	o�B	k�B	kQB	kB	jKB	j�B	jB	k�B	i�B	jB	jKB	jB	jB	jB	i�B	jB	j�B	kQB	k�B	k�B	k�B	j�B	j�B	j�B	i�B	i�B	jB	jKB	j�B	jB	jKB	j�B	j�B	j�B	kB	k�B	j�B	kQB	j�B	kB	j�B	kB	jB	j�B	jB	i�B	i�B	i�B	jKB	jB	kB	k�B	kQB	k�B	k�B	k�B	k�B	kB	kQB	j�B	j�B	jB	j�B	jKB	jB	jKB	i�B	jB	jB	i�B	jB	jKB	jKB	kQB	k�B	kQB	k�B	kQB	kQB	k�B	j�B	jB	jB	i�B	jKB	j�B	j�B	kB	kQB	k�B	k�B	k�B	k�B	k�B	k�B	kQB	kQB	kB	j�B	jB	jKB	jKB	jB	j�B	l"B	k�B	m�B	l�B	m�B	m�B	l�B	k�B	l"B	k�B	lWB	l�B	m�B	m�B	m�B	m�B	m�B	l�B	k�B	l"B	k�B	l"B	l�B	l�B	l�B	ncB	m�B	n�B	m�B	m�B	m]B	l�B	l�B	l�B	l�B	m�B	n/B	n/B	ncB	n�B	m�B	m�B	m�B	n/B	m�B	l�B	l�B	l�B	lWB	l�B	j�B	kQB	kQB	l"B	l"B	k�B	lWB	lWB	m�B	m)B	l"B	m�B	k�B	jB	j�B	j�B	kQB	l"B	k�B	l"B	k�B	jB	j�B	kB	j�B	jKB	jKB	jB	jKB	i�B	j�B	k�B	kB	kQB	j�B	jKB	i�B	iyB	iB	h�B	iDB	h�B	h�B	iDB	i�B	h�B	i�B	jKB	i�B	iDB	iyB	h�B	g�B	h
B	g8B	h
B	h�B	g�B	h>B	h�B	h�B	iyB	jB	iDB	jKB	i�B	j�B	uZB	zDB	~�B	��B	��B	�xB	�$B	�$B	�B	��B	�
B
�B
+�B
<�B
U�B
d&B
s�B
��B
��B
�B
��B
�	B
��B
�FB
�$B
�^B
��B
��B
ȀB
�6B
��B
�dB
�B
�2B
�cB
��B
��BoBuB�B�B�B�BGB�B(�B*�B0!B/B33B6zB9XB9�B:*B:*B9�B8�B=�B?�BA BC-BDgBH�BOBBN<BU2BR�BZ�BW?BV�BXBXEB\�BZ�B^Bm�BqvBu%Bm]Bl�BncBu�BxlB{Bz�Bx�By	B}VB�	B�(B�B�DB�bB�\B��B��B�=B��B�4B�hB�wB��B�6B��B��B�RB�BB�B��B��B�aBǮBܒBɆB�jBɆBʌB��B�B�<B��BҽB�?B��B�yB�gB�QB�/B��B�yB�KB��B�vB�|B�5B��B�B�5B�BخB�B�gB�2B�9B�B��B��BҽBԕBҽB�[B��B�&B҉B��B�TB��B�}B�BB�dB˒BɺBɆB��B�EB�B��B�RB�^BȀB�B��B��BɆB��B��B��BȀBɆBȀB��B�RB��B�zB�9BŢB�?B�B��B�-B�?B�zB�XBخB��B�B�pB�jBݘB�dB��B�dB��B��B�]B�BٴB�KB��B��B�yB�BרB�mB��BרB�?BбB�vB�BΥB�BBбB�<B�pB�B�pB�vBҽB��BϫB�,B� BӏB՛B�,B҉B��B�dBݘB�B��B�/B�dB�B�pB�jB�dB�B�B�B�B�B�B�B�ZB��B��B�&B�BߤB�;B�B�B�jB��B��B�;B��B�/B�]B�WB�WB�]BیB��BچBںB��B�B��BרB֡BݘB�B�[B�NB�vB��B��B�pB͟B�6B��B�BɺB�RBɺB�zB�B�zB�B�3B��B��B�B��B�3B��B��B��B��B��B�RB�B�BB�^B�-B��B��B�}B�6B��B�B��B��B�XB��B��B�tB�FB�FB��B�'B�zB�LB�4B�*B��B�*B��B�B��B��B�XB�RB�UB��B�bB�-B��B�!B��B��B�hB�tB��B��B��B��B��B��B��B�=B��B�YB��B��B�.B��B��B�JB��B��B��B��B��B��B�+B�B�GB��B�4B|�B|PB{JBz�By�Bw�Bx�By	Bw�Bp;Bo5Bd&BaHBb�B_pBs�B\)BP�BIBI�BC�B;0B9XB6zB2�B1�B/OB/�B/B.B/�B-CB,�B'�B'RB$@B"�B"�B$@B�B"4B�B�BB
�"B
��B
�	B
��B
��B
�AB
��B
�"B
�sB
��B
��B
�dB
�B
�]B
یB
ٴB
�aB
҉B
бB
ϫB
��B
˒B
�0B
�^B
ɆB
�zB
�zB
�mB
ĜB
ƨB
��B
�aB
�OB
��B
ÖB
��B
��B
�B
��B
�kB
�-B
��B
�kB
��B
��B
��B
��B
��B
�B
�B
�xB
��B
�B
�+B
�+B
��B
��B
�iB
}VB
~(B
zDB
t�B
u�B
sMB
l�B
l�B
kB
l"B
i�B
h�B
hsB
iB
g�B
c�B
YB
`B
^�B
W
B
V�B
PB
Q�B
N<B
E�B
?�B
>B
8B
/�B
>BB
0UB
,=B
/�B
>�B
,=B
 'B
B
�B
"B
PB
B
�B
B	��B	�JB	�rB	��B	��B	�;B	�/B	��B	��B	��B	��B	�WB	�B	ߤB	�B	�B	�B	�B	��B	ΥB	̘B	�aB	ҽB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021041323183320210413231833IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021042700033620210427000336QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021042700033620210427000336QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014620210427140146IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162941              CF      PSAL                            ?�\)G�O�D�L�G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163450    CF                  PSAL            G�O�?L��G�O�CH  G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                