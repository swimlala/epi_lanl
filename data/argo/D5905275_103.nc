CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-01T19:08:56Z creation; 2023-04-26T19:14:30Z DMQC;      
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
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201101190856  20230426191430  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               g   gAA  AOAO7316_008644_103                 7316_008644_103                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�D���@�D���11  @�D-�@�D-�@&�֡a��@&�֡a���c�\|ؘ��c�\|ؘ�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@@  @�G�@��
@�  @�p�A   A  A ��A,(�A?\)A_\)A�  A�  A��A��A�Q�A�  A�  A�  B   B  B  B  B   B'�B/�
B8(�B@  BH  BO�
BW�
B`(�Bh(�Bp��Bw�B�B��B��B��B�  B��B��
B��B��B�  B��B��B�  B�  B�  B�  B��B��B�  B�  B��B��B�  B�  B��B�  B�{B�{B��B�  B�  B��B��C��C
=C{C
=C

=C  C��C  C
=C  C  C  C
=C  C��C��C"
=C$
=C&  C(  C*  C,
=C.  C0
=C2
=C4  C6  C8  C:
=C<
=C>  C@
=CB
=CD  CF  CH
=CJ{CL{CN  CO��CR
=CT  CV  CX  CY��C[�C]�C_��Ca��Cd  Cf
=Ch  Ci��Cl
=Cn  Co��Cq��Cs��Cv{Cx
=Cz
=C|
=C}��C��C�  C�  C�  C�C�C�  C�  C���C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�
=C�  C���C���C���C�  C�C�  C�C�C���C���C���C�  C�  C�C�
=C�  C���C�  C�C�C�  C�  C�  C���C�C�C���C���C�  C�  C�C�C�  C���C���C���C�  C���C���C�  C�  C�C�  C���C�  C�C�C���C���C���C���C���C���C�  C�  C�  C�  C�C�
=C�  C���C���C�C�  C���C���C��C���C�  C���C���C�  C�C�  C���C���C��C��C���C�  C���C�C�
=C�C�  C���C���C���C���C�C���C�  C�
=C�C�C�  C���C���C���C���C���C���D ��D  D}qD  D��D�qDz�D�qD��D  D��D�D}qD  D�D  D��D	D	}qD
  D
� D
��D}qD�D� D�qD� D�D� D�qD� D�D� D��D� DD�D�D��D  D� D�qD��D  D� D�D��D�D� D��D� DD��D�D� D�qD��DD��D  D}qD�qD� D   D ��D!�D!� D"  D"� D#�D#��D#�qD$� D%�D%� D%�qD&}qD'  D'}qD'��D(}qD)  D)� D*  D*� D*�qD+z�D+�qD,}qD,�qD-� D-�qD.}qD/  D/��D0  D0}qD1  D1� D2  D2� D2�qD3}qD3�qD4}qD5  D5��D6�D6��D7�D7��D8�D8��D9�D9}qD9��D:}qD:�qD;}qD;�qD<� D=  D=��D>�D>�D?  D?}qD?�qD@� DA  DA}qDB  DB� DC  DC� DD  DD� DD�qDE}qDE�qDFz�DF�qDG��DG�qDH}qDI  DI��DI�qDJz�DJ�qDK}qDL  DL}qDM  DM��DN  DN� DO�DO� DO�qDP� DQ  DQ}qDQ�qDR��DS�DS}qDS�qDT� DT�qDU� DV  DV}qDW  DW}qDW��DX}qDY  DY}qDZ  DZ��D[  D[}qD[��D\}qD]  D]��D^�D^� D^�qD_}qD`�D`��Da  Da}qDa�qDb� DcDc��Dd�Dd� De  De� Df  Df� Dg  Dg� Dh  Dh}qDh��Di}qDj  Dj� Dj�qDk}qDl  Dl� Dl�qDm� Dn  Dn}qDn�qDo}qDp  Dp� Dp�qDq}qDr  Dr�Ds  Dsz�Ds��Dtz�Dt�qDu}qDv�Dv� Dw  Dw}qDw�qDx� Dy  Dy��Dz  Dz}qD{  D{��D|�D|}qD|�qD}� D}�qD~� D�D� D�qD�@ D�~�D���D���D�=qD�� D�� D���D�>�D�~�D���D���D�B�D�� D���D�  D�=qD�� D��HD�  D�@ D���D��HD���D�@ D��HD��HD�  D�@ D��HD��HD�  D�>�D�~�D��HD�HD�=qD�� D��HD�HD�AHD��HD��HD�HD�AHD�� D���D���D�>�D�� D�� D���D�>�D�~�D���D��qD�>�D�� D�� D�  D�@ D�� D���D�  D�AHD��HD��HD�  D�>�D��HD�D�  D�@ D��HD��HD�  D�AHD���D�� D���D�>�D�� D�� D���D�@ D��HD�� D���D�AHD���D��HD�  D�@ D��HD�� D���D�=qD�~�D���D���D�@ D�� D��HD�  D�@ D���D�D�  D�>�D��HD��HD�  D�@ D�� D���D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�AHD�~�D��qD�  D�@ D��HD�� D���D�@ D�� D��HD�  D�>�D�� D���D�  D�@ D�~�D�� D�HD�AHD��HD��HD�HD�B�D�� D�� D�HD�@ D��HD��HD�  D�>�D�� D�� D���D�@ D���D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�� D���D�  D�AHD�� D��qD���D�@ D�~�D���D�  D�@ D�� D�D�HD�@ D�� D�� D�HD�@ D��HD��HD�HD�@ D�}qD��qD���D�=qD�� D�D�HD�AHD�� D��qD�  D�>�D�� D�D�  D�=qD�~�D���D���D�@ D�~�D���D�  D�>�D�~�D��HD�  D�@ D�~�D���D���D�@ D�� D�D��D�AHD�~�D���D�  D�@ D��HD���D�  D�@ D�~�D��HD��D�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD��HD�� D���D�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHDHD�� D�HD�AHDÀ D��HD�HD�>�DĀ D�� D���D�>�D�~�D�� D�HD�AHDƀ D�� D�  D�@ Dǀ D��HD�HD�@ D�~�D�� D�HD�B�DɁHD��HD�  D�@ DʁHD��HD�HD�B�Dˀ D˽qD���D�@ D̂�D��HD�  D�>�D̀ D�� D�  D�AHD�~�Dξ�D�  D�>�DρHD�� D���D�@ DЀ D�� D���D�@ DсHDѾ�D�  D�@ D�~�D�� D�HD�>�DӁHD�� D���D�AHDԂ�D��HD��D�AHD�~�Dվ�D�  D�@ Dր D־�D�  D�AHD׀ D׾�D�  D�@ D؁HDؾ�D�  D�@ D�}qDپ�D���D�>�Dڀ D��HD�  D�B�Dۀ D�� D�  D�@ D�~�D�� D�HD�@ D�~�Dݾ�D�  D�@ Dހ D�� D�HD�AHD߁HD�� D�HD�@ D��HD��HD���D�@ D� D�� D�  D�>�D� D⾸D��qD�>�D� D��HD��D�AHD� D侸D�  D�B�D�HD��HD�  D�@ D�~�D�� D�HD�@ D� D�� D���D�>�D�~�D辸D��qD�@ D� D�� D�HD�@ D� D꾸D�  D�AHD�HD�� D�  D�@ D� D쾸D�  D�@ D�HD�� D���D�>�D� DD���D�>�D� D�� D���D�>�D��HD�� D�  D�>�D�~�D��HD�  D�@ D�D��HD�HD�@ D�~�D�� D�  D�AHD� D�� D��D�AHD�~�D��)D���D�AHD�� D�� D�HD�AHD��HD�� D���D�>�D��HD��HD���D�=qD�� D��HD�HD�AHD�y�D��H>��
?\)?u?��
?�
=@�@��@333@J=q@c�
@z�H@�=q@�@��\@�\)@��R@�=q@�@��
@�33A   AA��A�
A��A   A'�A.{A4z�A:=qAA�AHQ�AN{ATz�AZ=qAa�Ag
=Al(�As33AxQ�A|(�A���A��A�A�\)A���A��
A��A��RA���A�=qA��
A�A�\)A���A��HA�z�A�{A�\)A�G�A�33A���A�ffA�Q�A��A�33A�p�A�\)A���A��A��
A�{A�\)A���A�33A���A�{A�  A�=qA��
A��A�\)A�G�A��HA�(�A�ffAУ�A��AӅA�p�A�\)A�G�Aڏ\A���A޸RA�Q�A��A��
A�{A�\)A�G�A��HA��A�RA�  A�=qA�(�A�p�A�
=A���A��HA�z�A�{B   B ��B��BffB�BQ�B��B��B�RB�BQ�B	�B
=qB
�HB�B��BB�\B\)BQ�Bp�B=qB33BQ�BG�BffB33B  BG�BffB�BQ�BG�BffB�B ��B!��B"ffB#�B$��B%B&�RB'�B(��B)�B*�HB,  B,��B-�B/
=B0  B1G�B2ffB333B4Q�B5p�B6�RB7�B8��B9��B:�RB;�
B<��B>=qB?33B@(�BAG�BBffBC�BD��BF=qBG33BH(�BIp�BJ�RBK�
BM�BN=qBO33BPQ�BQp�BR�RBT  BU�BV=qBW\)BX��BZ{B[\)B\��B^{B_33B`z�Ba��Bb�HBdQ�Be��Bf�HBh(�Bip�Bj�RBk�Blz�Bmp�Bn=qBo
=Bo�
Bpz�Bq�Bq��Br{BrffBr�\Br�HBs33Bs�Bt  Btz�Bt��Bt��BuG�Bup�BuBv=qBv�\Bv�HBw\)Bw�
BxQ�Bx��Bx��ByG�ByBz=qBz�\B{33B{�B|(�B|��B}�B}��B~{B~ffB~�HB\)B�
B�=qB�z�B���B�
=B�\)B���B��
B�{B�Q�B���B���B�G�B��B��B�(�B�z�B���B�
=B�G�B���B��
B�(�B��\B��HB�33B���B��B�Q�B���B���B�\)B��B�  B�Q�B���B�
=B�\)B��B�{B�ffB��RB��B�p�B�B�(�B��\B��HB�G�B�B�{B�z�B���B�33B��B��
B�=qB���B���B�\)B��B�  B�Q�B���B���B�\)B��B�{B�ffB���B�33B��B��
B�=qB���B���B�\)B��B�  B�Q�B���B���B�G�B���B�  B�Q�B��RB�
=B�\)B�B�{B�z�B���B�33B��B��B�=qB���B���B�\)B��B�{B��\B��HB�G�B���B�  B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�ffB���B�G�B��B�{B��\B�
=B�p�B��B�Q�B���B�33B�B�(�B��\B�
=B��B�  B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�B��RB��B���B�(�B���B��B��B�(�B���B��B��B�(�B���B��B���B�(�B���B��B��B�{B���B��B���B�=qB��RB�33B�B�Q�B���B�\)B��
B�ffB��HB�p�B��B�ffB���B�p�B�  B�z�B���Bə�B�{Bʏ\B��BˮB�(�B̸RB�G�B��
B�Q�B���B�p�B�  BЏ\B�
=BхB�{Bң�B��Bә�B�(�Bԣ�B�33BծB�=qB���B�G�B��
B�ffB���BمB�  B�ffB��HB�G�B��
B�Q�B��HB�p�B��B�ffB���B�G�B�B�=qB���B�G�B��B�ffB���B�\)B�B�=qB���B�\)B��
B�ffB���B�\)B��
B�Q�B���B�\)B��
B�ffB�RB�G�B�B�{B�\B�
=B�B�{B�\B���B�\)B��
B�Q�B�RB�33B�B�(�B�\B���B�p�B��B�ffB���B�33B���B�  B�z�B�
=B�p�B��
B�(�B��\B�
=B��B��B�Q�B��RB��B��B�{B��\B�
=B�\)B�B�(�B���B�33B��C 
=C =qC p�C ��C �C(�C\)C�\CC  C=qCz�C�RC�C�C\)C��C�HC�C\)C�\CC  C=qC�C�RC�C(�CffC�C�C�CG�C�\C��C{CG�Cz�CC	  C	G�C	z�C	�C	�C
=qC
�C
C
��C33Cp�CC
=CG�C�C�RC  CQ�C��C�HC�C\)C��C�C33Cz�CC
=CG�C�\C�
C(�CffC��C�C=qC�C�
C{C\)C��C�C=qC�C��C{CQ�C��C�HC33C�C��C
=CQ�C��C��CG�C�\C�
C�CffCC{C\)C��C�C=qC�\C�HC33Cp�CC�Cp�C�RC  C\)C�RC
=CG�C��C��C Q�C ��C �HC!33C!�\C!�C"33C"z�C"�
C#(�C#�C#��C${C$z�C$��C%{C%ffC%C&�C&ffC&�C'{C'ffC'�C(  C(Q�C(�C(��C)G�C)��C)��C*G�C*�\C*�HC+=qC+�\C+�
C,(�C,�C,�
C-�C-p�C-��C.�C.ffC.�RC/{C/ffC/�C0  C0\)C0�C0��C1G�C1��C2  C2G�C2�\C2��C3G�C3�\C3�C4G�C4�\C4�
C533C5�\C5�
C6(�C6�C6�
C7{C7p�C7��C8{C8\)C8�RC9{C9\)C9�C:  C:\)C:�C:��C;Q�C;�C;��C<G�C<��C<��C==qC=�C=�C>=qC>�\C>�
C?33C?�\C?�HC@33C@z�C@��CA(�CAz�CA��CB�CBz�CBCC{CCp�CCCD
=CDffCDCE{CE\)CE�CF
=CF\)CF��CF��CGQ�CG��CG�CHG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 ?u?��H@@  @�G�@��
@�  @�p�A   A  A ��A,(�A?\)A_\)A�  A�  A��A��A�Q�A�  A�  A�  B   B  B  B  B   B'�B/�
B8(�B@  BH  BO�
BW�
B`(�Bh(�Bp��Bw�B�B��B��B��B�  B��B��
B��B��B�  B��B��B�  B�  B�  B�  B��B��B�  B�  B��B��B�  B�  B��B�  B�{B�{B��B�  B�  B��B��C��C
=C{C
=C

=C  C��C  C
=C  C  C  C
=C  C��C��C"
=C$
=C&  C(  C*  C,
=C.  C0
=C2
=C4  C6  C8  C:
=C<
=C>  C@
=CB
=CD  CF  CH
=CJ{CL{CN  CO��CR
=CT  CV  CX  CY��C[�C]�C_��Ca��Cd  Cf
=Ch  Ci��Cl
=Cn  Co��Cq��Cs��Cv{Cx
=Cz
=C|
=C}��C��C�  C�  C�  C�C�C�  C�  C���C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�
=C�  C���C���C���C�  C�C�  C�C�C���C���C���C�  C�  C�C�
=C�  C���C�  C�C�C�  C�  C�  C���C�C�C���C���C�  C�  C�C�C�  C���C���C���C�  C���C���C�  C�  C�C�  C���C�  C�C�C���C���C���C���C���C���C�  C�  C�  C�  C�C�
=C�  C���C���C�C�  C���C���C��C���C�  C���C���C�  C�C�  C���C���C��C��C���C�  C���C�C�
=C�C�  C���C���C���C���C�C���C�  C�
=C�C�C�  C���C���C���C���C���C���D ��D  D}qD  D��D�qDz�D�qD��D  D��D�D}qD  D�D  D��D	D	}qD
  D
� D
��D}qD�D� D�qD� D�D� D�qD� D�D� D��D� DD�D�D��D  D� D�qD��D  D� D�D��D�D� D��D� DD��D�D� D�qD��DD��D  D}qD�qD� D   D ��D!�D!� D"  D"� D#�D#��D#�qD$� D%�D%� D%�qD&}qD'  D'}qD'��D(}qD)  D)� D*  D*� D*�qD+z�D+�qD,}qD,�qD-� D-�qD.}qD/  D/��D0  D0}qD1  D1� D2  D2� D2�qD3}qD3�qD4}qD5  D5��D6�D6��D7�D7��D8�D8��D9�D9}qD9��D:}qD:�qD;}qD;�qD<� D=  D=��D>�D>�D?  D?}qD?�qD@� DA  DA}qDB  DB� DC  DC� DD  DD� DD�qDE}qDE�qDFz�DF�qDG��DG�qDH}qDI  DI��DI�qDJz�DJ�qDK}qDL  DL}qDM  DM��DN  DN� DO�DO� DO�qDP� DQ  DQ}qDQ�qDR��DS�DS}qDS�qDT� DT�qDU� DV  DV}qDW  DW}qDW��DX}qDY  DY}qDZ  DZ��D[  D[}qD[��D\}qD]  D]��D^�D^� D^�qD_}qD`�D`��Da  Da}qDa�qDb� DcDc��Dd�Dd� De  De� Df  Df� Dg  Dg� Dh  Dh}qDh��Di}qDj  Dj� Dj�qDk}qDl  Dl� Dl�qDm� Dn  Dn}qDn�qDo}qDp  Dp� Dp�qDq}qDr  Dr�Ds  Dsz�Ds��Dtz�Dt�qDu}qDv�Dv� Dw  Dw}qDw�qDx� Dy  Dy��Dz  Dz}qD{  D{��D|�D|}qD|�qD}� D}�qD~� D�D� D�qD�@ D�~�D���D���D�=qD�� D�� D���D�>�D�~�D���D���D�B�D�� D���D�  D�=qD�� D��HD�  D�@ D���D��HD���D�@ D��HD��HD�  D�@ D��HD��HD�  D�>�D�~�D��HD�HD�=qD�� D��HD�HD�AHD��HD��HD�HD�AHD�� D���D���D�>�D�� D�� D���D�>�D�~�D���D��qD�>�D�� D�� D�  D�@ D�� D���D�  D�AHD��HD��HD�  D�>�D��HD�D�  D�@ D��HD��HD�  D�AHD���D�� D���D�>�D�� D�� D���D�@ D��HD�� D���D�AHD���D��HD�  D�@ D��HD�� D���D�=qD�~�D���D���D�@ D�� D��HD�  D�@ D���D�D�  D�>�D��HD��HD�  D�@ D�� D���D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�AHD�~�D��qD�  D�@ D��HD�� D���D�@ D�� D��HD�  D�>�D�� D���D�  D�@ D�~�D�� D�HD�AHD��HD��HD�HD�B�D�� D�� D�HD�@ D��HD��HD�  D�>�D�� D�� D���D�@ D���D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�� D���D�  D�AHD�� D��qD���D�@ D�~�D���D�  D�@ D�� D�D�HD�@ D�� D�� D�HD�@ D��HD��HD�HD�@ D�}qD��qD���D�=qD�� D�D�HD�AHD�� D��qD�  D�>�D�� D�D�  D�=qD�~�D���D���D�@ D�~�D���D�  D�>�D�~�D��HD�  D�@ D�~�D���D���D�@ D�� D�D��D�AHD�~�D���D�  D�@ D��HD���D�  D�@ D�~�D��HD��D�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD��HD�� D���D�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHDHD�� D�HD�AHDÀ D��HD�HD�>�DĀ D�� D���D�>�D�~�D�� D�HD�AHDƀ D�� D�  D�@ Dǀ D��HD�HD�@ D�~�D�� D�HD�B�DɁHD��HD�  D�@ DʁHD��HD�HD�B�Dˀ D˽qD���D�@ D̂�D��HD�  D�>�D̀ D�� D�  D�AHD�~�Dξ�D�  D�>�DρHD�� D���D�@ DЀ D�� D���D�@ DсHDѾ�D�  D�@ D�~�D�� D�HD�>�DӁHD�� D���D�AHDԂ�D��HD��D�AHD�~�Dվ�D�  D�@ Dր D־�D�  D�AHD׀ D׾�D�  D�@ D؁HDؾ�D�  D�@ D�}qDپ�D���D�>�Dڀ D��HD�  D�B�Dۀ D�� D�  D�@ D�~�D�� D�HD�@ D�~�Dݾ�D�  D�@ Dހ D�� D�HD�AHD߁HD�� D�HD�@ D��HD��HD���D�@ D� D�� D�  D�>�D� D⾸D��qD�>�D� D��HD��D�AHD� D侸D�  D�B�D�HD��HD�  D�@ D�~�D�� D�HD�@ D� D�� D���D�>�D�~�D辸D��qD�@ D� D�� D�HD�@ D� D꾸D�  D�AHD�HD�� D�  D�@ D� D쾸D�  D�@ D�HD�� D���D�>�D� DD���D�>�D� D�� D���D�>�D��HD�� D�  D�>�D�~�D��HD�  D�@ D�D��HD�HD�@ D�~�D�� D�  D�AHD� D�� D��D�AHD�~�D��)D���D�AHD�� D�� D�HD�AHD��HD�� D���D�>�D��HD��HD���D�=qD�� D��HD�HD�AHD�y�G�O�>��
?\)?u?��
?�
=@�@��@333@J=q@c�
@z�H@�=q@�@��\@�\)@��R@�=q@�@��
@�33A   AA��A�
A��A   A'�A.{A4z�A:=qAA�AHQ�AN{ATz�AZ=qAa�Ag
=Al(�As33AxQ�A|(�A���A��A�A�\)A���A��
A��A��RA���A�=qA��
A�A�\)A���A��HA�z�A�{A�\)A�G�A�33A���A�ffA�Q�A��A�33A�p�A�\)A���A��A��
A�{A�\)A���A�33A���A�{A�  A�=qA��
A��A�\)A�G�A��HA�(�A�ffAУ�A��AӅA�p�A�\)A�G�Aڏ\A���A޸RA�Q�A��A��
A�{A�\)A�G�A��HA��A�RA�  A�=qA�(�A�p�A�
=A���A��HA�z�A�{B   B ��B��BffB�BQ�B��B��B�RB�BQ�B	�B
=qB
�HB�B��BB�\B\)BQ�Bp�B=qB33BQ�BG�BffB33B  BG�BffB�BQ�BG�BffB�B ��B!��B"ffB#�B$��B%B&�RB'�B(��B)�B*�HB,  B,��B-�B/
=B0  B1G�B2ffB333B4Q�B5p�B6�RB7�B8��B9��B:�RB;�
B<��B>=qB?33B@(�BAG�BBffBC�BD��BF=qBG33BH(�BIp�BJ�RBK�
BM�BN=qBO33BPQ�BQp�BR�RBT  BU�BV=qBW\)BX��BZ{B[\)B\��B^{B_33B`z�Ba��Bb�HBdQ�Be��Bf�HBh(�Bip�Bj�RBk�Blz�Bmp�Bn=qBo
=Bo�
Bpz�Bq�Bq��Br{BrffBr�\Br�HBs33Bs�Bt  Btz�Bt��Bt��BuG�Bup�BuBv=qBv�\Bv�HBw\)Bw�
BxQ�Bx��Bx��ByG�ByBz=qBz�\B{33B{�B|(�B|��B}�B}��B~{B~ffB~�HB\)B�
B�=qB�z�B���B�
=B�\)B���B��
B�{B�Q�B���B���B�G�B��B��B�(�B�z�B���B�
=B�G�B���B��
B�(�B��\B��HB�33B���B��B�Q�B���B���B�\)B��B�  B�Q�B���B�
=B�\)B��B�{B�ffB��RB��B�p�B�B�(�B��\B��HB�G�B�B�{B�z�B���B�33B��B��
B�=qB���B���B�\)B��B�  B�Q�B���B���B�\)B��B�{B�ffB���B�33B��B��
B�=qB���B���B�\)B��B�  B�Q�B���B���B�G�B���B�  B�Q�B��RB�
=B�\)B�B�{B�z�B���B�33B��B��B�=qB���B���B�\)B��B�{B��\B��HB�G�B���B�  B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�ffB���B�G�B��B�{B��\B�
=B�p�B��B�Q�B���B�33B�B�(�B��\B�
=B��B�  B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�B��RB��B���B�(�B���B��B��B�(�B���B��B��B�(�B���B��B���B�(�B���B��B��B�{B���B��B���B�=qB��RB�33B�B�Q�B���B�\)B��
B�ffB��HB�p�B��B�ffB���B�p�B�  B�z�B���Bə�B�{Bʏ\B��BˮB�(�B̸RB�G�B��
B�Q�B���B�p�B�  BЏ\B�
=BхB�{Bң�B��Bә�B�(�Bԣ�B�33BծB�=qB���B�G�B��
B�ffB���BمB�  B�ffB��HB�G�B��
B�Q�B��HB�p�B��B�ffB���B�G�B�B�=qB���B�G�B��B�ffB���B�\)B�B�=qB���B�\)B��
B�ffB���B�\)B��
B�Q�B���B�\)B��
B�ffB�RB�G�B�B�{B�\B�
=B�B�{B�\B���B�\)B��
B�Q�B�RB�33B�B�(�B�\B���B�p�B��B�ffB���B�33B���B�  B�z�B�
=B�p�B��
B�(�B��\B�
=B��B��B�Q�B��RB��B��B�{B��\B�
=B�\)B�B�(�B���B�33B��C 
=C =qC p�C ��C �C(�C\)C�\CC  C=qCz�C�RC�C�C\)C��C�HC�C\)C�\CC  C=qC�C�RC�C(�CffC�C�C�CG�C�\C��C{CG�Cz�CC	  C	G�C	z�C	�C	�C
=qC
�C
C
��C33Cp�CC
=CG�C�C�RC  CQ�C��C�HC�C\)C��C�C33Cz�CC
=CG�C�\C�
C(�CffC��C�C=qC�C�
C{C\)C��C�C=qC�C��C{CQ�C��C�HC33C�C��C
=CQ�C��C��CG�C�\C�
C�CffCC{C\)C��C�C=qC�\C�HC33Cp�CC�Cp�C�RC  C\)C�RC
=CG�C��C��C Q�C ��C �HC!33C!�\C!�C"33C"z�C"�
C#(�C#�C#��C${C$z�C$��C%{C%ffC%C&�C&ffC&�C'{C'ffC'�C(  C(Q�C(�C(��C)G�C)��C)��C*G�C*�\C*�HC+=qC+�\C+�
C,(�C,�C,�
C-�C-p�C-��C.�C.ffC.�RC/{C/ffC/�C0  C0\)C0�C0��C1G�C1��C2  C2G�C2�\C2��C3G�C3�\C3�C4G�C4�\C4�
C533C5�\C5�
C6(�C6�C6�
C7{C7p�C7��C8{C8\)C8�RC9{C9\)C9�C:  C:\)C:�C:��C;Q�C;�C;��C<G�C<��C<��C==qC=�C=�C>=qC>�\C>�
C?33C?�\C?�HC@33C@z�C@��CA(�CAz�CA��CB�CBz�CBCC{CCp�CCCD
=CDffCDCE{CE\)CE�CF
=CF\)CF��CF��CGQ�CG��CG�CHG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A� �A��A��yA��A��`A囦A�\A�A�~�A�v�A�r�A�hsA�`BA�G�A�=qA�/A�$�A�VA��A�ƨA䛦A�5?A�bA�A۝�Aغ^Aհ!A�oA��A�JAŁA�$�A�$�A�VA��A��9A�"�A���A��A��DA��-A�ȴA�jA�?}A���A�M�A���A��A��A��A�`BA�C�A��#A� �A���A"�A}t�AyoAv�At~�Ao��AjVAg�mAg\)AbjA^�!A]AZ(�AT�RAS?}ARJAO�#AMp�AK�AHffAG`BADĜACC�AA"�A>9XA9�-A8bA4�uA2��A2��A2z�A2n�A2^5A2{A1��A0��A/�TA.5?A-��A-/A,��A,��A,A�A+A*9XA(�A(�\A(�A'A'��A'p�A'%A'
=A&�A%ƨA$�uA#7LA"-A!&�A =qA�wA33A��AM�AJA��A%AE�A�A�
A\)A&�A%A�A�HAȴA�\A(�A��A`BA��AȴA�\AbNAI�A$�A�^A��A�PAx�A\)A"�AffAA�A-A{A1A�;A�
A��A�FAt�A��A(�A�A��A�7AG�A�/A9XA��A�A|�AhsA"�A��A~�A�Al�A"�AoA�`A�jA�A�uA^5AVAVA(�A��A\)Ap�AO�A%A�DA1'A��A�^Al�A
�A	dZA��A�jAQ�A�A��A��A��A�A��A�jA5?A��A�mA��A��A|�AdZA&�A%A��Av�AZAZA$�A{A��A��AdZA+AVA�A1'A��Al�AK�A �yA ��A ffA $�A   @���@�K�@��@��@��@���@��7@���@��F@�C�@�@�v�@��#@���@�X@��j@�1'@�K�@�-@���@�7L@��@�Q�@��m@�+@�V@�`B@�z�@��@��m@�@�C�@��H@@���@�h@��@���@웦@��;@땁@�"�@�+@�J@�@�@�@�^5@��@�hs@�Ĝ@���@�\)@���@�J@�?}@�Ĝ@��;@ߍP@�;d@�V@���@��@�%@��/@� �@�;d@���@�?}@�I�@�|�@�{@Ձ@���@���@�Q�@Ӆ@�
=@ҟ�@�=q@ѩ�@��@�(�@���@�|�@�;d@��@���@�~�@͉7@�&�@̼j@�Q�@��m@�+@�ff@�=q@��@�p�@��@�r�@��m@��H@�v�@�@š�@��`@�A�@�C�@��H@�J@���@�O�@��/@�j@��@�33@�"�@�;d@�S�@�dZ@���@��
@��F@��!@��@�hs@�Ĝ@���@�;d@��!@��-@���@���@�`B@��/@�j@���@�ȴ@��+@�~�@���@���@��+@�n�@�E�@�J@�`B@�?}@�/@��@� �@�t�@�
=@�~�@�E�@��@��j@�9X@��;@��@�V@�x�@���@�(�@�ƨ@�C�@�n�@��T@���@��@�A�@�b@��m@�|�@�
=@�E�@�J@�@��^@�/@���@��9@�I�@�  @�ƨ@���@�K�@���@��+@�@�G�@��`@��j@�bN@�/@�%@��@�9X@�  @�|�@��@���@�M�@�@���@�hs@�Ĝ@���@�Ĝ@���@�9X@��P@��@�n�@��@���@�@�&�@���@��u@�j@�j@�Q�@��@�l�@�^5@���@��7@�?}@��@��@��@���@���@���@�S�@�33@��@�o@��@��!@�E�@�@�`B@�V@�Ĝ@��9@�z�@�(�@��F@�K�@��@��@�ȴ@�ff@�=q@��@�@�@��@���@�X@���@���@���@�\)@�"�@��y@��@��T@��7@�V@���@��j@��D@�Q�@��;@�S�@�
=@��@��@���@���@�v�@�M�@��@��^@��7@��@�hs@��@���@�Z@� �@�  @��@��
@��@���@�l�@�K�@�o@��H@��\@��+@�5?@�@��T@��^@�p�@�G�@��/@��j@�z�@� �@�  @��@l�@K�@;d@
=@~V@}`B@}?}@}/@}�@}V@}V@|��@|�/@{t�@z��@z��@z�\@z^5@z-@y��@yX@y%@x�`@xQ�@w�;@w�P@w+@v�y@v��@vv�@vV@v$�@u�T@u��@u`B@t�@t(�@s��@s"�@r��@r^5@r=q@q��@q��@qhs@qX@q7L@q&�@p��@p�u@p�u@pQ�@o��@oK�@n��@n�@nE�@m�-@l�j@l�@k�m@k�F@kt�@ko@j�!@j�\@j^5@i�#@ihs@h��@hbN@hQ�@g�;@g
=@f5?@e�-@e/@d��@d(�@c�F@ct�@b��@a��@a�7@a7L@`��@`�@`1'@_��@_K�@^�@^E�@]�T@]�h@]�@[��@Z�@Z�!@Z^5@Y��@X��@X�9@X�u@XQ�@W�w@V��@V$�@U��@UO�@T�/@T�D@T9X@Sƨ@R�H@R=q@Qx�@P��@P�u@P1'@O�@O;d@N��@N��@N5?@M�T@M��@MO�@L�/@LZ@L9X@L�@K��@K�
@Kƨ@KS�@K@J��@I��@I%@HbN@G��@G�@G�P@G\)@GK�@F��@FE�@E@EV@Dj@D9X@D1@C�F@CS�@Co@B��@B�!@B�\@B=q@A�#@A�^@AX@@��@?�@?;d@>�@>$�@=O�@<�/@<j@;��@;33@:�@:��@:^5@:=q@:=q@:�@9�^@97L@8��@81'@7�@7��@7�@6ȴ@6�+@5�@5�-@5p�@5O�@5/@4��@4Z@49X@41@3�
@3ƨ@3��@3�@3�@3t�@3@2M�@1�^@1�7@1hs@0��@0��@0bN@0bN@0A�@0A�@01'@01'@0b@/��@/�@/l�@.�@.v�@-@-?}@-�@-�@-V@-V@-V@,�@,�j@,Z@+�m@+�F@+�@+t�@+t�@+�@+�@+t�@+dZ@+S�@+S�@+S�@+@*��@*��@*�!@*��@*�\@*^5@*J@)��@)��@)�7@)hs@)7L@)%@(�`@(��@(A�@'�;@'��@'|�@'
=@&��@&5?@%@%O�@%V@$�/@$�j@$�D@$Z@#�
@#�@#dZ@#dZ@#dZ@#dZ@#S�@#C�@#o@"��@"^5@"J@!�#@!��@!��@!hs@!G�@!�@ �`@ ��@ �@ 1'@  �@��@�w@|�@�@�+@E�@@@�@�T@��@`B@?}@�/@��@j@(�@�m@�F@��@dZ@dZ@S�@S�@33@@n�@-@�#@��@�^@��@hs@hs@X@&�@�`@�@A�@�@��@�@l�@�@��@ȴ@��@�+@V@$�@{@@@@��@�h@�h@�@`B@O�@/@/@�@��@��@�j@Z@ƨ@��@��@�@dZ@@��@��@��@�!@=q@��@hs@7L@%@��@�`@��@��@r�@bN@Q�@Q�@A�@1'@1'@ �@b@�;@�;@�@l�@K�@�@�R@��@��@�+@5?@$�@{@�@@�-@�h@`B@?}@�@�@j@9X@1@�m@ƨ@�F@��@�@�@t�@dZ@S�@33@"�@
�@
�H@
��@
~�@
n�@
^5@
�@	��@	�#@	�^@	�^@	�^@	��@	��@	�7@	x�@	x�@	G�@	&�@	7L@	7L@	&�@	&�@	�@	�@	%@��@	%A��A�$�A�"�A� �A�$�A� �A��A�"�A��A���A��A��A���A敁A�hA��A���A囦A嗍A�hA�DA�DA�DA�A�~�A�~�A�|�A�v�A�r�A�t�A�v�A�p�A�jA�jA�jA�hsA�bNA�ffA�hsA�dZA�`BA�`BA�^5A�XA�XA�XA�O�A�I�A�I�A�G�A�?}A�C�A�C�A�?}A�=qA�A�A�?}A�;dA�;dA�?}A�;dA�9XA�;dA�;dA�5?A�33A�5?A�1'A�/A�-A�-A�(�A�$�A�(�A�(�A�$�A�&�A�(�A�&�A� �A� �A�"�A��A�{A�{A��A�oA�JA�1A�A���A���A��A��A��A��A��A��A��yA��HA��;A���A���A���A�wA���A�jA�FA�FA�9A�A��A䟾A��A䝲A�hA�PA�A�n�A�dZA�^5A�XA�K�A�A�A�7LA� �A��HA�^A�A�+A�dZA�A�A��A��yA╁A�;dA��A�\)A��TAޝ�A�S�A�1'A���A�Q�A���A܃A�=qA���AۅA�{AڑhA�1'AٶFA�n�A���Aغ^A�Q�A��A���A�v�A�+A�x�Aգ�A�Q�A�;dA��HA�+A��
A�ZAҾwA�C�A���A�?}A��AЃA�JA���Aϴ9A�t�A���A�VA�9XA�ȴA���A�ffA���AȋDAǺ^A��A�t�A�-Aş�A�(�A��yAğ�AăA��A�t�A�A�A�A�XA���A���A�E�A�+A� �A��uA�;dA���A��+A���A���A�;dA��-A�+A��hA��;A�~�A���A���A�\)A�(�A�A��A��;A���A��wA��A���A��PA��A�v�A�l�A�\)A�K�A�?}A�5?A�/A�(�A� �A�JA��A��jA�bNA�VA��-A���A���A���A�9XA��TA���A���A�|�A��hA��A��A�M�A�S�A�/A���A��A�?}A�
=A�A��RA��hA��A�^5A�E�A�7LA�9XA�-A� �A�{A��A���A�XA�oA���A��HA��jA��PA�O�A� �A��7A�E�A��7A�bA���A���A�ffA�7LA��A�(�A���A���A���A�?}A���A��wA��hA�hsA�;dA�+A�
=A��/A��wA��A��DA�r�A�VA�/A�{A�  A��A��;A��
A�ȴA��jA��9A���A���A���A���A���A��uA��PA��+A�~�A�v�A�p�A�`BA�O�A�33A� �A���A��#A��RA��+A�+A���A�%A��A�VA���A���A���A��;A��wA���A�XA�+A�ĜA�$�A�A�A��+A��A��hA�n�A���A�~�A�M�A�&�A���A���A��9A��PA�S�A�(�A��yA���A�A�A��`A���A�x�A�$�A���A�$�A���A��A��+A��A�A��A�dZA��A�(�A��^A��A�O�A�1A���A��!A��+A�XA�oA��wA�G�A��`A���A�|�A�Q�A�1A��TA��#A��
A��jA��A�33A��/A�|�A�33A�A��A��A�|�A�9XA�l�A��A���A�G�A�A��A���A���A�n�A�1'A�
=A��wA�Q�A�  A��
A��9A��PA�bNA�-A�t�A�x�A��RA��7A�ffA�1'A�;Al�A;dA�AVA~�A~��A~��A~�DA~I�A}��A}��A}\)A|�RA{�Az�/AzbNAy�Ay%Ax�\Ax~�Aw�Aw��Awp�Av�`Av�!Av��Avn�AvZAvI�Av5?AvbAu�TAu�^AuS�AtZAs;dArv�Aq�-Aq�Ap��Ap^5Ap1Ao\)An��An{AmdZAm�Al��Akl�Ai�^Ai+Ah��AhbNAhA�Ah5?Ah5?Ah �Ag�Ag��Ag�-Ag��Ag��Ag�Ag�Ag��Ag�Agl�Agt�AgS�Af�!Ae�AeAdA�Ac`BAbffAa�A`��A`5?A_�TA_`BA_
=A^�/A^ĜA^��A^�\A^~�A^jA^Q�A^9XA^�A]��A]�#A]��A]�A]��A]�PA]x�A]C�A\��A\�RA\M�A[��A[
=AY�AV�AV  AU�#AU�wAU�AU"�AT�jAT��ATjAT$�AS�ASƨAS��ASt�ASXASC�AS33AS"�AS�AS%AR�`ARĜAR�+ARE�AR(�AR�ARAQ�^AQdZAQ
=AP��AP��APz�APE�AOAO;dAN��AN��ANI�AM��AN1AM�AM��AM;dAL��AL�DALv�ALz�ALn�ALE�ALbAK��AKXAJ�!AI�
AI�AI�AH�\AG�AG�;AG�
AG��AG�^AG��AG�hAGl�AG\)AG;dAG�AF��AF^5AE\)AD�yAD�DAD=qAC��AC�wAC��AC�-AC��AC�PACC�AB�ABbNAA��AA��AA��AA33A@�A@��A@ZA?��A?`BA>��A>�A>1A=`BA<~�A;��A:{A9�hA9t�A9l�A9K�A9+A9VA8��A8r�A8$�A7��A77LA6�A6�A6 �A4M�A3�FA3;dA3�A3oA3oA3
=A2��A2�HA2�HA2��A2��A2��A2��A2�HA2�`A2ȴA2��A2�DA2�A2z�A2bNA2ZA2ZA2^5A2r�A2~�A2n�A2n�A2Q�A2ZA2ZA2bNA2ffA2^5A2M�A25?A2$�A2JA2  A1�A1�;A1��A1�^A1��A1��A1p�A1C�A1oA0�A0ĜA0��A0�DA0z�A0n�A0bNA0M�A0  A/�A/�A.ĜA.~�A.Q�A. �A.A-�A-�;A-��A-�^A-��A-�A-dZA-XA-K�A-7LA-/A-&�A-"�A-
=A,�A,�`A,��A,�RA,�9A,�A,�!A,�RA,��A,��A,�\A,v�A,n�A,jA,^5A,M�A,=qA,�A+�A+�mA+�mA+��A+�-A+��A+�A+dZA+/A)�A)��A)7LA)%A)A)VA(�yA(��A(�!A(��A(��A(��A(��A(�\A(�DA(~�A(M�A(9XA( �A(JA'��A'�A'�A'�#A'ƨA'�FA'�-A'�A'��A'��A'��A'��A'��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 A�$�A� �A��A��yA��A��`A囦A�\A�A�~�A�v�A�r�A�hsA�`BA�G�A�=qA�/A�$�A�VA��A�ƨA䛦A�5?A�bA�A۝�Aغ^Aհ!A�oA��A�JAŁA�$�A�$�A�VA��A��9A�"�A���A��A��DA��-A�ȴA�jA�?}A���A�M�A���A��A��A��A�`BA�C�A��#A� �A���A"�A}t�AyoAv�At~�Ao��AjVAg�mAg\)AbjA^�!A]AZ(�AT�RAS?}ARJAO�#AMp�AK�AHffAG`BADĜACC�AA"�A>9XA9�-A8bA4�uA2��A2��A2z�A2n�A2^5A2{A1��A0��A/�TA.5?A-��A-/A,��A,��A,A�A+A*9XA(�A(�\A(�A'A'��A'p�A'%A'
=A&�A%ƨA$�uA#7LA"-A!&�A =qA�wA33A��AM�AJA��A%AE�A�A�
A\)A&�A%A�A�HAȴA�\A(�A��A`BA��AȴA�\AbNAI�A$�A�^A��A�PAx�A\)A"�AffAA�A-A{A1A�;A�
A��A�FAt�A��A(�A�A��A�7AG�A�/A9XA��A�A|�AhsA"�A��A~�A�Al�A"�AoA�`A�jA�A�uA^5AVAVA(�A��A\)Ap�AO�A%A�DA1'A��A�^Al�A
�A	dZA��A�jAQ�A�A��A��A��A�A��A�jA5?A��A�mA��A��A|�AdZA&�A%A��Av�AZAZA$�A{A��A��AdZA+AVA�A1'A��Al�AK�A �yA ��A ffA $�A   @���@�K�@��@��@��@���@��7@���@��F@�C�@�@�v�@��#@���@�X@��j@�1'@�K�@�-@���@�7L@��@�Q�@��m@�+@�V@�`B@�z�@��@��m@�@�C�@��H@@���@�h@��@���@웦@��;@땁@�"�@�+@�J@�@�@�@�^5@��@�hs@�Ĝ@���@�\)@���@�J@�?}@�Ĝ@��;@ߍP@�;d@�V@���@��@�%@��/@� �@�;d@���@�?}@�I�@�|�@�{@Ձ@���@���@�Q�@Ӆ@�
=@ҟ�@�=q@ѩ�@��@�(�@���@�|�@�;d@��@���@�~�@͉7@�&�@̼j@�Q�@��m@�+@�ff@�=q@��@�p�@��@�r�@��m@��H@�v�@�@š�@��`@�A�@�C�@��H@�J@���@�O�@��/@�j@��@�33@�"�@�;d@�S�@�dZ@���@��
@��F@��!@��@�hs@�Ĝ@���@�;d@��!@��-@���@���@�`B@��/@�j@���@�ȴ@��+@�~�@���@���@��+@�n�@�E�@�J@�`B@�?}@�/@��@� �@�t�@�
=@�~�@�E�@��@��j@�9X@��;@��@�V@�x�@���@�(�@�ƨ@�C�@�n�@��T@���@��@�A�@�b@��m@�|�@�
=@�E�@�J@�@��^@�/@���@��9@�I�@�  @�ƨ@���@�K�@���@��+@�@�G�@��`@��j@�bN@�/@�%@��@�9X@�  @�|�@��@���@�M�@�@���@�hs@�Ĝ@���@�Ĝ@���@�9X@��P@��@�n�@��@���@�@�&�@���@��u@�j@�j@�Q�@��@�l�@�^5@���@��7@�?}@��@��@��@���@���@���@�S�@�33@��@�o@��@��!@�E�@�@�`B@�V@�Ĝ@��9@�z�@�(�@��F@�K�@��@��@�ȴ@�ff@�=q@��@�@�@��@���@�X@���@���@���@�\)@�"�@��y@��@��T@��7@�V@���@��j@��D@�Q�@��;@�S�@�
=@��@��@���@���@�v�@�M�@��@��^@��7@��@�hs@��@���@�Z@� �@�  @��@��
@��@���@�l�@�K�@�o@��H@��\@��+@�5?@�@��T@��^@�p�@�G�@��/@��j@�z�@� �@�  @��@l�@K�@;d@
=@~V@}`B@}?}@}/@}�@}V@}V@|��@|�/@{t�@z��@z��@z�\@z^5@z-@y��@yX@y%@x�`@xQ�@w�;@w�P@w+@v�y@v��@vv�@vV@v$�@u�T@u��@u`B@t�@t(�@s��@s"�@r��@r^5@r=q@q��@q��@qhs@qX@q7L@q&�@p��@p�u@p�u@pQ�@o��@oK�@n��@n�@nE�@m�-@l�j@l�@k�m@k�F@kt�@ko@j�!@j�\@j^5@i�#@ihs@h��@hbN@hQ�@g�;@g
=@f5?@e�-@e/@d��@d(�@c�F@ct�@b��@a��@a�7@a7L@`��@`�@`1'@_��@_K�@^�@^E�@]�T@]�h@]�@[��@Z�@Z�!@Z^5@Y��@X��@X�9@X�u@XQ�@W�w@V��@V$�@U��@UO�@T�/@T�D@T9X@Sƨ@R�H@R=q@Qx�@P��@P�u@P1'@O�@O;d@N��@N��@N5?@M�T@M��@MO�@L�/@LZ@L9X@L�@K��@K�
@Kƨ@KS�@K@J��@I��@I%@HbN@G��@G�@G�P@G\)@GK�@F��@FE�@E@EV@Dj@D9X@D1@C�F@CS�@Co@B��@B�!@B�\@B=q@A�#@A�^@AX@@��@?�@?;d@>�@>$�@=O�@<�/@<j@;��@;33@:�@:��@:^5@:=q@:=q@:�@9�^@97L@8��@81'@7�@7��@7�@6ȴ@6�+@5�@5�-@5p�@5O�@5/@4��@4Z@49X@41@3�
@3ƨ@3��@3�@3�@3t�@3@2M�@1�^@1�7@1hs@0��@0��@0bN@0bN@0A�@0A�@01'@01'@0b@/��@/�@/l�@.�@.v�@-@-?}@-�@-�@-V@-V@-V@,�@,�j@,Z@+�m@+�F@+�@+t�@+t�@+�@+�@+t�@+dZ@+S�@+S�@+S�@+@*��@*��@*�!@*��@*�\@*^5@*J@)��@)��@)�7@)hs@)7L@)%@(�`@(��@(A�@'�;@'��@'|�@'
=@&��@&5?@%@%O�@%V@$�/@$�j@$�D@$Z@#�
@#�@#dZ@#dZ@#dZ@#dZ@#S�@#C�@#o@"��@"^5@"J@!�#@!��@!��@!hs@!G�@!�@ �`@ ��@ �@ 1'@  �@��@�w@|�@�@�+@E�@@@�@�T@��@`B@?}@�/@��@j@(�@�m@�F@��@dZ@dZ@S�@S�@33@@n�@-@�#@��@�^@��@hs@hs@X@&�@�`@�@A�@�@��@�@l�@�@��@ȴ@��@�+@V@$�@{@@@@��@�h@�h@�@`B@O�@/@/@�@��@��@�j@Z@ƨ@��@��@�@dZ@@��@��@��@�!@=q@��@hs@7L@%@��@�`@��@��@r�@bN@Q�@Q�@A�@1'@1'@ �@b@�;@�;@�@l�@K�@�@�R@��@��@�+@5?@$�@{@�@@�-@�h@`B@?}@�@�@j@9X@1@�m@ƨ@�F@��@�@�@t�@dZ@S�@33@"�@
�@
�H@
��@
~�@
n�@
^5@
�@	��@	�#@	�^@	�^@	�^@	��@	��@	�7@	x�@	x�@	G�@	&�@	7L@	7L@	&�@	&�@	�@	�@	%@��G�O�A��A�$�A�"�A� �A�$�A� �A��A�"�A��A���A��A��A���A敁A�hA��A���A囦A嗍A�hA�DA�DA�DA�A�~�A�~�A�|�A�v�A�r�A�t�A�v�A�p�A�jA�jA�jA�hsA�bNA�ffA�hsA�dZA�`BA�`BA�^5A�XA�XA�XA�O�A�I�A�I�A�G�A�?}A�C�A�C�A�?}A�=qA�A�A�?}A�;dA�;dA�?}A�;dA�9XA�;dA�;dA�5?A�33A�5?A�1'A�/A�-A�-A�(�A�$�A�(�A�(�A�$�A�&�A�(�A�&�A� �A� �A�"�A��A�{A�{A��A�oA�JA�1A�A���A���A��A��A��A��A��A��A��yA��HA��;A���A���A���A�wA���A�jA�FA�FA�9A�A��A䟾A��A䝲A�hA�PA�A�n�A�dZA�^5A�XA�K�A�A�A�7LA� �A��HA�^A�A�+A�dZA�A�A��A��yA╁A�;dA��A�\)A��TAޝ�A�S�A�1'A���A�Q�A���A܃A�=qA���AۅA�{AڑhA�1'AٶFA�n�A���Aغ^A�Q�A��A���A�v�A�+A�x�Aգ�A�Q�A�;dA��HA�+A��
A�ZAҾwA�C�A���A�?}A��AЃA�JA���Aϴ9A�t�A���A�VA�9XA�ȴA���A�ffA���AȋDAǺ^A��A�t�A�-Aş�A�(�A��yAğ�AăA��A�t�A�A�A�A�XA���A���A�E�A�+A� �A��uA�;dA���A��+A���A���A�;dA��-A�+A��hA��;A�~�A���A���A�\)A�(�A�A��A��;A���A��wA��A���A��PA��A�v�A�l�A�\)A�K�A�?}A�5?A�/A�(�A� �A�JA��A��jA�bNA�VA��-A���A���A���A�9XA��TA���A���A�|�A��hA��A��A�M�A�S�A�/A���A��A�?}A�
=A�A��RA��hA��A�^5A�E�A�7LA�9XA�-A� �A�{A��A���A�XA�oA���A��HA��jA��PA�O�A� �A��7A�E�A��7A�bA���A���A�ffA�7LA��A�(�A���A���A���A�?}A���A��wA��hA�hsA�;dA�+A�
=A��/A��wA��A��DA�r�A�VA�/A�{A�  A��A��;A��
A�ȴA��jA��9A���A���A���A���A���A��uA��PA��+A�~�A�v�A�p�A�`BA�O�A�33A� �A���A��#A��RA��+A�+A���A�%A��A�VA���A���A���A��;A��wA���A�XA�+A�ĜA�$�A�A�A��+A��A��hA�n�A���A�~�A�M�A�&�A���A���A��9A��PA�S�A�(�A��yA���A�A�A��`A���A�x�A�$�A���A�$�A���A��A��+A��A�A��A�dZA��A�(�A��^A��A�O�A�1A���A��!A��+A�XA�oA��wA�G�A��`A���A�|�A�Q�A�1A��TA��#A��
A��jA��A�33A��/A�|�A�33A�A��A��A�|�A�9XA�l�A��A���A�G�A�A��A���A���A�n�A�1'A�
=A��wA�Q�A�  A��
A��9A��PA�bNA�-A�t�A�x�A��RA��7A�ffA�1'A�;Al�A;dA�AVA~�A~��A~��A~�DA~I�A}��A}��A}\)A|�RA{�Az�/AzbNAy�Ay%Ax�\Ax~�Aw�Aw��Awp�Av�`Av�!Av��Avn�AvZAvI�Av5?AvbAu�TAu�^AuS�AtZAs;dArv�Aq�-Aq�Ap��Ap^5Ap1Ao\)An��An{AmdZAm�Al��Akl�Ai�^Ai+Ah��AhbNAhA�Ah5?Ah5?Ah �Ag�Ag��Ag�-Ag��Ag��Ag�Ag�Ag��Ag�Agl�Agt�AgS�Af�!Ae�AeAdA�Ac`BAbffAa�A`��A`5?A_�TA_`BA_
=A^�/A^ĜA^��A^�\A^~�A^jA^Q�A^9XA^�A]��A]�#A]��A]�A]��A]�PA]x�A]C�A\��A\�RA\M�A[��A[
=AY�AV�AV  AU�#AU�wAU�AU"�AT�jAT��ATjAT$�AS�ASƨAS��ASt�ASXASC�AS33AS"�AS�AS%AR�`ARĜAR�+ARE�AR(�AR�ARAQ�^AQdZAQ
=AP��AP��APz�APE�AOAO;dAN��AN��ANI�AM��AN1AM�AM��AM;dAL��AL�DALv�ALz�ALn�ALE�ALbAK��AKXAJ�!AI�
AI�AI�AH�\AG�AG�;AG�
AG��AG�^AG��AG�hAGl�AG\)AG;dAG�AF��AF^5AE\)AD�yAD�DAD=qAC��AC�wAC��AC�-AC��AC�PACC�AB�ABbNAA��AA��AA��AA33A@�A@��A@ZA?��A?`BA>��A>�A>1A=`BA<~�A;��A:{A9�hA9t�A9l�A9K�A9+A9VA8��A8r�A8$�A7��A77LA6�A6�A6 �A4M�A3�FA3;dA3�A3oA3oA3
=A2��A2�HA2�HA2��A2��A2��A2��A2�HA2�`A2ȴA2��A2�DA2�A2z�A2bNA2ZA2ZA2^5A2r�A2~�A2n�A2n�A2Q�A2ZA2ZA2bNA2ffA2^5A2M�A25?A2$�A2JA2  A1�A1�;A1��A1�^A1��A1��A1p�A1C�A1oA0�A0ĜA0��A0�DA0z�A0n�A0bNA0M�A0  A/�A/�A.ĜA.~�A.Q�A. �A.A-�A-�;A-��A-�^A-��A-�A-dZA-XA-K�A-7LA-/A-&�A-"�A-
=A,�A,�`A,��A,�RA,�9A,�A,�!A,�RA,��A,��A,�\A,v�A,n�A,jA,^5A,M�A,=qA,�A+�A+�mA+�mA+��A+�-A+��A+�A+dZA+/A)�A)��A)7LA)%A)A)VA(�yA(��A(�!A(��A(��A(��A(��A(�\A(�DA(~�A(M�A(9XA( �A(JA'��A'�A'�A'�#A'ƨA'�FA'�-A'�A'��A'��A'��A'��A'��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B)�B*eB)�B)�B&�B.�B&�B"4B!�B#B$tB$B$tB%�B%FB%B&�B+6B2-B7�BD�BK�BU2B{�BB�+B�B	F�B	��B	��B	�B	��B
 B
6�B
f�B
sB
hsB
ffB
yrB
z�B
�@B
�=B
��B
�XB
�
B
�KB
��B
qAB
Q�B
2�B
'RB
"hB
1�B
)�B
kB
�B	�mB	�TB	�B	��B	�B	��B	��B	y	B	s�B	l�B	Y�B	X�B	h�B	hsB	jB	r|B	v�B	��B	�3B	�UB	�}B	�qB	��B	��B	�	B	�	B	��B	�PB	��B	�OB	��B	��B	�?B	�B	�oB	�"B
�B
xB
�B
�B
�B

=B
B
VB
B
B
�B
�B
 �B
 'B
!-B
!-B
!bB
#nB
+kB
*�B
/B
/B
2-B
3�B
4nB
6B
6�B
8B
7�B
8�B
=<B
@�B
@�B
B�B
F?B
F�B
G�B
HB
HB
H�B
IRB
H�B
GzB
H�B
J�B
K^B
N<B
NB
MjB
OB
L�B
L�B
L0B
NB
QNB
T�B
WsB
V�B
W�B
XEB
XEB
X�B
XB
W�B
XEB
XB
[�B
[#B
ZB
Y�B
ZQB
YB
Z�B
Y�B
ZB
X�B
XEB
XB
XB
V�B
U�B
W?B
V�B
T�B
U2B
T�B
T�B
U2B
VB
W?B
V�B
W
B
ZB
W�B
V�B
Y�B
Z�B
[�B
[�B
XB
V9B
T�B
RTB
RTB
I�B
EB
D�B
EB
B�B
A�B
B'B
AUB
?}B
>�B
?HB
?�B
>BB
>B
=�B
=�B
=qB
=qB
?HB
>B
B�B
A�B
A B
C-B
DgB
C�B
D�B
E9B
GEB
F?B
D�B
E�B
CaB
D3B
AUB
@�B
A�B
?�B
?�B
?B
>BB
>wB
=�B
<�B
=B
<jB
:�B
:*B
;dB
9�B
8B
7�B
7LB
7B
5�B
5?B
5tB
33B
4�B
2�B
1�B
0�B
/�B
0�B
/B
/OB
.}B
-�B
,�B
+6B
*�B
+B
+6B
)�B
)�B
)�B
($B
(�B
'B
'RB
'B
%zB
%�B
$�B
$B
%FB
#�B
"�B
"hB
!-B
!�B
 'B
 \B
�B
�B
VB
IB
�B
B
CB
�B
xB
�B
�B
kB
kB
kB
eB
kB
�B
�B
�B
MB
�B
�B
�B
B
B
�B
uB
{B
@B
�B
@B
@B
oB
B
4B
�B
�B
bB
�B
�B
�B
"B
"B
B
�B
PB
B
�B
�B
JB
PB
JB
JB
B
�B
JB
xB
xB
�B
B
JB
~B
�B
�B
�B
PB
"B
�B
�B
4B
B
B
�B
oB
�B
PB
�B

�B
B
�B
4B
�B
@B
B
4B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
YB
SB
SB
�B
_B
YB
�B
�B
�B
�B
�B
�B
B
$B
B
SB
{B
{B
�B
B
{B
�B
�B
SB
�B
B
�B
B
�B
�B
MB
B
�B
{B
�B
�B
B
�B
�B
B
B
�B
�B
FB
B
B
�B
�B
xB
VB
�B
 �B
 �B
!�B
!�B
"hB
"4B
!-B
 �B
�B
 'B
!-B
$B
#�B
#B
$tB
$@B
$tB
%B
%FB
%B
%B
$B
$tB
%FB
%zB
'B
&�B
%�B
%B
"�B
"�B
"4B
!�B
#B
#�B
&�B
'RB
%zB
$�B
$tB
$tB
$tB
$�B
$�B
&�B
&�B
&�B
&�B
'�B
(�B
)*B
)�B
)�B
+6B
,�B
,�B
,�B
,B
+�B
,qB
,qB
/�B
0�B
1[B
2�B
3�B
4�B
4B
4B
4�B
4�B
6FB
5�B
6zB
7B
6�B
7B
7B
7�B
8RB
9$B
9$B
9$B
9�B
9XB
9�B
9�B
9�B
9�B
:�B
:^B
9�B
9�B
:�B
:�B
:�B
:�B
;0B
:�B
;0B
;0B
;0B
:�B
;0B
;�B
<B
<�B
<jB
=�B
=�B
=�B
>B
>BB
>wB
?HB
>�B
?�B
@B
@B
@OB
@�B
@�B
@OB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
AUB
C�B
C�B
CaB
C-B
C-B
CaB
C�B
DgB
DgB
D3B
D�B
E9B
EmB
E�B
E�B
FB
E�B
FB
FB
F?B
F�B
FB
GEB
F�B
G�B
GEB
G�B
HB
HB
HB
H�B
H�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J#B
J#B
K�B
K)B
K)B
K^B
K^B
K�B
L0B
K�B
L0B
L�B
L�B
MjB
M�B
MjB
M�B
N�B
N�B
OBB
OvB
O�B
PHB
P}B
PHB
QB
Q�B
Q�B
R�B
R�B
R�B
R�B
S&B
S[B
S[B
S�B
S�B
T,B
T,B
U�B
UgB
U2B
U2B
VB
V9B
V9B
V9B
V�B
V�B
W�B
XB
XEB
X�B
X�B
X�B
X�B
YKB
ZB
Z�B
[�B
[�B
[�B
\)B
\)B
]/B
\�B
]dB
]dB
]�B
]�B
^B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_;B
^�B
_pB
`vB
`�B
a|B
aHB
a|B
a|B
a|B
aB
bB
bNB
b�B
cTB
c�B
c�B
c�B
d&B
dZB
c�B
d�B
dZB
dZB
d�B
d�B
d�B
e,B
e`B
ffB
f�B
e�B
gmB
h
B
g�B
h
B
iyB
i�B
iyB
i�B
i�B
jB
i�B
i�B
jKB
jB
j�B
k�B
kB
k�B
lWB
l"B
lWB
l�B
l�B
m]B
l�B
m)B
m�B
m�B
m�B
ncB
m�B
ncB
m�B
ncB
m�B
m�B
n�B
o5B
o�B
o�B
o�B
poB
p�B
p�B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q�B
q�B
r�B
sB
sB
sB
sB
sB
sB
sB
sMB
s�B
s�B
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
u%B
u%B
u%B
u%B
uZB
u�B
u�B
u�B
v+B
v+B
v+B
v`B
v+B
v�B
v�B
w2B
v�B
wfB
w�B
xB
x�B
xlB
y	B
x�B
y	B
y	B
yrB
y>B
y�B
zB
y�B
y�B
y�B
y�B
zB
y�B
zB
z�B
{JB
{B
{B
{B
{�B
{�B
|B
{�B
{�B
{�B
|PB
|B
|�B
|�B
|PB
}"B
}�B
}�B
~�B
~�B
~�B
~]B
~�B
~�B
.B
� B
�iB
�B
� B
��B
�B
��B
�B
��B
�B
��B
�B
�;B
�oB
��B
��B
��B
��B
��B
��B
�GB
��B
�{B
�GB
�{B
��B
��B
�MB
��B
�MB
��B
�SB
��B
��B
�B
��B
�SB
��B
��B
��B
��B
��B
�%B
�YB
�YB
�YB
�YB
��B
��B
��B
��B
��B
��B
��B
�_B
�1B
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
�	B
�=B
�rB
��B
��B
��B
��B
��B
�DB
�DB
�B
�xB
�DB
�DB
�xB
��B
�DB
��B
�xB
�~B
��B
�~B
�JB
��B
��B
�B
��B
�PB
��B
��B
��B
��B
��B
��B
��B
�"B
�"B
�(B
��B
��B
��B
��B
�.B
��B
��B
��B
��B
��B
��B
��B
� B
� B
��B
�bB
�4B
��B
�4B
��B
��B
��B
��B
�B
�B
��B
��B
�:B
�B
��B
��B
��B
�B
�oB
�:B
�B
��B
�:B
�:B
��B
��B
��B,=B)*B+6B*�B(�B)�B,�B($B(�B,�B)_B%zB'�B7B!-B,�B'�B&�B �B!�B#:B#B �B!�B#:B#nB"hB$@B$�B#�B"�B$B%FB$�B#nB#�B%zB%B$B%zB&B%�B%zB&�B&�B%�B&�B&LB$�B$B%�B#�B#�B$tB%FB#�B$@B%�B%B$B%�B&�B%�B%FB%�B&B%B%�B'B'RB'RB(XB)�B(�B(�B*�B*�B)�B*eB-CB-�B-�B/�B0UB.�B/�B1�B4nB5B4�B4nB6B6�B6FB6FB8RB9�B8�B9�B<6B=qB@�BEBGEBGBFBGzBH�BH�BG�BI�BK�BK�BJ#BJ�BMjBMBNpBR BR BP�BO�BQ�BQNBP�BU2BbNBg8Bf�Bn�BqvBu�BzDB~�B�~B��B��B��B 4B7LB&�B/OBC�BW�BsB|B�{B��B�OB�LB��B��B�dB�B�cB�/B��B��B	�B	bB	"hB	IB	M�B	HKB	E�B	YB	r�B	iyB	�B	�YB	��B	��B	�'B	��B	��B	�RB	�B	�RB	�*B	��B	�B	�HB	��B	��B	�jB	�B	�KB	�5B	��B	�B	�KB	��B	�|B	�B	�B	�B	��B	��B
�B
5�B
�B
�B
�B
�B
E�B
YB
;0B
@�B
R�B
c�B
y>B
\�B
dZB
p�B
qB
.B
�B
tB
{�B
wfB
t�B
rGB
pB
m�B
m)B
m)B
m�B
l�B
l"B
k�B
kB
k�B
k�B
l�B
lWB
k�B
jB
iB
g�B
gmB
gmB
f�B
jB
lWB
hsB
k�B
d&B
_�B
`vB
^�B
^�B
_pB
v�B
h
B
]dB
]/B
\�B
d�B
U�B
ZQB
j�B
Z�B
j�B
h
B
iyB
zB
qAB
qvB
u�B
w�B
tB
t�B
y	B
x�B
yrB
�B
�{B
�+B
|PB
~(B
y>B
zDB
|�B
n�B
e�B
y�B
��B
~]B
{JB
y	B
zB
yrB
~�B
��B
�B
�B
�oB
��B
��B
�-B
��B
��B
�kB
�B
�@B
��B
��B
�0B
��B
��B
�B
��B
�[B
�3B
�'B
��B
��B
�tB
�B
��B
��B
�jB
��B
��B
��B
�[B
��B
B
ĜB
��B
��B
�XB
̘B
�B
��B
�aB
�B
�NB
��B
��B
��B
ϫB
��B
�vB
� B
�WB
�KB
ܒB
�B
�TB
�B
��B
�QB
�/B
�B
��B
�mB
��B
��B
�'B
��B
��B
��B
�B
��B
�hB
��B
�VB
��B
�lB
~(B
��B
{JB
y>B
n�B
h�B
b�B
e,B
e`B
YKB
V�B
PHB
T,B
I�B
N�B
VmB
M�B
L�B
JXB
9�B
2�B
/�B
2�B
/�B
(XB
($B
&LB
.B
)_B
2�B
+kB
"�B
�B
"�B
�B
�B
7B
_B
�B
!�B
$@B
-wB
33B
0�B
+�B
.IB
-CB
)�B
1[B
8�B
:�B
.}B
2�B
+�B
&�B
($B
'�B
&�B
$B
VB
%�B
%FB
~B
+B
FB
�B
	�B
�B
'�B
.�B
�B	�)B	�B	��B	�B	�5B	�B	�>B	��B	�B	�B	�TB	��B	�&B	� B	ݘB	�BB	�HB	��B	�jB	��B	�&B	�|B	��B	ĜB	��B	��B	��B	��B	��B	�dB	�qB	�XB	�LB	��B	�FB	��B	�-B	��B	ÖB	��B	��B	��B	��B	�-B	��B	�!B	��B	�VB	�B	��B	�	B	��B	��B	��B	�AB	��B	�B	}VB	kB	zxB	z�B	zB	y>B	zxB	y	B	u�B	s�B	sB	w�B	k�B	qAB	qvB	f�B	o B	v�B	x�B	qB	qvB	r|B	jKB	l"B	[�B	^5B	_�B	\�B	X�B	YB	YB	WsB	W�B	WsB	XB	XEB	Y�B	Y�B	YKB	W
B	X�B	XEB	W�B	W?B	Z�B	X�B	X�B	^5B	[#B	e�B	v�B	�B	iB	d�B	d�B	gmB	m)B	h
B	e,B	i�B	iB	h
B	i�B	jKB	hsB	h>B	jB	jB	jKB	j�B	jB	lWB	kB	rB	s�B	q�B	pB	qAB	y>B	sMB	x8B	u�B	rGB	t�B	s�B	{B	}�B	t�B	w�B	�B	��B	��B	�1B	��B	�B	��B	�B	��B	�bB	�$B	�qB	��B	��B	��B	��B	��B	�0B	�aB	�^B	��B	�}B	�BB	��B	��B	��B	��B	�UB	��B	�}B	�B	�HB	�BB	ȀB	��B	��B	�B	�tB	�9B	�B	��B	�UB	��B	��B	�hB	��B	�aB	��B	�=B	�CB	��B	��B	��B	��B	�LB	��B	��B	�$B	�B	�SB	��B	��B	�+B	��B	�oB	��B	��B	�GB	�B	�_B	�AB	~(B	��B	~(B	zB	��B	�B	�_B	� B	�:B	��B	��B	��B	�YB	��B	��B	�B	��B	�IB	�VB	�!B	��B	��B	��B	��B	�@B	�hB	��B	�B	��B	�zB	��B	�6B	�wB	�-B	�<B	҉B	��B	�
B	��B	�vB	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�/B	�B	��B	�rB	��B	��B	��B
 4B	��B	��B
�B
B
�B
B
�B
.B
DB
xB
~B

�B
fB
�B
	lB
�B
�B
	B
�B
�B
�B
+B
_B
+B
�B
�B
	B
_B
_B

=B
	7B
�B
+B
B
	�B
�B

�B
�B
B

�B
DB
�B
PB
:B
�B
B
�B
�B
(B
�B
�B
�B
�B
-�B
4B
�B
�B
~B

	B
qB
�B
B
�B
$B
�B
qB
B
�B
B
!-B
�B
 \B
 \B
 'B
�B
�B
 \B
!�B
 �B
�B
 �B
 �B
�B
�B
 \B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 B*qB+MB,]B-TB-�B0�B'eB"�B"OB#]B$�B$EB$�B&VB%�B%dB'-B+�B2�B8�BE�BNiB]B�vB(5B�RB�gB	Y�B	��B	��B	�>B
}B
,�B
TEB
~�B
��B
} B
s�B
��B
��B
�8B
��B
��B
�QB
�FB
��B
��B
�VB
h�B
@�B
1�B
0YB
@�B
6B
/�B
aB	�{B	�B	�B	�B	�$B	�B	��B	|?B	��B	x\B	^JB	e�B	yvB	m�B	n{B	y�B	pB	�DB	�	B	�wB	��B	��B	�pB	�B	��B	��B	�yB	��B	�{B	�kB	�B	��B	�B	��B	�pB
 �B
VB
�B

B
	$B
	fB
�B
;B
�B
�B
(B
OB
!B
!B
 �B
"�B
!EB
" B
'wB
/�B
/rB
2�B
2�B
5cB
5nB
6SB
7vB
8{B
9B
8�B
;bB
?�B
A8B
A�B
D9B
F�B
GRB
H
B
HNB
H�B
IlB
J�B
I�B
IB
JB
KQB
L/B
N�B
NmB
NB
PaB
L�B
MB
L�B
NB
RHB
W6B
W�B
W,B
XB
X~B
X�B
X�B
X0B
XZB
YOB
Z�B
^B
\B
Z�B
Z�B
[IB
[B
]?B
[CB
[B
X�B
X�B
YB
Y�B
W�B
WB
Y�B
W�B
T�B
U�B
U�B
UCB
U�B
V�B
W_B
V�B
W�B
\B
X�B
V�B
Z0B
\
B
]�B
]B
X�B
W9B
U�B
T}B
WVB
K�B
E�B
F=B
E�B
CkB
C{B
DRB
A�B
?�B
?eB
A>B
@�B
>�B
>gB
>OB
>=B
=�B
>RB
?�B
?B
D
B
BXB
AFB
C�B
D�B
D�B
E�B
FB
HB
F�B
F;B
G�B
D�B
E�B
A�B
BB
B�B
@�B
@�B
?�B
>�B
?B
>
B
=8B
>HB
=6B
;aB
;�B
=XB
:�B
8�B
8�B
8[B
7�B
67B
6XB
6�B
4�B
6�B
3�B
2�B
1XB
1B
1cB
0}B
0�B
0QB
/MB
-^B
+�B
+LB
+�B
+�B
*�B
*�B
*�B
(�B
) B
'�B
(�B
'�B
&^B
'B
%�B
&�B
'RB
$�B
#�B
#@B
"5B
"�B
!�B
!6B
�B
 �B
 �B
OB
�B
�B
�B
mB
{B
�B
B
�B
�B
/B
�B
�B
�B
eB
RB
gB
�B
B
�B
wB
B
�B
CB
�B
�B
FB
�B
�B
�B
LB
�B
�B
HB
$B
eB
�B
�B
�B
�B
yB
�B
?B
�B
�B
B
B
(B
0B
B
vB
�B
B
XB
B
wB
�B
*B
gB
=B
�B
�B
,B
�B
�B
#B
�B
tB
�B
!B
zB
�B
ZB
�B
�B
�B
�B
UB
`B
@B
�B
�B
AB
B
�B
�B
�B
"B
�B
�B
B
B
�B
�B
�B
B
�B
6B
�B
HB
�B
�B
�B
UB
�B
�B
�B
�B
�B
KB
�B
�B
�B
yB
�B
�B
�B
�B
�B

B
�B
gB
�B
DB
�B
1B
'B
�B
�B
�B
QB
�B
�B
�B
%B
=B
�B
qB
hB
�B
�B
 BB
�B
!DB
!�B
"�B
"KB
"�B
"�B
!�B
!�B
!#B
 nB
!B
$YB
$lB
$VB
%�B
%IB
%ZB
%YB
%�B
&?B
%�B
$�B
$�B
%RB
%�B
(TB
',B
'�B
&*B
#cB
#*B
"�B
"�B
#TB
#�B
(B
( B
&
B
$�B
$�B
$�B
$�B
%>B
%�B
'�B
'�B
'+B
'vB
'�B
)gB
)�B
*�B
*�B
+�B
,�B
-B
-�B
,dB
,cB
,�B
,B
0B
1<B
2XB
3�B
4�B
5CB
4�B
4~B
53B
6TB
6�B
6�B
7eB
7�B
7B
7zB
7�B
8bB
9VB
9�B
9]B
9ZB
9�B
9�B
9�B
9�B
:*B
:�B
:�B
:yB
:5B
:lB
;mB
;�B
;<B
:�B
;UB
;6B
;�B
;hB
;�B
;B
;�B
<B
<�B
<�B
=	B
><B
>"B
>4B
>�B
>�B
?DB
?�B
?lB
@�B
@bB
@�B
@�B
@�B
@�B
@�B
A=B
B�B
BB
BB
BB
BB
A�B
A�B
A�B
B�B
DdB
C�B
CzB
CbB
ChB
C�B
DmB
D�B
D�B
D�B
EB
E�B
E�B
FB
E�B
F?B
E�B
FAB
FNB
F�B
F�B
F�B
G�B
GoB
H#B
G�B
HPB
HCB
H_B
HxB
H�B
H�B
HpB
H�B
H�B
IB
H�B
H�B
I�B
JB
I�B
I�B
JB
J�B
KB
L,B
K^B
KaB
K�B
K�B
LZB
LWB
L9B
L�B
M>B
MGB
M�B
M�B
M�B
N�B
OxB
O0B
O�B
PB
PB
P�B
P�B
P�B
Q�B
RbB
RB
SB
R�B
R�B
SSB
S�B
S�B
S�B
TZB
TB
T�B
UKB
V�B
U�B
U�B
U�B
V�B
V�B
VbB
V�B
W;B
WgB
XnB
XjB
X�B
Y!B
YB
Y:B
YhB
Z+B
Z�B
[JB
\>B
[�B
\[B
\zB
\�B
]vB
]0B
]�B
]�B
]�B
^%B
^xB
^�B
^�B
^�B
^�B
^�B
^�B
_B
_�B
_JB
`oB
aB
aNB
bB
asB
a�B
a�B
a�B
atB
b�B
b�B
c7B
c�B
c�B
c�B
c�B
d�B
d�B
d4B
d�B
d�B
d�B
eYB
e%B
d�B
e�B
fB
gB
gB
f�B
h=B
h�B
hYB
h�B
i�B
i�B
i�B
j%B
jB
jB
jB
jJB
j�B
j�B
k�B
l.B
kTB
l0B
l�B
lpB
l�B
mB
mB
m~B
l�B
m�B
n:B
m�B
m�B
n�B
nB
n�B
nB
ngB
nB
n@B
oJB
o�B
o�B
o�B
pB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
p�B
q�B
rB
rWB
s-B
s<B
sB
s+B
sB
s B
s@B
sSB
s�B
t'B
t!B
t�B
t�B
t�B
t~B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u1B
u_B
u9B
u9B
u<B
u\B
u�B
v#B
vB
vB
vOB
v_B
v`B
v�B
vuB
v�B
wZB
wLB
wUB
w�B
xB
x~B
yB
x�B
yIB
yB
y0B
y@B
y�B
y�B
y�B
z0B
y�B
y�B
y�B
y�B
z%B
zB
z�B
z�B
{�B
{KB
{�B
{�B
|%B
{�B
|MB
|B
{�B
|B
|�B
|6B
|�B
|�B
|�B
}�B
~GB
~	B
~�B
~�B
~�B
~yB
:B
=B
[B
�]B
��B
�B
�FB
��B
�6B
��B
�7B
��B
�B
��B
�0B
�wB
� B
�!B
�,B
��B
��B
��B
�B
�KB
��B
��B
��B
��B
�)B
�7B
�sB
��B
��B
�B
�wB
� B
��B
�EB
��B
��B
�B
�B
��B
�2B
�B
�8B
�^B
�nB
�{B
�pB
��B
��B
��B
��B
��B
�B
�*B
��B
�bB
��B
�B
�%B
�`B
��B
��B
��B
��B
�B
�7B
�?B
�qB
��B
��B
��B
��B
�B
�B
�WB
�VB
�B
��B
�WB
�JB
��B
��B
�vB
��B
��B
��B
��B
��B
��B
��B
��B
�2B
�8B
�fB
��B
��B
��B
��B
�B
�"B
��B
�PB
��B
�gB
�(B
��B
��B
��B
�?B
��B
��B
��B
�B
��B
��B
��B
�B
�0B
��B
��B
�VB
��B
�MB
�B
��B
��B
��B
�	B
�
B
��B
��B
�NB
�B
��B
�	B
��B
� B
�tB
�PB
�B
��B
�?B
�NB
��B
��G�O�B,=B)*B+6B*�B(�B)�B,�B($B(�B,�B)_B%zB'�B7B!-B,�B'�B&�B �B!�B#:B#B �B!�B#:B#nB"hB$@B$�B#�B"�B$B%FB$�B#nB#�B%zB%B$B%zB&B%�B%zB&�B&�B%�B&�B&LB$�B$B%�B#�B#�B$tB%FB#�B$@B%�B%B$B%�B&�B%�B%FB%�B&B%B%�B'B'RB'RB(XB)�B(�B(�B*�B*�B)�B*eB-CB-�B-�B/�B0UB.�B/�B1�B4nB5B4�B4nB6B6�B6FB6FB8RB9�B8�B9�B<6B=qB@�BEBGEBGBFBGzBH�BH�BG�BI�BK�BK�BJ#BJ�BMjBMBNpBR BR BP�BO�BQ�BQNBP�BU2BbNBg8Bf�Bn�BqvBu�BzDB~�B�~B��B��B��B 4B7LB&�B/OBC�BW�BsB|B�{B��B�OB�LB��B��B�dB�B�cB�/B��B��B	�B	bB	"hB	IB	M�B	HKB	E�B	YB	r�B	iyB	�B	�YB	��B	��B	�'B	��B	��B	�RB	�B	�RB	�*B	��B	�B	�HB	��B	��B	�jB	�B	�KB	�5B	��B	�B	�KB	��B	�|B	�B	�B	�B	��B	��B
�B
5�B
�B
�B
�B
�B
E�B
YB
;0B
@�B
R�B
c�B
y>B
\�B
dZB
p�B
qB
.B
�B
tB
{�B
wfB
t�B
rGB
pB
m�B
m)B
m)B
m�B
l�B
l"B
k�B
kB
k�B
k�B
l�B
lWB
k�B
jB
iB
g�B
gmB
gmB
f�B
jB
lWB
hsB
k�B
d&B
_�B
`vB
^�B
^�B
_pB
v�B
h
B
]dB
]/B
\�B
d�B
U�B
ZQB
j�B
Z�B
j�B
h
B
iyB
zB
qAB
qvB
u�B
w�B
tB
t�B
y	B
x�B
yrB
�B
�{B
�+B
|PB
~(B
y>B
zDB
|�B
n�B
e�B
y�B
��B
~]B
{JB
y	B
zB
yrB
~�B
��B
�B
�B
�oB
��B
��B
�-B
��B
��B
�kB
�B
�@B
��B
��B
�0B
��B
��B
�B
��B
�[B
�3B
�'B
��B
��B
�tB
�B
��B
��B
�jB
��B
��B
��B
�[B
��B
B
ĜB
��B
��B
�XB
̘B
�B
��B
�aB
�B
�NB
��B
��B
��B
ϫB
��B
�vB
� B
�WB
�KB
ܒB
�B
�TB
�B
��B
�QB
�/B
�B
��B
�mB
��B
��B
�'B
��B
��B
��B
�B
��B
�hB
��B
�VB
��B
�lB
~(B
��B
{JB
y>B
n�B
h�B
b�B
e,B
e`B
YKB
V�B
PHB
T,B
I�B
N�B
VmB
M�B
L�B
JXB
9�B
2�B
/�B
2�B
/�B
(XB
($B
&LB
.B
)_B
2�B
+kB
"�B
�B
"�B
�B
�B
7B
_B
�B
!�B
$@B
-wB
33B
0�B
+�B
.IB
-CB
)�B
1[B
8�B
:�B
.}B
2�B
+�B
&�B
($B
'�B
&�B
$B
VB
%�B
%FB
~B
+B
FB
�B
	�B
�B
'�B
.�B
�B	�)B	�B	��B	�B	�5B	�B	�>B	��B	�B	�B	�TB	��B	�&B	� B	ݘB	�BB	�HB	��B	�jB	��B	�&B	�|B	��B	ĜB	��B	��B	��B	��B	��B	�dB	�qB	�XB	�LB	��B	�FB	��B	�-B	��B	ÖB	��B	��B	��B	��B	�-B	��B	�!B	��B	�VB	�B	��B	�	B	��B	��B	��B	�AB	��B	�B	}VB	kB	zxB	z�B	zB	y>B	zxB	y	B	u�B	s�B	sB	w�B	k�B	qAB	qvB	f�B	o B	v�B	x�B	qB	qvB	r|B	jKB	l"B	[�B	^5B	_�B	\�B	X�B	YB	YB	WsB	W�B	WsB	XB	XEB	Y�B	Y�B	YKB	W
B	X�B	XEB	W�B	W?B	Z�B	X�B	X�B	^5B	[#B	e�B	v�B	�B	iB	d�B	d�B	gmB	m)B	h
B	e,B	i�B	iB	h
B	i�B	jKB	hsB	h>B	jB	jB	jKB	j�B	jB	lWB	kB	rB	s�B	q�B	pB	qAB	y>B	sMB	x8B	u�B	rGB	t�B	s�B	{B	}�B	t�B	w�B	�B	��B	��B	�1B	��B	�B	��B	�B	��B	�bB	�$B	�qB	��B	��B	��B	��B	��B	�0B	�aB	�^B	��B	�}B	�BB	��B	��B	��B	��B	�UB	��B	�}B	�B	�HB	�BB	ȀB	��B	��B	�B	�tB	�9B	�B	��B	�UB	��B	��B	�hB	��B	�aB	��B	�=B	�CB	��B	��B	��B	��B	�LB	��B	��B	�$B	�B	�SB	��B	��B	�+B	��B	�oB	��B	��B	�GB	�B	�_B	�AB	~(B	��B	~(B	zB	��B	�B	�_B	� B	�:B	��B	��B	��B	�YB	��B	��B	�B	��B	�IB	�VB	�!B	��B	��B	��B	��B	�@B	�hB	��B	�B	��B	�zB	��B	�6B	�wB	�-B	�<B	҉B	��B	�
B	��B	�vB	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�/B	�B	��B	�rB	��B	��B	��B
 4B	��B	��B
�B
B
�B
B
�B
.B
DB
xB
~B

�B
fB
�B
	lB
�B
�B
	B
�B
�B
�B
+B
_B
+B
�B
�B
	B
_B
_B

=B
	7B
�B
+B
B
	�B
�B

�B
�B
B

�B
DB
�B
PB
:B
�B
B
�B
�B
(B
�B
�B
�B
�B
-�B
4B
�B
�B
~B

	B
qB
�B
B
�B
$B
�B
qB
B
�B
B
!-B
�B
 \B
 \B
 'B
�B
�B
 \B
!�B
 �B
�B
 �B
 �B
�B
�B
 \B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<�"�<�)=<�q�<�h�<�-=�L<�9<��<���<�N<Ŕ�<i7<�p�<cO�<�"w<��<@�Y<#�
<#�
<#�
<���<�@�<�;�<���<��G<oC�<:b<l�'<~ٔ<R�|<��S<[7@<#�
<cF+<#�
<#�
<��^<���<#�
<#�
<�S�<Ie%<#�
<[�<�4�<#�
<#�
<#�
<#�
<#�
<=�t<#�
<#�
<#�
<#�
<7��<x�,<#�
<L[�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020110119085620201101190856IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400410420210224004104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400410420210224004104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714020120210427140201IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                