CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-12-16T14:01:38Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     8  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     8  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �P   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ɉ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 >�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 e�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221216140138  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_241                 6810_008521_241                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��d�@��d�11  @��8�YK@��8�YK@2k�׈�@2k�׈��d��*0U�d��*0U11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?��H@B�\@z�H@��R@��R@�  AG�A�A!G�A,��AAG�A`  A~�RA��A��A��A�  AϮA߮A�A��B  BQ�B(�B z�B'�
B/�B7�
B?�
BH  BP(�BW�
B_�
Bg�
Bo�Bw�
B�{B�  B��B�{B��B��B�  B�  B�  B�{B�{B�{B�(�B�{B��B��B�  B��B��B�{B�(�B�(�B�(�B�{B�  B�  B�(�B�{B��B�  B�{B�  C   C  C
=C  C��C
  C  C��C  C
=C  C
=C
=C  C  C
=C 
=C!��C$
=C&{C(  C*  C,
=C.  C0
=C2
=C3��C5��C8  C:  C;��C>  C@
=CB{CD�CF
=CH
=CJ  CL  CN{CP{CR  CT
=CV
=CW��CZ
=C\
=C^
=C`  Cb  Cc��Ce��Ch  Cj  Cl  Cm��Co��Cr  Ct  Cv  Cx
=Cz
=C{��C~  C�C�C�  C���C���C���C�C�C�  C�  C���C���C�
=C���C�  C�
=C�  C�  C�  C�C�  C�C�C�C�C�  C�C�\C�
=C�C�  C�  C���C���C���C�  C�  C�  C�  C�  C�C�C�C�C�C�
=C�  C�  C���C�
=C�
=C�C�C�  C���C�  C�C�  C�  C�  C�  C�C�C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C�C���C���C���C�  C���C���C���C�  C�
=C�
=C�C���C���C�  C�  C�C�C�C�
=C�C���C���C���C�  C�
=C�C���C���C���C���C���C���C�  C�C�  C�
=C�
=C�  C�C�C���C���C���C���C���C�C�  C���C�  C�  D �D ��D  D}qD  D��D  D� D  Dz�D�qDz�D�qD� D�qD� D�D��D	D	��D
  D
�DD}qD��D}qD�qDz�D�qD� D�qDz�D�qD}qD��Dz�D�qD��D�D��D�qD}qD�qD� D  D��D  D}qD  D��D  Dz�D��Dz�D�qD� D�D��D�qD}qD�D�DD�D   D }qD ��D!}qD!�qD"}qD#  D#}qD#��D$}qD$�qD%}qD&  D&��D&�qD'� D(  D(}qD(��D)� D*  D*� D*�qD+� D+�qD,z�D-  D-��D.  D.� D/  D/��D0�D0}qD1  D1� D1�qD2}qD2��D3� D4�D4� D5�D5��D6D6��D6�qD7� D7�qD8}qD8�qD9� D:�D:�D;D;� D<  D<}qD<�qD=� D>  D>� D?�D?��D@  D@� DA�DA}qDA�qDB}qDC�DC��DDDD� DE�DE��DF�DF� DG  DG}qDH�DH�DIDI� DI�qDJ� DK  DK��DK�qDL}qDM  DM}qDM��DN}qDN�qDO� DP�DP��DQ�DQ� DR  DR}qDR��DS}qDS�qDT� DT�qDU� DV�DV��DW  DW}qDX  DX}qDX�qDY}qDY�qDZ}qD[  D[}qD\�D\� D]  D]� D]�qD^}qD^��D_��D`  D`� DaDa� Db  Db}qDc  Dc��Dd  Dd� De�De}qDe�qDf}qDf�qDg}qDh�Dh�Di  DixRDi�qDj��Dk  Dk� Dk�qDl}qDl��Dm}qDn  Dn� Do�Do� Do�qDp��Dq�Dq��Dr�Dr��Ds  Ds� Dt�Dt� Du  Du��Dv  Dv}qDw  Dw� Dx�Dxz�Dx��Dyz�Dy�qDz��D{�D{� D{�qD|}qD}�D}� D}�qD~}qD~�qD��D�  D�@ D�� D���D��qD�@ D��HD��HD��D�C�D��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D���D���D�>�D�~�D���D��qD�@ D�� D��qD��qD�>�D��HD��HD�  D�>�D�~�D���D�  D�@ D�� D���D�  D�@ D��HD�D�HD�@ D��HD��HD���D�=qD��HD��HD�  D�AHD�� D���D�  D�@ D�� D��HD�  D�>�D�� D���D�  D�AHD��HD��HD�HD�B�D���D�� D�HD�AHD�� D��HD�HD�@ D�~�D���D�  D�>�D�}qD�� D�  D�@ D�� D�� D���D�@ D�� D��HD�HD�@ D��HD���D��qD�>�D�� D��HD�HD�>�D�� D���D���D�@ D��HD��HD�  D�@ D�� D��HD�HD�>�D�~�D��HD�  D�>�D�� D��qD��qD�@ D�~�D�� D�HD�@ D�� D��HD��D�AHD�� D�� D�  D�AHD�~�D���D�HD�AHD�� D�� D���D�>�D�}qD�� D�  D�@ D��HD�� D���D�>�D�~�D��qD���D�>�D�~�D���D��qD�@ D��HD��HD���D�=qD�~�D��HD�HD�@ D�� D��HD�HD�>�D�� D�� D���D�AHD��HD�� D�HD�AHD�� D�� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�=qD�� D�D�HD�>�D�~�D�� D���D�=qD�~�D��HD��D�B�D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�~�D��HD�  D�>�D��HD��HD�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�@ D��HD�D�  D�@ D�� D��HD�HD�>�D�~�D��HD�HD�@ D�� D�D�HD�AHD�� D���D���D�<)D�~�D�� D���D�=qD�� D�� D���D�>�D�� D��HD�  D�@ D D�� D�  D�>�D�~�D�� D���D�@ DĀ D��HD�  D�@ Dŀ D�� D���D�<)D�}qD�� D��D�B�Dǀ DǾ�D�  D�@ DȁHD��HD�HD�B�Dɀ Dɾ�D��qD�=qD�~�D��HD��D�B�DˁHD˾�D�  D�B�D́HD��HD���D�>�D̀ D�� D�  D�AHD΁HD�� D�HD�AHDς�D�� D��qD�>�D�~�Dо�D���D�>�DсHD��HD�  D�@ DҁHD��HD�HD�AHDӀ D��HD�  D�>�D�~�DԾ�D�  D�AHDՀ Dվ�D�  D�@ DցHD��HD�HD�@ D�~�D�� D�  D�>�D�~�DؽqD���D�@ Dـ D��HD�  D�>�Dڀ DڽqD�  D�B�DہHD�� D�HD�AHD܂�D��HD�  D�>�D�~�D�� D�HD�@ Dހ D�� D�  D�AHD߁HD߾�D���D�@ D�� D�� D�  D�@ D� D��HD��D�AHD�HD�� D���D�=qD� D㾸D���D�@ D�~�D侸D��qD�<)D� D�� D�  D�>�D� D��HD�  D�>�D� D��HD�  D�>�D� D辸D���D�>�D�~�D龸D���D�@ D�HD��HD��D�@ D�~�D��HD��D�@ D� D�� D���D�>�D�HD��HD�HD�@ D� D��HD�  D�@ D�~�D�qD���D�@ D��HD�� D�  D�B�D��D�D��D�AHD�HD��HD�  D�>�D� D�D�HD�@ D� D�� D�  D�AHD��HD��HD���D�>�D�� D���D���D�>�D�~�D�� D��D�:�?�?\)?L��?�z�?\?�
=@�@z�@(��@@  @O\)@aG�@z�H@��
@���@��H@��\@�{@�@��R@˅@�@�p�@�=q@��@��RAz�A
=Ap�A�\AA�HA ��A#�
A)��A-p�A333A6ffA:�HA@��AC�
AJ=qAMp�AQ�AW
=AZ=qA`��Ac�
Aj=qAmp�As�
Ax��A|��A���A��A��RA���A�33A��RA���A��A�{A�  A��
A�A���A��HA��A�Q�A�=qA�A�\)A��\A���A�  A�=qA�z�A��A���A��
A�\)A���A�z�A�{A�G�AӅA�p�Aأ�A�=qA�A߮A��A�z�A�RA�G�A�33A�ffA�  A��HA�p�A�\)A��\A�z�A�
=B ��B�B\)B��B��B\)B(�B	B
�HB�
Bp�B�\B�B��B=qB
=B��Bp�B�HB  B��B�\B\)B��BB�RB z�B!�B"�\B#�B$z�B&{B&�HB((�B)��B*ffB,  B,��B.=qB/\)B0z�B1�B2�RB4Q�B5p�B6=qB7�B8��B9B;33B<(�B=p�B>�HB?�BAG�BB{BC�BD��BE��BG\)BHQ�BIp�BK
=BK�
BMG�BNffBO33BP��BR{BR�HBTz�BUBV�RBXQ�BYG�BZ�\B\  B\��B^ffB_�B`z�Bb{Bc
=Bd(�Be��BfffBg�Bi�Bj=qBk33Bl��Bm�Bn�HBpQ�Bq��Br�\Bt  Bu�Bv{Bw�Bx��By�B{�B|Q�B}�B~�HB�{B��HB�G�B�  B���B��B�  B�Q�B���B��B�(�B���B�p�B��B�z�B�33B���B�ffB���B�p�B�=qB��RB��B�=qB��RB�\)B�(�B���B�p�B�{B���B�p�B�(�B��RB��B�{B���B���B�{B��HB��B�{B���B��B�{B���B���B�=qB�
=B���B�ffB�33B��B�ffB�G�B�B���B�p�B��B��RB�p�B��B��HB�p�B�{B���B��B�Q�B���B��B�ffB��B��B���B��B��
B���B��B�  B��\B�33B�  B��\B��B�  B���B��B�  B��RB�33B�B���B��B��
B���B��B�B���B�
=B�B�z�B���BîB�z�B��HBŮB�ffB��HBǮB�Q�B���BɅB�(�Bʏ\B�\)BˮB�Q�B���B�G�B��B�ffBθRB�\)Bϙ�B�{BЏ\BиRB��Bљ�B�B�{B�z�Bҏ\B���B�33B�\)B�B��
B�(�B�z�Bԏ\B��HB�33B�33BծB��
B�  B�ffB�z�B֣�B�
=B�33B�G�B׮B��
B�  B�ffB�z�Bأ�B���B�33B�G�Bٙ�B��B�  B�=qBڏ\Bڏ\B���B��B�33B�p�B��
B�{B�{B�z�Bܣ�B���B��B�33B�p�B��
B��B�=qB�z�Bޏ\B��HB�33B�G�Bߙ�B�  B�(�B�Q�B���B���B��BᙚB��B�  B�ffB���B��HB�\)B㙚B�B�(�B�z�B��B��B�B噚B��
B�Q�B��B���B��B癚B��
B�  B�Q�B���B��HB�33B陚B�B�  B�ffB���B��HB��B�B��B�(�B�=qB��B�
=B��B�\)B��
B�{B�=qB��B�
=B�33B�\)B��
B�=qB�ffB��\B�
=B�\)B�p�B��
B�Q�B�z�B��B�33B�p�B�B�(�B�Q�B�\B�
=B�p�B��B��B�Q�B���B���B�G�B��
B�{B�Q�B���B��B�G�B�B�(�B�Q�B���B��B�\)B���B�{B�ffB���B�
=B�p�B�B��B�Q�B���B���B�\)B��B��
C �C 33C \)C �\C ��C �
C
=C{CG�Cz�C�CC�C  C=qC\)Cz�C�RC�
C  C=qCQ�Cz�C�RC�
C  C=qCQ�Cp�C�RC�HC��C33CffCz�C�RC�HC  C(�CffC�\C�C��C�CG�Cp�C�RC�
C��C�C\)C��C�RC�HC	(�C	G�C	ffC	��C	�
C	��C
(�C
\)C
�C
��C
�HC�C=qCp�C�\C�RC��C33CG�C�C�RC�HC
=CG�Cz�C��C��C��C33Cz�C��CC
=CG�Cp�C�\C��C
=C=qCp�C�C�C��C=qCQ�Cz�C�RC�C
=C=qCp�C��C�HC��C�CffC�\C�C�C�C\)C�C��C��C
=C=qCp�C�CC  C(�CG�Cz�C�RC�
C  C33Cp�C��C��C��C{CG�C�CC�
C
=C33Cp�C�C�HC  C(�CQ�C��C��C��C�CG�Cz�C�C�C(�C\)Cz�C��C��C{CQ�C�C��C��C  C=qCz�C��C��C��C=qCp�C�\CC 
=C 33C Q�C z�C �RC ��C!�C!=qC!p�C!��C!�HC"{C"=qC"ffC"�C"C"��C#33C#Q�C#z�C#��C#�
C${C$Q�C$z�C$�C$��C$��C%=qC%p�C%�C%�
C&  C&(�C&ffC&��C&�HC'  C'(�C'\)C'��C'��C(
=C(G�C(ffC(�\C(C)  C)G�C)p�C)�\C)C*  C*G�C*p�C*�\C*��C+  C+=qC+�C+�C+�HC,{C,=qC,z�C,�RC,��C-(�C-\)C-�C-��C.
=C.G�C.z�C.��C.��C/  C/G�C/�\C/C/�C0�C0G�C0�C0C1  C1=qC1z�C1��C1�
C2
=C2=qC2�C2��C2��C3�C3Q�C3�\C3�
C4{C4=qC4ffC4��C4�
C5(�C5\)C5�C5�C5�HC6(�C6p�C6�C6�
C7
=C7=qC7z�C7C8  C8(�C8ffC8�\C8C9
=C9Q�C9��C9C9��C:33C:p�C:�RC:��C;33C;p�C;��C;��C<  C<Q�C<��C<�
C=
=C==qC=p�C=�C=�C>33C>z�C>��C>�HC?{C?G�C?��C?��C@
=C@=qC@ffC@��C@�
CA{CAQ�CA�\CA�
CB
=CB=qCBp�CB��CB�HCC�CC\)CC��CC�HCD{CDG�CD�CD�CD�HCE�CEQ�CE��CE�
CF{CFQ�CF�\CFCF��CG(�CGp�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   ?k�?��H@B�\@z�H@��R@��R@�  AG�A�A!G�A,��AAG�A`  A~�RA��A��A��A�  AϮA߮A�A��B  BQ�B(�B z�B'�
B/�B7�
B?�
BH  BP(�BW�
B_�
Bg�
Bo�Bw�
B�{B�  B��B�{B��B��B�  B�  B�  B�{B�{B�{B�(�B�{B��B��B�  B��B��B�{B�(�B�(�B�(�B�{B�  B�  B�(�B�{B��B�  B�{B�  C   C  C
=C  C��C
  C  C��C  C
=C  C
=C
=C  C  C
=C 
=C!��C$
=C&{C(  C*  C,
=C.  C0
=C2
=C3��C5��C8  C:  C;��C>  C@
=CB{CD�CF
=CH
=CJ  CL  CN{CP{CR  CT
=CV
=CW��CZ
=C\
=C^
=C`  Cb  Cc��Ce��Ch  Cj  Cl  Cm��Co��Cr  Ct  Cv  Cx
=Cz
=C{��C~  C�C�C�  C���C���C���C�C�C�  C�  C���C���C�
=C���C�  C�
=C�  C�  C�  C�C�  C�C�C�C�C�  C�C�\C�
=C�C�  C�  C���C���C���C�  C�  C�  C�  C�  C�C�C�C�C�C�
=C�  C�  C���C�
=C�
=C�C�C�  C���C�  C�C�  C�  C�  C�  C�C�C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C�C���C���C���C�  C���C���C���C�  C�
=C�
=C�C���C���C�  C�  C�C�C�C�
=C�C���C���C���C�  C�
=C�C���C���C���C���C���C���C�  C�C�  C�
=C�
=C�  C�C�C���C���C���C���C���C�C�  C���C�  C�  D �D ��D  D}qD  D��D  D� D  Dz�D�qDz�D�qD� D�qD� D�D��D	D	��D
  D
�DD}qD��D}qD�qDz�D�qD� D�qDz�D�qD}qD��Dz�D�qD��D�D��D�qD}qD�qD� D  D��D  D}qD  D��D  Dz�D��Dz�D�qD� D�D��D�qD}qD�D�DD�D   D }qD ��D!}qD!�qD"}qD#  D#}qD#��D$}qD$�qD%}qD&  D&��D&�qD'� D(  D(}qD(��D)� D*  D*� D*�qD+� D+�qD,z�D-  D-��D.  D.� D/  D/��D0�D0}qD1  D1� D1�qD2}qD2��D3� D4�D4� D5�D5��D6D6��D6�qD7� D7�qD8}qD8�qD9� D:�D:�D;D;� D<  D<}qD<�qD=� D>  D>� D?�D?��D@  D@� DA�DA}qDA�qDB}qDC�DC��DDDD� DE�DE��DF�DF� DG  DG}qDH�DH�DIDI� DI�qDJ� DK  DK��DK�qDL}qDM  DM}qDM��DN}qDN�qDO� DP�DP��DQ�DQ� DR  DR}qDR��DS}qDS�qDT� DT�qDU� DV�DV��DW  DW}qDX  DX}qDX�qDY}qDY�qDZ}qD[  D[}qD\�D\� D]  D]� D]�qD^}qD^��D_��D`  D`� DaDa� Db  Db}qDc  Dc��Dd  Dd� De�De}qDe�qDf}qDf�qDg}qDh�Dh�Di  DixRDi�qDj��Dk  Dk� Dk�qDl}qDl��Dm}qDn  Dn� Do�Do� Do�qDp��Dq�Dq��Dr�Dr��Ds  Ds� Dt�Dt� Du  Du��Dv  Dv}qDw  Dw� Dx�Dxz�Dx��Dyz�Dy�qDz��D{�D{� D{�qD|}qD}�D}� D}�qD~}qD~�qD��D�  D�@ D�� D���D��qD�@ D��HD��HD��D�C�D��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D���D���D�>�D�~�D���D��qD�@ D�� D��qD��qD�>�D��HD��HD�  D�>�D�~�D���D�  D�@ D�� D���D�  D�@ D��HD�D�HD�@ D��HD��HD���D�=qD��HD��HD�  D�AHD�� D���D�  D�@ D�� D��HD�  D�>�D�� D���D�  D�AHD��HD��HD�HD�B�D���D�� D�HD�AHD�� D��HD�HD�@ D�~�D���D�  D�>�D�}qD�� D�  D�@ D�� D�� D���D�@ D�� D��HD�HD�@ D��HD���D��qD�>�D�� D��HD�HD�>�D�� D���D���D�@ D��HD��HD�  D�@ D�� D��HD�HD�>�D�~�D��HD�  D�>�D�� D��qD��qD�@ D�~�D�� D�HD�@ D�� D��HD��D�AHD�� D�� D�  D�AHD�~�D���D�HD�AHD�� D�� D���D�>�D�}qD�� D�  D�@ D��HD�� D���D�>�D�~�D��qD���D�>�D�~�D���D��qD�@ D��HD��HD���D�=qD�~�D��HD�HD�@ D�� D��HD�HD�>�D�� D�� D���D�AHD��HD�� D�HD�AHD�� D�� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�=qD�� D�D�HD�>�D�~�D�� D���D�=qD�~�D��HD��D�B�D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�~�D��HD�  D�>�D��HD��HD�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�@ D��HD�D�  D�@ D�� D��HD�HD�>�D�~�D��HD�HD�@ D�� D�D�HD�AHD�� D���D���D�<)D�~�D�� D���D�=qD�� D�� D���D�>�D�� D��HD�  D�@ D D�� D�  D�>�D�~�D�� D���D�@ DĀ D��HD�  D�@ Dŀ D�� D���D�<)D�}qD�� D��D�B�Dǀ DǾ�D�  D�@ DȁHD��HD�HD�B�Dɀ Dɾ�D��qD�=qD�~�D��HD��D�B�DˁHD˾�D�  D�B�D́HD��HD���D�>�D̀ D�� D�  D�AHD΁HD�� D�HD�AHDς�D�� D��qD�>�D�~�Dо�D���D�>�DсHD��HD�  D�@ DҁHD��HD�HD�AHDӀ D��HD�  D�>�D�~�DԾ�D�  D�AHDՀ Dվ�D�  D�@ DցHD��HD�HD�@ D�~�D�� D�  D�>�D�~�DؽqD���D�@ Dـ D��HD�  D�>�Dڀ DڽqD�  D�B�DہHD�� D�HD�AHD܂�D��HD�  D�>�D�~�D�� D�HD�@ Dހ D�� D�  D�AHD߁HD߾�D���D�@ D�� D�� D�  D�@ D� D��HD��D�AHD�HD�� D���D�=qD� D㾸D���D�@ D�~�D侸D��qD�<)D� D�� D�  D�>�D� D��HD�  D�>�D� D��HD�  D�>�D� D辸D���D�>�D�~�D龸D���D�@ D�HD��HD��D�@ D�~�D��HD��D�@ D� D�� D���D�>�D�HD��HD�HD�@ D� D��HD�  D�@ D�~�D�qD���D�@ D��HD�� D�  D�B�D��D�D��D�AHD�HD��HD�  D�>�D� D�D�HD�@ D� D�� D�  D�AHD��HD��HD���D�>�D�� D���D���D�>�D�~�D�� D��G�O�?�?\)?L��?�z�?\?�
=@�@z�@(��@@  @O\)@aG�@z�H@��
@���@��H@��\@�{@�@��R@˅@�@�p�@�=q@��@��RAz�A
=Ap�A�\AA�HA ��A#�
A)��A-p�A333A6ffA:�HA@��AC�
AJ=qAMp�AQ�AW
=AZ=qA`��Ac�
Aj=qAmp�As�
Ax��A|��A���A��A��RA���A�33A��RA���A��A�{A�  A��
A�A���A��HA��A�Q�A�=qA�A�\)A��\A���A�  A�=qA�z�A��A���A��
A�\)A���A�z�A�{A�G�AӅA�p�Aأ�A�=qA�A߮A��A�z�A�RA�G�A�33A�ffA�  A��HA�p�A�\)A��\A�z�A�
=B ��B�B\)B��B��B\)B(�B	B
�HB�
Bp�B�\B�B��B=qB
=B��Bp�B�HB  B��B�\B\)B��BB�RB z�B!�B"�\B#�B$z�B&{B&�HB((�B)��B*ffB,  B,��B.=qB/\)B0z�B1�B2�RB4Q�B5p�B6=qB7�B8��B9B;33B<(�B=p�B>�HB?�BAG�BB{BC�BD��BE��BG\)BHQ�BIp�BK
=BK�
BMG�BNffBO33BP��BR{BR�HBTz�BUBV�RBXQ�BYG�BZ�\B\  B\��B^ffB_�B`z�Bb{Bc
=Bd(�Be��BfffBg�Bi�Bj=qBk33Bl��Bm�Bn�HBpQ�Bq��Br�\Bt  Bu�Bv{Bw�Bx��By�B{�B|Q�B}�B~�HB�{B��HB�G�B�  B���B��B�  B�Q�B���B��B�(�B���B�p�B��B�z�B�33B���B�ffB���B�p�B�=qB��RB��B�=qB��RB�\)B�(�B���B�p�B�{B���B�p�B�(�B��RB��B�{B���B���B�{B��HB��B�{B���B��B�{B���B���B�=qB�
=B���B�ffB�33B��B�ffB�G�B�B���B�p�B��B��RB�p�B��B��HB�p�B�{B���B��B�Q�B���B��B�ffB��B��B���B��B��
B���B��B�  B��\B�33B�  B��\B��B�  B���B��B�  B��RB�33B�B���B��B��
B���B��B�B���B�
=B�B�z�B���BîB�z�B��HBŮB�ffB��HBǮB�Q�B���BɅB�(�Bʏ\B�\)BˮB�Q�B���B�G�B��B�ffBθRB�\)Bϙ�B�{BЏ\BиRB��Bљ�B�B�{B�z�Bҏ\B���B�33B�\)B�B��
B�(�B�z�Bԏ\B��HB�33B�33BծB��
B�  B�ffB�z�B֣�B�
=B�33B�G�B׮B��
B�  B�ffB�z�Bأ�B���B�33B�G�Bٙ�B��B�  B�=qBڏ\Bڏ\B���B��B�33B�p�B��
B�{B�{B�z�Bܣ�B���B��B�33B�p�B��
B��B�=qB�z�Bޏ\B��HB�33B�G�Bߙ�B�  B�(�B�Q�B���B���B��BᙚB��B�  B�ffB���B��HB�\)B㙚B�B�(�B�z�B��B��B�B噚B��
B�Q�B��B���B��B癚B��
B�  B�Q�B���B��HB�33B陚B�B�  B�ffB���B��HB��B�B��B�(�B�=qB��B�
=B��B�\)B��
B�{B�=qB��B�
=B�33B�\)B��
B�=qB�ffB��\B�
=B�\)B�p�B��
B�Q�B�z�B��B�33B�p�B�B�(�B�Q�B�\B�
=B�p�B��B��B�Q�B���B���B�G�B��
B�{B�Q�B���B��B�G�B�B�(�B�Q�B���B��B�\)B���B�{B�ffB���B�
=B�p�B�B��B�Q�B���B���B�\)B��B��
C �C 33C \)C �\C ��C �
C
=C{CG�Cz�C�CC�C  C=qC\)Cz�C�RC�
C  C=qCQ�Cz�C�RC�
C  C=qCQ�Cp�C�RC�HC��C33CffCz�C�RC�HC  C(�CffC�\C�C��C�CG�Cp�C�RC�
C��C�C\)C��C�RC�HC	(�C	G�C	ffC	��C	�
C	��C
(�C
\)C
�C
��C
�HC�C=qCp�C�\C�RC��C33CG�C�C�RC�HC
=CG�Cz�C��C��C��C33Cz�C��CC
=CG�Cp�C�\C��C
=C=qCp�C�C�C��C=qCQ�Cz�C�RC�C
=C=qCp�C��C�HC��C�CffC�\C�C�C�C\)C�C��C��C
=C=qCp�C�CC  C(�CG�Cz�C�RC�
C  C33Cp�C��C��C��C{CG�C�CC�
C
=C33Cp�C�C�HC  C(�CQ�C��C��C��C�CG�Cz�C�C�C(�C\)Cz�C��C��C{CQ�C�C��C��C  C=qCz�C��C��C��C=qCp�C�\CC 
=C 33C Q�C z�C �RC ��C!�C!=qC!p�C!��C!�HC"{C"=qC"ffC"�C"C"��C#33C#Q�C#z�C#��C#�
C${C$Q�C$z�C$�C$��C$��C%=qC%p�C%�C%�
C&  C&(�C&ffC&��C&�HC'  C'(�C'\)C'��C'��C(
=C(G�C(ffC(�\C(C)  C)G�C)p�C)�\C)C*  C*G�C*p�C*�\C*��C+  C+=qC+�C+�C+�HC,{C,=qC,z�C,�RC,��C-(�C-\)C-�C-��C.
=C.G�C.z�C.��C.��C/  C/G�C/�\C/C/�C0�C0G�C0�C0C1  C1=qC1z�C1��C1�
C2
=C2=qC2�C2��C2��C3�C3Q�C3�\C3�
C4{C4=qC4ffC4��C4�
C5(�C5\)C5�C5�C5�HC6(�C6p�C6�C6�
C7
=C7=qC7z�C7C8  C8(�C8ffC8�\C8C9
=C9Q�C9��C9C9��C:33C:p�C:�RC:��C;33C;p�C;��C;��C<  C<Q�C<��C<�
C=
=C==qC=p�C=�C=�C>33C>z�C>��C>�HC?{C?G�C?��C?��C@
=C@=qC@ffC@��C@�
CA{CAQ�CA�\CA�
CB
=CB=qCBp�CB��CB�HCC�CC\)CC��CC�HCD{CDG�CD�CD�CD�HCE�CEQ�CE��CE�
CF{CFQ�CF�\CFCF��CG(�CGp�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�l�AԅAԃAԃAԉ7Aԉ7AԋDAԍPAԋDAԋDAԍPAԅA�v�Aԕ�Aԕ�Aԗ�Aԙ�Aԙ�Aԛ�Aԛ�Aԛ�Aԛ�Aԙ�Aԛ�Aԟ�Aԣ�Aԟ�Aԥ�Aԟ�Aԝ�Aԝ�Aԡ�Aԡ�Aԣ�Aԥ�Aԥ�Aԧ�Aԧ�Aԧ�AԮAԴ9AԴ9AԴ9AԶFAԴ9AԶFAԸRAԺ^AԺ^AԺ^AԼjAԾwAԾwAԾwA���A���A�AԮA�hsAҲ-A�ĜA���A�ffAƉ7A�O�A�1'A�
=A�1'A���A�%A�oA���A�  A�v�A��hA��DA���A�oA��^A���A�M�A���A�"�A� �A�|�A�\)A��A��
A��A��FA��A���A��7A�dZA�ZA���A�G�A�^5A��uA�z�A�K�A��PA��DA��yA�C�A�=qA�ȴA� �A��A�A��9A���A���A�bNA��uA��RA�7LA��A�  A~�\A}&�A|��A{XAu�At-AsK�Aq��Aq�Ao�Ai�Ae��Ad1Abn�A`�HA`JA]%A[�hA[oAX�AU�AR�AP�yAMhsAI�TAFz�AD�\AC��ABjA@$�A>jA=33A;�A:A8�A6��A6=qA5�hA4�`A4 �A3VA2JA1�A1�A0VA/dZA-��A-oA,z�A+dZA+"�A+%A*Q�A)��A(jA'��A&��A&z�A%�-A$�A#hsA"�9A!�mA!7LA M�A��A�A�/A
=A\)A��A
=Ap�A�mA�A��AO�A�HA��AI�A�;Ap�A��A�
A�`A�A��AVA
Q�A	�mA	��A�AA�A`BA9XA�A-AVAv�A��A ��A ff@���@�Q�@��m@�|�@��y@��^@��D@��F@��\@���@�9X@�P@�\@���@��@��
@�{@��T@�{@�E�@�7L@� �@�E�@�X@�j@���@��@�X@��D@�|�@�@�
=@�@ޗ�@���@�@݁@�A�@ڟ�@ٲ-@ؼj@��
@֗�@�V@ՙ�@�O�@�`B@��@�S�@�^5@�5?@�5?@�$�@���@�r�@�  @�Z@�b@�;d@�E�@��@�r�@���@˾w@˅@�;d@��H@ʇ+@ɡ�@�7L@���@�j@��
@ǥ�@�o@�o@Ǯ@ǅ@ƸR@Ƨ�@��@�M�@�ff@őh@�?}@�  @�@���@�X@�&�@��j@�j@�(�@�C�@�V@�M�@���@�J@�@���@�x�@�`B@���@��!@�5?@��-@�?}@���@��`@��@��@�G�@�X@�Z@���@��@�33@��+@��#@�p�@�V@��@���@�Ĝ@���@� �@��w@��P@�t�@�l�@�l�@�t�@�+@���@��@�hs@�/@�V@��j@�Z@�(�@�b@�1@�  @��;@���@��
@��P@���@�$�@��@��^@�hs@�G�@�/@�Ĝ@�1'@���@��;@�dZ@��@���@�
=@�M�@���@���@���@���@���@��@�7L@��D@��F@�;d@�~�@�5?@��T@�O�@�/@���@��@��@�z�@�j@�Q�@�  @�|�@�t�@�\)@�C�@��y@���@�~�@���@���@�7L@�%@��@��j@�1'@��m@���@��P@�\)@�C�@��@���@�dZ@�;d@��@��+@��@��^@���@�bN@�(�@�  @��m@���@�ƨ@���@�K�@�
=@��@���@��\@�n�@�M�@��@��-@�x�@�O�@��@��j@�r�@�Q�@�b@�"�@��R@��\@��@��T@��@��@�X@��@�z�@�A�@��@��F@���@�l�@�;d@�@��R@��\@���@���@�^5@�J@��@�@��h@�p�@�7L@��/@���@��@���@��;@���@�33@���@���@���@��R@��R@�v�@��@���@�hs@�G�@���@��@�I�@�1'@��@��F@��y@�v�@�V@�E�@�@���@�X@�7L@�/@�%@��@���@�Z@�(�@���@��m@��@�t�@��@���@��@�K�@�@���@��!@�v�@�@�V@��u@�j@�Z@�1@�bN@��;@��@��w@�dZ@�C�@��y@��\@�v�@�n�@�@��@�7L@��j@�  @�w@�1@�9X@�Z@��@�@}�@}`B@}�@|��@|�/@|Z@{�m@{�
@{��@{S�@z~�@y��@y%@x��@x�@w��@w+@v��@v@u�h@up�@u`B@u/@uV@t��@t�D@tj@t�@t1@t1@s�m@sƨ@s��@s��@s�@sC�@r�@q��@q��@q��@q�@pĜ@p�9@p �@o\)@n��@nv�@n5?@n$�@m�-@m`B@m`B@l�@l��@lj@k�m@kS�@k@j�\@jM�@i�#@i&�@h��@h��@h �@g�w@gK�@g�@f��@f�y@f�y@f�R@f�+@e��@e?}@d�@dj@c��@cƨ@c�@cS�@c"�@b�H@b�!@b=q@a��@`�u@_�w@^��@^5?@]�T@]�-@]�h@\�/@\z�@\j@\Z@\9X@[��@[�
@[t�@Z�@Z��@ZM�@Z�@Y�#@YX@Y7L@Y7L@Y7L@Y7L@Y7L@Y7L@Y�@XĜ@Xb@W��@Wl�@W;d@W+@W�@Vȴ@V�R@V�R@V5?@U@U�@U?}@Tz�@TI�@T(�@St�@R��@R=q@R-@RJ@Q��@Qx�@QG�@Q%@P�9@O�;@Ol�@O\)@N�@NE�@M�-@M?}@L��@LZ@K�m@Kt�@K@J�!@J-@I��@I�^@I��@I&�@H�9@Hr�@H  @G��@G�w@G�P@G;d@G
=@Fȴ@F��@Fff@F5?@F@Ep�@D��@D�D@D1@C�
@C��@C"�@B��@B^5@B-@A��@Ahs@A�@@�`@@�u@@1'@?�;@?�P@?;d@>�R@>v�@>$�@>@=�T@=��@=�@=`B@=?}@=V@<z�@<9X@<9X@<�@;��@;o@:�\@:M�@:-@:J@9�#@9�7@9G�@9%@8��@8r�@8  @7�@7|�@7;d@6�R@6�R@6V@6$�@6{@5�@5��@5�@4�@4�@41@41@3��@3��@3�@3dZ@2��@2^5@2�@1�#@1x�@1X@1G�@17L@1&�@0�`@0�@0Q�@0b@/|�@/+@.�y@.�R@.�+@.{@-�@-�T@-�-@-�h@-`B@,��@,�@,�D@,I�@,9X@,�@+�F@+��@+�@+S�@*�H@*��@*��@*��@*^5@*=q@*-@)�^@)�7@)hs@)G�@)&�@(��@(��@(r�@( �@( �@(b@'�@'��@';d@&��@&v�@&E�@&@%�@%�T@%�@%/@$��@$j@$(�@$(�@$(�@$(�@#��@#�
@#�F@#dZ@"�@"��@"~�@"^5@"J@!��@!hs@!&�@!%@ Ĝ@ ��@ �u@ r�@ 1'@ b@�@��@��@|�@;d@��@E�@@@@�@@�@O�@V@�@�/@�D@9X@(�@�@�m@�
@��@33@�\@n�@=q@J@�@��@��@x�@&�@%@��@��@�9@�u@r�@A�@  @��@�P@l�@;d@��@�R@��@v�@5?@{@@�@�-@p�@O�@/@/@V@�@��@�D@Z@9X@1@ƨ@��@C�@33@@��@��@�!@~�@M�@�@�^@�^@X@%@Ĝ@��@r�@1'@��@�@�P@\)@��@�R@�+@v�@v�@v�@ff@E�@{@��@�h@`B@V@�/@��@�j@�D@9X@�@1@�m@ƨA�^5A�^5A�O�A�^5A�S�A�\)A�r�A�v�AԃAԇ+Aԇ+AԃAԇ+A�~�AԃAԁAԁAԇ+Aԇ+AԅAԋDAԅAԉ7AԋDAԋDAԋDAԍPAԋDAԏ\AԑhAԉ7AԍPAԍPAԉ7AԍPAԉ7AԍPAԋDAԍPAԍPAԋDAԏ\Aԉ7Aԉ7AԍPAԅAԁAԉ7AԅA�z�A�x�A�t�A�l�A�z�A�p�AԍPAԕ�Aԏ\Aԗ�AԓuAԗ�Aԗ�AԓuAԙ�AԓuAԗ�Aԕ�Aԕ�Aԗ�Aԕ�Aԛ�Aԕ�Aԛ�Aԕ�Aԛ�Aԙ�Aԗ�Aԛ�Aԗ�Aԙ�Aԙ�Aԕ�Aԛ�Aԗ�Aԛ�Aԙ�Aԗ�Aԝ�Aԗ�Aԝ�Aԙ�Aԛ�Aԛ�Aԛ�Aԝ�Aԙ�Aԟ�Aԙ�Aԛ�Aԛ�Aԗ�Aԝ�Aԙ�Aԙ�Aԝ�Aԗ�Aԝ�Aԛ�Aԙ�Aԝ�Aԙ�Aԟ�Aԝ�Aԗ�Aԙ�Aԗ�Aԕ�Aԙ�Aԛ�Aԗ�Aԛ�Aԗ�Aԝ�Aԙ�Aԙ�Aԣ�Aԡ�Aԣ�Aԣ�Aԡ�Aԣ�Aԙ�Aԝ�Aԛ�Aԛ�Aԣ�Aԣ�Aԥ�Aԧ�Aԣ�Aԧ�Aԡ�Aԡ�Aԝ�Aԟ�Aԟ�Aԙ�Aԧ�Aԧ�Aԣ�Aԧ�Aԥ�Aԝ�Aԣ�Aԡ�Aԟ�Aԟ�Aԛ�Aԟ�Aԛ�Aԡ�Aԝ�Aԛ�Aԡ�Aԟ�Aԛ�Aԡ�Aԛ�Aԟ�Aԟ�Aԛ�Aԟ�Aԡ�Aԛ�Aԡ�Aԡ�Aԟ�Aԣ�Aԟ�Aԡ�Aԥ�Aԟ�Aԣ�Aԣ�Aԟ�Aԣ�Aԟ�Aԡ�Aԥ�Aԡ�Aԡ�Aԥ�Aԡ�Aԡ�Aԥ�Aԥ�Aԡ�Aԧ�Aԧ�Aԣ�Aԧ�Aԧ�Aԣ�Aԧ�Aԥ�Aԣ�Aԧ�Aԥ�Aԩ�Aԩ�AԬAԮAԣ�Aԩ�Aԩ�Aԥ�Aԩ�Aԥ�Aԥ�Aԩ�Aԧ�Aԥ�Aԩ�Aԧ�Aԧ�Aԩ�Aԣ�Aԩ�Aԩ�Aԩ�AԬAԮAԴ9AԶFAԲ-AԲ-AԶFAԲ-AԶFAԴ9A԰!AԶFAԴ9AԲ-AԸRAԲ-AԴ9AԶFAԲ-AԶFAԴ9AԴ9AԸRAԶFAԲ-AԸRAԴ9AԴ9AԸRAԲ-AԲ-AԶFA԰!A԰!AԸRAԴ9AԸRAԺ^AԴ9AԸRAԸRAԶFAԺ^AԶFAԶFAԼjAԸRAԺ^AԺ^AԸRAԼjAԺ^AԸRAԼjAԸRAԺ^AԼjAԸRAԼjAԸRAԺ^AԾwAԼjAԸRAԾwAԾwAԺ^AԾwA���AԺ^AԼjA���AԼjAԾwA���AԼjAԾwA���AԼjA���A���AԼjAԾwA���AԼjA���A�AԼjA���A�AԼjA�A�AԾwA�ĜAԾwA���A�ĜA���A�ĜA���A���A�ĜA���A�ĜA�ĜA���A�A�ĜA���A���A�ĜAԾwA���A���AԼjAԼjAԶFAԲ-Aԡ�Aԝ�Aԟ�Aԝ�Aԙ�Aԟ�AԑhAԏ\AԑhAԅAԇ+AԍPAԇ+A�|�A�~�A�z�A�z�A�jA�`BA�XA�VA�Q�A�7LA�=qA�9XA�$�A��A��A��AӾwA�
=A��HAҮAҴ9Aҩ�Aҙ�Aҏ\A�z�A�S�A�7LA�JA���A�jA�A��/A��A���AГuA�A�A�A�ƨAϛ�A�dZA�%A�ffA�K�A��#A�7LA��A��
AʶFAʝ�A�|�A�r�A�`BA��A�Aɰ!A�E�A��A���AȑhA�Q�A�&�AǼjAǉ7A�~�A�hsA�VA�E�A�9XA�7LA�5?A�1'A�33A�/A�(�A�&�A�VA��A��#A���AƧ�AƍPA�|�A�M�A�33A��A�1A��A��;A���A���AŸRA�x�A�/AāA��Aß�A�n�A��A���A�A�~�A�XA���A�ƨA���A�r�A�ffA�A�A��#A���A��hA�v�A�S�A�$�A�oA�A��^A�x�A�VA���A���A�x�A�hsA�\)A�ZA�XA�O�A�M�A�K�A�?}A�5?A�33A�/A�&�A��A�bA�
=A���A���A��A��TA��TA��A�ƨA�ĜA��-A���A���A��A�v�A�hsA�C�A�bA�ƨA�l�A�I�A�-A��A���A�bNA�G�A��A��A��!A��A�`BA�E�A�=qA�(�A�bA�  A��A��HA��#A��^A���A�l�A�?}A�JA��mA���A��!A��A�XA�;dA�1A��uA��mA��;A�ȴA�A��jA��-A��A��A���A���A���A���A��hA��7A��PA��+A�z�A�x�A�v�A�n�A�VA�;dA�A��uA��A���A���A�v�A�VA�33A��A��A���A���A��hA�t�A�S�A�
=A�S�A���A�ȴA���A�p�A�ffA�K�A�7LA�$�A�oA�  A��mA�ȴA��-A���A��DA�p�A�O�A�5?A�(�A��A�bA�A���A��TA���A���A��A�jA�Q�A�5?A�
=A��A�p�A��A�ȴA��wA��FA��-A��A���A���A���A���A���A���A��+A�~�A�r�A�ZA�5?A��A��uA�A�A�%A��A��^A���A�z�A�O�A�C�A�;dA�7LA��A�A��A��;A�ȴA���A�ZA�VA���A��uA�E�A��A��A���A���A��+A�~�A�x�A�l�A�I�A�;dA�&�A�$�A��A�JA���A��
A��A��DA��A�z�A�z�A�|�A�z�A�z�A�x�A�x�A�l�A�n�A�ffA�bNA�dZA�`BA�`BA�S�A�S�A�O�A�Q�A�I�A�?}A�?}A�7LA�33A�$�A�{A�bA�%A���A��TA��jA��A���A��+A�5?A�A�S�A�K�A�M�A�-A��A�bA�%A���A���A��A��`A��A���A���A���A���A�ƨA�ĜA�A�ƨA�ƨA���A��A��hA�C�A�+A� �A��A��FA���A�r�A�Q�A�/A��A�1A��A��A��yA��TA��DA�=qA�-A�{A��A��A���A��A��A�v�A�t�A�r�A�r�A�p�A�r�A�n�A�bNA�^5A�dZA�`BA�`BA�`BA�^5A�\)A�ZA�ZA�\)A�^5A�^5A�VA�XA�ZA�^5A�Q�A�5?A��A��+A��PA��A�bNA��7A�E�A�oA��;A��!A���A�z�A�  A�(�A���A�ȴA���A���A�~�A�Q�A�I�A�-A�1A��TA���A���A��PA��7A��7A��PA��PA��hA��PA��A�~�A��A�x�A�x�A�n�A�v�A�n�A�r�A�n�A�bNA�Q�A�E�A�=qA�E�A�5?A�(�A�$�A�"�A��A�A�A��A�hsA��RA�O�A�+A��TA�A��!A��uA�p�A�G�A�7LA�-A�(�A�$�A��A�bA��A��A���A���A��uA��A�hsA�ZA�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   A�VA�l�AԅAԃAԃAԉ7Aԉ7AԋDAԍPAԋDAԋDAԍPAԅA�v�Aԕ�Aԕ�Aԗ�Aԙ�Aԙ�Aԛ�Aԛ�Aԛ�Aԛ�Aԙ�Aԛ�Aԟ�Aԣ�Aԟ�Aԥ�Aԟ�Aԝ�Aԝ�Aԡ�Aԡ�Aԣ�Aԥ�Aԥ�Aԧ�Aԧ�Aԧ�AԮAԴ9AԴ9AԴ9AԶFAԴ9AԶFAԸRAԺ^AԺ^AԺ^AԼjAԾwAԾwAԾwA���A���A�AԮA�hsAҲ-A�ĜA���A�ffAƉ7A�O�A�1'A�
=A�1'A���A�%A�oA���A�  A�v�A��hA��DA���A�oA��^A���A�M�A���A�"�A� �A�|�A�\)A��A��
A��A��FA��A���A��7A�dZA�ZA���A�G�A�^5A��uA�z�A�K�A��PA��DA��yA�C�A�=qA�ȴA� �A��A�A��9A���A���A�bNA��uA��RA�7LA��A�  A~�\A}&�A|��A{XAu�At-AsK�Aq��Aq�Ao�Ai�Ae��Ad1Abn�A`�HA`JA]%A[�hA[oAX�AU�AR�AP�yAMhsAI�TAFz�AD�\AC��ABjA@$�A>jA=33A;�A:A8�A6��A6=qA5�hA4�`A4 �A3VA2JA1�A1�A0VA/dZA-��A-oA,z�A+dZA+"�A+%A*Q�A)��A(jA'��A&��A&z�A%�-A$�A#hsA"�9A!�mA!7LA M�A��A�A�/A
=A\)A��A
=Ap�A�mA�A��AO�A�HA��AI�A�;Ap�A��A�
A�`A�A��AVA
Q�A	�mA	��A�AA�A`BA9XA�A-AVAv�A��A ��A ff@���@�Q�@��m@�|�@��y@��^@��D@��F@��\@���@�9X@�P@�\@���@��@��
@�{@��T@�{@�E�@�7L@� �@�E�@�X@�j@���@��@�X@��D@�|�@�@�
=@�@ޗ�@���@�@݁@�A�@ڟ�@ٲ-@ؼj@��
@֗�@�V@ՙ�@�O�@�`B@��@�S�@�^5@�5?@�5?@�$�@���@�r�@�  @�Z@�b@�;d@�E�@��@�r�@���@˾w@˅@�;d@��H@ʇ+@ɡ�@�7L@���@�j@��
@ǥ�@�o@�o@Ǯ@ǅ@ƸR@Ƨ�@��@�M�@�ff@őh@�?}@�  @�@���@�X@�&�@��j@�j@�(�@�C�@�V@�M�@���@�J@�@���@�x�@�`B@���@��!@�5?@��-@�?}@���@��`@��@��@�G�@�X@�Z@���@��@�33@��+@��#@�p�@�V@��@���@�Ĝ@���@� �@��w@��P@�t�@�l�@�l�@�t�@�+@���@��@�hs@�/@�V@��j@�Z@�(�@�b@�1@�  @��;@���@��
@��P@���@�$�@��@��^@�hs@�G�@�/@�Ĝ@�1'@���@��;@�dZ@��@���@�
=@�M�@���@���@���@���@���@��@�7L@��D@��F@�;d@�~�@�5?@��T@�O�@�/@���@��@��@�z�@�j@�Q�@�  @�|�@�t�@�\)@�C�@��y@���@�~�@���@���@�7L@�%@��@��j@�1'@��m@���@��P@�\)@�C�@��@���@�dZ@�;d@��@��+@��@��^@���@�bN@�(�@�  @��m@���@�ƨ@���@�K�@�
=@��@���@��\@�n�@�M�@��@��-@�x�@�O�@��@��j@�r�@�Q�@�b@�"�@��R@��\@��@��T@��@��@�X@��@�z�@�A�@��@��F@���@�l�@�;d@�@��R@��\@���@���@�^5@�J@��@�@��h@�p�@�7L@��/@���@��@���@��;@���@�33@���@���@���@��R@��R@�v�@��@���@�hs@�G�@���@��@�I�@�1'@��@��F@��y@�v�@�V@�E�@�@���@�X@�7L@�/@�%@��@���@�Z@�(�@���@��m@��@�t�@��@���@��@�K�@�@���@��!@�v�@�@�V@��u@�j@�Z@�1@�bN@��;@��@��w@�dZ@�C�@��y@��\@�v�@�n�@�@��@�7L@��j@�  @�w@�1@�9X@�Z@��@�@}�@}`B@}�@|��@|�/@|Z@{�m@{�
@{��@{S�@z~�@y��@y%@x��@x�@w��@w+@v��@v@u�h@up�@u`B@u/@uV@t��@t�D@tj@t�@t1@t1@s�m@sƨ@s��@s��@s�@sC�@r�@q��@q��@q��@q�@pĜ@p�9@p �@o\)@n��@nv�@n5?@n$�@m�-@m`B@m`B@l�@l��@lj@k�m@kS�@k@j�\@jM�@i�#@i&�@h��@h��@h �@g�w@gK�@g�@f��@f�y@f�y@f�R@f�+@e��@e?}@d�@dj@c��@cƨ@c�@cS�@c"�@b�H@b�!@b=q@a��@`�u@_�w@^��@^5?@]�T@]�-@]�h@\�/@\z�@\j@\Z@\9X@[��@[�
@[t�@Z�@Z��@ZM�@Z�@Y�#@YX@Y7L@Y7L@Y7L@Y7L@Y7L@Y7L@Y�@XĜ@Xb@W��@Wl�@W;d@W+@W�@Vȴ@V�R@V�R@V5?@U@U�@U?}@Tz�@TI�@T(�@St�@R��@R=q@R-@RJ@Q��@Qx�@QG�@Q%@P�9@O�;@Ol�@O\)@N�@NE�@M�-@M?}@L��@LZ@K�m@Kt�@K@J�!@J-@I��@I�^@I��@I&�@H�9@Hr�@H  @G��@G�w@G�P@G;d@G
=@Fȴ@F��@Fff@F5?@F@Ep�@D��@D�D@D1@C�
@C��@C"�@B��@B^5@B-@A��@Ahs@A�@@�`@@�u@@1'@?�;@?�P@?;d@>�R@>v�@>$�@>@=�T@=��@=�@=`B@=?}@=V@<z�@<9X@<9X@<�@;��@;o@:�\@:M�@:-@:J@9�#@9�7@9G�@9%@8��@8r�@8  @7�@7|�@7;d@6�R@6�R@6V@6$�@6{@5�@5��@5�@4�@4�@41@41@3��@3��@3�@3dZ@2��@2^5@2�@1�#@1x�@1X@1G�@17L@1&�@0�`@0�@0Q�@0b@/|�@/+@.�y@.�R@.�+@.{@-�@-�T@-�-@-�h@-`B@,��@,�@,�D@,I�@,9X@,�@+�F@+��@+�@+S�@*�H@*��@*��@*��@*^5@*=q@*-@)�^@)�7@)hs@)G�@)&�@(��@(��@(r�@( �@( �@(b@'�@'��@';d@&��@&v�@&E�@&@%�@%�T@%�@%/@$��@$j@$(�@$(�@$(�@$(�@#��@#�
@#�F@#dZ@"�@"��@"~�@"^5@"J@!��@!hs@!&�@!%@ Ĝ@ ��@ �u@ r�@ 1'@ b@�@��@��@|�@;d@��@E�@@@@�@@�@O�@V@�@�/@�D@9X@(�@�@�m@�
@��@33@�\@n�@=q@J@�@��@��@x�@&�@%@��@��@�9@�u@r�@A�@  @��@�P@l�@;d@��@�R@��@v�@5?@{@@�@�-@p�@O�@/@/@V@�@��@�D@Z@9X@1@ƨ@��@C�@33@@��@��@�!@~�@M�@�@�^@�^@X@%@Ĝ@��@r�@1'@��@�@�P@\)@��@�R@�+@v�@v�@v�@ff@E�@{@��@�h@`B@V@�/@��@�j@�D@9X@�@1@�mG�O�A�^5A�^5A�O�A�^5A�S�A�\)A�r�A�v�AԃAԇ+Aԇ+AԃAԇ+A�~�AԃAԁAԁAԇ+Aԇ+AԅAԋDAԅAԉ7AԋDAԋDAԋDAԍPAԋDAԏ\AԑhAԉ7AԍPAԍPAԉ7AԍPAԉ7AԍPAԋDAԍPAԍPAԋDAԏ\Aԉ7Aԉ7AԍPAԅAԁAԉ7AԅA�z�A�x�A�t�A�l�A�z�A�p�AԍPAԕ�Aԏ\Aԗ�AԓuAԗ�Aԗ�AԓuAԙ�AԓuAԗ�Aԕ�Aԕ�Aԗ�Aԕ�Aԛ�Aԕ�Aԛ�Aԕ�Aԛ�Aԙ�Aԗ�Aԛ�Aԗ�Aԙ�Aԙ�Aԕ�Aԛ�Aԗ�Aԛ�Aԙ�Aԗ�Aԝ�Aԗ�Aԝ�Aԙ�Aԛ�Aԛ�Aԛ�Aԝ�Aԙ�Aԟ�Aԙ�Aԛ�Aԛ�Aԗ�Aԝ�Aԙ�Aԙ�Aԝ�Aԗ�Aԝ�Aԛ�Aԙ�Aԝ�Aԙ�Aԟ�Aԝ�Aԗ�Aԙ�Aԗ�Aԕ�Aԙ�Aԛ�Aԗ�Aԛ�Aԗ�Aԝ�Aԙ�Aԙ�Aԣ�Aԡ�Aԣ�Aԣ�Aԡ�Aԣ�Aԙ�Aԝ�Aԛ�Aԛ�Aԣ�Aԣ�Aԥ�Aԧ�Aԣ�Aԧ�Aԡ�Aԡ�Aԝ�Aԟ�Aԟ�Aԙ�Aԧ�Aԧ�Aԣ�Aԧ�Aԥ�Aԝ�Aԣ�Aԡ�Aԟ�Aԟ�Aԛ�Aԟ�Aԛ�Aԡ�Aԝ�Aԛ�Aԡ�Aԟ�Aԛ�Aԡ�Aԛ�Aԟ�Aԟ�Aԛ�Aԟ�Aԡ�Aԛ�Aԡ�Aԡ�Aԟ�Aԣ�Aԟ�Aԡ�Aԥ�Aԟ�Aԣ�Aԣ�Aԟ�Aԣ�Aԟ�Aԡ�Aԥ�Aԡ�Aԡ�Aԥ�Aԡ�Aԡ�Aԥ�Aԥ�Aԡ�Aԧ�Aԧ�Aԣ�Aԧ�Aԧ�Aԣ�Aԧ�Aԥ�Aԣ�Aԧ�Aԥ�Aԩ�Aԩ�AԬAԮAԣ�Aԩ�Aԩ�Aԥ�Aԩ�Aԥ�Aԥ�Aԩ�Aԧ�Aԥ�Aԩ�Aԧ�Aԧ�Aԩ�Aԣ�Aԩ�Aԩ�Aԩ�AԬAԮAԴ9AԶFAԲ-AԲ-AԶFAԲ-AԶFAԴ9A԰!AԶFAԴ9AԲ-AԸRAԲ-AԴ9AԶFAԲ-AԶFAԴ9AԴ9AԸRAԶFAԲ-AԸRAԴ9AԴ9AԸRAԲ-AԲ-AԶFA԰!A԰!AԸRAԴ9AԸRAԺ^AԴ9AԸRAԸRAԶFAԺ^AԶFAԶFAԼjAԸRAԺ^AԺ^AԸRAԼjAԺ^AԸRAԼjAԸRAԺ^AԼjAԸRAԼjAԸRAԺ^AԾwAԼjAԸRAԾwAԾwAԺ^AԾwA���AԺ^AԼjA���AԼjAԾwA���AԼjAԾwA���AԼjA���A���AԼjAԾwA���AԼjA���A�AԼjA���A�AԼjA�A�AԾwA�ĜAԾwA���A�ĜA���A�ĜA���A���A�ĜA���A�ĜA�ĜA���A�A�ĜA���A���A�ĜAԾwA���A���AԼjAԼjAԶFAԲ-Aԡ�Aԝ�Aԟ�Aԝ�Aԙ�Aԟ�AԑhAԏ\AԑhAԅAԇ+AԍPAԇ+A�|�A�~�A�z�A�z�A�jA�`BA�XA�VA�Q�A�7LA�=qA�9XA�$�A��A��A��AӾwA�
=A��HAҮAҴ9Aҩ�Aҙ�Aҏ\A�z�A�S�A�7LA�JA���A�jA�A��/A��A���AГuA�A�A�A�ƨAϛ�A�dZA�%A�ffA�K�A��#A�7LA��A��
AʶFAʝ�A�|�A�r�A�`BA��A�Aɰ!A�E�A��A���AȑhA�Q�A�&�AǼjAǉ7A�~�A�hsA�VA�E�A�9XA�7LA�5?A�1'A�33A�/A�(�A�&�A�VA��A��#A���AƧ�AƍPA�|�A�M�A�33A��A�1A��A��;A���A���AŸRA�x�A�/AāA��Aß�A�n�A��A���A�A�~�A�XA���A�ƨA���A�r�A�ffA�A�A��#A���A��hA�v�A�S�A�$�A�oA�A��^A�x�A�VA���A���A�x�A�hsA�\)A�ZA�XA�O�A�M�A�K�A�?}A�5?A�33A�/A�&�A��A�bA�
=A���A���A��A��TA��TA��A�ƨA�ĜA��-A���A���A��A�v�A�hsA�C�A�bA�ƨA�l�A�I�A�-A��A���A�bNA�G�A��A��A��!A��A�`BA�E�A�=qA�(�A�bA�  A��A��HA��#A��^A���A�l�A�?}A�JA��mA���A��!A��A�XA�;dA�1A��uA��mA��;A�ȴA�A��jA��-A��A��A���A���A���A���A��hA��7A��PA��+A�z�A�x�A�v�A�n�A�VA�;dA�A��uA��A���A���A�v�A�VA�33A��A��A���A���A��hA�t�A�S�A�
=A�S�A���A�ȴA���A�p�A�ffA�K�A�7LA�$�A�oA�  A��mA�ȴA��-A���A��DA�p�A�O�A�5?A�(�A��A�bA�A���A��TA���A���A��A�jA�Q�A�5?A�
=A��A�p�A��A�ȴA��wA��FA��-A��A���A���A���A���A���A���A��+A�~�A�r�A�ZA�5?A��A��uA�A�A�%A��A��^A���A�z�A�O�A�C�A�;dA�7LA��A�A��A��;A�ȴA���A�ZA�VA���A��uA�E�A��A��A���A���A��+A�~�A�x�A�l�A�I�A�;dA�&�A�$�A��A�JA���A��
A��A��DA��A�z�A�z�A�|�A�z�A�z�A�x�A�x�A�l�A�n�A�ffA�bNA�dZA�`BA�`BA�S�A�S�A�O�A�Q�A�I�A�?}A�?}A�7LA�33A�$�A�{A�bA�%A���A��TA��jA��A���A��+A�5?A�A�S�A�K�A�M�A�-A��A�bA�%A���A���A��A��`A��A���A���A���A���A�ƨA�ĜA�A�ƨA�ƨA���A��A��hA�C�A�+A� �A��A��FA���A�r�A�Q�A�/A��A�1A��A��A��yA��TA��DA�=qA�-A�{A��A��A���A��A��A�v�A�t�A�r�A�r�A�p�A�r�A�n�A�bNA�^5A�dZA�`BA�`BA�`BA�^5A�\)A�ZA�ZA�\)A�^5A�^5A�VA�XA�ZA�^5A�Q�A�5?A��A��+A��PA��A�bNA��7A�E�A�oA��;A��!A���A�z�A�  A�(�A���A�ȴA���A���A�~�A�Q�A�I�A�-A�1A��TA���A���A��PA��7A��7A��PA��PA��hA��PA��A�~�A��A�x�A�x�A�n�A�v�A�n�A�r�A�n�A�bNA�Q�A�E�A�=qA�E�A�5?A�(�A�$�A�"�A��A�A�A��A�hsA��RA�O�A�+A��TA�A��!A��uA�p�A�G�A�7LA�-A�(�A�$�A��A�bA��A��A���A���A��uA��A�hsA�ZA�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B�vB�B�BB�BB�}B�HBϫB�vBϫB�BB�pBбB�NB�NB�NBуB�NB�NB�B�BбB��BуB�NB�NB�NB�B�B��B��B�NB�NB�B�B�NB�B�NB�B��B��B��B��B��BѷB��B� B��BѷB� B� B��B��B��BѷBѷB��B֡B�	BC-B�rB��B��B��B�B�4B��B�lB�%BzxBp�Bd�BTaBOBBFB8RB0�B(�BqBSB+B��B�|B�]B�B��B� B�QB��BԕB��B��B��B�B��B��Bk�B^5BZ�BXyBS[B8�B.B$@BB1B
��B
�B
�>B
��B
�EB
��B
�qB
�lB
|B
o�B
S�B
HKB
9�B
)�B
!bB
kB

rB	�B	�B	�B	�BB	�
B	�gB	��B	�kB	��B	�	B	�B	w�B	kB	d&B	^B	L�B	A�B	9�B	0!B	!-B	�B	1B	�B��B�DB�B��B�>B�B�B�B�pB��B�/B��B��B�)B�)B��B�dBݘB�B�vB��B�fB�B��B�B�B�`B�B�TB�B�B�NB�WB��B��B�jB�B�B�6B�B��B�[B�B�TB�`B��B�zB�zB�XB� B��B�|B��B��B��B�B��B�&B��B��B�KB�)B�B�B��B�B�B��B�B�BخB��B�B��B�B�B�HB�B��B�B�NB� B�B�
B�B�
B��B�B��B�mB�B��B��B�B��B�PB�"B��B��B��B�.B��B�(B��B��B	 �B	�B	YB		lB	�B	B	�B	 'B	!�B	 'B	~B	�B	VB	�B	!bB	(XB	.}B	/�B	/OB	/�B	3�B	5tB	6zB	9�B	9XB	?�B	H�B	J�B	PHB	S�B	VmB	X�B	YKB	Z�B	`BB	b�B	d&B	h
B	g�B	h
B	iB	iDB	iyB	l�B	poB	v�B	y�B	y�B	{JB	�4B	� B	�fB	��B	��B	�PB	�\B	��B	��B	�FB	�eB	�7B	�7B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	�0B	��B	��B	�$B	�XB	��B	��B	�0B	��B	�'B	�B	�zB	��B	�$B	�B	��B	�$B	�BB	�qB	�wB	�B	�B	�}B	��B	�B	ǮB	��B	�B	��B	��B	��B	�B	�B	� B	�&B	��B	՛B	�EB	��B	�)B	�)B	��B	ܒB	�dB	�;B	�BB	�HB	�|B	�NB	�B	�NB	�|B	�|B	� B	�ZB	��B	�B	�mB	�B	�2B	�B	�B	�B	�B	��B	�]B	��B	� B	�B	� B	�B	�WB	�cB	�/B	�B	�|B	�GB	�B	��B	��B	��B	��B	�>B	�xB	�B	�xB	�B	�B	��B	��B	��B	��B	�JB	��B	�"B	��B	�"B	�"B	��B	�]B	��B
 iB
�B
�B
�B
_B
+B

	B

�B

rB

rB
�B
�B
�B
+B
�B
�B

	B
DB
~B
B
�B
�B
�B
�B
�B
�B
4B
�B
�B
�B
:B
B
:B
oB
�B
\B
�B
�B
�B
:B
B
oB
�B
�B
:B
B
@B
oB
�B
�B
�B
�B
�B
�B
�B
kB
�B
=B
�B
IB
�B
�B
�B
!B
VB
B
�B
�B
�B
B
�B
�B
!B
"�B
"4B
"hB
!�B
!�B
"4B
"�B
#B
"hB
"hB
#�B
$�B
$B
"�B
"4B
!�B
!�B
!�B
"4B
"hB
#:B
#�B
$B
$�B
$�B
$@B
$�B
%FB
&B
'�B
)�B
*�B
,=B
,B
,qB
,qB
,B
,=B
,=B
+B
)�B
)�B
)�B
*�B
-wB
.�B
0�B
1�B
1�B
1�B
2�B
2aB
2�B
2�B
3�B
3hB
4B
4�B
3hB
33B
4nB
5tB
6FB
6�B
5�B
4�B
4nB
5tB
6zB
6�B
7�B
7B
7LB
7B
6�B
6B
6�B
7B
6�B
7�B
7�B
7B
7�B
7�B
7�B
7�B
8�B
8�B
9XB
9�B
9�B
;0B
<6B
<6B
=<B
=�B
>wB
>wB
>wB
>�B
>�B
?}B
@B
@�B
@�B
A�B
AUB
A�B
A�B
A B
AUB
A�B
A�B
A�B
B�B
A�B
A�B
B[B
B�B
B[B
B�B
B�B
C-B
C-B
C�B
C�B
DgB
D3B
DgB
D�B
EB
EmB
E9B
EmB
EmB
E9B
E9B
E9B
F?B
FtB
F�B
GEB
GB
GEB
G�B
G�B
G�B
GzB
GzB
G�B
HB
H�B
I�B
J#B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
LdB
L�B
MB
MB
MjB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
N�B
N�B
OB
OBB
OBB
OvB
O�B
O�B
O�B
PB
P}B
PHB
P�B
QNB
QB
P�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S&B
S&B
S[B
T,B
S�B
S�B
TaB
T�B
U�B
U�B
V9B
V9B
V�B
W
B
W�B
XB
X�B
X�B
X�B
YKB
ZQB
Z�B
Z�B
[WB
[#B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
]/B
^B
^B
^jB
^B
^5B
^jB
^�B
_B
_B
_B
`B
_�B
`B
`BB
`�B
`�B
aHB
a�B
a|B
a�B
bB
bNB
bNB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
c�B
d�B
e`B
e�B
f2B
f2B
e�B
f�B
f�B
gB
gB
g8B
g8B
g�B
h>B
h>B
hsB
h�B
h�B
iDB
iB
iB
iB
iDB
i�B
jKB
j�B
j�B
jB
j�B
j�B
j�B
j�B
kQB
kQB
k�B
k�B
lWB
lWB
l"B
lWB
lWB
l�B
m)B
l�B
m]B
m�B
n/B
n/B
ncB
ncB
o B
n�B
n�B
o5B
n�B
o B
o5B
o B
o5B
o�B
o�B
o�B
poB
p;B
poB
p�B
poB
pB
p;B
poB
p�B
qAB
q�B
q�B
q�B
rGB
rB
rB
r|B
r|B
r�B
r|B
r|B
r|B
rGB
r|B
r�B
rGB
r�B
sB
sB
sB
r�B
sMB
sMB
s�B
tTB
tTB
tTB
tTB
tB
t�B
tTB
t�B
t�B
uZB
u�B
u�B
u�B
v`B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
xlB
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zDB
zDB
zxB
z�B
{B
{B
{B
{JB
{B
{JB
{�B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~�B
~�B
~�B
.B
~�B
.B
�B
�B
�B
� B
�4B
�4B
�iB
�iB
��B
�B
�;B
�;B
�;B
�oB
�oB
��B
��B
�B
�B
�uB
��B
�B
�B
�GB
�{B
��B
�{B
�{B
��B
��B
��B
�MB
�MB
��B
��B
�B
�B
��B
��B
��B
�%B
�%B
�YB
��B
�+B
�_B
�_B
�+B
�_B
�_B
��B
��B
�1B
�1B
�fB
��B
��B
�B
��B
�7B
��B
��B
��B
�	B
�=B��B��B��B��BΥB��BуB�B�B��B��B��B�B�}BΥBϫB�B�<B�B�B�<BϫBΥBϫB�B�}B�BBуB�B�BB��B��B��B�}B�pB�}B��BϫB��B�vB�NB�BB��B��BΥB�}B�6B�}B��B�B�pB�}BΥBʌB��B�B�BѷBϫBѷB�}B�}B�TB�B�TB��BѷB�BбBѷB��BҽB�B� B�}B�NBѷB�}BѷB�NBбB҉B�B�TB�B�B� B�B��B�HBѷBбB�NB��B�HB҉B�HBѷBѷB�NB� B�B��BѷB�}BҽB�HB��B��B�B��B�HBбB� B�HB�BуB�B�B�NB��BѷB��B�B�NB�pB��B�NBѷB҉BуBҽBбBуB�NB�pB� BѷB��BҽB�}B�&BуBуB� B�NB�TB�B�}B҉B�B�TB��B�HB�B�TBуB� B��B��B�HB�B��B�B�NB�NB�B��BбB�}B� B�}BϫB��B��B�}BѷB�BѷBуB��B� B�HB�NB�TB�HB�TBуB��B� B� B��B��B��B��BуB� B�}BбB��B�HB�B� B�}BуBѷB�HBуB�HB�B�B��B�TBбB��B� B�B��B��B�HB�NB�TB�HBуB�NB�HB҉BбB�HB�TBбBϫBуBбB҉B҉BбB��B��BѷB�&B��B�TB҉B��B� BуBбB��BуB�TB҉BбBуB�[BбB�TB� B��B�&B� B�}B҉B��B�}B��BѷB��B�&BѷB�NB҉BбB� B��B�}BҽBуBѷB��BуB��B�&B��B�TB�NB��B҉B��BҽB� BбBѷB�&BуBѷB��BуB�B��BҽB��B��BѷBбB��B� B�B�&BѷB�NB�[B�TB�B��BѷB�B�&BѷBуB�&B�B�NB҉BбB�&B҉BбB҉B�B� B� B�}B�TB�NB�NB�TB��B�}B�TBѷBбB��BуBуBѷBуB� B��B��B��B��B�[BԕB�TB�9BԕBӏB��B�gB�&B�aB�
B�aB�B�gB�sB��BخB՛B�mB�sB�B��B��B��BרB�B�B�B��B�DB�%B�B�B��B��B�.B��BuB�B'�B �B�B�B�B �B%B*0B,qB)�B0�B��BQ�BV9Bf�B~]B��B�B}�B��B��B�B�GB�B�B��B�1B��B�FB��B�@B�=B��B�eB��B��B�$B��B��B��B��B��B��B��B�MB�uB�SB�SB��B��B�B�{B��B��B�B�FB�$B�hB�:B�hB��B�oB��B�4B�IB��B��B�B��B�SB��B��B�SB��B�B�B�B��B�FB�B�oB��B�oB�.B��B��B��B�B��B�B�{B�oB�VB�PB��B�B��B��B��B��B�B��B��B�B�JB��B�~B��B�~B�7B�xB�rB��B��B�~B�1B��B��B��B�xB�B�%B�_B�rB�YB�MB�oB��B�7B��B��B��B�B�4B��B.B~(B{JBzxB{B{JBw2Bx�Bv�Bu�BzDBw2Bx�BwfBu�Bo�Bn�Bn�Bp;Bl�Bg�Bj�Bt�B�B��BYBWsBW�BY�BX�BW?BVBW�BW�BU2BU�BW
BS�BT�BT�BS�BRTBRTBS&BT�BV9B]dBWsBP�BOvBK^BJ�BI�BI�BIRBIBFBDgBB�BD3BJ�B_�BHKB<jBI�B=B;dB<�B9�B9$B9�B6�B7�B6�B6FB6FB5B49B5B3�B0UB/�B0�B0!B/�B.}B-�B0�B.B*�B)�B*�B,�B%zBDgB�B%�B!B \BB�B�BCBB�B�B=BkB�B�B�BkB 'B�BSB$BB�BB�B�B	lB_B%B	�B�B�B�BMB�BDB�BGB�B��B��B�B�fB��B��B�B�B�MB��B�GB��B�B�AB�oB�vB�`B�B��B��B�]B�B�"B�B�"B�B�B� B�B�oB��B�B�B�B�B�KB�QB�B�QB�B�DB�QB��B��B��B�8B��B�B�DB�B�QB�B��B�WB�B��B�dB�B��B�QB��BܒB�BٴB�KBخBیB�QBٴB�EB��B�B�B�QB��B�EB�QB�#B�]B�`B�,B�,B�,B��B��B�&B�HB��B��B�dB�#B��B�RB�RB��B�2B�gB�gB�6B�B��B��B��B�BB�B��B��B�B�^B�0B�^B��B��B�dB�^B��B��B��B��B��B�FB�?B�B�B�9B�3B��B��B�qB�}B˒B��B��B�nB�tB�B��B�PB��B�B��B��B��Bp;By�BjBk�BqBs�Bh�Bj�Bl�BdZBw2B\�B\]B\�B]dBZ�BZ�BY�BZ�B]�BW
B[�BZBZB\)BW�B\]BZ�B[#BW�BX�BZB\�BPHBS�BV�BS&BP�BP�BT�BI�BW�BXyBbNBDgBA�B>�B;dB7�B:�B4�B7LB2�B0�B1'B/�B.}B1�B/B+�B,�B-wB(�B*0B(�B$B$@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022121614013820221216140138IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022122610014620221226100146QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022122610014620221226100146QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                