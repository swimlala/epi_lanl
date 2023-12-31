CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  u   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-11-25T22:01:59Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  _�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {D   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � !<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � <�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � C�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` _x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   _�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   e�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   k�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T q�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   r,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   r4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   r<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   rD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � rL   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   r�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   r�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    r�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        s   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        s   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       s    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    s(Argo profile    3.1 1.2 19500101000000  20221125220159  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_238                 6810_008521_238                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @� ����2@� ����211  @� ���"�@� ���"�@2��"�@2��"��d�<��*��d�<��*�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@}p�@�  @�  @�G�@��RA�RA\)A,(�A@  A`  A~{A�\)A�\)A�\)A��A�Q�A�  A�B   BQ�B(�B(�B�
B'�
B0  B7�
B@(�BH(�BO�
BX(�B`(�Bh  Bp(�Bx(�B�{B��B��B�  B�{B��
B��
B��
B��
B��
B�  B�(�B�{B�{B�{B��B�  B��
B��B��
B��B�  B�{B�  B�  B�{B�{B�{B�(�B�(�B�  B�  C   C
=C  C��C  C
  C  C��C  C  C  C  C  C  C��C��C 
=C"{C$  C&  C(  C*
=C+��C.  C0  C2
=C4
=C6  C8{C:  C;�C>  C@
=CB{CD{CE��CH  CJ  CL  CN  CO��CR  CS��CU�CW��CZ
=C\  C]��C`  Cb  Cd  Ce��Cg��Ci��Ck�Cm��Cp  Cr  Cs�HCu��Cx
=Cz  C{��C~  C�  C���C�  C���C���C���C�
=C�
=C�C�C�C���C���C���C�  C���C���C�  C�C�
=C�C�
=C�C�  C���C���C�  C�C�  C�  C�  C�  C�  C�C�
=C�
=C�
=C�
=C�  C���C�  C�C�C�  C�C�C�C�  C���C���C�  C�C�  C�C�
=C�
=C�C�C�  C�  C�  C�C�
=C�  C���C���C���C���C���C�
=C�
=C�C�C�  C���C���C���C�  C�C�  C���C���C���C�  C�  C���C���C���C���C�  C�C�
=C�C���C���C�  C���C�  C�C�C�
=C�  C���C�C�C�  C�  C�C�C�  C�C�  C���C���C���C���C���C���C�C�  C���C���C���C���C���C���C���C�  C���D xRD �qD� D�D�D�D� D  D� D�qD}qD  D��D  D}qD�qD� D�qD	}qD
  D
� D�D}qD�qD��D�D}qD  D� D�qDz�D  D��D�D� D�qD}qD�qD� D�D� D�qDz�D�RDz�D  D��D�qD}qD�qD� D  Dz�D�qD��D�D� D  D� D  D� D�qD� D�qD � D!�D!}qD"  D"��D#D#� D#��D$}qD$�qD%� D&�D&��D'  D'}qD'�qD(}qD(�qD)z�D)�qD*��D*�qD+}qD,  D,�D-�D-�D.�D.� D/  D/��D/�qD0}qD1�D1��D2�D2��D2�qD3}qD4  D4��D5  D5� D6D6��D7  D7� D8  D8� D9�D9��D9�qD:��D;  D;}qD<  D<��D=D=� D=�qD>��D?�D?}qD?�qD@� DA�DA� DB  DB� DC  DC� DDDD� DE  DE��DF  DF� DF�qDG}qDH  DH��DI�DI� DJ  DJ}qDK  DK� DL�DL}qDL�RDM� DNDN�DODO�DPDP�DQ�DQ�DR�DR� DS  DS��DT�DT}qDU  DU� DV  DV}qDV�qDW��DX�DX� DY  DY� DZ�DZ� DZ�qD[� D[�qD\}qD]  D]}qD]�qD^��D^�qD_}qD_��D`}qDa  Da� Db�Db��Dc  Dc� Dc�qDd� De�De��De�qDf}qDg  Dg� Dh  Dh��Di  Di}qDj  Dj� Dk�Dk��Dl�Dl��Dm  Dmz�Dn  Dn}qDo  Do� Do�qDp}qDp�qDqz�Dq�qDr� Dr�qDs� Dt�Dt��Du�Du��Dv�Dv��Dw  Dw� Dx  Dx}qDx��Dyz�Dy�qDz��D{�D{� D|  D|��D}�D}� D~�D~�D�D��D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D���D�@ D��HD��HD�HD�B�D��HD�� D�HD�B�D�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�AHD�� D�� D��qD�=qD�� D�� D�HD�B�D�� D�� D�HD�<)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?.{?�=q?�33?���?�@�\@(�@&ff@B�\@L��@aG�@n{@��
@��@���@��\@�=q@�
=@��R@�=q@�33@�  @���@��@�p�A�\A��A(�A��AffA��A\)A#�
A(��A.�RA2�\A8Q�A9��A?\)AB�\AG
=AK�AN�RATz�AW�A\��AaG�Ac�
Ah��Aj�HAp��Ar�\Aw�Az=qA~{A��A��A�A�  A���A�z�A�A���A�=qA��A��A���A�(�A�ffA���A�z�A��RA���A��HA�ffA�G�A��
A��A�G�A�z�A��RA��A���A�
=Aʏ\A�z�A�  A��A��A׮A��A�p�A�
=A�\A�(�A�
=A���A��
A�A�Q�A�\A��A��A�G�A�(�A�p�B Q�B ��BffB\)B��B��B�RB(�B��B
�\B33B��B��B
=B(�B�B�RB�B�B�B�Bz�BffB33Bz�BG�B�RB�B ��B!��B"�RB$(�B$��B&ffB'\)B(z�B*=qB+
=B,��B-�B.�HB0��B1��B2�HB4Q�B5�B6�RB7�B9�B:=qB;�B<��B>=qB?�B@z�BBffBC\)BD��BE�BG\)BH��BIp�BK
=BL(�BMG�BN�\BO�BP��BQBS33BT(�BUG�BV�RBW\)BX��BY��B[
=B\  B\��B^�\B_�B`��Bb=qBb�HBdQ�Be��Bf�RBh(�Bi�Bj�RBk�
Bl��BnffBo33Bpz�Bq��Br�\Bt  Bt��Bu�Bw�Bxz�By��Bz�HB{�B}G�B~ffB33B�=qB���B��B��
B�ffB���B��B��
B�z�B�
=B�\)B�{B�ffB��HB��B��
B�ffB���B�G�B��B�(�B��RB�33B��B�=qB�z�B��B��B��B��\B���B�p�B��
B�Q�B���B�33B��
B�{B���B�33B�p�B�{B��RB�
=B��B�(�B���B�G�B���B�=qB��RB��B��
B�(�B���B��B��B�(�B�z�B�33B�p�B�{B�z�B��HB���B�  B��\B��B���B�ffB���B�\)B�{B�ffB�G�B��
B�Q�B��B���B�Q�B�
=B�p�B�Q�B���B�\)B�(�B�z�B�33B��B�{B��RB�
=B��B�  B�ffB�
=B�33B��B�{B�Q�B��HB�
=B�\)B�B��
B�Q�B��\B��RB�
=B�p�B��B�  B�=qB�ffB��HB��B�G�B�B��B�{B��\B���B��HB�\)B�p�B��B��B�(�B�=qB��RB���B���B�p�B�p�B�B�(�B�(�B�ffB��RB���B�G�B�\)B���B�{B�(�B�z�B���B��HB�G�B���B��B�  B�ffB�ffB��RB�
=B�
=B�p�B��B�B�(�B�{B�ffB¸RB���B�
=B�G�B�\)BîB��B��B�(�Bď\Bď\B���B�33B�G�B�p�B��B�{B�ffB��HB���B�G�BǮB�B�{Bȏ\Bȣ�B�
=B�\)B�p�B��
B�(�B�=qBʣ�B���B�
=B�\)B�B��
B�(�B̏\Ḅ�B��HB�\)BͅB�B�=qB�ffBΏ\B�
=B�\)BυB��
B�=qB�ffBиRB��B�G�BхB�  B�(�B�Q�B���B���B�33BӮB��B�{B�ffB���B��B�\)B�B��B�ffB��HB���B�G�B�B�{B�Q�B���B�33B�\)BٮB�(�B�z�Bڏ\B�
=B�p�Bۙ�B�  B�ffBܣ�B���B�G�BݮB�B�Q�Bޏ\B޸RB�33B�p�Bߙ�B�(�B�ffB��\B�
=B�\)BᙚB��B�ffB��B��HB�p�B㙚B��B�ffB�\B���B�\)B噚B�B�(�B��B���B��B�p�B��B�=qB�z�B���B�G�B�B��
B�Q�B��HB��B�\)B��
B�Q�B�\B�
=B�B�B�{B��B�
=B�G�B�B�{B�ffB�RB�G�B�B�B�=qB���B�
=B�\)B�B�Q�B���B��HB�p�B��
B�{B��\B�
=B�\)B��B�{B��\B��RB��B��B�{B�Q�B��HB�\)B��B�  B���B��HB�G�B��
B�  B��\B��HB��B��B��C =qC p�C �\C �HC  C(�Cz�C�\CC
=C(�CffC��CC��C=qCQ�C��C��C�C33CffC�C�
C  C�Cp�C�CC
=C=qCffC�C�
C  CG�C�C��C�C(�CG�C�\CC�C	33C	p�C	�\C	C
{C
(�C
p�C
�C
��C{CQ�Cp�C�RC��C{Cp�C��CC
=C=qCffC�C�
C  CG�Cz�C��C�C(�CG�Cz�CC  C{CQ�C��CC��C33Cp�C��C�
C�CG�Cp�C�RC  C(�CG�C��C�
C��C(�Cp�C�C�
C  CG�C�C�C�
C{C\)C�\C�C�C(�Cp�C�\CC{C33CffC�C�HC  CG�C�\C�RC�HC33CffC�\CC�C=qCp�C�RC��C{CffC��CC  CG�Cp�C��C�HC(�CffC�CC
=CG�Cz�C��C�HC 33C p�C ��C ��C!  C!G�C!�\C!��C!��C"�C"p�C"�C"�C#
=C#G�C#�\C#��C$
=C$G�C$z�C$��C$�HC%33C%z�C%�RC%�C&{C&Q�C&��C&�HC'{C'=qC'z�C'�RC(
=C(Q�C(�\C(C(��C)(�C)ffC)��C)�C*33C*p�C*�C*�HC+�C+Q�C+��C+�C,�C,Q�C,�\C,�
C-�C-Q�C-z�C-�
C.{C.Q�C.�C.�RC.�C/(�C/p�C/�RC0  C033C0ffC0��C0�HC133C1�C1C2  C233C2p�C2�C3  C3G�C3�C3�RC3��C4(�C4p�C4�RC5  C533C5ffC5��C5�
C6(�C6p�C6��C6�
C7(�C7p�C7��C7�
C8(�C8p�C8��C8�
C9�C9p�C9�C9�HC:{C:\)C:��C:�HC;(�C;\)C;�\C;C<  C<=qC<�C<��C={C=Q�C=�\C=��C>  C>=qC>p�C>�C>�C?33C?p�C?�RC@
=C@G�C@�C@�RC@��CA33CAz�CA��CB{CBG�CBz�CB�RCC  CCQ�CC�\CC��CD  CDG�CD�CD�RCD��CE33CEp�CE�RCE��CF=qCF�CF��CG
=CGQ�CGz�CG�RCH  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @=p�@}p�@�  @�  @�G�@��RA�RA\)A,(�A@  A`  A~{A�\)A�\)A�\)A��A�Q�A�  A�B   BQ�B(�B(�B�
B'�
B0  B7�
B@(�BH(�BO�
BX(�B`(�Bh  Bp(�Bx(�B�{B��B��B�  B�{B��
B��
B��
B��
B��
B�  B�(�B�{B�{B�{B��B�  B��
B��B��
B��B�  B�{B�  B�  B�{B�{B�{B�(�B�(�B�  B�  C   C
=C  C��C  C
  C  C��C  C  C  C  C  C  C��C��C 
=C"{C$  C&  C(  C*
=C+��C.  C0  C2
=C4
=C6  C8{C:  C;�C>  C@
=CB{CD{CE��CH  CJ  CL  CN  CO��CR  CS��CU�CW��CZ
=C\  C]��C`  Cb  Cd  Ce��Cg��Ci��Ck�Cm��Cp  Cr  Cs�HCu��Cx
=Cz  C{��C~  C�  C���C�  C���C���C���C�
=C�
=C�C�C�C���C���C���C�  C���C���C�  C�C�
=C�C�
=C�C�  C���C���C�  C�C�  C�  C�  C�  C�  C�C�
=C�
=C�
=C�
=C�  C���C�  C�C�C�  C�C�C�C�  C���C���C�  C�C�  C�C�
=C�
=C�C�C�  C�  C�  C�C�
=C�  C���C���C���C���C���C�
=C�
=C�C�C�  C���C���C���C�  C�C�  C���C���C���C�  C�  C���C���C���C���C�  C�C�
=C�C���C���C�  C���C�  C�C�C�
=C�  C���C�C�C�  C�  C�C�C�  C�C�  C���C���C���C���C���C���C�C�  C���C���C���C���C���C���C���C�  C���D xRD �qD� D�D�D�D� D  D� D�qD}qD  D��D  D}qD�qD� D�qD	}qD
  D
� D�D}qD�qD��D�D}qD  D� D�qDz�D  D��D�D� D�qD}qD�qD� D�D� D�qDz�D�RDz�D  D��D�qD}qD�qD� D  Dz�D�qD��D�D� D  D� D  D� D�qD� D�qD � D!�D!}qD"  D"��D#D#� D#��D$}qD$�qD%� D&�D&��D'  D'}qD'�qD(}qD(�qD)z�D)�qD*��D*�qD+}qD,  D,�D-�D-�D.�D.� D/  D/��D/�qD0}qD1�D1��D2�D2��D2�qD3}qD4  D4��D5  D5� D6D6��D7  D7� D8  D8� D9�D9��D9�qD:��D;  D;}qD<  D<��D=D=� D=�qD>��D?�D?}qD?�qD@� DA�DA� DB  DB� DC  DC� DDDD� DE  DE��DF  DF� DF�qDG}qDH  DH��DI�DI� DJ  DJ}qDK  DK� DL�DL}qDL�RDM� DNDN�DODO�DPDP�DQ�DQ�DR�DR� DS  DS��DT�DT}qDU  DU� DV  DV}qDV�qDW��DX�DX� DY  DY� DZ�DZ� DZ�qD[� D[�qD\}qD]  D]}qD]�qD^��D^�qD_}qD_��D`}qDa  Da� Db�Db��Dc  Dc� Dc�qDd� De�De��De�qDf}qDg  Dg� Dh  Dh��Di  Di}qDj  Dj� Dk�Dk��Dl�Dl��Dm  Dmz�Dn  Dn}qDo  Do� Do�qDp}qDp�qDqz�Dq�qDr� Dr�qDs� Dt�Dt��Du�Du��Dv�Dv��Dw  Dw� Dx  Dx}qDx��Dyz�Dy�qDz��D{�D{� D|  D|��D}�D}� D~�D~�D�D��D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D���D�@ D��HD��HD�HD�B�D��HD�� D�HD�B�D�� D�� D�HD�AHD��HD�� D�  D�>�D�~�D���D�  D�AHD�� D�� D��qD�=qD�� D�� D�HD�B�D�� D�� D�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?.{?�=q?�33?���?�@�\@(�@&ff@B�\@L��@aG�@n{@��
@��@���@��\@�=q@�
=@��R@�=q@�33@�  @���@��@�p�A�\A��A(�A��AffA��A\)A#�
A(��A.�RA2�\A8Q�A9��A?\)AB�\AG
=AK�AN�RATz�AW�A\��AaG�Ac�
Ah��Aj�HAp��Ar�\Aw�Az=qA~{A��A��A�A�  A���A�z�A�A���A�=qA��A��A���A�(�A�ffA���A�z�A��RA���A��HA�ffA�G�A��
A��A�G�A�z�A��RA��A���A�
=Aʏ\A�z�A�  A��A��A׮A��A�p�A�
=A�\A�(�A�
=A���A��
A�A�Q�A�\A��A��A�G�A�(�A�p�B Q�B ��BffB\)B��B��B�RB(�B��B
�\B33B��B��B
=B(�B�B�RB�B�B�B�Bz�BffB33Bz�BG�B�RB�B ��B!��B"�RB$(�B$��B&ffB'\)B(z�B*=qB+
=B,��B-�B.�HB0��B1��B2�HB4Q�B5�B6�RB7�B9�B:=qB;�B<��B>=qB?�B@z�BBffBC\)BD��BE�BG\)BH��BIp�BK
=BL(�BMG�BN�\BO�BP��BQBS33BT(�BUG�BV�RBW\)BX��BY��B[
=B\  B\��B^�\B_�B`��Bb=qBb�HBdQ�Be��Bf�RBh(�Bi�Bj�RBk�
Bl��BnffBo33Bpz�Bq��Br�\Bt  Bt��Bu�Bw�Bxz�By��Bz�HB{�B}G�B~ffB33B�=qB���B��B��
B�ffB���B��B��
B�z�B�
=B�\)B�{B�ffB��HB��B��
B�ffB���B�G�B��B�(�B��RB�33B��B�=qB�z�B��B��B��B��\B���B�p�B��
B�Q�B���B�33B��
B�{B���B�33B�p�B�{B��RB�
=B��B�(�B���B�G�B���B�=qB��RB��B��
B�(�B���B��B��B�(�B�z�B�33B�p�B�{B�z�B��HB���B�  B��\B��B���B�ffB���B�\)B�{B�ffB�G�B��
B�Q�B��B���B�Q�B�
=B�p�B�Q�B���B�\)B�(�B�z�B�33B��B�{B��RB�
=B��B�  B�ffB�
=B�33B��B�{B�Q�B��HB�
=B�\)B�B��
B�Q�B��\B��RB�
=B�p�B��B�  B�=qB�ffB��HB��B�G�B�B��B�{B��\B���B��HB�\)B�p�B��B��B�(�B�=qB��RB���B���B�p�B�p�B�B�(�B�(�B�ffB��RB���B�G�B�\)B���B�{B�(�B�z�B���B��HB�G�B���B��B�  B�ffB�ffB��RB�
=B�
=B�p�B��B�B�(�B�{B�ffB¸RB���B�
=B�G�B�\)BîB��B��B�(�Bď\Bď\B���B�33B�G�B�p�B��B�{B�ffB��HB���B�G�BǮB�B�{Bȏ\Bȣ�B�
=B�\)B�p�B��
B�(�B�=qBʣ�B���B�
=B�\)B�B��
B�(�B̏\Ḅ�B��HB�\)BͅB�B�=qB�ffBΏ\B�
=B�\)BυB��
B�=qB�ffBиRB��B�G�BхB�  B�(�B�Q�B���B���B�33BӮB��B�{B�ffB���B��B�\)B�B��B�ffB��HB���B�G�B�B�{B�Q�B���B�33B�\)BٮB�(�B�z�Bڏ\B�
=B�p�Bۙ�B�  B�ffBܣ�B���B�G�BݮB�B�Q�Bޏ\B޸RB�33B�p�Bߙ�B�(�B�ffB��\B�
=B�\)BᙚB��B�ffB��B��HB�p�B㙚B��B�ffB�\B���B�\)B噚B�B�(�B��B���B��B�p�B��B�=qB�z�B���B�G�B�B��
B�Q�B��HB��B�\)B��
B�Q�B�\B�
=B�B�B�{B��B�
=B�G�B�B�{B�ffB�RB�G�B�B�B�=qB���B�
=B�\)B�B�Q�B���B��HB�p�B��
B�{B��\B�
=B�\)B��B�{B��\B��RB��B��B�{B�Q�B��HB�\)B��B�  B���B��HB�G�B��
B�  B��\B��HB��B��B��C =qC p�C �\C �HC  C(�Cz�C�\CC
=C(�CffC��CC��C=qCQ�C��C��C�C33CffC�C�
C  C�Cp�C�CC
=C=qCffC�C�
C  CG�C�C��C�C(�CG�C�\CC�C	33C	p�C	�\C	C
{C
(�C
p�C
�C
��C{CQ�Cp�C�RC��C{Cp�C��CC
=C=qCffC�C�
C  CG�Cz�C��C�C(�CG�Cz�CC  C{CQ�C��CC��C33Cp�C��C�
C�CG�Cp�C�RC  C(�CG�C��C�
C��C(�Cp�C�C�
C  CG�C�C�C�
C{C\)C�\C�C�C(�Cp�C�\CC{C33CffC�C�HC  CG�C�\C�RC�HC33CffC�\CC�C=qCp�C�RC��C{CffC��CC  CG�Cp�C��C�HC(�CffC�CC
=CG�Cz�C��C�HC 33C p�C ��C ��C!  C!G�C!�\C!��C!��C"�C"p�C"�C"�C#
=C#G�C#�\C#��C$
=C$G�C$z�C$��C$�HC%33C%z�C%�RC%�C&{C&Q�C&��C&�HC'{C'=qC'z�C'�RC(
=C(Q�C(�\C(C(��C)(�C)ffC)��C)�C*33C*p�C*�C*�HC+�C+Q�C+��C+�C,�C,Q�C,�\C,�
C-�C-Q�C-z�C-�
C.{C.Q�C.�C.�RC.�C/(�C/p�C/�RC0  C033C0ffC0��C0�HC133C1�C1C2  C233C2p�C2�C3  C3G�C3�C3�RC3��C4(�C4p�C4�RC5  C533C5ffC5��C5�
C6(�C6p�C6��C6�
C7(�C7p�C7��C7�
C8(�C8p�C8��C8�
C9�C9p�C9�C9�HC:{C:\)C:��C:�HC;(�C;\)C;�\C;C<  C<=qC<�C<��C={C=Q�C=�\C=��C>  C>=qC>p�C>�C>�C?33C?p�C?�RC@
=C@G�C@�C@�RC@��CA33CAz�CA��CB{CBG�CBz�CB�RCC  CCQ�CC�\CC��CD  CDG�CD�CD�RCD��CE33CEp�CE�RCE��CF=qCF�CF��CG
=CGQ�CGz�CG�RCH  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aء�AجAخAخAخAش9Aز-Aز-Aش9Aز-Aز-Aذ!Aز-Aش9AظRAغ^AظRAظRAغ^Aغ^AؾwAؾwAؾwAؾwAؾwA���A�A�A�A�A�ĜA�ĜA�ĜA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��A���A��
A��
A��
A��A��A��#A�ƨAЧ�A�|�A˺^A���A�ffA���A�p�AŇ+AÑhA�(�A�33A��;A���A���A���A�bNA���A���A�7LA�;dA��A���A�O�A��hA��A�G�A�C�A�XA��^A�+A�^5A�x�A���A�1A��A��A�O�A��A�+A���A��;A�K�A�S�A��-A��A��A��;A�S�A�`BA�bA�5?A��FA�M�A��A���A�ZA�S�A���A���A��;A���A��!A���A���A��A�ffA��DA\)A|5?Az�Au��Aq��Ao\)Aln�Ai��Ah��Af�+Ad�AcC�A`�RA_XA^1AZ5?AV��AS7LAQ7LAOXAL��AG�AEl�AC��A@�9A=�mA;�A8r�A6��A6v�A5O�A3��A2�9A1�A0I�A.9XA,�A+�#A+�hA*�!A*�A)`BA(�A&��A&�A&��A'VA&z�A$�A"�A!K�AdZAr�A��A�HA �A��A�hA�
A33A��A�!A��A�7Az�A��AI�A�DA�FAS�A=qA��AAA�A
��AG�A�A��A�A�Ar�A	?}A�A{A�A+A��A�yA�#AȴAVA�9A�;A�hA�A ��A ��A E�A @�l�@���@�=q@�@�@��@��T@�E�@���@�hs@���@���@�p�@�?}@�9@�E�@�@�
=@@�;d@�J@�u@�1@�ff@�O�@�%@�@��H@畁@��/@�|�@�t�@�1@�D@�j@��`@�G�@�r�@�33@◍@�&�@��@݁@�%@�bN@۶F@���@��@�;d@�E�@؛�@؃@׶F@�ff@���@�(�@�7L@��H@��y@�M�@ա�@ԋD@��@���@�  @Ӿw@�S�@җ�@���@�/@϶F@�ȴ@�v�@Η�@�J@͑h@�J@͙�@̛�@�j@˾w@�S�@�n�@ɑh@�V@��/@�ƨ@�V@�$�@ũ�@���@ă@ċD@���@Ĵ9@Å@��@��@�{@�x�@�%@�1@�|�@�33@�;d@�dZ@�"�@��\@�n�@���@���@�`B@��7@�{@��T@���@���@�5?@��@���@��h@�X@���@���@���@�V@��@�@�`B@��@��@��
@���@��@�dZ@���@�n�@��#@�?}@��`@���@�1'@��@�\)@�v�@��!@�^5@���@�`B@�p�@��@�S�@�"�@��R@�E�@���@�M�@��\@�M�@�-@�7L@���@�%@��h@�7L@��@��@��!@��-@��@�x�@�&�@�bN@�(�@��@���@��P@�dZ@�o@���@�J@���@���@���@�V@�%@�?}@�hs@�p�@��@�r�@�b@��m@�ƨ@�
=@��R@�ff@���@��@�Z@��w@���@�t�@�"�@��y@�^5@���@���@�hs@�%@�%@�/@�7L@�V@���@��j@��u@�1'@��@���@�l�@�C�@�o@���@�-@�@�x�@�X@�&�@���@���@�r�@�  @���@�;d@�o@���@���@�-@��#@���@�`B@�G�@��@��/@�z�@�9X@��;@���@�S�@�ȴ@���@��+@�5?@���@���@��@�hs@�%@���@��D@�r�@�Z@�(�@��m@��@�l�@�;d@�;d@���@�M�@��h@�&�@�&�@���@��j@��@�bN@��m@��F@��P@�|�@�t�@�l�@�+@�ȴ@���@�ff@�$�@��T@��^@�`B@�&�@�%@�G�@��D@��
@�t�@�33@�C�@�@�@���@��H@��!@�ff@�@��^@���@��@�G�@��`@��j@��@��@��@��@�(�@���@���@�l�@�@��!@��+@�@��#@���@�X@�G�@�?}@�V@��`@��j@���@�Z@� �@|�@~��@~$�@}@}��@}�hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A؟�Aا�Aأ�Aء�Aء�Aة�Aز-Aش9Aذ!Aز-Aة�Aذ!AجAذ!AجAجAخAز-AضFAذ!AضFAذ!Aز-Aش9Aذ!Aش9Aذ!Aش9Aز-Aز-AضFAذ!Aش9Aذ!Aذ!Aذ!AجAز-AخAز-Aذ!Aز-Aز-Aذ!Aش9AخAز-Aز-AخAش9AخAضFAخAضFAذ!Aز-AظRAز-AضFAضFAز-AظRAش9AؼjAظRAؼjAغ^AضFAؾwAظRAغ^Aغ^AظRAغ^AضFAظRAظRAضFAغ^AضFAغ^Aش9AظRAغ^Aش9Aغ^AضFAؼjAظRAؼjAغ^AظRAؼjAظRAؼjAغ^A���Aغ^A���AؼjAؾwAؼjAؾwAؾwAؼjAؾwAغ^A�AؼjA���AؾwA���AؾwAؼjAؾwAغ^A���Aغ^AؾwAغ^A���AؼjAؼjA�AؼjA�AؼjA���AؼjA�AؼjA�AؼjA�AؼjA�AؾwA�A�ĜA���A�A���A���A�ƨA���A�ĜA�AؾwA�ƨA���A�A�ƨA���A�ĜAؾwA�ĜA���A�A�ĜA�A�ĜA���A�ƨA���A�A�A�ƨA�ƨA�A�ƨA�A�ĜA�ĜA�A�ƨA���A�ȴA�ĜA�ĜA�ĜA�A�ƨA�ĜA���A�ƨA�ĜA���A�ƨA�ȴA���A�ƨA���A�ȴA�ȴA���A�ƨA���A���A�ƨA���A�ƨA���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A�ȴA���A���A���A���A���A�ƨA���A�ƨA���A���A�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A�ȴA���A���A���A��A��
A��#A��A��
A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A��/A��
A��A��#A��
A��#A���A���A��A���A���A��
A���A��
A���A���A��A���A��A��
A���A��A���A��A���A��
A��A���A��A��A���A��A���A��A��A���A��A��A���A��A��#A���A��#A��A���A��#A��#A��
A��#A��A��
A��#A��
A��A��/A��A��
A��#A��#A��
A��#A��#A��
A��/A��
A��A��#A��
A��#A��#A��
A��/A��A��A��/A��
A��A��#A��
A��#A��/A��A��#A��/A��A��#A��/A��
A��/A��#A��A��/A��
A��A��#A��
A��#A��#A��
A��#A��/A��A���A���A�ĜA���Aش9A؛�A؉7A�E�A�7LA���Aִ9A�I�A��AθRA�l�A�M�A�A�A��A���A��TA���A͸RAͩ�Aͣ�Aͥ�Aͧ�A͡�A͛�A͗�A͉7A�n�A�jA�ffA�ZA�I�A�/A�JA��;A�jA�&�A��A˧�AˁA�z�A�t�A�l�A�hsA�jA�`BA�ZA�XA�O�A�E�A�C�A�/A�"�A�JA��#AʶFAʣ�AʁA�l�A�G�A�-A�bA���A���A���A��`Aɟ�A�5?A�A���A��A��mA��TA��`A��HA��
A��A��
A���A���A���A�ȴA�A�A���AȺ^AȸRAȶFAȮAȮAȩ�Aȥ�Aȡ�AȍPA�p�A�=qA���A��;Aǉ7A��;AƋDA��A���AŮAœuA�t�A�{A��TA���A�AĸRAĶFAĶFAĬAĝ�A�v�A�AÇ+A��A+A�G�A�+A��A���A���A��uA�O�A��A�JA��A��;A��jA��\A�v�A�n�A�VA�9XA�5?A�5?A�-A��A��A�bA�
=A��A��TA���A��A���A�ƨA�|�A�E�A�VA��A��TA��#A���A��FA��A���A���A��+A�x�A�dZA�E�A�1A�
=A�
=A���A��9A��A�^5A�?}A�1A��`A��HA��/A��A��
A���A���A���A���A�ĜA��^A��-A��A���A��7A�ffA�1'A�+A�+A� �A��A��A��mA�9XA���A��jA��A�7LA�%A��HA�A���A�|�A�bNA�?}A��A��
A���A�E�A���A��7A�K�A��mA�`BA�JA���A���A���A���A���A���A���A���A���A��uA�ffA�A���A�p�A�ZA�S�A�A�A�5?A��A�%A���A��A��TA��;A��#A���A���A���A�ƨA�ȴA�ĜA��jA��^A��-A���A��DA��A�r�A�G�A�+A�A��HA�A��+A�$�A���A�z�A�jA�K�A�oA�bA�JA�A�  A�A�A���A��A��A��/A���A���A���A�t�A�\)A�G�A�7LA�/A��A���A���A�ĜA���A�n�A�S�A�?}A��A�A���A���A���A���A��PA�z�A�l�A�ZA�-A�  A��A��TA���A���A�ȴA�A��RA��9A���A��DA�p�A�ZA�G�A�9XA�"�A�$�A�bA���A���A��A��/A���A���A���A�(�A�A��yA�ƨA��-A���A��\A��7A�x�A�n�A�l�A�bNA�\)A�7LA�/A�
=A��A��A��FA���A�~�A�VA��A��uA�-A��`A�ĜA��A���A�|�A�p�A�hsA�dZA�`BA�O�A�JA�ȴA��\A�v�A�l�A�-A��A�ȴA�dZA���A���A�ĜA�A��RA��A���A�Q�A�JA��!A���A�z�A�K�A�A���A���A�1'A�ƨA�z�A�`BA�Q�A�?}A�/A�$�A��A��A��A��A��A�1A��A��/A��jA��uA�t�A�hsA�ZA�7LA�{A��A��A��\A��A�hsA�XA�-A��TA��+A�;dA��;A�l�A�
=A���A���A��A�ffA�I�A�1'A�oA���A��A��`A���A���A���A��\A�|�A�jA�Q�A�&�A�
=A��A��#A���A��9A���A��\A�r�A�G�A�{A��HA���A���A��A�l�A�O�A�+A�A��TA��-A��uA��hA��hA��+A�z�A�l�A�ZA�M�A�;dA�"�A�1A���A���A���A�v�A�?}A�JA��A��A�?}A�+A��A�{A�1A���A��#A��-A�z�A�^5A�5?A�"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aء�AجAخAخAخAش9Aز-Aز-Aش9Aز-Aز-Aذ!Aز-Aش9AظRAغ^AظRAظRAغ^Aغ^AؾwAؾwAؾwAؾwAؾwA���A�A�A�A�A�ĜA�ĜA�ĜA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��A���A��
A��
A��
A��A��A��#A�ƨAЧ�A�|�A˺^A���A�ffA���A�p�AŇ+AÑhA�(�A�33A��;A���A���A���A�bNA���A���A�7LA�;dA��A���A�O�A��hA��A�G�A�C�A�XA��^A�+A�^5A�x�A���A�1A��A��A�O�A��A�+A���A��;A�K�A�S�A��-A��A��A��;A�S�A�`BA�bA�5?A��FA�M�A��A���A�ZA�S�A���A���A��;A���A��!A���A���A��A�ffA��DA\)A|5?Az�Au��Aq��Ao\)Aln�Ai��Ah��Af�+Ad�AcC�A`�RA_XA^1AZ5?AV��AS7LAQ7LAOXAL��AG�AEl�AC��A@�9A=�mA;�A8r�A6��A6v�A5O�A3��A2�9A1�A0I�A.9XA,�A+�#A+�hA*�!A*�A)`BA(�A&��A&�A&��A'VA&z�A$�A"�A!K�AdZAr�A��A�HA �A��A�hA�
A33A��A�!A��A�7Az�A��AI�A�DA�FAS�A=qA��AAA�A
��AG�A�A��A�A�Ar�A	?}A�A{A�A+A��A�yA�#AȴAVA�9A�;A�hA�A ��A ��A E�A @�l�@���@�=q@�@�@��@��T@�E�@���@�hs@���@���@�p�@�?}@�9@�E�@�@�
=@@�;d@�J@�u@�1@�ff@�O�@�%@�@��H@畁@��/@�|�@�t�@�1@�D@�j@��`@�G�@�r�@�33@◍@�&�@��@݁@�%@�bN@۶F@���@��@�;d@�E�@؛�@؃@׶F@�ff@���@�(�@�7L@��H@��y@�M�@ա�@ԋD@��@���@�  @Ӿw@�S�@җ�@���@�/@϶F@�ȴ@�v�@Η�@�J@͑h@�J@͙�@̛�@�j@˾w@�S�@�n�@ɑh@�V@��/@�ƨ@�V@�$�@ũ�@���@ă@ċD@���@Ĵ9@Å@��@��@�{@�x�@�%@�1@�|�@�33@�;d@�dZ@�"�@��\@�n�@���@���@�`B@��7@�{@��T@���@���@�5?@��@���@��h@�X@���@���@���@�V@��@�@�`B@��@��@��
@���@��@�dZ@���@�n�@��#@�?}@��`@���@�1'@��@�\)@�v�@��!@�^5@���@�`B@�p�@��@�S�@�"�@��R@�E�@���@�M�@��\@�M�@�-@�7L@���@�%@��h@�7L@��@��@��!@��-@��@�x�@�&�@�bN@�(�@��@���@��P@�dZ@�o@���@�J@���@���@���@�V@�%@�?}@�hs@�p�@��@�r�@�b@��m@�ƨ@�
=@��R@�ff@���@��@�Z@��w@���@�t�@�"�@��y@�^5@���@���@�hs@�%@�%@�/@�7L@�V@���@��j@��u@�1'@��@���@�l�@�C�@�o@���@�-@�@�x�@�X@�&�@���@���@�r�@�  @���@�;d@�o@���@���@�-@��#@���@�`B@�G�@��@��/@�z�@�9X@��;@���@�S�@�ȴ@���@��+@�5?@���@���@��@�hs@�%@���@��D@�r�@�Z@�(�@��m@��@�l�@�;d@�;d@���@�M�@��h@�&�@�&�@���@��j@��@�bN@��m@��F@��P@�|�@�t�@�l�@�+@�ȴ@���@�ff@�$�@��T@��^@�`B@�&�@�%@�G�@��D@��
@�t�@�33@�C�@�@�@���@��H@��!@�ff@�@��^@���@��@�G�@��`@��j@��@��@��@��@�(�@���@���@�l�@�@��!@��+@�@��#@���@�X@�G�@�?}@�V@��`@��j@���@�Z@� �@|�@~��@~$�@}@}��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A؟�Aا�Aأ�Aء�Aء�Aة�Aز-Aش9Aذ!Aز-Aة�Aذ!AجAذ!AجAجAخAز-AضFAذ!AضFAذ!Aز-Aش9Aذ!Aش9Aذ!Aش9Aز-Aز-AضFAذ!Aش9Aذ!Aذ!Aذ!AجAز-AخAز-Aذ!Aز-Aز-Aذ!Aش9AخAز-Aز-AخAش9AخAضFAخAضFAذ!Aز-AظRAز-AضFAضFAز-AظRAش9AؼjAظRAؼjAغ^AضFAؾwAظRAغ^Aغ^AظRAغ^AضFAظRAظRAضFAغ^AضFAغ^Aش9AظRAغ^Aش9Aغ^AضFAؼjAظRAؼjAغ^AظRAؼjAظRAؼjAغ^A���Aغ^A���AؼjAؾwAؼjAؾwAؾwAؼjAؾwAغ^A�AؼjA���AؾwA���AؾwAؼjAؾwAغ^A���Aغ^AؾwAغ^A���AؼjAؼjA�AؼjA�AؼjA���AؼjA�AؼjA�AؼjA�AؼjA�AؾwA�A�ĜA���A�A���A���A�ƨA���A�ĜA�AؾwA�ƨA���A�A�ƨA���A�ĜAؾwA�ĜA���A�A�ĜA�A�ĜA���A�ƨA���A�A�A�ƨA�ƨA�A�ƨA�A�ĜA�ĜA�A�ƨA���A�ȴA�ĜA�ĜA�ĜA�A�ƨA�ĜA���A�ƨA�ĜA���A�ƨA�ȴA���A�ƨA���A�ȴA�ȴA���A�ƨA���A���A�ƨA���A�ƨA���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A�ȴA���A���A���A���A���A�ƨA���A�ƨA���A���A�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A�ȴA���A���A���A��A��
A��#A��A��
A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A��/A��
A��A��#A��
A��#A���A���A��A���A���A��
A���A��
A���A���A��A���A��A��
A���A��A���A��A���A��
A��A���A��A��A���A��A���A��A��A���A��A��A���A��A��#A���A��#A��A���A��#A��#A��
A��#A��A��
A��#A��
A��A��/A��A��
A��#A��#A��
A��#A��#A��
A��/A��
A��A��#A��
A��#A��#A��
A��/A��A��A��/A��
A��A��#A��
A��#A��/A��A��#A��/A��A��#A��/A��
A��/A��#A��A��/A��
A��A��#A��
A��#A��#A��
A��#A��/A��A���A���A�ĜA���Aش9A؛�A؉7A�E�A�7LA���Aִ9A�I�A��AθRA�l�A�M�A�A�A��A���A��TA���A͸RAͩ�Aͣ�Aͥ�Aͧ�A͡�A͛�A͗�A͉7A�n�A�jA�ffA�ZA�I�A�/A�JA��;A�jA�&�A��A˧�AˁA�z�A�t�A�l�A�hsA�jA�`BA�ZA�XA�O�A�E�A�C�A�/A�"�A�JA��#AʶFAʣ�AʁA�l�A�G�A�-A�bA���A���A���A��`Aɟ�A�5?A�A���A��A��mA��TA��`A��HA��
A��A��
A���A���A���A�ȴA�A�A���AȺ^AȸRAȶFAȮAȮAȩ�Aȥ�Aȡ�AȍPA�p�A�=qA���A��;Aǉ7A��;AƋDA��A���AŮAœuA�t�A�{A��TA���A�AĸRAĶFAĶFAĬAĝ�A�v�A�AÇ+A��A+A�G�A�+A��A���A���A��uA�O�A��A�JA��A��;A��jA��\A�v�A�n�A�VA�9XA�5?A�5?A�-A��A��A�bA�
=A��A��TA���A��A���A�ƨA�|�A�E�A�VA��A��TA��#A���A��FA��A���A���A��+A�x�A�dZA�E�A�1A�
=A�
=A���A��9A��A�^5A�?}A�1A��`A��HA��/A��A��
A���A���A���A���A�ĜA��^A��-A��A���A��7A�ffA�1'A�+A�+A� �A��A��A��mA�9XA���A��jA��A�7LA�%A��HA�A���A�|�A�bNA�?}A��A��
A���A�E�A���A��7A�K�A��mA�`BA�JA���A���A���A���A���A���A���A���A���A��uA�ffA�A���A�p�A�ZA�S�A�A�A�5?A��A�%A���A��A��TA��;A��#A���A���A���A�ƨA�ȴA�ĜA��jA��^A��-A���A��DA��A�r�A�G�A�+A�A��HA�A��+A�$�A���A�z�A�jA�K�A�oA�bA�JA�A�  A�A�A���A��A��A��/A���A���A���A�t�A�\)A�G�A�7LA�/A��A���A���A�ĜA���A�n�A�S�A�?}A��A�A���A���A���A���A��PA�z�A�l�A�ZA�-A�  A��A��TA���A���A�ȴA�A��RA��9A���A��DA�p�A�ZA�G�A�9XA�"�A�$�A�bA���A���A��A��/A���A���A���A�(�A�A��yA�ƨA��-A���A��\A��7A�x�A�n�A�l�A�bNA�\)A�7LA�/A�
=A��A��A��FA���A�~�A�VA��A��uA�-A��`A�ĜA��A���A�|�A�p�A�hsA�dZA�`BA�O�A�JA�ȴA��\A�v�A�l�A�-A��A�ȴA�dZA���A���A�ĜA�A��RA��A���A�Q�A�JA��!A���A�z�A�K�A�A���A���A�1'A�ƨA�z�A�`BA�Q�A�?}A�/A�$�A��A��A��A��A��A�1A��A��/A��jA��uA�t�A�hsA�ZA�7LA�{A��A��A��\A��A�hsA�XA�-A��TA��+A�;dA��;A�l�A�
=A���A���A��A�ffA�I�A�1'A�oA���A��A��`A���A���A���A��\A�|�A�jA�Q�A�&�A�
=A��A��#A���A��9A���A��\A�r�A�G�A�{A��HA���A���A��A�l�A�O�A�+A�A��TA��-A��uA��hA��hA��+A�z�A�l�A�ZA�M�A�;dA�"�A�1A���A���A���A�v�A�?}A�JA��A��A�?}A�+A��A�{A�1A���A��#A��-A�z�A�^5A�5?A�"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B�tB��B�nB�nB�:B��B�:B�nB��B�:B��B�:B�:B�B��B��B��B�:B��B��B��B��B�B��B��B��B��B��B��B�B��B��B�B��B��B��B��B��B��B�B�nB�B�nB�B�nB�B��B�:B��B�B�B�B��B�-B�Bt�B��B�$B�*B��B�$B��B��B��B��B�JB�Bu�Bw2BqvBncBj�BbNBa|B\]BT�BOBBNBFtB9�B4�B-wB&LB 'B�B�BSB�B�B�lB�2B�GB��B�6B� B�)B��B��B�"B��B�B}�B{�Bs�B_pBR�BS�B_�B^�B]�BU2BGB3�B�B
�JB
�]B
��B
��B
|B
k�B
c�B
W�B
I�B
-CB
#�B
�B	��B	�ZB	֡B	�wB	�$B	�kB	��B	��B	�lB	}�B	zxB	h�B	R B	E9B	2�B	)_B	!�B	bB�.B�xB��B�iB�WB�B�sB�TB��B��B��B�dB��B�B�[B�B�0B��B�zB�zB� B�OB�0B�aB��B�&BɺB��B��B��B�\B��B�wB��B�B�6B�)B�,B��B�9B�B�B��B�gB�pBĜB��B��B�B�RB�-B�RB�\B��B��B�B��B��B��B��B��B�aBߤB�dB� B	�B�PB�GB�B��B	 �B��B�`B�|B�B�B�B�B��B�|B��B�B�+B��B	�B	
rB��B�MB�B� B� B�B��B��B	SB	
�B	
rB		B	uB��B��B�B��B	B	�B	�B	
	B	B	B	1B	�B	�B	�B	'B	1�B	49B	0�B	.IB	*�B	&B	'�B	,=B	,qB	.IB	2�B	6�B	;dB	8RB	;dB	=B	@OB	A�B	B�B	LdB	\�B	c B	ffB	f�B	gB	g�B	h�B	j�B	k�B	l"B	m�B	j�B	m�B	k�B	i�B	lWB	p;B	xlB	wfB	�4B	�B	��B	�YB	��B	�B	��B	��B	��B	��B	�@B	��B	��B	�VB	�PB	��B	�.B	�YB	�kB	��B	��B	��B	�FB	�oB	��B	�oB	��B	�YB	��B	�B	�VB	�!B	�!B	��B	��B	��B	�@B	��B	��B	�kB	�B	��B	�zB	�B	��B	�XB	��B	��B	�0B	��B	��B	��B	�B	��B	��B	��B	ǮB	ɆB	�jB	�B	��B	�B	�HB	бB	бB	��B	�}B	��B	�&B	��B	רB	��B	�B	�B	�EB	��B	ԕB	רB	��B	ںB	�)B	�B	�B	�&B	��B	�B	�ZB	�yB	�"B	�B	�B	�DB	��B	�DB	�B	��B	�DB	�>B	�B	�B	�B	�B	�B	�B	�oB	�vB	�MB	�%B	�B	��B	�xB	��B	�VB	��B	��B	��B	�cB
 �B
B
 4B	�cB	��B	�.B	�.B	��B	��B	�cB
 4B
GB
{B
GB
MB
�B
YB
	B
�B
�B
"B
�B
�B
.B
�B
B
oB
�B
@B
B
uB
�B
{B
�B
�B
�B
�B
�B
MB
�B
$B
�B
�B
�B
�B
$B
YB
+B
�B
_B
1B
�B
�B
�B
1B
kB
CB
IB
B
B
!B
!B
VB
VB
�B
 'B
 �B
!-B
!�B
!�B
!�B
!�B
!�B
!�B
"hB
"�B
#nB
!�B
 �B
!B
 \B
 �B
 �B
 �B
 �B
 �B
 'B
 \B
 \B
 'B
 'B
"4B
!�B
!bB
!-B
"4B
!�B
#nB
"�B
#:B
#�B
'RB
'B
%�B
&�B
&�B
'B
($B
($B
($B
(�B
(�B
)�B
'�B
'�B
($B
($B
'�B
)*B
*0B
.B
/OB
/B
/B
.IB
.�B
.IB
.}B
/�B
.}B
.}B
.IB
-�B
.}B
/OB
0UB
0UB
0�B
1'B
1�B
1�B
2�B
2�B
1�B
1'B
1�B
1�B
1�B
1�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B�FB�tB�FB��B�4B�B��B��B��B��B��B��B��B��B��B��B�@B��B�@B�B��B�tB��B�@B�4B�nB��B�bB�B�4B��B��B��B�tB��B�@B��B��B��B�:B�nB�4B�tB��B��B�tB�hB�@B��B��B��B��B�:B��B��B�B��B�@B��B��B��B�:B��B�hB��B�-B�nB�4B��B�:B�4B�B��B��B��B��B�B�4B��B��B��B�nB��B�@B��B��B�hB�B��B��B��B�4B��B��B�B��B��B��B�:B��B�4B�nB�hB��B��B��B��B�nB��B��B�nB�4B��B��B�@B�4B�B��B��B�nB��B��B��B�@B��B�B��B��B��B��B��B��B��B��B�hB��B�B��B�:B�B�bB�nB�4B��B�B�bB�nB��B��B�nB��B�B�4B�nB��B��B�nB��B��B��B��B�hB�:B�4B�hB�@B��B�B��B��B�:B�4B�B��B��B�:B�hB�B��B��B��B�:B�:B��B�nB��B�bB�B�4B�B�B��B��B�hB��B�:B��B��B�hB�hB�nB��B��B��B��B�nB�B��B��B��B�hB��B�hB�4B�B�4B�:B�:B��B��B�hB�4B�@B��B�B�B��B�@B�:B�:B��B�bB�@B�hB�nB��B�4B�tB��B�:B��B��B�@B�hB��B�:B�:B��B��B�@B�nB��B�tB�B�hB��B�B��B��B��B��B�hB��B�:B��B�B�B��B�nB�nB��B�4B�@B��B��B�hB�4B�zB��B��B��B�bB�nB�B��B�B��B�B��B�bB�B�nB��B�B�hB�:B�nB�bB�B��B��B��B��B�B�4B�B�:B��B�B��B�hB�B�4B��B��B�4B�@B�hB��B��B��B��B�B�4B��B�tB�4B�hB��B��B�nB��B��B��B�nB��B�:B��B�4B��B�B��B�4B�nB��B��B��B��B�nB��B�hB�:B�-B��B�B�-B��B�4B�bB�B��B��B�4B�bB��B��B��B��B��B��B��B�'B�B�'B��B�IB��B�CB��B�B��B��B�B�7B�$B��B��B�7B�_B��B��B��B�FBfB�B�6B�FB��B��B��B��B��B�~B�hB�\B��B�'B��B��B��B��B��B��B�hB��B�!B�hB��B��B��B��B��B��B�B�-B��B��B��B�LB��B��B�B��B��B�LB��B��B�*B��B�$B�B��B�*B��B�_B�wB��B��B�*B��B�RB��B�B��B��B�B�0B��B�0B�XB��B��B��B�$B��B�$B�B�RB��B��B�B��B��B��B�B��B�B�@B�hB��B�B��B��B�qB��B��B��B��B�:B�:B�4B��B��B�hB�4B��B�\B��B��B�~B��B�FB��B��B�kB��B��B�=B��B�!B��B��B�OB��B��B�B�FB��B��B��B� B��B��B�B��B�B��B��B��B�B��B��B��B��B~�B�7B{B��B|�BxBzxBxlBu�Bu�Bu�Br�Bt�Bu�BtTBv�Bw�Bt�Br�BrBv�Bt�Bx�Bv`Bw2Bu�Bw�Bq�Br�Br�BrBp�BqABp�BpoBn�BpBp�Bn/BncBt�BoiBs�Bk�BjBkBkBh�BuZB�;Bg8Bb�Bm�Bg�Be,Bc�B`vBb�B`�B_BaHBa|B`�B`vBh>Bg�Be�Bh
Bi�Bo5B]dB^jB^�BYKBZ�BZQBX�BW�BYBW�BW
BaBm�BZB_�BVBTaBWsBU�BU�BVmBR�BR�BR�BP}BPBP�BN�BPBP�BNBNBNpBL0BL�BMBL�BL�BL�BQ�BP�BMBN�BNBR BR�BQ�B@�B?}BD3BB�B:�B<�B;0B;�B9�B9$B:�B;dB8B;0B7�B7LB>B:�B3�B49B2aB0�B0�B49B.B,�B.�B2-B+�B,B0�B+�B+�B-wB+B*�B$tB&LB&�B"�B(�B#�B!�B!�B �B�BB�B �B�B"hB!�B"hBBxBB�B�B~BB�BB�B�B�B$�B�B	B�B�B�BkBMBB�BBBFB�BSB�BBVB�B
=B~B�BxB:BPB�B	�BoB;B��B�PB�B��B�fB�`B��B��B��B��B�B�B �B�B�MB�B�fB�B�B�|B��B�]B�B��B�+B�B�B�>B��B�B��B��B�B��BϫB̘B��B˒B��B�RB�B��B��B�,B�B��B�B�NB�2B��B�B�;BޞB��B�5B�B��B�B��BуB�<B҉B�NB��B��B�-B�B��B�B��B��B��B��B��B�B�4B�"B�VB�"B��B��B�VB��B�VB��B�\B�JB��B��B��B��B��B��B�AB�MBcB�{B�iB{�B}�B� BcB�iB�B{�B.B{JBzBz�B~�B}"B|�Bz�ByrB{B|Bx8BuZBy�Bt�BxBp�Bp;Bm�BsMBf2B_B^5B]/B\�B\�B\�B\�BY�BUgBV9BO44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                  444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                  444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022112522015920221125220159IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022120518013220221205180132QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022120518013220221205180132QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                