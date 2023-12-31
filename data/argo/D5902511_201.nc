CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-02-15T12:07:42Z creation; 2023-02-10T23:09:43Z DMQC;      
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
_FillValue        G�O�     (  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \0   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     (  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( (   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( >   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( e   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220215120742  20230210230943  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_201                 6810_008521_201                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٹ�!laR@ٹ�!laR11  @ٹ�PH�@ٹ�PH�@0<����@0<�����d�n�Y!�d�n�Y!11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?�@=p�@�G�@�G�@�G�@�  A   A  A   A*�HA?\)A`  A�  A���A���A�  A�Q�AϮA߮A�  B (�B  B  B�
B�
B'�B/�B7�
B@(�BH  BO�
BW�B_�
Bh  Bp  Bx  B�  B�{B�=qB�=qB��B��B��B�  B�  B�  B�{B�  B�{B�  B�  B�  B�{B��B��
B�{B�(�B�  B��B�  B�{B�  B��B�{B�{B�  B�(�B�  B��
C��C�C  C  C
  C  C  C��C  C��C�C��C  C��C��C   C!��C$  C&
=C(  C*
=C,
=C.
=C0
=C2
=C4  C5��C7�C9�C<  C>
=C@  CB  CD  CF  CH  CJ  CL
=CN  CP
=CR  CT  CV
=CX
=CZ  C[��C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co��Cq�Ct  Cv
=Cx  Cz  C|
=C}�C�  C�
=C���C�  C���C���C�  C�C�  C�  C�C�C�
=C�
=C�C�C�  C�  C�  C�
=C�C�  C�  C�C�C���C���C�  C�  C���C�C�
=C�C�  C���C�  C�C�  C�  C���C���C�  C�
=C�C�  C�  C�  C�  C���C�  C�  C�  C�C�
=C�  C�  C�  C�  C�  C�  C�C���C���C���C�  C�C�C�  C�C�  C�C�C�C�  C���C���C���C���C���C���C�C�C�  C�  C�  C�C�C�C�C�  C�C�C���C���C�  C�C�  C���C�  C�  C���C�  C�  C���C�C�C�  C�  C�  C���C�  C�C���C���C���C�  C�C���C���C�C���C�  C�  C�  C�C�  C�  C�D �D ��D�D��D  D� D�D� D�qD� DD}qD�qD� D  Dz�D�qD}qD�qD	� D
  D
}qD  D�DD� D  D��D�qD� D�qDz�D�qD� D�D� D  D� D  Dz�D�qD��DD��D  Dz�D�qD}qD  D}qD��D}qD�qD� D  D� D�D�D�D� D�D�D�D� D�qD ��D!  D!}qD!�qD"}qD#  D#}qD$  D$}qD$�qD%��D&�D&}qD'  D'��D(D(��D)D)�D)�qD*}qD*�qD+}qD,  D,� D-  D-� D.�D.�D/�D/��D0  D0� D1D1� D2�D2� D2�qD3� D4  D4�D5�D5}qD5�qD6��D7D7�D8�D8� D8�qD9}qD:�D:��D;  D;��D<  D<}qD<�qD=��D>D>��D?�D?� D@  D@��DA�DA}qDB  DB�DC�DC� DD�DD� DD��DE}qDF  DF��DG�DG��DH  DH}qDI  DI��DJDJ�DK  DK� DL  DL}qDL��DM}qDN  DN��DODO��DO�qDP� DQ  DQ}qDR  DR� DS  DS}qDS�qDT� DU�DU� DV  DV� DV�qDWz�DW�qDX}qDX�qDY� DZ  DZ�D[D[��D\�D\��D\�qD]� D^�D^}qD^�qD_� D`�D`��Da  Da��Da�qDbz�Dc  Dc}qDc�qDd� De�De�Df  Df� Dg�Dg��Dh  Dh� Dh�qDi}qDj  Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDn  Dn��Do�Do� Do�qDp� Dq�Dq��Dr�Dr}qDr�qDs}qDs�qDt}qDu�Du� Du�qDv}qDv�qDw��Dx  Dx� Dy�Dy� Dz  Dz��D{  D{� D|  D|}qD|�qD}}qD~  D~� D~�qD� D�  D�=qD�~�D���D�  D�@ D�� D��HD�HD�AHD�~�D���D�HD�AHD�� D�� D���D�@ D��HD��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D�~�D�� D�HD�B�D�� D��qD���D�@ D�~�D���D���D�AHD��HD���D��qD�>�D�� D��HD�HD�@ D�~�D���D�  D�>�D�� D��HD�  D�AHD���D��HD���D�AHD���D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�AHD��HD�� D���D�>�D�}qD���D�  D�AHD�� D���D�  D�@ D�� D��HD�HD�AHD���D��HD�  D�@ D�}qD�� D�  D�AHD�� D��qD���D�@ D�� D�� D�  D�@ D�� D���D�HD�AHD��HD��qD��qD�@ D��HD�� D�  D�>�D�� D��HD�HD�B�D��HD���D���D�@ D�~�D�� D��D�@ D�~�D���D���D�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D�~�D���D�  D�=qD�~�D���D���D�>�D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D���D���D�AHD�� D��qD���D�>�D�}qD���D���D�>�D���D�D���D�>�D�~�D��HD��D�C�D���D��HD���D�@ D�� D���D�  D�=qD�}qD���D�  D�AHD���D��HD�  D�AHD�� D���D�  D�>�D�� D��HD�  D�AHD��HD�� D�HD�AHD�~�D���D���D�>�D��HD��HD��D�AHD��HD��HD�  D�>�D�~�D�� D�HD�AHD�� D���D���D�>�D�� D��HD�  D�=qD��HD��HD���D�@ D��HD��qD�  D�B�D��HD��HD��D�AHD�}qD���D�HD�AHD�� D���D���D�>�D D��HD�  D�AHDÂ�D��HD���D�>�DĀ D�� D�  D�>�D�}qD��HD�HD�AHDƀ Dƾ�D���D�>�D�~�DǾ�D��qD�>�D�~�DȾ�D���D�=qD�~�D�� D�HD�@ DʁHD�D�  D�@ DˁHD�� D���D�@ D́HD��HD���D�=qD̀ D��HD�  D�@ D΀ D��HD�  D�AHDρHDϾ�D�  D�>�D�~�D�� D�HD�@ DсHD��HD�  D�AHDҁHDҾ�D��qD�@ DӀ D��HD�HD�AHDԁHDԾ�D���D�@ DՁHD�� D��qD�=qDցHD��HD���D�>�D�~�D�� D���D�@ D؁HD�� D���D�>�Dـ D��HD�  D�=qD�}qDھ�D�  D�@ D�}qD�� D�  D�=qD܀ D�� D���D�@ D݂�D��HD��qD�=qD�~�D�� D�  D�AHD߀ D߾�D���D�>�D��HD�� D���D�@ D� D��HD�HD�@ D₏D��HD�  D�>�D� D�� D�  D�>�D�}qD侸D�  D�@ D� D��HD�HD�>�D�}qD�qD�  D�AHD� D�� D���D�@ D肏D��HD�HD�@ D� D龸D��qD�>�D�~�D�� D�  D�>�D�HD뾸D���D�AHD� D�� D�HD�@ D�~�D��HD�  D�AHD�HD�� D��D�AHD� D��HD�HD�@ D�}qD��HD�HD�AHD�D�D��qD�>�D� D�� D�  D�>�D�~�D�� D�HD�AHD�HD�� D���D�=qD�~�D�� D���D�>�D�~�D���D���D�=qD�~�D���?�?.{?aG�?���?Ǯ?�G�@�\@��@&ff@=p�@W
=@c�
@z�H@��@�\)@��H@��
@���@���@��R@�=q@�z�@��H@�@���@�z�A   A�
A
=Ap�A\)A�
A��A(�A ��A%A(Q�A-p�A2�\A5A9��A@  AC33AG�AMp�AQ�AUA\(�Aa�AeAk�AqG�AuA{�A���A�33A�A���A��
A�{A�G�A�z�A��RA�G�A���A�
=A���A���A�  A���A���A�  A�=qA�z�A�\)A��HA��A�\)A�=qA�p�A�\)A��A��A�\)A�G�A�(�A�\)A���A��
A޸RA���A�33A�ffA�Q�A�A�RA�Q�A�33A�ffA���A��HA��RB z�B��B33B��Bp�B�HB��B	��B
�HB��Bp�B
=Bz�Bp�B�RBz�Bp�B�RBQ�Bp�B�RBQ�Bp�B�RB Q�B!p�B"�RB$Q�B%G�B&�\B((�B)p�B*ffB,  B-��B.�RB/�B1p�B2ffB4  B5G�B6=qB8  B9p�B:ffB;�
B=G�B>=qB?�
BAG�BBffBC�BE�BF{BG�BH��BI�BK�BL��BM�BO\)BP��BR{BS\)BT��BU�BW\)BX��BZ{B[33B\��B^=qB_\)B`��BbffBc\)Bd��Bf=qBg\)Bi�Bj�\Bk�Bm�BnffBo�Bq�Br�\Bs�Bt��Bv�\Bw�Bx��Bz=qB{�B}�B~=qB�B���B�G�B�B��\B�G�B��
B��RB�\)B��B��RB�p�B��B���B��B�  B��RB��B�  B��RB��B��B��\B�\)B�  B�z�B�G�B�{B�z�B�33B�  B�ffB�33B��B�ffB��B��B�z�B���B�B�z�B���B��B�ffB�
=B���B�Q�B��B��B�=qB���B�B�=qB���B�B�ffB���B��B�Q�B��HB�p�B�=qB�
=B��B�{B��HB���B�(�B��RB��B�ffB���B���B�Q�B���B�p�B�Q�B�
=B��B�=qB�
=B��
B��\B��B��B�ffB�G�B��B�ffB�33B��B���B��B��
B���B�G�B��
B��\B�\)B��
B�z�B�\)B�  Bď\B�\)B�{BƏ\B�G�B�(�BȸRB�G�B�  B���B�\)B��B���B�p�B��BθRB�p�B��BУ�B�p�B�  Bҏ\B�\)B�(�Bԣ�B�33B�{B���B�\)B�{B��HBٙ�B�{BڸRBۙ�B�=qB���B݅B�Q�B��HB߅B�Q�B�
=BᙚB�(�B�
=B�B�Q�B��HB�B�Q�B���B�B�=qB���B�B�{B���B�B��B�z�B�33B��
B�=qB���B�B��B�RB�G�B�B�ffB�
=B�B�  B���B��B��B�{B���B�
=B�G�B�B�Q�B�z�B���B�G�B��B�B�{B��\B���B���B�\)B��B�B�=qB�Q�B��\B�
=B�\)B�p�B�B�=qB�Q�B�z�B���B�G�B�\)B�C {C (�C G�C �C ��C �RC ��C{C(�Cp�C��C��C�HC  C(�CQ�C�C��C�RC�C�C=qC\)C��C��C�
C  C=qC\)Cp�C�C�HC{C33CQ�C�\CC�HC
=CG�Cp�C�\CC��C
=CG�Cz�C�\C��C�C
=CQ�CffC�\C�RC��C	
=C	=qC	p�C	�C	�C	�C
{C
(�C
ffC
��C
�RC
�HC{CQ�Cp�C�C��C  C�CG�C�CC�
C{CQ�C�C�C�HC{CQ�Cp�C��C�C�CG�Cp�C�C�HC  C=qC�C�C�
C�CQ�Cp�C�RC�C
=CG�C�\C�C�HC(�C\)Cz�C��C�C(�CG�Cz�C��C�HC(�CG�Cz�C�RC�
C{CQ�Cp�C��C�C{C=qCp�C�RC�
C
=C=qC�C��C�
C�CQ�Cz�C��C�HC�CQ�Cz�C��C�HC(�CG�Cz�CC��C�CG�C�C��C��C{CffC��C�RC�C=qCp�C�\CC{C33C\)C��C�HC   C =qC �C �RC �
C!{C!ffC!�\C!�C!��C"=qC"ffC"�\C"�
C#�C#Q�C#p�C#�C#��C$(�C$Q�C$��C$�HC%  C%33C%p�C%�C%��C&�C&\)C&z�C&�C&��C'33C'ffC'�C'��C(
=C((�C(ffC(�C(�
C)  C)=qC)�C)��C)�
C*�C*\)C*�C*�C*�HC+(�C+ffC+�C+C,
=C,=qC,ffC,��C,�
C-�C-G�C-p�C-�C-��C.{C.=qC.�C.C.�HC/{C/\)C/�\C/�C/�
C0�C0Q�C0p�C0��C0�
C1�C1Q�C1z�C1��C1�
C2{C2\)C2�C2�C2�HC3�C3Q�C3z�C3��C3�HC4�C4Q�C4p�C4��C4�C5(�C5G�C5ffC5�C5�C6�C6G�C6ffC6��C6�
C7{C7G�C7p�C7�\C7�
C8{C8(�C8\)C8��C8�
C9
=C933C9p�C9�RC9�HC:
=C:=qC:z�C:�RC:�C;{C;33C;z�C;�RC;�
C;��C<(�C<ffC<��C<C<�HC=�C=\)C=�C=�C=�
C>  C>33C>p�C>�C>��C>��C?�C?\)C?��C?��C?��C@�C@Q�C@��C@C@�HCA(�CA\)CA�\CA�RCA�HCB
=CB\)CBz�CB�CB�
CC(�CC\)CCz�CC�CC��CD33CDQ�CD�CDCE  CE=qCEffCE�\CE��CF{CFG�CFffCF��CF�
CG{CGQ�CG��CG�
CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                           ?k�?�@=p�@�G�@�G�@�G�@�  A   A  A   A*�HA?\)A`  A�  A���A���A�  A�Q�AϮA߮A�  B (�B  B  B�
B�
B'�B/�B7�
B@(�BH  BO�
BW�B_�
Bh  Bp  Bx  B�  B�{B�=qB�=qB��B��B��B�  B�  B�  B�{B�  B�{B�  B�  B�  B�{B��B��
B�{B�(�B�  B��B�  B�{B�  B��B�{B�{B�  B�(�B�  B��
C��C�C  C  C
  C  C  C��C  C��C�C��C  C��C��C   C!��C$  C&
=C(  C*
=C,
=C.
=C0
=C2
=C4  C5��C7�C9�C<  C>
=C@  CB  CD  CF  CH  CJ  CL
=CN  CP
=CR  CT  CV
=CX
=CZ  C[��C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co��Cq�Ct  Cv
=Cx  Cz  C|
=C}�C�  C�
=C���C�  C���C���C�  C�C�  C�  C�C�C�
=C�
=C�C�C�  C�  C�  C�
=C�C�  C�  C�C�C���C���C�  C�  C���C�C�
=C�C�  C���C�  C�C�  C�  C���C���C�  C�
=C�C�  C�  C�  C�  C���C�  C�  C�  C�C�
=C�  C�  C�  C�  C�  C�  C�C���C���C���C�  C�C�C�  C�C�  C�C�C�C�  C���C���C���C���C���C���C�C�C�  C�  C�  C�C�C�C�C�  C�C�C���C���C�  C�C�  C���C�  C�  C���C�  C�  C���C�C�C�  C�  C�  C���C�  C�C���C���C���C�  C�C���C���C�C���C�  C�  C�  C�C�  C�  C�D �D ��D�D��D  D� D�D� D�qD� DD}qD�qD� D  Dz�D�qD}qD�qD	� D
  D
}qD  D�DD� D  D��D�qD� D�qDz�D�qD� D�D� D  D� D  Dz�D�qD��DD��D  Dz�D�qD}qD  D}qD��D}qD�qD� D  D� D�D�D�D� D�D�D�D� D�qD ��D!  D!}qD!�qD"}qD#  D#}qD$  D$}qD$�qD%��D&�D&}qD'  D'��D(D(��D)D)�D)�qD*}qD*�qD+}qD,  D,� D-  D-� D.�D.�D/�D/��D0  D0� D1D1� D2�D2� D2�qD3� D4  D4�D5�D5}qD5�qD6��D7D7�D8�D8� D8�qD9}qD:�D:��D;  D;��D<  D<}qD<�qD=��D>D>��D?�D?� D@  D@��DA�DA}qDB  DB�DC�DC� DD�DD� DD��DE}qDF  DF��DG�DG��DH  DH}qDI  DI��DJDJ�DK  DK� DL  DL}qDL��DM}qDN  DN��DODO��DO�qDP� DQ  DQ}qDR  DR� DS  DS}qDS�qDT� DU�DU� DV  DV� DV�qDWz�DW�qDX}qDX�qDY� DZ  DZ�D[D[��D\�D\��D\�qD]� D^�D^}qD^�qD_� D`�D`��Da  Da��Da�qDbz�Dc  Dc}qDc�qDd� De�De�Df  Df� Dg�Dg��Dh  Dh� Dh�qDi}qDj  Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDn  Dn��Do�Do� Do�qDp� Dq�Dq��Dr�Dr}qDr�qDs}qDs�qDt}qDu�Du� Du�qDv}qDv�qDw��Dx  Dx� Dy�Dy� Dz  Dz��D{  D{� D|  D|}qD|�qD}}qD~  D~� D~�qD� D�  D�=qD�~�D���D�  D�@ D�� D��HD�HD�AHD�~�D���D�HD�AHD�� D�� D���D�@ D��HD��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D�~�D�� D�HD�B�D�� D��qD���D�@ D�~�D���D���D�AHD��HD���D��qD�>�D�� D��HD�HD�@ D�~�D���D�  D�>�D�� D��HD�  D�AHD���D��HD���D�AHD���D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�AHD��HD�� D���D�>�D�}qD���D�  D�AHD�� D���D�  D�@ D�� D��HD�HD�AHD���D��HD�  D�@ D�}qD�� D�  D�AHD�� D��qD���D�@ D�� D�� D�  D�@ D�� D���D�HD�AHD��HD��qD��qD�@ D��HD�� D�  D�>�D�� D��HD�HD�B�D��HD���D���D�@ D�~�D�� D��D�@ D�~�D���D���D�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D�~�D���D�  D�=qD�~�D���D���D�>�D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D���D���D�AHD�� D��qD���D�>�D�}qD���D���D�>�D���D�D���D�>�D�~�D��HD��D�C�D���D��HD���D�@ D�� D���D�  D�=qD�}qD���D�  D�AHD���D��HD�  D�AHD�� D���D�  D�>�D�� D��HD�  D�AHD��HD�� D�HD�AHD�~�D���D���D�>�D��HD��HD��D�AHD��HD��HD�  D�>�D�~�D�� D�HD�AHD�� D���D���D�>�D�� D��HD�  D�=qD��HD��HD���D�@ D��HD��qD�  D�B�D��HD��HD��D�AHD�}qD���D�HD�AHD�� D���D���D�>�D D��HD�  D�AHDÂ�D��HD���D�>�DĀ D�� D�  D�>�D�}qD��HD�HD�AHDƀ Dƾ�D���D�>�D�~�DǾ�D��qD�>�D�~�DȾ�D���D�=qD�~�D�� D�HD�@ DʁHD�D�  D�@ DˁHD�� D���D�@ D́HD��HD���D�=qD̀ D��HD�  D�@ D΀ D��HD�  D�AHDρHDϾ�D�  D�>�D�~�D�� D�HD�@ DсHD��HD�  D�AHDҁHDҾ�D��qD�@ DӀ D��HD�HD�AHDԁHDԾ�D���D�@ DՁHD�� D��qD�=qDցHD��HD���D�>�D�~�D�� D���D�@ D؁HD�� D���D�>�Dـ D��HD�  D�=qD�}qDھ�D�  D�@ D�}qD�� D�  D�=qD܀ D�� D���D�@ D݂�D��HD��qD�=qD�~�D�� D�  D�AHD߀ D߾�D���D�>�D��HD�� D���D�@ D� D��HD�HD�@ D₏D��HD�  D�>�D� D�� D�  D�>�D�}qD侸D�  D�@ D� D��HD�HD�>�D�}qD�qD�  D�AHD� D�� D���D�@ D肏D��HD�HD�@ D� D龸D��qD�>�D�~�D�� D�  D�>�D�HD뾸D���D�AHD� D�� D�HD�@ D�~�D��HD�  D�AHD�HD�� D��D�AHD� D��HD�HD�@ D�}qD��HD�HD�AHD�D�D��qD�>�D� D�� D�  D�>�D�~�D�� D�HD�AHD�HD�� D���D�=qD�~�D�� D���D�>�D�~�D���D���D�=qD�~�G�O�?�?.{?aG�?���?Ǯ?�G�@�\@��@&ff@=p�@W
=@c�
@z�H@��@�\)@��H@��
@���@���@��R@�=q@�z�@��H@�@���@�z�A   A�
A
=Ap�A\)A�
A��A(�A ��A%A(Q�A-p�A2�\A5A9��A@  AC33AG�AMp�AQ�AUA\(�Aa�AeAk�AqG�AuA{�A���A�33A�A���A��
A�{A�G�A�z�A��RA�G�A���A�
=A���A���A�  A���A���A�  A�=qA�z�A�\)A��HA��A�\)A�=qA�p�A�\)A��A��A�\)A�G�A�(�A�\)A���A��
A޸RA���A�33A�ffA�Q�A�A�RA�Q�A�33A�ffA���A��HA��RB z�B��B33B��Bp�B�HB��B	��B
�HB��Bp�B
=Bz�Bp�B�RBz�Bp�B�RBQ�Bp�B�RBQ�Bp�B�RB Q�B!p�B"�RB$Q�B%G�B&�\B((�B)p�B*ffB,  B-��B.�RB/�B1p�B2ffB4  B5G�B6=qB8  B9p�B:ffB;�
B=G�B>=qB?�
BAG�BBffBC�BE�BF{BG�BH��BI�BK�BL��BM�BO\)BP��BR{BS\)BT��BU�BW\)BX��BZ{B[33B\��B^=qB_\)B`��BbffBc\)Bd��Bf=qBg\)Bi�Bj�\Bk�Bm�BnffBo�Bq�Br�\Bs�Bt��Bv�\Bw�Bx��Bz=qB{�B}�B~=qB�B���B�G�B�B��\B�G�B��
B��RB�\)B��B��RB�p�B��B���B��B�  B��RB��B�  B��RB��B��B��\B�\)B�  B�z�B�G�B�{B�z�B�33B�  B�ffB�33B��B�ffB��B��B�z�B���B�B�z�B���B��B�ffB�
=B���B�Q�B��B��B�=qB���B�B�=qB���B�B�ffB���B��B�Q�B��HB�p�B�=qB�
=B��B�{B��HB���B�(�B��RB��B�ffB���B���B�Q�B���B�p�B�Q�B�
=B��B�=qB�
=B��
B��\B��B��B�ffB�G�B��B�ffB�33B��B���B��B��
B���B�G�B��
B��\B�\)B��
B�z�B�\)B�  Bď\B�\)B�{BƏ\B�G�B�(�BȸRB�G�B�  B���B�\)B��B���B�p�B��BθRB�p�B��BУ�B�p�B�  Bҏ\B�\)B�(�Bԣ�B�33B�{B���B�\)B�{B��HBٙ�B�{BڸRBۙ�B�=qB���B݅B�Q�B��HB߅B�Q�B�
=BᙚB�(�B�
=B�B�Q�B��HB�B�Q�B���B�B�=qB���B�B�{B���B�B��B�z�B�33B��
B�=qB���B�B��B�RB�G�B�B�ffB�
=B�B�  B���B��B��B�{B���B�
=B�G�B�B�Q�B�z�B���B�G�B��B�B�{B��\B���B���B�\)B��B�B�=qB�Q�B��\B�
=B�\)B�p�B�B�=qB�Q�B�z�B���B�G�B�\)B�C {C (�C G�C �C ��C �RC ��C{C(�Cp�C��C��C�HC  C(�CQ�C�C��C�RC�C�C=qC\)C��C��C�
C  C=qC\)Cp�C�C�HC{C33CQ�C�\CC�HC
=CG�Cp�C�\CC��C
=CG�Cz�C�\C��C�C
=CQ�CffC�\C�RC��C	
=C	=qC	p�C	�C	�C	�C
{C
(�C
ffC
��C
�RC
�HC{CQ�Cp�C�C��C  C�CG�C�CC�
C{CQ�C�C�C�HC{CQ�Cp�C��C�C�CG�Cp�C�C�HC  C=qC�C�C�
C�CQ�Cp�C�RC�C
=CG�C�\C�C�HC(�C\)Cz�C��C�C(�CG�Cz�C��C�HC(�CG�Cz�C�RC�
C{CQ�Cp�C��C�C{C=qCp�C�RC�
C
=C=qC�C��C�
C�CQ�Cz�C��C�HC�CQ�Cz�C��C�HC(�CG�Cz�CC��C�CG�C�C��C��C{CffC��C�RC�C=qCp�C�\CC{C33C\)C��C�HC   C =qC �C �RC �
C!{C!ffC!�\C!�C!��C"=qC"ffC"�\C"�
C#�C#Q�C#p�C#�C#��C$(�C$Q�C$��C$�HC%  C%33C%p�C%�C%��C&�C&\)C&z�C&�C&��C'33C'ffC'�C'��C(
=C((�C(ffC(�C(�
C)  C)=qC)�C)��C)�
C*�C*\)C*�C*�C*�HC+(�C+ffC+�C+C,
=C,=qC,ffC,��C,�
C-�C-G�C-p�C-�C-��C.{C.=qC.�C.C.�HC/{C/\)C/�\C/�C/�
C0�C0Q�C0p�C0��C0�
C1�C1Q�C1z�C1��C1�
C2{C2\)C2�C2�C2�HC3�C3Q�C3z�C3��C3�HC4�C4Q�C4p�C4��C4�C5(�C5G�C5ffC5�C5�C6�C6G�C6ffC6��C6�
C7{C7G�C7p�C7�\C7�
C8{C8(�C8\)C8��C8�
C9
=C933C9p�C9�RC9�HC:
=C:=qC:z�C:�RC:�C;{C;33C;z�C;�RC;�
C;��C<(�C<ffC<��C<C<�HC=�C=\)C=�C=�C=�
C>  C>33C>p�C>�C>��C>��C?�C?\)C?��C?��C?��C@�C@Q�C@��C@C@�HCA(�CA\)CA�\CA�RCA�HCB
=CB\)CBz�CB�CB�
CC(�CC\)CCz�CC�CC��CD33CDQ�CD�CDCE  CE=qCEffCE�\CE��CF{CFG�CFffCF��CF�
CG{CGQ�CG��CG�
CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                           @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�G�A�I�A�I�A�O�A�O�A�O�A�M�A�O�A�I�A�C�A�I�A�M�A�VA�ZA�VA�S�A�S�A�A�A�?}A�oA��A��/A��#A��#A��;A��#A��/A��;A��`A��TA��#A���A���Aʩ�AʑhAʝ�A���A���A���A���A��HA�%A�bA�$�A�ZA˅A˛�A�S�A�{A���Aʙ�A�~�A�S�A���A�r�A�bA�G�A�\)AƗ�A��A���AŇ+A�{A��#A�A��FA�oA���A� �A��uA���A�C�A��#A�K�A�hsA���A���A�\)A���A�x�A�%A�\)A�E�A�bNA���A���A�ZA��FA�{A�{A��TA�-A��A��A���A�1A��-A��A�-A�%A�ȴA�ȴA�VA�A�dZA���A���A���A���A��A�-A��A�|�A��FA~{A{VAx�AwoAs�-An��AkVAfI�AdZAa�A[VAU�AO\)AK��AI�wAH5?AGt�AD�`AA�A@��A=�
A;\)A9�-A7��A5�PA3S�A0�yA-�-A,��A+��A*�+A)�A'C�A$��A"�\A!33A �A�^A��A�-A%Av�AhsA~�A9XA�mA�^AA�A�AA�A$�A=qAbA��A��A��A�#AƨA��AhsA��A�A�+AbA�-A7LA��A$�A��A`BA�AbAp�A�A��A�PA�+A1'AO�A
M�A
5?A
 �A	�PAn�A�FAȴAA�-A��A�AXA�/AZA  A�A��A��AjA�AG�A ��A {@��w@�"�@��\@���@���@�b@��y@�X@�r�@�\)@���@�=q@�-@�$�@�J@���@��9@�1@�@�o@��@�$�@���@�@��@�1@��H@��@�@���@�7L@�9X@�\)@�
=@��@�p�@���@�-@�x�@�I�@���@�P@�l�@�|�@�"�@��@���@�Q�@�P@�
=@�!@�M�@�?}@�@�j@� �@ߕ�@ޗ�@���@ܣ�@�A�@�  @���@�t�@�
=@���@��@�E�@��@ش9@�b@�"�@֧�@�=q@�@�hs@�%@�%@�G�@��`@�(�@�33@ҧ�@��@���@��T@д9@��@�ȴ@�^5@�-@�M�@�ȴ@���@���@�~�@�J@���@́@���@̃@�1@�\)@���@��@ɑh@�hs@�z�@Ǖ�@ǅ@Ǿw@��@��@�33@�+@Ɨ�@��@�@�/@��@�?}@Ų-@��@őh@�p�@���@�dZ@��-@�?}@���@���@��T@�%@��u@�K�@�V@�@��#@���@�{@��`@�Q�@��
@���@��F@���@���@���@�33@�E�@�x�@��-@�p�@�%@��j@�j@�b@��m@��F@��\@��@�O�@�/@���@���@���@�b@��@�K�@��@���@�x�@�O�@�7L@��@��@�z�@��@��
@��
@��;@��;@��;@��
@��
@���@�K�@��R@���@��+@�{@��T@��7@�?}@�%@���@��D@��;@�t�@�S�@��@�M�@�@�x�@���@�I�@��@��m@��@�;d@���@�~�@�5?@�@�@��h@��@�x�@�x�@�hs@�O�@�/@��j@���@�z�@�bN@��m@���@�\)@��H@��R@��@��T@��#@���@��-@���@���@�G�@�j@�  @���@�|�@���@���@�v�@��h@�&�@���@�z�@�9X@��m@�\)@�33@�
=@��@���@�~�@�5?@���@�X@��@��@� �@��;@��w@��@�dZ@�K�@�+@�"�@�@���@�n�@�$�@��T@�hs@���@��`@��@���@��;@��P@�"�@��y@���@���@�n�@�-@�@��^@��h@�7L@�z�@�o@��@���@��\@��+@�v�@�ff@�V@�-@��@�@���@�O�@��@��9@�Q�@���@�\)@�S�@�"�@�o@�\)@��@���@�^5@��T@�/@��j@��@�(�@��@��F@�dZ@��@��y@��@�ȴ@���@�^5@�J@���@�7L@��/@���@��@�z�@�r�@�Q�@�1@|�@\)@�@~��@~�+@}p�@|�/@|��@|�D@|�@{��@{S�@z�H@z�!@zn�@z=q@zJ@y%@x��@xr�@xA�@w�P@w+@v��@vȴ@v��@v�+@vE�@v@up�@t�@t��@tj@t�@s33@r^5@q�#@q�^@q��@qx�@p��@pA�@p  @o��@o�@nȴ@n��@nff@m�-@l�@l�D@lI�@l9X@l�@l1@k�
@kdZ@k@j�\@i�#@i��@i��@ix�@iX@i%@hĜ@h1'@g+@f�R@fȴ@f��@fE�@e�@ep�@eV@d�@d�@c�F@cC�@c@b��@b��@b�!@b��@b��@b~�@bn�@bM�@a&�@`Q�@_�w@_;d@_�@^�R@^��@^��@^�+@^v�@^V@^$�@]@]/@\��@\�/@\z�@[�F@Z�H@Z�\@Z-@Y��@Yhs@Y&�@X��@XĜ@X�u@Xr�@XbN@XA�@XA�@XA�@X �@X  @W�@V�R@V@U@U��@Up�@UO�@U/@T�@T��@Tj@TI�@T(�@S�m@Sƨ@St�@R�H@R^5@RM�@R-@Q�@Q�^@Q��@Q�7@QX@Q7L@Q%@P  @O�P@O
=@Nff@M��@L��@L�@L1@K�@KS�@J��@I��@I7L@H��@H�@HQ�@Hb@G��@G�P@G\)@G;d@G+@F��@F��@F�@F��@Fv�@E�@E�h@E/@D�@DZ@C��@B�@BM�@A��@A�#@A��@AG�@A%@@��@@Ĝ@@r�@?�;@?�@?\)@?�@>��@>ȴ@>�+@>V@>{@=O�@<��@<��@<��@<�@<z�@;��@;��@;S�@;"�@:�@:=q@9�^@9�7@8��@8�u@8�@8Q�@8Q�@8 �@6ȴ@6V@65?@5�@5@5�@5/@5V@4j@3��@3��@3��@3�@3t�@3C�@333@3o@3@2�H@2��@2M�@1��@1��@1&�@0�u@01'@/��@/�@/\)@/�@.�R@.ff@-�T@-p�@,��@,�j@,(�@+ƨ@+ƨ@+�F@+��@+t�@+S�@+"�@*�H@*��@*��@*��@*��@*��@*n�@)��@)�7@)&�@)%@(�`@(��@(��@(�@(r�@(  @'�@'�P@'|�@'\)@'�@&�@&��@&�+@&$�@%�T@%�@$�/@$��@$�j@$j@$I�@#�m@#�@#C�@#33@#33@#"�@#"�@#@"��@"^5@!��@!�7@!hs@!�@ ��@ �9@ ��@ bN@ 1'@�@|�@l�@\)@;d@�y@ff@{@�@@�h@p�@V@�j@�@��@j@Z@I�@��@��@"�@@�@��@�!@��@n�@-@J@�#@��@��@��@x�@�@�9@bN@�@�w@�@|�@;d@ȴ@��@��@v�@E�@�T@p�@V@��@��@��@��@��@��@��@��@�@��@�D@j@1@�@o@�H@��@��@M�@-@��@�#@�#@�^@�7@hs@&�@�u@bN@1'@b@�;@|�@�@�y@�@�@�@�R@�+@V@�@�-@�h@O�@/@V@��@j@(�@�m@�
@�
@�F@��@t�@t�@C�@"�@@
�@
�@
�H@
�H@
��@
�\@
n�@
M�Aʉ7A���A�&�A�;dA�E�A�A�A�I�A�M�A�I�A�I�A�G�A�A�A�K�A�M�A�G�A�O�A�Q�A�M�A�Q�A�M�A�O�A�O�A�O�A�S�A�M�A�O�A�Q�A�M�A�I�A�S�A�M�A�M�A�S�A�G�A�I�A�E�A�A�A�G�A�G�A�C�A�G�A�M�A�M�A�E�A�M�A�M�A�A�A�I�A�O�A�S�A�XA�XA�S�A�XA�ZA�Q�A�VA�\)A�\)A�VA�XA�\)A�VA�VA�ZA�XA�S�A�VA�XA�S�A�XA�XA�VA�O�A�Q�A�VA�Q�A�Q�A�S�A�XA�Q�A�M�A�=qA�;dA�7LA�=qA�G�A�A�A�A�A�E�A�?}A�=qA�=qA�/A�"�A��A�VA�  A�A�  A��A��A��A��yA��mA��HA��/A��/A��;A��#A��#A��;A��A��/A��/A��A��A��/A��A��A��/A��/A��#A��HA��;A��/A��HA��#A��/A��;A��#A��#A��;A��#A��A��#A��/A��/A��#A��HA��/A��HA��;A��;A��;A��HA��HA��TA��`A��`A��mA��yA��mA��`A��yA��TA��mA��`A��/A��;A��/A���A��
A��;A��#A��/A��/A���A���A���A���A�ȴA�ȴA�ĜAʾwA�Aʺ^AʮAʲ-AʬAʥ�Aʧ�Aʧ�Aʣ�Aʥ�AʑhAʉ7AʋDAʉ7AʋDAʓuAʙ�AʓuAʗ�Aʩ�AʶFA���AʼjAʾwA�ƨA�ƨA�ĜA���A���A�ȴA���A���A���A��
A���A���A���A���A���A���A���A���A���A��
A���A���A��A��
A��mA���A�%A�A�%A�1A�A�%A�
=A�
=A�VA�{A�{A�bA�bA�bA�JA�JA��A�=qA�K�A�O�A�XA�ZA�XA�`BA�ffA�hsA�r�A�~�AˋDA˓uA˥�AˬA˧�A˝�A˝�A˗�Aˏ\AˁA�l�A�VA�I�A�?}A�7LA�1'A�$�A��A��A�JA��A��yA��TA��#A���A���AʸRAʴ9Aʣ�Aʗ�AʍPAʋDAʉ7AʅAʁAʃA�|�A�x�A�x�A�x�A�t�A�l�A�M�A�/A�%A���A��`A��#A���Aɩ�Aɗ�AɅA�~�A�v�A�dZA�M�A�A�A�9XA�$�A�{A�A��yA��
A���AȼjAȃA���A���AǺ^AǮAǏ\A�M�A�5?A�oA���AƾwAƲ-Aƙ�A�z�A�dZA�G�A�"�A�"�A��A�{A�bA�1A��A�ȴA�ȴA���A�ƨAžwAŴ9AōPA�v�A�ZA�S�A�E�A�5?A��A�  A���A��A��A��A��A��`A���A�ȴA�ƨAĺ^Aė�A�dZA���AÇ+A��#A�-A��A�$�A���A���A�O�A�1'A� �A��A��A��A��A��A��A�oA�VA�VA�
=A���A��A��mA���A�ȴA��PA�$�A���A��A�O�A�1'A��A��
A�ĜA���A���A��PA��PA�r�A�G�A�(�A�+A��A�VA��A��/A�ȴA��A�p�A�K�A���A���A�ƨA��A���A���A��A�jA�I�A�$�A�A��TA��RA�I�A�A��A���A�l�A�I�A�"�A�bA�%A�JA�bA�JA��yA�l�A�+A��A�JA�JA�VA�VA�
=A�VA�JA�
=A�VA�JA�1A�JA�
=A�  A��A��A��+A�l�A�jA�dZA�ffA�ffA�\)A�ZA�\)A�O�A�E�A�E�A�;dA�(�A�oA���A���A��^A���A���A��uA��7A�x�A�?}A�ƨA��A�oA��7A�K�A� �A��yA���A�v�A�O�A�+A��HA�ƨA��9A��!A��A���A���A��uA��hA��+A�z�A�t�A�ffA�ZA�`BA�\)A�XA�ZA�ZA�O�A�G�A�C�A�9XA�"�A�1A���A���A���A�jA�\)A�Q�A�;dA��A�bA�  A�p�A�$�A��HA���A���A���A���A�n�A�1'A���A��A��A���A��wA��wA��A��-A��A���A���A�~�A�Q�A�&�A�oA�A��HA��wA���A���A���A��7A�"�A�  A���A���A���A���A��hA�x�A�l�A�\)A�VA�I�A�?}A�33A�33A�oA�
=A���A��A��HA���A��FA��PA�`BA�1A��!A�\)A���A�/A�JA���A��HA���A���A��A��+A�l�A�S�A�=qA�$�A��A�{A�A���A��yA���A���A��A���A��PA��A�x�A�l�A�XA�?}A�9XA�"�A�1A��A���A���A��A�dZA�I�A�1'A��A�bA�1A��A���A��RA���A���A�t�A�I�A�+A��
A�|�A�1'A��;A�n�A���A�\)A�A�A�/A�{A��A��TA�ȴA��A���A�x�A�K�A�{A��A�ȴA���A��7A�S�A�1'A�  A��!A�XA���A���A�n�A�=qA��A���A��A��A��mA��A���A��FA���A�~�A�n�A�dZA�ZA�Q�A�C�A�?}A�33A��A���A��mA��
A���A��-A���A�n�A�33A���A��uA�r�A�XA�I�A�9XA� �A���A�A�z�A�;dA��-A�dZA�O�A�O�A�M�A�G�A�E�A�=qA�A�A�7LA�1'A�-A�(�A�+A�$�A�oA�
=A�1A�  A��A�ȴA��PA�+A��;A�t�A���A��RA���A�hsA�7LA�$�A�A��yA�ĜA��+A�S�A�$�A�1A��#A��9A�|�A�A�A���A�n�A�^5A�=qA���A��jA�I�A�33A��A��A��A�?}A���A��A�K�A�;dA� �A�JA�
=A���A���A���A�p�A�O�A�5?A�{A��HA��9A��PA�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                           A�JA�G�A�I�A�I�A�O�A�O�A�O�A�M�A�O�A�I�A�C�A�I�A�M�A�VA�ZA�VA�S�A�S�A�A�A�?}A�oA��A��/A��#A��#A��;A��#A��/A��;A��`A��TA��#A���A���Aʩ�AʑhAʝ�A���A���A���A���A��HA�%A�bA�$�A�ZA˅A˛�A�S�A�{A���Aʙ�A�~�A�S�A���A�r�A�bA�G�A�\)AƗ�A��A���AŇ+A�{A��#A�A��FA�oA���A� �A��uA���A�C�A��#A�K�A�hsA���A���A�\)A���A�x�A�%A�\)A�E�A�bNA���A���A�ZA��FA�{A�{A��TA�-A��A��A���A�1A��-A��A�-A�%A�ȴA�ȴA�VA�A�dZA���A���A���A���A��A�-A��A�|�A��FA~{A{VAx�AwoAs�-An��AkVAfI�AdZAa�A[VAU�AO\)AK��AI�wAH5?AGt�AD�`AA�A@��A=�
A;\)A9�-A7��A5�PA3S�A0�yA-�-A,��A+��A*�+A)�A'C�A$��A"�\A!33A �A�^A��A�-A%Av�AhsA~�A9XA�mA�^AA�A�AA�A$�A=qAbA��A��A��A�#AƨA��AhsA��A�A�+AbA�-A7LA��A$�A��A`BA�AbAp�A�A��A�PA�+A1'AO�A
M�A
5?A
 �A	�PAn�A�FAȴAA�-A��A�AXA�/AZA  A�A��A��AjA�AG�A ��A {@��w@�"�@��\@���@���@�b@��y@�X@�r�@�\)@���@�=q@�-@�$�@�J@���@��9@�1@�@�o@��@�$�@���@�@��@�1@��H@��@�@���@�7L@�9X@�\)@�
=@��@�p�@���@�-@�x�@�I�@���@�P@�l�@�|�@�"�@��@���@�Q�@�P@�
=@�!@�M�@�?}@�@�j@� �@ߕ�@ޗ�@���@ܣ�@�A�@�  @���@�t�@�
=@���@��@�E�@��@ش9@�b@�"�@֧�@�=q@�@�hs@�%@�%@�G�@��`@�(�@�33@ҧ�@��@���@��T@д9@��@�ȴ@�^5@�-@�M�@�ȴ@���@���@�~�@�J@���@́@���@̃@�1@�\)@���@��@ɑh@�hs@�z�@Ǖ�@ǅ@Ǿw@��@��@�33@�+@Ɨ�@��@�@�/@��@�?}@Ų-@��@őh@�p�@���@�dZ@��-@�?}@���@���@��T@�%@��u@�K�@�V@�@��#@���@�{@��`@�Q�@��
@���@��F@���@���@���@�33@�E�@�x�@��-@�p�@�%@��j@�j@�b@��m@��F@��\@��@�O�@�/@���@���@���@�b@��@�K�@��@���@�x�@�O�@�7L@��@��@�z�@��@��
@��
@��;@��;@��;@��
@��
@���@�K�@��R@���@��+@�{@��T@��7@�?}@�%@���@��D@��;@�t�@�S�@��@�M�@�@�x�@���@�I�@��@��m@��@�;d@���@�~�@�5?@�@�@��h@��@�x�@�x�@�hs@�O�@�/@��j@���@�z�@�bN@��m@���@�\)@��H@��R@��@��T@��#@���@��-@���@���@�G�@�j@�  @���@�|�@���@���@�v�@��h@�&�@���@�z�@�9X@��m@�\)@�33@�
=@��@���@�~�@�5?@���@�X@��@��@� �@��;@��w@��@�dZ@�K�@�+@�"�@�@���@�n�@�$�@��T@�hs@���@��`@��@���@��;@��P@�"�@��y@���@���@�n�@�-@�@��^@��h@�7L@�z�@�o@��@���@��\@��+@�v�@�ff@�V@�-@��@�@���@�O�@��@��9@�Q�@���@�\)@�S�@�"�@�o@�\)@��@���@�^5@��T@�/@��j@��@�(�@��@��F@�dZ@��@��y@��@�ȴ@���@�^5@�J@���@�7L@��/@���@��@�z�@�r�@�Q�@�1@|�@\)@�@~��@~�+@}p�@|�/@|��@|�D@|�@{��@{S�@z�H@z�!@zn�@z=q@zJ@y%@x��@xr�@xA�@w�P@w+@v��@vȴ@v��@v�+@vE�@v@up�@t�@t��@tj@t�@s33@r^5@q�#@q�^@q��@qx�@p��@pA�@p  @o��@o�@nȴ@n��@nff@m�-@l�@l�D@lI�@l9X@l�@l1@k�
@kdZ@k@j�\@i�#@i��@i��@ix�@iX@i%@hĜ@h1'@g+@f�R@fȴ@f��@fE�@e�@ep�@eV@d�@d�@c�F@cC�@c@b��@b��@b�!@b��@b��@b~�@bn�@bM�@a&�@`Q�@_�w@_;d@_�@^�R@^��@^��@^�+@^v�@^V@^$�@]@]/@\��@\�/@\z�@[�F@Z�H@Z�\@Z-@Y��@Yhs@Y&�@X��@XĜ@X�u@Xr�@XbN@XA�@XA�@XA�@X �@X  @W�@V�R@V@U@U��@Up�@UO�@U/@T�@T��@Tj@TI�@T(�@S�m@Sƨ@St�@R�H@R^5@RM�@R-@Q�@Q�^@Q��@Q�7@QX@Q7L@Q%@P  @O�P@O
=@Nff@M��@L��@L�@L1@K�@KS�@J��@I��@I7L@H��@H�@HQ�@Hb@G��@G�P@G\)@G;d@G+@F��@F��@F�@F��@Fv�@E�@E�h@E/@D�@DZ@C��@B�@BM�@A��@A�#@A��@AG�@A%@@��@@Ĝ@@r�@?�;@?�@?\)@?�@>��@>ȴ@>�+@>V@>{@=O�@<��@<��@<��@<�@<z�@;��@;��@;S�@;"�@:�@:=q@9�^@9�7@8��@8�u@8�@8Q�@8Q�@8 �@6ȴ@6V@65?@5�@5@5�@5/@5V@4j@3��@3��@3��@3�@3t�@3C�@333@3o@3@2�H@2��@2M�@1��@1��@1&�@0�u@01'@/��@/�@/\)@/�@.�R@.ff@-�T@-p�@,��@,�j@,(�@+ƨ@+ƨ@+�F@+��@+t�@+S�@+"�@*�H@*��@*��@*��@*��@*��@*n�@)��@)�7@)&�@)%@(�`@(��@(��@(�@(r�@(  @'�@'�P@'|�@'\)@'�@&�@&��@&�+@&$�@%�T@%�@$�/@$��@$�j@$j@$I�@#�m@#�@#C�@#33@#33@#"�@#"�@#@"��@"^5@!��@!�7@!hs@!�@ ��@ �9@ ��@ bN@ 1'@�@|�@l�@\)@;d@�y@ff@{@�@@�h@p�@V@�j@�@��@j@Z@I�@��@��@"�@@�@��@�!@��@n�@-@J@�#@��@��@��@x�@�@�9@bN@�@�w@�@|�@;d@ȴ@��@��@v�@E�@�T@p�@V@��@��@��@��@��@��@��@��@�@��@�D@j@1@�@o@�H@��@��@M�@-@��@�#@�#@�^@�7@hs@&�@�u@bN@1'@b@�;@|�@�@�y@�@�@�@�R@�+@V@�@�-@�h@O�@/@V@��@j@(�@�m@�
@�
@�F@��@t�@t�@C�@"�@@
�@
�@
�H@
�H@
��@
�\@
n�G�O�Aʉ7A���A�&�A�;dA�E�A�A�A�I�A�M�A�I�A�I�A�G�A�A�A�K�A�M�A�G�A�O�A�Q�A�M�A�Q�A�M�A�O�A�O�A�O�A�S�A�M�A�O�A�Q�A�M�A�I�A�S�A�M�A�M�A�S�A�G�A�I�A�E�A�A�A�G�A�G�A�C�A�G�A�M�A�M�A�E�A�M�A�M�A�A�A�I�A�O�A�S�A�XA�XA�S�A�XA�ZA�Q�A�VA�\)A�\)A�VA�XA�\)A�VA�VA�ZA�XA�S�A�VA�XA�S�A�XA�XA�VA�O�A�Q�A�VA�Q�A�Q�A�S�A�XA�Q�A�M�A�=qA�;dA�7LA�=qA�G�A�A�A�A�A�E�A�?}A�=qA�=qA�/A�"�A��A�VA�  A�A�  A��A��A��A��yA��mA��HA��/A��/A��;A��#A��#A��;A��A��/A��/A��A��A��/A��A��A��/A��/A��#A��HA��;A��/A��HA��#A��/A��;A��#A��#A��;A��#A��A��#A��/A��/A��#A��HA��/A��HA��;A��;A��;A��HA��HA��TA��`A��`A��mA��yA��mA��`A��yA��TA��mA��`A��/A��;A��/A���A��
A��;A��#A��/A��/A���A���A���A���A�ȴA�ȴA�ĜAʾwA�Aʺ^AʮAʲ-AʬAʥ�Aʧ�Aʧ�Aʣ�Aʥ�AʑhAʉ7AʋDAʉ7AʋDAʓuAʙ�AʓuAʗ�Aʩ�AʶFA���AʼjAʾwA�ƨA�ƨA�ĜA���A���A�ȴA���A���A���A��
A���A���A���A���A���A���A���A���A���A��
A���A���A��A��
A��mA���A�%A�A�%A�1A�A�%A�
=A�
=A�VA�{A�{A�bA�bA�bA�JA�JA��A�=qA�K�A�O�A�XA�ZA�XA�`BA�ffA�hsA�r�A�~�AˋDA˓uA˥�AˬA˧�A˝�A˝�A˗�Aˏ\AˁA�l�A�VA�I�A�?}A�7LA�1'A�$�A��A��A�JA��A��yA��TA��#A���A���AʸRAʴ9Aʣ�Aʗ�AʍPAʋDAʉ7AʅAʁAʃA�|�A�x�A�x�A�x�A�t�A�l�A�M�A�/A�%A���A��`A��#A���Aɩ�Aɗ�AɅA�~�A�v�A�dZA�M�A�A�A�9XA�$�A�{A�A��yA��
A���AȼjAȃA���A���AǺ^AǮAǏ\A�M�A�5?A�oA���AƾwAƲ-Aƙ�A�z�A�dZA�G�A�"�A�"�A��A�{A�bA�1A��A�ȴA�ȴA���A�ƨAžwAŴ9AōPA�v�A�ZA�S�A�E�A�5?A��A�  A���A��A��A��A��A��`A���A�ȴA�ƨAĺ^Aė�A�dZA���AÇ+A��#A�-A��A�$�A���A���A�O�A�1'A� �A��A��A��A��A��A��A�oA�VA�VA�
=A���A��A��mA���A�ȴA��PA�$�A���A��A�O�A�1'A��A��
A�ĜA���A���A��PA��PA�r�A�G�A�(�A�+A��A�VA��A��/A�ȴA��A�p�A�K�A���A���A�ƨA��A���A���A��A�jA�I�A�$�A�A��TA��RA�I�A�A��A���A�l�A�I�A�"�A�bA�%A�JA�bA�JA��yA�l�A�+A��A�JA�JA�VA�VA�
=A�VA�JA�
=A�VA�JA�1A�JA�
=A�  A��A��A��+A�l�A�jA�dZA�ffA�ffA�\)A�ZA�\)A�O�A�E�A�E�A�;dA�(�A�oA���A���A��^A���A���A��uA��7A�x�A�?}A�ƨA��A�oA��7A�K�A� �A��yA���A�v�A�O�A�+A��HA�ƨA��9A��!A��A���A���A��uA��hA��+A�z�A�t�A�ffA�ZA�`BA�\)A�XA�ZA�ZA�O�A�G�A�C�A�9XA�"�A�1A���A���A���A�jA�\)A�Q�A�;dA��A�bA�  A�p�A�$�A��HA���A���A���A���A�n�A�1'A���A��A��A���A��wA��wA��A��-A��A���A���A�~�A�Q�A�&�A�oA�A��HA��wA���A���A���A��7A�"�A�  A���A���A���A���A��hA�x�A�l�A�\)A�VA�I�A�?}A�33A�33A�oA�
=A���A��A��HA���A��FA��PA�`BA�1A��!A�\)A���A�/A�JA���A��HA���A���A��A��+A�l�A�S�A�=qA�$�A��A�{A�A���A��yA���A���A��A���A��PA��A�x�A�l�A�XA�?}A�9XA�"�A�1A��A���A���A��A�dZA�I�A�1'A��A�bA�1A��A���A��RA���A���A�t�A�I�A�+A��
A�|�A�1'A��;A�n�A���A�\)A�A�A�/A�{A��A��TA�ȴA��A���A�x�A�K�A�{A��A�ȴA���A��7A�S�A�1'A�  A��!A�XA���A���A�n�A�=qA��A���A��A��A��mA��A���A��FA���A�~�A�n�A�dZA�ZA�Q�A�C�A�?}A�33A��A���A��mA��
A���A��-A���A�n�A�33A���A��uA�r�A�XA�I�A�9XA� �A���A�A�z�A�;dA��-A�dZA�O�A�O�A�M�A�G�A�E�A�=qA�A�A�7LA�1'A�-A�(�A�+A�$�A�oA�
=A�1A�  A��A�ȴA��PA�+A��;A�t�A���A��RA���A�hsA�7LA�$�A�A��yA�ĜA��+A�S�A�$�A�1A��#A��9A�|�A�A�A���A�n�A�^5A�=qA���A��jA�I�A�33A��A��A��A�?}A���A��A�K�A�;dA� �A�JA�
=A���A���A���A�p�A�O�A�5?A�{A��HA��9A��PA�XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                           ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ffB
gB
f�B
ffB
f�B
f�B
g8B
f�B
g8B
gmB
g8B
gB
gB
gB
gB
g�B
gmB
k�B
k�B
}�B
�SB
�-B
�B
�B
�=B
��B
�B
��B
�B
��B
�mB
�0B
�^B
�B
�B
ȴB
уB
��B
��B
�EB
��B
�cB
��B!BgmB��B��BȀB�6B�BB�[BԕB�EB�B��B�B�GB��B�B
	B�BVB�BPBkB.IB)_B/�B4�B7BA�BR�BT�BS&BYBR B=qBA�BA�BOBB_BbBk�Bb�B\�BZBMBF�B>wB4�B.�BxB;B�B�#BӏBȀB�LB�RB��B�4BiDBM�B49B"hB�B
�(B
�B
�jB
��B
�qB
��B
|PB
x�B
^5B
OB
<�B
3hB
�B	�VB	�)B	ȀB	�XB	�tB	��B	oiB	YB	M6B	H�B	@�B	;0B	;�B	,=B	'�B	&LB	kB	�B	�B	!bB	!bB	"hB	%�B	$�B	(�B	+�B	.�B	0�B	<6B	F�B	L�B	NpB	[WB	r�B	t�B	s�B	qvB	poB	p�B	poB	qB	h>B	j�B	tTB	w2B	y	B	z�B	~�B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�B	�B	��B	�0B	�B	�B	��B	�}B	��B	�-B	�B	��B	��B	�B	�RB	�tB	�dB	�dB	��B	�$B	�dB	��B	��B	�B	��B	��B	�'B	�'B	��B	�B	�tB	�B	�B	ȴB	ƨB	�tB	ǮB	��B	�^B	��B	��B	�B	��B	�<B	��B	�jB	�B	ΥB	�<B	�BB	�vB	ѷB	�TB	� B	� B	� B	��B	�,B	�sB	ںB	ܒB	�]B	��B	ܒB	�WB	�B	�EB	�B	�B	��B	�BB	�B	�B	�B	ߤB	ߤB	�B	�mB	�mB	�B	��B	� B	�iB	��B	�`B	��B	��B	�B	�B	�B	�;B	�iB	�;B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	��B	�%B	�ZB	�TB	�+B	��B	�B	�|B	��B	�oB	�B	��B	�AB	�B	��B	�"B	�WB	�)B	��B	��B	��B	�KB	�DB	�QB	�WB	�/B	�B	�%B	�ZB	��B	�MB	��B	�AB	�;B	�B	�/B	�QB	�B	�B	��B	��B	�B	�]B	�]B	�B	�2B	�B	��B	��B	�8B	�B	�	B	��B	�B
B
�B
B
VB
�B
�B
�B
�B

=B
fB
�B
B	�(B	�cB
�B
B
oB
oB
AB
	�B
�B
MB
B
�B
�B
�B
�B
1B

	B

rB
B
uB
@B
oB
hB
4B
�B
(B
�B
4B
B
hB
�B
:B
oB
�B
B
B
�B
�B
�B
�B
�B
+B
_B
�B
B
qB
�B
!B
!B
VB
�B
!B
�B
OB
 �B
!�B
!�B
!�B
"4B
!�B
#B
#�B
$B
$B
$tB
&LB
'�B
'�B
($B
)_B
(�B
(�B
*�B
)�B
)�B
*0B
*�B
+kB
,B
+�B
,=B
,B
-CB
-wB
-wB
-�B
-wB
-�B
-�B
.B
.�B
.�B
.�B
.�B
/�B
0!B
/�B
/�B
1'B
33B
3�B
3�B
3�B
3�B
33B
2�B
3�B
5tB
5B
5B
49B
5�B
4�B
5tB
5�B
5�B
5tB
6zB
6zB
7B
7�B
8RB
8�B
8�B
8�B
8�B
9$B
9�B
:*B
:*B
:�B
:�B
;0B
;dB
<�B
<�B
<�B
=<B
=�B
?�B
A�B
A�B
B'B
B�B
C�B
D�B
D�B
E9B
FB
FB
F�B
G�B
G�B
G�B
HB
H�B
H�B
HKB
HKB
H�B
HKB
GB
C�B
D3B
D�B
D�B
D�B
D�B
D�B
D�B
EB
EmB
EmB
EmB
E�B
FB
F�B
GB
G�B
HB
HB
H�B
IB
K)B
J�B
JXB
J�B
I�B
I�B
J#B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
M6B
MjB
M�B
MjB
M�B
M�B
MjB
M�B
NpB
PB
PB
P}B
P�B
Q�B
S&B
S�B
S�B
T,B
UgB
U�B
VB
VB
VmB
V�B
W
B
W?B
Y�B
ZQB
Z�B
Z�B
\�B
]�B
]�B
^B
^5B
^5B
^jB
^�B
^�B
_pB
_;B
_B
_pB
`B
`�B
`�B
`�B
`�B
`�B
aHB
a|B
a�B
bB
b�B
b�B
b�B
b�B
b�B
cTB
cTB
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
e�B
e�B
f2B
f2B
ffB
f�B
gmB
h>B
iB
iDB
iDB
iyB
iyB
iDB
iyB
iyB
i�B
jB
k�B
j�B
jB
jKB
jKB
jB
jB
jB
j�B
j�B
k�B
l�B
l�B
m)B
m�B
n�B
o5B
o B
o5B
o B
o B
o B
o B
o B
o5B
o B
n�B
o5B
oiB
poB
p�B
qAB
qvB
q�B
q�B
rB
rB
r|B
rGB
rGB
r|B
rGB
rGB
rGB
rB
rGB
s�B
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u�B
v+B
u�B
v+B
v�B
v�B
v�B
v�B
v�B
v`B
v`B
w�B
wfB
w�B
x8B
x�B
yrB
yrB
zxB
z�B
z�B
z�B
{B
|B
{�B
|B
{�B
|B
{�B
|PB
|PB
|PB
|PB
|�B
|�B
|�B
|�B
|�B
}VB
}�B
~(B
~]B
~�B
�B
�4B
�B
�B
�B
�oB
�AB
�AB
�AB
��B
��B
��B
�B
�GB
��B
��B
�MB
��B
��B
��B
�%B
�YB
�%B
�%B
��B
��B
��B
��B
�+B
��B
�+B
��B
��B
�fB
�B
�lB
�lB
��B
�lB
�lB
�xB
�DB
�DB
�xB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�PB
�PB
��B
�PB
��B
�PB
��B
�"B
�VB
�(B
��B
��B
�.B
�.B
��B
��B
��B
�4B
�hB
�B
�:B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�FB
�FB
�FB
�FB
�FB
�FB
��B
�MB
��B
�SB
��B
�SB
��B
�SB
��B
��B
��B
��B
�+B
�+B
�+B
��B
��B
��B
��B
�eB
�1B
��B
�7B
�B
�7B
��B
��B
��B
�=B
�qB
�qB
�qB
�qB
�qB
�qB
��B
�xB
�B
�B
�~B
�B
�B
�OB
�OB
��B
��B
�!B
��B
�VB
�VB
�!B
�VB
��B
��B
�'B
�'B
��B
�\B
�-B
��B
�bB
��B
��B
��B
��B
�4B
�hB
�B
�B
��B
�B
�:B
�:B
�nB
��B
��B
�B
�@B
�B
�B
�B
��B
�FB
�FB
�B
��B
��B
��B
�LB
��B
��B
��B
��B
��B
�RB
�B
�B
��B
�B
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
�$B
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
�*B
�*B
�*B
��B
��B
��B
�0B
��B
�6B
�kB
��B
�kB
��B
�kB
��B
��B
��B
�=B
�qB
��B
��B
�CB
�B
�wB
��B
��B
�B
�B
�}B
��B
��B
��B
��B
��B
��B
��B
�!B
�!B
��B
�UB
�UB
��B
��B
�[@zx@Um]B
j�B
e`B
e�B
g�B
ffB
e�B
f�B
f�B
f2B
hsB
f�B
f2B
h
B
e,B
e�B
g�B
e�B
h>B
g�B
gB
f�B
e�B
g�B
g8B
f2B
g8B
hsB
d�B
g�B
g�B
e`B
h
B
g8B
ffB
h>B
gB
f�B
h>B
f�B
f�B
g�B
k�B
c�B
g�B
hsB
e�B
e�B
h>B
f�B
f2B
h
B
gB
e�B
gmB
ffB
f2B
f2B
h>B
gB
e�B
g�B
h>B
e�B
f�B
h
B
f�B
f2B
g�B
gB
f�B
g8B
iDB
hsB
f�B
h�B
h
B
gmB
e�B
g8B
h�B
l�B
m]B
o B
m)B
jKB
lWB
k�B
i�B
l"B
l�B
l"B
p�B
t�B
w�B
}�B
��B
�rB
��B
��B
�FB
�_B
�7B
��B
��B
��B
��B
�'B
�B
�nB
��B
�@B
��B
�B
�FB
�zB
�@B
��B
��B
��B
�B
��B
�XB
��B
��B
�eB
�B
�!B
��B
��B
��B
��B
�-B
��B
�hB
��B
��B
��B
�?B
�LB
�B
��B
��B
�XB
��B
��B
�dB
��B
�wB
�B
�BB
��B
� B
�OB
�'B
��B
�UB
ÖB
�'B
B
�B
�3B
�mB
�B
��B
�)B
��B
��B
�#B
�B
��B
�^B
��B
�)B
�XB
��B
��B
ȴB
�#B
�B
ƨB
ƨB
�KB
��B
��B
�tB
�gB
ÖB
��B
ȴB
�B
ȴB
�B
��B
�HB
�B
�BB
��B
�B
� B
��B
ҽB
��B
�2B
��B
��B
�sB
��B
֡B
�B
��B
�mB
��B
�?B
�
B
��B
�yB
��B
�B
�B
��B
ٴB
�B
�B
�B
�cB
�cB
��B
��B
�iB
�B
��B
��B
��B
�BuBB�B	lBDB�B=�BT�BV�B\�Bg�Bm�Bo5Br�B}�B�MB��B��B��B��B�FB��B�B�B��B��B��B��BȴBʌB˒B��B�)B̘B̘B�^B�B�TB��B̘BΥB�B�TB��B��B�aBԕB՛B�&B��B�2B��B�&BԕB�gBԕB�[B��B�9B֡B��B�pB�5B�B�;B�B�B��B�B��B��B�mB�yB��B�B�B�B�WB�QB��B��B��B��B�B�cB�B�B��B�MB��B�lBSB�VB iB �BAB�B�B
�B�B1B	�B�BBPB�B
=B	lB
�BDB�B.B�B�B�B�B�B�B�B"B�B�B�B~BPBPBB
	BJBbB�BCB�B'�B2�B4nB,�B-�B,qB2�B,�B-B+�B)�B)�B+6B)�B(XB)*B)�B(XB'�B(�B(�B%FB'RB&B1[B)�B/B,=B0�B0�B33B0�B5�B7�B2�B2-B2-B3�B5?B4�B2aB49B3hB8�B2�B49B=qB33B7�B:�B7�B2�B6FB4�B4B6zB6FB6�B7�B8�B7�B9�BGzB?HB@�BD3B?�BF�BB�BCaBDgBB�BA�BB�BNB\�BT,BR�BU2BT�BR�BS&BTaBR�BR�BT,BR BRTBS[BQNBR BS�BYKBXyBW
B\)BR�BU�BT�BR�BT�BT,BR�BS[BS�BP�BOBBR�BS&BS�BVmBV�BVBT�BT�BT�BV9B_pBd&Ba�Bi�B_�BS�BR�BT�BS�BF�BFtBGzBEmB=�B;�B;�B;�B<�B?}B=�B<jB?�B@OB?B?�B?�B>B?BB[BC�BC-BFBEBC-BB�BC�BD�BA�BA�BF�B<�B;0B:*B=qB@�B>�B@�B\�B^�B_�BX�BXB_;Be�Ba�BaB_;B]/B\�B\�BYB[�B_BYBYBZ�B_pBf�Bf�Be�Bd�BgmBjBi�BiyBe�Bd&BkBu�BkBu%Be�Bc�Bb�Bb�BaBc�Ba�B`�BaB`�B_�B]dBc�B^�B_;B\�B]�B\]BXB[#BXyBgBVmB]�Bh>Be�BN<BQ�BQ�BOBOvBM�BRTBOBBMjBK)BJ�BH�BJXBJ�BGzBIBI�BIRBFtBFBD�BC�BA�B@�BB'BA�B=�B<�B@B<6B?}B9�B9$B8B49B5B4nB1[B2-B2aB4B33B33B0!B3�B1'B/�B4�B.�B-CB&B"hB'�B�BfB�B�B�B�B�B;B �B;B�B�BGB��B��B��BGB��B�PB�VB�8B�B�B��B��B�B�|B�;B��B��B�dB��B�)B��BںB�KB�B�sB�gB�EB��B�B�B� B��B�}B�NB�}B�<B�pB�<B��B�XB�gBB��B��B�B�[B�B��B��BǮB�B�}B�0B��B�0B��B��B��B�$B�$B��B��B��B��B�_B�B��B��B�VB��B�'B�B�IB��B�FB��B�=B��B�B�B�GB~�B�AB|�B|Bv`Bs�Br|Bm�Bl"Bp�By	BdZB]�B\�BcTB]�Bf�BK�BU2BI�BIRBLdBM6BGB>BB8B9�B8RB5�B4�B2-B.}B=�B+�B-B)�B*�B*0B#�B!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                           G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022021512074220220215120742IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022022508010320220225080103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022022508010320220225080103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                