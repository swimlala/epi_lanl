CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  u   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2023-01-25T18:03:20Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  P�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  U�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  iD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  n0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  ƨ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   <   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�           HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (Argo profile    3.1 1.2 19500101000000  20230125180320  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_246                 6810_008521_246                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���h4@���h411  @���D�@���D�@2})4���@2})4����d��K]�d�d��K]�d11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@�\@@  @�  @��@��
@�\AG�AG�A!G�A,��A>�RA^{A~�RA�\)A�  A��A�\)AϮA߮A�A��B  B(�B  B�
B(  B0  B8  B?�BG�BP(�BXQ�B`Q�Bh  Bp  Bx  B�
B��B��
B��
B�  B�{B�{B��B��
B�  B�(�B�=qB�{B��B��B�(�B�{B�  B��B��B�{B�  B�  B�{B�  B��B�  B��B��
B��
B��
B�  C   C��C  C  C  C

=C
=C  C�C��C��C  C
=C  C  C�C   C"
=C$  C&
=C(
=C*
=C,  C-��C0  C2  C3��C5��C7��C9��C<
=C>�C@  CB  CC��CE�CH  CI��CK�HCM��CP
=CR  CS��CV  CX
=CZ  C\  C^  C_��Ca��Cd  Cf
=Ch{Cj
=Cl
=Cn{Cp  Cr  Ct
=Cv
=Cx
=Cz
=C|
=C}��C�C�C���C�  C�
=C�C�C�C�C�  C���C���C�
=C�C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�  C���C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C���C�  C�  C�  C���C���C���C��C���C�  C�  C�  C�  C���C�  C�  C���C���C���C���C���C�  C�C�
=C�
=C�C�C�
=C�C�C�C�  C�C�C�  C�C�C�C�  C���C�  C�
=C�C�  C���C�C�  C���C�  C�  C�C�C���C�  C�  C�C�C���C���C�  C�  C���C���C�  C�C�
=C�  C�  C�C�  C�  C�C�  C���C�  C�C�  C���C���C�C�C���C���C�C�C�
=C�C���C���C���C���D �D  D� D  D}qD��D}qD�qD}qD�qD}qD�qDz�D�qD� D�qD}qD�qD	}qD	�qD
z�D
��D� D�D� D��D� DD��D  D}qD�D� D  D� DD� D�qD}qD  D� D  D� D  D}qD�qD� D�qD� DD�D  Dz�D  D� D  D��D�D� D  D��D�D� D �D ��D!D!�D"  D"}qD#  D#� D$D$��D%  D%� D%�qD&��D'D'��D(�D(��D)�D)��D*�D*}qD*��D+}qD+�qD,z�D,��D-}qD.�D.��D/  D/� D0D0}qD1  D1��D2�D2� D3  D3� D3��D4}qD5  D5� D6  D6��D7�D7� D8  D8� D9  D9��D:�D:��D;�D;�D<  D<}qD<�qD=� D>�D>� D?  D?��D@  D@}qD@��DAz�DA�qDB� DC  DC}qDC�qDD}qDD�qDE}qDE�qDF��DG  DG� DH�DH��DH�qDI��DJ  DJ� DKDK�DL�DLz�DL��DM}qDN  DN}qDO  DO�DP  DP}qDQ  DQ}qDR  DR� DS  DS� DT  DT�DUDU��DV  DV� DW  DW� DX  DX� DY  DY��DZ  DZz�D[  D[��D\  D\��D]  D]z�D]�qD^� D_  D_}qD`  D`��Da�Da��Db�Db��Dc  Dc��Dd�Dd}qDd�qDe}qDf�Df}qDf��Dg� Dh�Dh��Di  Di� Di�qDj� Dk  Dk}qDk��Dl� Dm�Dm� Dm�qDn� Do�Do� DpDp��Dq  Dq�DrDr� Ds�Ds�Dt�Dt� Du  Du}qDu�qDvz�Dw  Dw}qDw��Dxz�Dx�qDy� Dz�Dz}qDz�qD{��D|  D|� D}  D}� D~  D~� D  D}qD�qD�@ D�� D��qD���D�=qD�~�D�D�  D�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�~�D��HD��D�AHD��HD�� D�HD�@ D�~�D���D�  D�@ D��HD���D���D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�HD�@ D�~�D��qD�  D�@ D�~�D�� D���D�@ D�c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?��?k�?��R?�33?�(�@�@z�@#�
@=p�@Q�@aG�@s33@��
@�=q@�
=@��
@�=q@�@��
@���@�@�\@���@�z�A ��Az�A�A  Az�A�A ��A$z�A+�A1G�A5�A:=qA@��AE�AI��AP��ATz�AY��A`��Adz�Ai��Ap��AuAz=qA���A��\A��A�Q�A��HA��A�Q�A�=qA�p�A�  A��A���A��A���A���A��RA���A��
A�p�A���A��HA��A�Q�A��\A�z�A��A���A�(�A�\)A���A˅A�{A�  A��HA���A�
=A��AۅA�ffA���A�\A�{A�  A�=qA��A�  A�A�(�A��A�G�A�(�A�\)B�B�B\)B��B=qB33B��B	�B
=Bz�Bp�B
=B  B�B�RB�
B��B=qB�BQ�B�B
=B  B��B�RB�
B!G�B"=qB#�B%�B&=qB'33B(��B*=qB+
=B,��B.{B/
=B0Q�B1B2�RB4Q�B5p�B6�\B8(�B9�B:=qB;�
B<��B=�B?�B@��BABC33BDz�BEp�BF�HBH  BH��BJ�\BK�
BL��BN=qBO�BPz�BR{BS\)BT(�BUBW33BX(�BY��B[
=B\  B]G�B^�HB_�
Ba�Bb�\Bc�
Bd��Bf�\Bg�Bh��BjffBk�Bl��Bn{Bo�Bpz�Bq�Bs
=Bt  Bu��Bv�RBw�ByG�BzffB{�B|��B~{B
=B�Q�B��RB�\)B�(�B��\B�G�B�  B�z�B�
=B��
B�z�B���B��B�ffB��HB��B�Q�B���B���B�=qB���B�\)B�(�B��RB�33B��B��RB�G�B�B�z�B�G�B��B�Q�B��B���B�{B��HB��B�  B��RB�p�B�  B�z�B�\)B��B�z�B�G�B�  B��\B�
=B�B��\B�
=B�B��\B���B���B�ffB��HB�p�B�Q�B���B�p�B�(�B��HB�\)B�  B���B�p�B��B���B��B�  B��\B�\)B�  B�z�B��B��B��\B�
=B�B�z�B���B���B�ffB���B��B�ffB���B�p�B�=qB���B���B�  B���B��B��B���B�\)B�  B�z�B�G�B��B�ffB�
=B�B�z�B���BŅB�=qB��HB�p�B��BȸRB�p�B�{Bʏ\B�33B��Ḅ�B��B��
BΣ�B�G�B�B�z�B�G�B�B�ffB��B��Bԏ\B��B�B�z�B�33B�B�ffB��B��B�z�B�
=B�B܏\B�33Bݙ�B�Q�B��B�B�=qB���B�B�Q�B���B㙚B�Q�B���B�p�B�{B��HB�B�{B�RB�p�B�=qB���B�\)B�  B�RB�B�  B��B�B�  B��\B�G�B�{B�\B��B��B�z�B���B��B�ffB��HB�p�B�=qB��HB�\)B�{B��HB��B�{B��HB���B�(�B���B���C 
=C \)C C{CQ�C��C  CQ�C�C��C33Cz�C�
C(�CffCC(�C\)CC{C\)C�RC�CffC�RC�Cp�C�RC	{C	z�C	C

=C
p�C
��C
=CffCC�C\)C��C��CQ�C�C�HC33C�\C�C33Cz�CC  C\)C�RC
=C\)C��C�C=qC��C�CG�C�\C�
C(�C�\C�C33CffC�RC�Cp�C�RC��CQ�C�C�C33C�\C��C=qC�C��C(�Cz�C��C�CffC��C��CQ�C��C��C33Cp�CC{C\)C�RC
=CQ�C�\C�HC =qC ��C �HC!�C!p�C!C"{C"ffC"��C"�C#33C#�\C#�HC$�C$ffC$�RC%
=C%\)C%�C%��C&=qC&�C&��C'
=C'\)C'�RC({C(\)C(��C(�C)(�C)p�C)�
C*(�C*p�C*�RC*��C+=qC+�\C+�C,33C,�\C,�C-=qC-z�C-C.
=C.ffC.��C/{C/Q�C/��C0
=C0\)C0��C0�HC133C1z�C1��C2(�C2�C2�
C3{C3\)C3�C4  C4\)C4�C5
=C5G�C5��C5��C6Q�C6�C7  C7=qC7�\C7�
C8(�C8�\C8�C9=qC9�\C9�HC:(�C:z�C:�HC;=qC;�\C;�HC<33C<z�C<�
C=�C=�C=�HC>33C>�\C>�HC?(�C?p�C?��C@(�C@�\C@�CAG�CA�\CA�HCB33CB�\CB�CCG�CC��CD  CDQ�CD�RCE
=CEQ�CE��CE��CFG�CF�CG
=CGffCG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                     11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?u@�\@@  @�  @��@��
@�\AG�AG�A!G�A,��A>�RA^{A~�RA�\)A�  A��A�\)AϮA߮A�A��B  B(�B  B�
B(  B0  B8  B?�BG�BP(�BXQ�B`Q�Bh  Bp  Bx  B�
B��B��
B��
B�  B�{B�{B��B��
B�  B�(�B�=qB�{B��B��B�(�B�{B�  B��B��B�{B�  B�  B�{B�  B��B�  B��B��
B��
B��
B�  C   C��C  C  C  C

=C
=C  C�C��C��C  C
=C  C  C�C   C"
=C$  C&
=C(
=C*
=C,  C-��C0  C2  C3��C5��C7��C9��C<
=C>�C@  CB  CC��CE�CH  CI��CK�HCM��CP
=CR  CS��CV  CX
=CZ  C\  C^  C_��Ca��Cd  Cf
=Ch{Cj
=Cl
=Cn{Cp  Cr  Ct
=Cv
=Cx
=Cz
=C|
=C}��C�C�C���C�  C�
=C�C�C�C�C�  C���C���C�
=C�C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�  C���C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C���C�  C�  C�  C���C���C���C��C���C�  C�  C�  C�  C���C�  C�  C���C���C���C���C���C�  C�C�
=C�
=C�C�C�
=C�C�C�C�  C�C�C�  C�C�C�C�  C���C�  C�
=C�C�  C���C�C�  C���C�  C�  C�C�C���C�  C�  C�C�C���C���C�  C�  C���C���C�  C�C�
=C�  C�  C�C�  C�  C�C�  C���C�  C�C�  C���C���C�C�C���C���C�C�C�
=C�C���C���C���C���D �D  D� D  D}qD��D}qD�qD}qD�qD}qD�qDz�D�qD� D�qD}qD�qD	}qD	�qD
z�D
��D� D�D� D��D� DD��D  D}qD�D� D  D� DD� D�qD}qD  D� D  D� D  D}qD�qD� D�qD� DD�D  Dz�D  D� D  D��D�D� D  D��D�D� D �D ��D!D!�D"  D"}qD#  D#� D$D$��D%  D%� D%�qD&��D'D'��D(�D(��D)�D)��D*�D*}qD*��D+}qD+�qD,z�D,��D-}qD.�D.��D/  D/� D0D0}qD1  D1��D2�D2� D3  D3� D3��D4}qD5  D5� D6  D6��D7�D7� D8  D8� D9  D9��D:�D:��D;�D;�D<  D<}qD<�qD=� D>�D>� D?  D?��D@  D@}qD@��DAz�DA�qDB� DC  DC}qDC�qDD}qDD�qDE}qDE�qDF��DG  DG� DH�DH��DH�qDI��DJ  DJ� DKDK�DL�DLz�DL��DM}qDN  DN}qDO  DO�DP  DP}qDQ  DQ}qDR  DR� DS  DS� DT  DT�DUDU��DV  DV� DW  DW� DX  DX� DY  DY��DZ  DZz�D[  D[��D\  D\��D]  D]z�D]�qD^� D_  D_}qD`  D`��Da�Da��Db�Db��Dc  Dc��Dd�Dd}qDd�qDe}qDf�Df}qDf��Dg� Dh�Dh��Di  Di� Di�qDj� Dk  Dk}qDk��Dl� Dm�Dm� Dm�qDn� Do�Do� DpDp��Dq  Dq�DrDr� Ds�Ds�Dt�Dt� Du  Du}qDu�qDvz�Dw  Dw}qDw��Dxz�Dx�qDy� Dz�Dz}qDz�qD{��D|  D|� D}  D}� D~  D~� D  D}qD�qD�@ D�� D��qD���D�=qD�~�D�D�  D�>�D�~�D���D���D�@ D�� D�� D�HD�@ D�~�D��HD��D�AHD��HD�� D�HD�@ D�~�D���D�  D�@ D��HD���D���D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�HD�@ D�~�D��qD�  D�@ D�~�D�� D���D�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?��?k�?��R?�33?�(�@�@z�@#�
@=p�@Q�@aG�@s33@��
@�=q@�
=@��
@�=q@�@��
@���@�@�\@���@�z�A ��Az�A�A  Az�A�A ��A$z�A+�A1G�A5�A:=qA@��AE�AI��AP��ATz�AY��A`��Adz�Ai��Ap��AuAz=qA���A��\A��A�Q�A��HA��A�Q�A�=qA�p�A�  A��A���A��A���A���A��RA���A��
A�p�A���A��HA��A�Q�A��\A�z�A��A���A�(�A�\)A���A˅A�{A�  A��HA���A�
=A��AۅA�ffA���A�\A�{A�  A�=qA��A�  A�A�(�A��A�G�A�(�A�\)B�B�B\)B��B=qB33B��B	�B
=Bz�Bp�B
=B  B�B�RB�
B��B=qB�BQ�B�B
=B  B��B�RB�
B!G�B"=qB#�B%�B&=qB'33B(��B*=qB+
=B,��B.{B/
=B0Q�B1B2�RB4Q�B5p�B6�\B8(�B9�B:=qB;�
B<��B=�B?�B@��BABC33BDz�BEp�BF�HBH  BH��BJ�\BK�
BL��BN=qBO�BPz�BR{BS\)BT(�BUBW33BX(�BY��B[
=B\  B]G�B^�HB_�
Ba�Bb�\Bc�
Bd��Bf�\Bg�Bh��BjffBk�Bl��Bn{Bo�Bpz�Bq�Bs
=Bt  Bu��Bv�RBw�ByG�BzffB{�B|��B~{B
=B�Q�B��RB�\)B�(�B��\B�G�B�  B�z�B�
=B��
B�z�B���B��B�ffB��HB��B�Q�B���B���B�=qB���B�\)B�(�B��RB�33B��B��RB�G�B�B�z�B�G�B��B�Q�B��B���B�{B��HB��B�  B��RB�p�B�  B�z�B�\)B��B�z�B�G�B�  B��\B�
=B�B��\B�
=B�B��\B���B���B�ffB��HB�p�B�Q�B���B�p�B�(�B��HB�\)B�  B���B�p�B��B���B��B�  B��\B�\)B�  B�z�B��B��B��\B�
=B�B�z�B���B���B�ffB���B��B�ffB���B�p�B�=qB���B���B�  B���B��B��B���B�\)B�  B�z�B�G�B��B�ffB�
=B�B�z�B���BŅB�=qB��HB�p�B��BȸRB�p�B�{Bʏ\B�33B��Ḅ�B��B��
BΣ�B�G�B�B�z�B�G�B�B�ffB��B��Bԏ\B��B�B�z�B�33B�B�ffB��B��B�z�B�
=B�B܏\B�33Bݙ�B�Q�B��B�B�=qB���B�B�Q�B���B㙚B�Q�B���B�p�B�{B��HB�B�{B�RB�p�B�=qB���B�\)B�  B�RB�B�  B��B�B�  B��\B�G�B�{B�\B��B��B�z�B���B��B�ffB��HB�p�B�=qB��HB�\)B�{B��HB��B�{B��HB���B�(�B���B���C 
=C \)C C{CQ�C��C  CQ�C�C��C33Cz�C�
C(�CffCC(�C\)CC{C\)C�RC�CffC�RC�Cp�C�RC	{C	z�C	C

=C
p�C
��C
=CffCC�C\)C��C��CQ�C�C�HC33C�\C�C33Cz�CC  C\)C�RC
=C\)C��C�C=qC��C�CG�C�\C�
C(�C�\C�C33CffC�RC�Cp�C�RC��CQ�C�C�C33C�\C��C=qC�C��C(�Cz�C��C�CffC��C��CQ�C��C��C33Cp�CC{C\)C�RC
=CQ�C�\C�HC =qC ��C �HC!�C!p�C!C"{C"ffC"��C"�C#33C#�\C#�HC$�C$ffC$�RC%
=C%\)C%�C%��C&=qC&�C&��C'
=C'\)C'�RC({C(\)C(��C(�C)(�C)p�C)�
C*(�C*p�C*�RC*��C+=qC+�\C+�C,33C,�\C,�C-=qC-z�C-C.
=C.ffC.��C/{C/Q�C/��C0
=C0\)C0��C0�HC133C1z�C1��C2(�C2�C2�
C3{C3\)C3�C4  C4\)C4�C5
=C5G�C5��C5��C6Q�C6�C7  C7=qC7�\C7�
C8(�C8�\C8�C9=qC9�\C9�HC:(�C:z�C:�HC;=qC;�\C;�HC<33C<z�C<�
C=�C=�C=�HC>33C>�\C>�HC?(�C?p�C?��C@(�C@�\C@�CAG�CA�\CA�HCB33CB�\CB�CCG�CC��CD  CDQ�CD�RCE
=CEQ�CE��CE��CFG�CF�CG
=CGffCG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                     11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�G�A�G�A�K�A�K�A�O�A�M�A�E�A�G�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�O�A�^5A�bNA�ZAϧ�A��A���A�bA��A�1'A�=qA�-A��A�%A�  A���A��#AϏ\A�ZA�Q�A�1'A��A���A��A���AήAΣ�AΗ�AΏ\AΉ7A΁A�K�A���A��A͡�A�Q�A��mA�K�A˾wA�n�A���A�C�A�Q�A���A���A�dZA��#A�`BA���A�?}A�^5A�C�A���A��A��mA��A���A�?}A��jA��\A�|�A�t�A��A��A�;dA� �A�Q�A��jA�1A���A��A�K�A�S�A�
=A�\)A�x�A��A�XA���A��A�%A��HA���A��FA��`A�I�A��A�/A���A���A�I�A�dZA�K�A���A�{A�ƨA���A�G�A���A�G�A��A��yA�x�A�/A�I�A��A�bA�A}�Ay�AvE�At �Aq�-Ap�Ao&�AlQ�Aj�HAh��Af5?AdQ�AaS�A_%A]�AZQ�AX�!AUhsAS`BARE�AQ�APJANM�AM33ALz�AIVAD��AC�PAB�`AA�AAoA@A�A>��A=�A:�9A9"�A7��A6��A41'A2�A1l�A0�jA.~�A,(�A*��A)�PA(E�A'dZA%�^A$~�A#ƨA"=qA �jAp�A�HA��A�AbAĜA5?A��A�7A"�Av�AJA�7A(�A;dAO�A�Ar�A��AO�A�HA{A&�A1A��A�A��AȴA=qA��A��A�`AdZA
�/A
�A	`BA�A1A�;A�A`BA�A�;AK�A33A�/A9XAn�A  Al�A5?A-AA�#A n�@�5?@�p�@��@��F@��@�ff@��@��@��-@�x�@�&�@���@�ƨ@��\@�hs@�+@�P@�!@�@�r�@�@�K�@��T@�/@��/@��
@��H@�ȴ@��@�J@�`B@��@�@�dZ@��u@���@ߕ�@ް!@���@݁@��@ޏ\@ޟ�@ޟ�@�~�@�E�@�J@ݡ�@ݩ�@݁@��;@��@ڗ�@�1@�\)@��@�ff@Չ7@�9X@�1@�b@�b@��@�Q�@ԃ@�ƨ@щ7@��@θR@�M�@�$�@���@̃@��
@�t�@�\)@�l�@�C�@��y@��T@�O�@���@ț�@��@�\)@ư!@��@őh@�&�@�I�@���@ÍP@�l�@�S�@+@�$�@�n�@¸R@�V@�$�@���@�%@�Ĝ@��@�b@�  @��m@��P@��R@�-@�@�V@�Ĝ@�r�@��@�~�@�5?@�J@��@�/@���@���@���@��u@��D@�j@�A�@�1'@� �@��
@�K�@���@���@��^@�O�@�/@�%@���@���@��@���@��u@�A�@��@�"�@��@���@��@��7@�G�@��@��u@�bN@���@���@�n�@�J@��@��T@��T@���@���@���@��@�7L@�Ĝ@��@��@��R@�J@��-@�/@���@��9@�r�@��@��
@���@��@���@�~�@�^5@�V@�5?@���@�/@��@���@�|�@��@���@���@�V@�@��j@�1'@��m@��w@��@�"�@��H@���@�-@��T@��^@���@��h@�/@�Ĝ@�j@�9X@�b@��w@�S�@�33@�o@�@�@��@��@�ȴ@��\@�v�@�M�@��@�hs@�Ĝ@��@�r�@�Q�@��@���@�S�@�"�@�
=@��@��R@���@�v�@�V@��@��7@�&�@���@�Ĝ@��9@�z�@�9X@�b@�  @��@�ƨ@���@�K�@��@��@���@�~�@��@��^@���@���@���@��@���@���@���@��@�A�@���@��@�o@��R@�v�@�^5@�V@��@��^@��7@���@��D@�Z@�(�@��m@��w@��@�C�@�o@�o@��@��R@��!@��+@���@���@��@�hs@�`B@�/@�V@���@�z�@�r�@�bN@�b@���@�@��R@���@��H@��@�E�@�@�O�@�G�@���@�Q�@�A�@�A�@�1'@��
@���@�\)@�"�@�@��y@���@���@���@�~�@�v�@�M�@�5?@��@��7@�X@�G�@�7L@��@��@��@�@�;@��@l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�E�A�G�A�K�A�E�A�G�A�G�A�G�A�G�A�O�A�M�A�G�A�K�A�M�A�K�A�Q�A�Q�A�K�A�K�A�S�A�M�A�G�A�G�A�C�A�G�A�I�A�E�A�I�A�E�A�C�A�I�A�G�A�C�A�G�A�I�A�E�A�G�A�K�A�I�A�G�A�K�A�G�A�G�A�K�A�G�A�G�A�K�A�I�A�I�A�O�A�I�A�I�A�K�A�I�A�G�A�M�A�G�A�I�A�I�A�E�A�I�A�M�A�VA�XA�S�A�XA�^5A�\)A�`BA�\)A�dZA�^5A�^5A�ZA�ffA�ffA�ffA�ffA�hsA�ffA�ZA�VA�S�A�O�A�XA�p�Aϗ�Aϙ�Aϥ�A���A���A��A��A��A���A��`A��HA��mA��TA��A�A�  A�  A�bA�oA�JA�bA�{A�oA��A��A��A��A��A��A�$�A�&�A�&�A�-A�/A�/A�7LA�9XA�=qA�I�A�E�A�=qA�;dA�33A�-A�1'A�/A�+A�/A�/A�(�A�-A�-A�"�A�oA�VA�%A�
=A�1A�A�1A�A�A�%A�  A���A�A�  A���A�A�  A���A�  A���A���A���A���A��A��`A��HA���A���A�Aϲ-AϬAϥ�AϏ\A�~�A�r�A�hsA�ffA�\)A�XA�VA�XA�S�A�Q�A�XA�O�A�M�A�Q�A�O�A�E�A�33A�-A�&�A�-A�(�A� �A�"�A��A��A�{A�
=A�A�%A�  A���A���A��A��A��A��yA��A��A��A��mA��`A��HA��#A���A���A�AξwAζFAήAήAΰ!AΩ�AΩ�AάAΥ�AΡ�AΡ�AΣ�AΟ�AΛ�AΝ�AΝ�AΗ�AΕ�AΗ�AΑhAΏ\AΓuAΑhA΋DA΍PAΑhA΍PAΉ7AΏ\A΋DA΅A·+A΋DA΅A΃A΃A΃A�x�A�n�A�jA�`BA�S�A�?}A�9XA�33A�&�A�{A���A���A���A��yA��mA��TA��HA��/A��A���A�ȴAͮAͧ�A͡�A͛�A͙�A̓A�t�A�hsA�\)A�K�A�7LA��A�%A���A��A��yA��;A̾wA̮A̧�A̋DA�5?A��A���A��#A���A���A���A˾wA˥�Aˉ7A�x�A�v�A�v�A�r�A�jA�^5A�M�A�?}A�&�A���Aʺ^AʋDA�v�A�ffA�ZA�Q�A�=qA�{A��Aɴ9A�l�A�(�A� �A�$�A�$�A� �A�1A���AȾwAȍPA�jA���A��A���AǴ9Aǲ-AǮAǣ�AǗ�A�v�A�S�A�9XA� �A�bA���A��HA���AƾwAƶFAƟ�A�|�A�l�A�VA�33A�-A�+A��A���AżjAŉ7Aŉ7AŇ+A�`BA�VA�E�A�(�A���A�ĜAĲ-A�|�A�O�A�A�A���AÝ�A�r�A�hsA�dZA�K�A�  A�A�9XA�{A���A��A��HA���A�1'A�dZA���A�XA�bA���A�jA��A�`BA���A�  A���A��FA���A��DA�Q�A���A�VA��A���A�I�A�A��HA��DA�dZA�  A��/A��-A�JA��FA���A��A�dZA�7LA���A�O�A��
A�jA�%A���A�hsA�C�A�1A��TA��-A��!A���A�dZA�?}A���A��A���A�p�A�p�A�7LA�(�A��A�ȴA��FA���A��uA�~�A�O�A�7LA�VA�^5A���A��`A�{A��A��DA�O�A��A���A���A�r�A�O�A�-A��A���A��`A�ȴA��FA���A�jA�;dA���A���A��A��#A�p�A�ffA�`BA�VA�33A�"�A�JA�A���A��A��A��wA���A���A���A���A���A�v�A�K�A�33A��A�JA��A��mA���A��-A��-A��A�(�A��A�|�A��A�r�A�p�A�ZA�A�A�{A��A�p�A�G�A� �A��mA���A�v�A�JA�dZA���A�hsA�jA�C�A�&�A�VA���A��A�ƨA�r�A��FA���A�n�A�+A��A��`A��#A�A���A��hA�v�A�A�A��A�1A��A���A�ƨA���A�|�A�^5A�K�A�-A�bA���A��TA�A��A�|�A�G�A��A���A��A��A�~�A�O�A�$�A��A���A�^5A�=qA�&�A�VA��A���A�O�A�+A�oA��#A��!A��uA��A�\)A�E�A�+A�1A���A���A��A�p�A�^5A�I�A�A�A�;dA�1'A�&�A��A�  A��A���A��\A�~�A�^5A�$�A��A�1A��jA�C�A���A�ĜA��A�\)A�E�A�+A���A�p�A�Q�A�&�A��#A��wA���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                     11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�G�A�K�A�K�A�O�A�M�A�E�A�G�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�O�A�^5A�bNA�ZAϧ�A��A���A�bA��A�1'A�=qA�-A��A�%A�  A���A��#AϏ\A�ZA�Q�A�1'A��A���A��A���AήAΣ�AΗ�AΏ\AΉ7A΁A�K�A���A��A͡�A�Q�A��mA�K�A˾wA�n�A���A�C�A�Q�A���A���A�dZA��#A�`BA���A�?}A�^5A�C�A���A��A��mA��A���A�?}A��jA��\A�|�A�t�A��A��A�;dA� �A�Q�A��jA�1A���A��A�K�A�S�A�
=A�\)A�x�A��A�XA���A��A�%A��HA���A��FA��`A�I�A��A�/A���A���A�I�A�dZA�K�A���A�{A�ƨA���A�G�A���A�G�A��A��yA�x�A�/A�I�A��A�bA�A}�Ay�AvE�At �Aq�-Ap�Ao&�AlQ�Aj�HAh��Af5?AdQ�AaS�A_%A]�AZQ�AX�!AUhsAS`BARE�AQ�APJANM�AM33ALz�AIVAD��AC�PAB�`AA�AAoA@A�A>��A=�A:�9A9"�A7��A6��A41'A2�A1l�A0�jA.~�A,(�A*��A)�PA(E�A'dZA%�^A$~�A#ƨA"=qA �jAp�A�HA��A�AbAĜA5?A��A�7A"�Av�AJA�7A(�A;dAO�A�Ar�A��AO�A�HA{A&�A1A��A�A��AȴA=qA��A��A�`AdZA
�/A
�A	`BA�A1A�;A�A`BA�A�;AK�A33A�/A9XAn�A  Al�A5?A-AA�#A n�@�5?@�p�@��@��F@��@�ff@��@��@��-@�x�@�&�@���@�ƨ@��\@�hs@�+@�P@�!@�@�r�@�@�K�@��T@�/@��/@��
@��H@�ȴ@��@�J@�`B@��@�@�dZ@��u@���@ߕ�@ް!@���@݁@��@ޏ\@ޟ�@ޟ�@�~�@�E�@�J@ݡ�@ݩ�@݁@��;@��@ڗ�@�1@�\)@��@�ff@Չ7@�9X@�1@�b@�b@��@�Q�@ԃ@�ƨ@щ7@��@θR@�M�@�$�@���@̃@��
@�t�@�\)@�l�@�C�@��y@��T@�O�@���@ț�@��@�\)@ư!@��@őh@�&�@�I�@���@ÍP@�l�@�S�@+@�$�@�n�@¸R@�V@�$�@���@�%@�Ĝ@��@�b@�  @��m@��P@��R@�-@�@�V@�Ĝ@�r�@��@�~�@�5?@�J@��@�/@���@���@���@��u@��D@�j@�A�@�1'@� �@��
@�K�@���@���@��^@�O�@�/@�%@���@���@��@���@��u@�A�@��@�"�@��@���@��@��7@�G�@��@��u@�bN@���@���@�n�@�J@��@��T@��T@���@���@���@��@�7L@�Ĝ@��@��@��R@�J@��-@�/@���@��9@�r�@��@��
@���@��@���@�~�@�^5@�V@�5?@���@�/@��@���@�|�@��@���@���@�V@�@��j@�1'@��m@��w@��@�"�@��H@���@�-@��T@��^@���@��h@�/@�Ĝ@�j@�9X@�b@��w@�S�@�33@�o@�@�@��@��@�ȴ@��\@�v�@�M�@��@�hs@�Ĝ@��@�r�@�Q�@��@���@�S�@�"�@�
=@��@��R@���@�v�@�V@��@��7@�&�@���@�Ĝ@��9@�z�@�9X@�b@�  @��@�ƨ@���@�K�@��@��@���@�~�@��@��^@���@���@���@��@���@���@���@��@�A�@���@��@�o@��R@�v�@�^5@�V@��@��^@��7@���@��D@�Z@�(�@��m@��w@��@�C�@�o@�o@��@��R@��!@��+@���@���@��@�hs@�`B@�/@�V@���@�z�@�r�@�bN@�b@���@�@��R@���@��H@��@�E�@�@�O�@�G�@���@�Q�@�A�@�A�@�1'@��
@���@�\)@�"�@�@��y@���@���@���@�~�@�v�@�M�@�5?@��@��7@�X@�G�@�7L@��@��@��@�@�;@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�E�A�G�A�K�A�E�A�G�A�G�A�G�A�G�A�O�A�M�A�G�A�K�A�M�A�K�A�Q�A�Q�A�K�A�K�A�S�A�M�A�G�A�G�A�C�A�G�A�I�A�E�A�I�A�E�A�C�A�I�A�G�A�C�A�G�A�I�A�E�A�G�A�K�A�I�A�G�A�K�A�G�A�G�A�K�A�G�A�G�A�K�A�I�A�I�A�O�A�I�A�I�A�K�A�I�A�G�A�M�A�G�A�I�A�I�A�E�A�I�A�M�A�VA�XA�S�A�XA�^5A�\)A�`BA�\)A�dZA�^5A�^5A�ZA�ffA�ffA�ffA�ffA�hsA�ffA�ZA�VA�S�A�O�A�XA�p�Aϗ�Aϙ�Aϥ�A���A���A��A��A��A���A��`A��HA��mA��TA��A�A�  A�  A�bA�oA�JA�bA�{A�oA��A��A��A��A��A��A�$�A�&�A�&�A�-A�/A�/A�7LA�9XA�=qA�I�A�E�A�=qA�;dA�33A�-A�1'A�/A�+A�/A�/A�(�A�-A�-A�"�A�oA�VA�%A�
=A�1A�A�1A�A�A�%A�  A���A�A�  A���A�A�  A���A�  A���A���A���A���A��A��`A��HA���A���A�Aϲ-AϬAϥ�AϏ\A�~�A�r�A�hsA�ffA�\)A�XA�VA�XA�S�A�Q�A�XA�O�A�M�A�Q�A�O�A�E�A�33A�-A�&�A�-A�(�A� �A�"�A��A��A�{A�
=A�A�%A�  A���A���A��A��A��A��yA��A��A��A��mA��`A��HA��#A���A���A�AξwAζFAήAήAΰ!AΩ�AΩ�AάAΥ�AΡ�AΡ�AΣ�AΟ�AΛ�AΝ�AΝ�AΗ�AΕ�AΗ�AΑhAΏ\AΓuAΑhA΋DA΍PAΑhA΍PAΉ7AΏ\A΋DA΅A·+A΋DA΅A΃A΃A΃A�x�A�n�A�jA�`BA�S�A�?}A�9XA�33A�&�A�{A���A���A���A��yA��mA��TA��HA��/A��A���A�ȴAͮAͧ�A͡�A͛�A͙�A̓A�t�A�hsA�\)A�K�A�7LA��A�%A���A��A��yA��;A̾wA̮A̧�A̋DA�5?A��A���A��#A���A���A���A˾wA˥�Aˉ7A�x�A�v�A�v�A�r�A�jA�^5A�M�A�?}A�&�A���Aʺ^AʋDA�v�A�ffA�ZA�Q�A�=qA�{A��Aɴ9A�l�A�(�A� �A�$�A�$�A� �A�1A���AȾwAȍPA�jA���A��A���AǴ9Aǲ-AǮAǣ�AǗ�A�v�A�S�A�9XA� �A�bA���A��HA���AƾwAƶFAƟ�A�|�A�l�A�VA�33A�-A�+A��A���AżjAŉ7Aŉ7AŇ+A�`BA�VA�E�A�(�A���A�ĜAĲ-A�|�A�O�A�A�A���AÝ�A�r�A�hsA�dZA�K�A�  A�A�9XA�{A���A��A��HA���A�1'A�dZA���A�XA�bA���A�jA��A�`BA���A�  A���A��FA���A��DA�Q�A���A�VA��A���A�I�A�A��HA��DA�dZA�  A��/A��-A�JA��FA���A��A�dZA�7LA���A�O�A��
A�jA�%A���A�hsA�C�A�1A��TA��-A��!A���A�dZA�?}A���A��A���A�p�A�p�A�7LA�(�A��A�ȴA��FA���A��uA�~�A�O�A�7LA�VA�^5A���A��`A�{A��A��DA�O�A��A���A���A�r�A�O�A�-A��A���A��`A�ȴA��FA���A�jA�;dA���A���A��A��#A�p�A�ffA�`BA�VA�33A�"�A�JA�A���A��A��A��wA���A���A���A���A���A�v�A�K�A�33A��A�JA��A��mA���A��-A��-A��A�(�A��A�|�A��A�r�A�p�A�ZA�A�A�{A��A�p�A�G�A� �A��mA���A�v�A�JA�dZA���A�hsA�jA�C�A�&�A�VA���A��A�ƨA�r�A��FA���A�n�A�+A��A��`A��#A�A���A��hA�v�A�A�A��A�1A��A���A�ƨA���A�|�A�^5A�K�A�-A�bA���A��TA�A��A�|�A�G�A��A���A��A��A�~�A�O�A�$�A��A���A�^5A�=qA�&�A�VA��A���A�O�A�+A�oA��#A��!A��uA��A�\)A�E�A�+A�1A���A���A��A�p�A�^5A�I�A�A�A�;dA�1'A�&�A��A�  A��A���A��\A�~�A�^5A�$�A��A�1A��jA�C�A���A�ĜA��A�\)A�E�A�+A���A�p�A�Q�A�&�A��#A��wA���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                     11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B�!B��B��B��B��B��B��B��B��B��B�B��B�B�wB�wB��B�3B�9B�3B�^B�]BB �B-B=qBU2BZ�B]dB_;Ba�Bd�Bo Bx�B��B�B�@B��B�7B�B�\B��B��B��B��B��B��B��B��B�nB��B��B�XB�mB��B��B�5B��B��B�.B�xB��B�B�B�B�;B�B�B��B�B�XB��B�B��B�CB��B�rB��B|�B�B\�BI�B>�B+6BSBB�B'�B(�B0UB!bB��B��B��B��B�nB��B��By�Bh�B]/BN�BJXB0�B#nB�BuB
��B
ݘB
�TB
�)B
�tB
��B
�qB
��B
��B
��B
�!B
�1B
�uB
�B
v�B
gB
P�B
C-B
0�B
B
�B
fB
{B	�VB	��B	��B	�)B	�dB	��B	��B	�B	�eB	��B	�4B	rB	e�B	\)B	YB	QB	I�B	?�B	8�B	0�B	+B	PB		lB	SB	 iB��B��B�5B�]B�ZB�vBܒBٴB҉B�6BΥB��B��B� B��B�9BʌB��B�[B�
B�EBݘB�5B�5B�B�2B��B�B��B�DB�B�B�"B�QB�]B� B�B	oB	�B		�B	B	JB	�B	�B	�B	
�B	DB	B	
�B	�B	JB	�B	�B	�B	�B	4B	�B	_B	VB	%�B	%zB	%zB	&B	'�B	-wB	2�B	:�B	@�B	B�B	MjB	RTB	Q�B	S[B	UgB	V9B	T�B	S[B	GEB	FB	JXB	GEB	J�B	OB	R�B	V�B	]/B	d&B	m]B	v`B	y�B	z�B	y�B	tB	j�B	iB	e�B	b�B	`�B	^�B	^B	\)B	[�B	[�B	Y�B	Z�B	Z�B	]�B	aB	`B	^�B	`�B	`vB	e�B	hsB	kQB	m�B	o5B	tTB	{�B	~�B	~�B	��B	��B	��B	�7B	�DB	�PB	��B	�7B	�fB	�\B	��B	��B	��B	�YB	��B	��B	�fB	��B	��B	�PB	��B	��B	��B	��B	�@B	��B	��B	�B	�1B	��B	��B	�~B	��B	��B	�-B	�bB	�'B	��B	�bB	��B	�tB	��B	��B	��B	��B	�eB	�wB	��B	��B	�LB	��B	��B	�$B	��B	��B	��B	ĜB	ÖB	�B	ĜB	�3B	�gB	ĜB	�B	�RB	�RB	�KB	ȴB	�KB	ȴB	��B	��B	ɺB	ɆB	ɆB	�6B	�B	�<B	�<B	�pB	�BB	��B	�TB	ҽB	��B	�,B	֡B	��B	��B	ٴB	�KB	�KB	�B	�KB	�KB	�KB	�B	ٴB	�QB	��B	��B	ݘB	��B	��B	�TB	�B	�&B	�`B	��B	�2B	�B	�B	��B	�fB	�fB	�fB	�fB	�B	�mB	�mB	��B	�B	�KB	��B	��B	��B	�)B	�5B	�B	�oB	�B	��B	�AB	�B	��B	�%B	��B	�TB	��B	�B	�TB	�B	�ZB	��B	�DB	�B	�xB	��B	�B	��B	�PB	��B	��B	��B	�B	�PB	��B	��B	��B	��B	�]B	�(B	�]B	��B
 �B
�B
�B
B
GB
B
B
�B
�B
�B
�B
MB
�B
SB
SB
�B
�B
+B
�B
1B
1B
�B
	B
	lB
	�B
	�B
	�B

	B

=B

�B
B
JB
�B
(B
bB
�B
�B
�B
�B
�B
�B
�B
�B
B
B
oB
B
�B
oB
B
@B
uB
{B
�B
SB
�B
1B
1B
1B
�B
�B
�B
	B
=B
B
xB
CB
CB
�B
~B
�B
�B
�B
�B
 �B
!bB
!�B
!�B
"4B
"4B
"4B
"4B
#B
#nB
#�B
$B
#�B
$@B
$tB
$�B
$�B
$�B
%B
%zB
%zB
%zB
%�B
&LB
&�B
&LB
&�B
(�B
)�B
)�B
)�B
(�B
(XB
)_B
(�B
(�B
(�B
(�B
(�B
*eB
+B
*�B
*�B
+6B
+kB
,B
,qB
,=B
,qB
,�B
,�B
-�B
.B
.B
.}B
.�B
.�B
.}B
/�B
0�B
0�B
0�B
0UG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�[B��B�B��B�UB��B��B�[B�IB��B��B��B�CB��B��B�!B��B��B��B�UB��B�kB�wB�qB�qB�CB��B��B�wB�=B��B�wB�=B��B�CB�B�qB��B�B�qB�B�B�B�CB�wB�B�B��B�qB�}B�IB��B�wB�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�LB��B��B�6B��B��B��B��B��B��BʌBȴB��B�B��B��BuB iB �B��B�fB��B��B�rB
rB~B�B!bB �B�B�B �B#�B#�B'�B+6B*�B,B/�B0�B3hB7�B8�B;dB?�B?}BC�BIBJ�BU�BYBXyBZQB[WBYKBZQB[�BZBZB\]BZ�BZ�B\�B^�B\�B`vB^5B_B`BB^5B_�B`BB_BaBc B`�Ba�Bb�BaBa�BcTBb�Be`BhsBgmBg�Bk�Bm�Bn/Bp�Bo Br�Bu�BtTBu�BzDB{B{B~�B~�B��B�SB��B�+B�7B�=B�7B��B��B�DB��B�(B��B�uB��B�B�B�$B��B��B�$B�$B�+B��B�+B�eB�B�1B�B�CB�=B�~B��B��B�~B�!B��B�B��B�\B��B��B��B�hB��B�@B��B��B�LB�zB��B�XB�RB�B��B�$B��B��B�*B�XB��B��B��B�RB�B��B��B�LB��B��B�zB��B��B��B�FB��B��B��B�B��B��B��B��B�OB�}B�OB�'B��B��B�9B�[B��B�B�B�3B��B�B�B�B��B��B��B��B��B�XB��B�B�BB�HB��B�aB��BǮB�B��B�RB�XBϫB��B�6B��B��B՛BںBݘB�B�yB�KB�jB�B�]B�BB�B��B�B�`B��B�sB��B�B�B��B�B�B��B�`B��B��B�lB�B�B�8B{B��B��B�lB��BoB�B�VBBB�B�lB�	B��B��B�%B�ZB��B��B��B�B�B�|B�vB�B�B��B��B�cB�cB�B�QB�B�>B�>B�B�B�B�B�B��B�`B�B�GB�5B��B�vB�B�GB�fB��B�B�2B�&B��B�5B�5B��B��B�B��B��B��B�B�[B��B҉B��B�HB��B�LB��B��B�UB��B��B�XB�B�B��B�'B�qBɺB�=B��B�B��B��B�kB��B��B��B�bB�SB�:B�eB��B�+B��B��B��B��B�OB��B�1B� B�JB�"B�JB�%B��B��B��B�_B��B�B{B�bB�_BwfB|PB|PB~�B}�B{�B{JBw2BxlBu�B{�B�~B�JB�B��BY�B[�B_pB[�BX�BR�BP�BNBL�BE�BF?BDgBFtBA�BA BA�B>B@B;dBPB<jBxB�B�B�B�B�B_B�B�BFB�B�B�B@B�BbB4B$B$BoBB�B�BoBBB!bB.B0UB0UB!�B!�B&LB"�B*�B(�B0�B49B3�B.�B0�B1�B)�B2-B5�B33B'B(�B+6B{BAB iB�B�	B�lBuB�8B#:B�sBʌB�KB�RB�HB��B�}B��B��B��B��B��B��B�}B��B��B�B��B�XB��B�LB��B�-B��B��B�OB�IB�kB��B��B�B��B��B�oB�+B��B��B�BzxByrBy�BcB|�Bg�Bh�BpBg�Be�Bc Bd�B_;B`�B]�BaHBW�BV�BS�BS[BO�BN�BJ�BJ�BK�BHBGzBV�B3�B2�B/�B4�B/�B+kB*�B9$B2�B%�BB�BIB@B�B"�BSBfB	�B�BB�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                     44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                     44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2023012518032020230125180320IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023020413020720230204130207QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023020413020720230204130207QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194920230210131949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                