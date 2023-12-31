CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  "   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-10-26T13:02:29Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  V   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       \\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  ul   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       {�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D $�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      +    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` D0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   D�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   J�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   P�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T V�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   V�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   V�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   V�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   V�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � W   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   W�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   W�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    W�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        W�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        W�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       W�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    W�Argo profile    3.1 1.2 19500101000000  20221026130229  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_234                 6810_008521_234                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��-V@��-V11  @��[�7@��[�7@2`�N�@2`�N��d�E���d�E��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@5@}p�@�G�@�  @޸R@�p�A\)A\)A*�HA>�RA`  A�  A�  A���A���A�Q�A�  A�Q�A�  B   B(�BQ�B(�B   B(  B0  B7�
B?�
BH  BO�
BW�
B_�
Bg�Bp  Bx  B�
B��
B��B��
B��
B��B��B�(�B�(�B�  B�{B�{B�  B�  B�  B��B��B�{B�  B�  B�  B�{B�{B�{B�{B��B�  B�  B��B�{B��B��C 
=C{C{C  C��C	��C��C  C
=C  C
=C��C  C  C  C{C�C!�C#��C&  C'��C*
=C,{C.  C0  C1��C4
=C5��C7��C:  C<
=C=��C?��CB  CD  CF  CH  CJ
=CL  CN  CO��CR  CT  CU��CW�CZ  C[��C]��C`
=Cb
=Cd  Cf
=Cg��Cj  Cl
=Cn
=Cp  Cr  Ct  Cv{Cx
=Cz  C{��C~  C�C�\C�
=C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C���C���C���C�  C�  C�C�C�  C���C���C�  C�  C���C�C�C�  C�C�C�  C���C�  C�  C���C���C�  C�  C���C�  C�C�  C�  C���C���C���C�  C�C�
=C�C���C�  C�  C�  C�  C�  C�  C���C�  C�C���C�  C���C�  C���C���C���C�C�C�  C�C�
=C�C�
=C�  C���C���C�  C���C���C���C�
=C�
=C�  C�C�  C�  C�C���C���C�C�
=C�C�  C���C�  C�C�  C���C���C���C�C�  C�  C�  C���C���C���C���C���C�  C�C�C�  C�  C�  C�  C���C���C�  D   D z�D �RD}qD  D� DD�DD}qD�qD� D�D��D�qD}qD�qD� D	�D	��D
  D
}qD  D}qD�qD� D�D� D  D� D  D��D  D}qD�D��D  D}qD  D� D�qD� D�qDz�D  D� D  D� D�D� D�qDz�D�RD}qD  D� D  D� D�qD}qD�D� D�qD� D   D ��D!  D!� D"�D"� D#  D#��D$�D$� D%�D%��D&  D&� D'�D'� D(�D(�D)�D)�D*�D*��D+  D+� D,  D,� D-�D-��D.  D.� D/�D/��D0  D0z�D0�qD1}qD2�D2� D2�qD3}qD3��D4}qD5  D5� D6  D6� D7  D7� D7�qD8��D9�D9}qD:  D:��D;�D;��D<  D<� D=�D=��D>  D>� D>�qD?� D@D@� DA  DA� DA�qDB� DC  DC� DD  DD}qDE  DE�DF�DF}qDF�qDG}qDH�DH�DI�DI��DJDJ��DK  DK� DK�qDL� DM�DM� DM�qDN� DO�DO� DP  DP��DQ  DQ� DR  DR� DS  DS� DTDT�DU  DU}qDU�qDV� DW  DW}qDX  DX��DY  DY}qDY�qDZ}qDZ�qD[}qD\  D\��D]�D]�D^D^}qD^�qD_}qD_�qD`��Da  Da� Db  Db}qDb��Dc}qDd  Dd� De�De��Df�Df��Dg  Dg� Dh  Dh� Di  Di}qDi�qDjz�Dj�qDk� Dl�Dl��Dl�qDm}qDm��Dn}qDn�qDoz�Do�qDp}qDq  Dq��Dq�qDr}qDs  Ds��Dt  Dt� Du�Du}qDv  Dv�Dw�Dw}qDw�qDx��Dy  Dy}qDz  Dz��D{�D{� D{�qD|� D|�qD}z�D~�D~��D  D� D�  D�@ D�~�D�� D�  D�>�D�}qD���D�HD�>�D�� D��HD���D�@ D�� D���D�  D�AHD�~�D�� D�HD�AHD��HD���D���D�AHD��HD�� D�  D�AHD��HD�� D��D�5�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�>���?L��?���?���?�ff?��H@(�@+�@@  @c�
@n{@�ff@�{@�p�@���@���@�  @���@�z�@�  @���@�
=@��RA�A�A  AffA=qA   A%A(��A0  A3�
A9��A?\)AC33AJ=qAN�RAS33AY��A]p�Adz�Ah��An{Atz�Ax��A\)A��A�z�A��A���A�z�A�
=A���A��
A�
=A���A�(�A��RA�G�A�z�A�ffA�G�A���A�ffA���A�z�A�ffA�G�A��
A�{A�  AÅA�p�A�\)Aʏ\A�(�A�
=A���A��HA�Aأ�A�=qA���A�\)A���A�(�A�{A�  A��HA�(�A�
=A�G�A�33A�{A�Q�A�=qA��A�\)B z�B�B�HB�B�B�B
=B(�B��B
�\B�Bz�B{B�HB(�BB�\B  Bp�B=qB�B��BB
=Bz�BG�B�\B�
B ��B"{B#�B$(�B%��B&�RB'�B(��B*{B*�HB,(�B-p�B.{B/\)B0��B1G�B2�HB3�B4z�B5�B6�RB7�B9�B9�B:�HB<(�B<��B>=qB?�B@Q�BAp�BB�RBC�BD��BF=qBG
=BH(�BIBJ�\BK�BM�BN{BO
=BPz�BQp�BR�HBT  BU�BV�HBW�
BX��BZ�\B[�
B\��B^ffB`  Ba�Bb=qBd  Be�BfffBh  Bip�BjffBl(�BmBn�RBpz�Bq��Bs
=Bt��Bu��Bw33Bxz�Byp�Bz�RB|Q�B}G�B~ffB�
B���B�
=B��B�ffB���B��B�=qB��RB�p�B�(�B���B�33B�  B�ffB��B��
B�ffB�
=B��
B�=qB���B��B�=qB���B���B�=qB��RB�\)B�(�B��RB�33B�  B���B���B��B�=qB���B�\)B�B�Q�B���B�\)B��
B�ffB���B�G�B��B�  B�z�B��RB�33B��B��
B�=qB��RB���B�p�B��B�  B���B���B�33B�B��B�ffB��HB��B��B�  B�=qB���B��B�\)B��B�Q�B��\B��B�p�B�B�ffB��\B�
=B���B��
B�=qB���B�
=B��B�{B�Q�B��HB�\)B��B�(�B�Q�B��HB�\)B���B�(�B���B��HB��B��B�=qB��RB�33B��B�{B��\B��RB�\)B���B�  B���B���B�G�B�B�  B��\B�
=B�G�B��B�(�B�z�B���B�\)B��
B�  B�z�B���B�33B�B�(�B�z�B���B�p�B���B�{B���B��HB�G�B��
B�{B�z�B�
=B�\)BÙ�B�(�Bď\B��HB��BŮB�  B�=qBƸRB�
=B�G�BǮB�(�B�ffBȸRB�33BɮB��
B�=qB���B�
=B�G�B��
B�Q�B�z�B���B�\)BͅB�  BΏ\B��HB�
=B�p�B�  B�=qBЏ\B��BхBѮB�  Bҏ\B���B��BӮB�(�B�Q�BԸRB�G�Bՙ�B��B�Q�B��HB�
=BׅB�{B�=qBأ�B�33B�\)B�B�=qBڣ�B��HB�G�B��
B�(�B�ffB��HB�\)Bݙ�B�  B�z�B�
=B�\)B߮B�=qB�RB�
=B�\)B�B�Q�B�RB���B�B�  B�z�B���B��B�B�(�B�ffB���B�\)B��
B�{B�z�B�
=B�p�B�B�=qB�RB��B�p�B�B�Q�B�RB�
=B�B�{B�Q�B��B�
=B�B�  B�=qB���B�G�B�B��
B�ffB���B���B�B�  B�Q�B�\B�
=B�p�B��
B�{B�z�B���B�\)B��B�  B�z�B��HB��B��B�  B�z�B��RB�
=B���B��B�(�B��RB�33B�p�B��
B�ffB���B�
=B���C   C (�C ffC �C �
C  CQ�Cp�C��C�C{C=qC�CC�HC�CffC�\CC{C33Cp�C�RC�
C{CffC�C�RC  C=qCp�C��C�HC�CG�Cz�CC  C(�CQ�C��C�
C	  C	(�C	p�C	�RC	�
C
{C
\)C
z�C
C
��C�CQ�C��C�
C  C=qC�C��C��C(�C\)C��C�HC
=CG�C��C��C��CG�C�C�RC�C(�C\)C�C��C�CQ�C��C�HC
=CG�C��C��C��C33C�CC�C(�Cz�CC�C(�Cp�C�RC�HC{CffC�RC��C(�C\)C��C�C(�C\)C��C��C{C\)C��C�
C
=CG�Cz�CC  C=qC�C�RC��C(�CffC��C�
C�Cp�C�C��C33Cp�C�RC�C�C\)C��C�
C {C \)C ��C �HC!(�C!ffC!��C!�HC"
=C"Q�C"��C"�C#33C#z�C#�C#�C$33C$�C$��C%{C%G�C%z�C%�RC&
=C&Q�C&��C&�HC'{C'G�C'�C'C(
=C(G�C(�C(��C)�C)\)C)��C)�HC*{C*Q�C*�C*��C+{C+\)C+��C+�HC,(�C,p�C,��C,�HC-{C-Q�C-��C-�HC.(�C.ffC.�C.�C/33C/p�C/�C/��C0(�C0ffC0��C0�HC1�C1ffC1��C1�HC2�C2ffC2�C2��C3G�C3�C3C4
=C4G�C4�C4C5
=C5Q�C5�\C5��C6
=C6G�C6�C6��C7
=C7Q�C7�\C7��C8{C8\)C8��C8�HC9(�C9p�C9�RC:
=C:Q�C:�\C:�
C;�C;\)C;�C;��C<33C<ffC<��C<�C=(�C=p�C=�RC>  C>G�C>��C>�
C?�C?p�C?�RC@  C@=qC@�C@CA  CAG�CA�CA��CB{CB\)CB��CB�HCC{CC\)CC��CC�HCD(�CDffCD�CD�CE33CEp�CE�CE��CF=qCF�CF��CG{CG\)CG��CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  ?��H@5@}p�@�G�@�  @޸R@�p�A\)A\)A*�HA>�RA`  A�  A�  A���A���A�Q�A�  A�Q�A�  B   B(�BQ�B(�B   B(  B0  B7�
B?�
BH  BO�
BW�
B_�
Bg�Bp  Bx  B�
B��
B��B��
B��
B��B��B�(�B�(�B�  B�{B�{B�  B�  B�  B��B��B�{B�  B�  B�  B�{B�{B�{B�{B��B�  B�  B��B�{B��B��C 
=C{C{C  C��C	��C��C  C
=C  C
=C��C  C  C  C{C�C!�C#��C&  C'��C*
=C,{C.  C0  C1��C4
=C5��C7��C:  C<
=C=��C?��CB  CD  CF  CH  CJ
=CL  CN  CO��CR  CT  CU��CW�CZ  C[��C]��C`
=Cb
=Cd  Cf
=Cg��Cj  Cl
=Cn
=Cp  Cr  Ct  Cv{Cx
=Cz  C{��C~  C�C�\C�
=C�  C�  C�  C�  C�  C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C���C���C���C�  C�  C�C�C�  C���C���C�  C�  C���C�C�C�  C�C�C�  C���C�  C�  C���C���C�  C�  C���C�  C�C�  C�  C���C���C���C�  C�C�
=C�C���C�  C�  C�  C�  C�  C�  C���C�  C�C���C�  C���C�  C���C���C���C�C�C�  C�C�
=C�C�
=C�  C���C���C�  C���C���C���C�
=C�
=C�  C�C�  C�  C�C���C���C�C�
=C�C�  C���C�  C�C�  C���C���C���C�C�  C�  C�  C���C���C���C���C���C�  C�C�C�  C�  C�  C�  C���C���C�  D   D z�D �RD}qD  D� DD�DD}qD�qD� D�D��D�qD}qD�qD� D	�D	��D
  D
}qD  D}qD�qD� D�D� D  D� D  D��D  D}qD�D��D  D}qD  D� D�qD� D�qDz�D  D� D  D� D�D� D�qDz�D�RD}qD  D� D  D� D�qD}qD�D� D�qD� D   D ��D!  D!� D"�D"� D#  D#��D$�D$� D%�D%��D&  D&� D'�D'� D(�D(�D)�D)�D*�D*��D+  D+� D,  D,� D-�D-��D.  D.� D/�D/��D0  D0z�D0�qD1}qD2�D2� D2�qD3}qD3��D4}qD5  D5� D6  D6� D7  D7� D7�qD8��D9�D9}qD:  D:��D;�D;��D<  D<� D=�D=��D>  D>� D>�qD?� D@D@� DA  DA� DA�qDB� DC  DC� DD  DD}qDE  DE�DF�DF}qDF�qDG}qDH�DH�DI�DI��DJDJ��DK  DK� DK�qDL� DM�DM� DM�qDN� DO�DO� DP  DP��DQ  DQ� DR  DR� DS  DS� DTDT�DU  DU}qDU�qDV� DW  DW}qDX  DX��DY  DY}qDY�qDZ}qDZ�qD[}qD\  D\��D]�D]�D^D^}qD^�qD_}qD_�qD`��Da  Da� Db  Db}qDb��Dc}qDd  Dd� De�De��Df�Df��Dg  Dg� Dh  Dh� Di  Di}qDi�qDjz�Dj�qDk� Dl�Dl��Dl�qDm}qDm��Dn}qDn�qDoz�Do�qDp}qDq  Dq��Dq�qDr}qDs  Ds��Dt  Dt� Du�Du}qDv  Dv�Dw�Dw}qDw�qDx��Dy  Dy}qDz  Dz��D{�D{� D{�qD|� D|�qD}z�D~�D~��D  D� D�  D�@ D�~�D�� D�  D�>�D�}qD���D�HD�>�D�� D��HD���D�@ D�� D���D�  D�AHD�~�D�� D�HD�AHD��HD���D���D�AHD��HD�� D�  D�AHD��HD�� D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�>���?L��?���?���?�ff?��H@(�@+�@@  @c�
@n{@�ff@�{@�p�@���@���@�  @���@�z�@�  @���@�
=@��RA�A�A  AffA=qA   A%A(��A0  A3�
A9��A?\)AC33AJ=qAN�RAS33AY��A]p�Adz�Ah��An{Atz�Ax��A\)A��A�z�A��A���A�z�A�
=A���A��
A�
=A���A�(�A��RA�G�A�z�A�ffA�G�A���A�ffA���A�z�A�ffA�G�A��
A�{A�  AÅA�p�A�\)Aʏ\A�(�A�
=A���A��HA�Aأ�A�=qA���A�\)A���A�(�A�{A�  A��HA�(�A�
=A�G�A�33A�{A�Q�A�=qA��A�\)B z�B�B�HB�B�B�B
=B(�B��B
�\B�Bz�B{B�HB(�BB�\B  Bp�B=qB�B��BB
=Bz�BG�B�\B�
B ��B"{B#�B$(�B%��B&�RB'�B(��B*{B*�HB,(�B-p�B.{B/\)B0��B1G�B2�HB3�B4z�B5�B6�RB7�B9�B9�B:�HB<(�B<��B>=qB?�B@Q�BAp�BB�RBC�BD��BF=qBG
=BH(�BIBJ�\BK�BM�BN{BO
=BPz�BQp�BR�HBT  BU�BV�HBW�
BX��BZ�\B[�
B\��B^ffB`  Ba�Bb=qBd  Be�BfffBh  Bip�BjffBl(�BmBn�RBpz�Bq��Bs
=Bt��Bu��Bw33Bxz�Byp�Bz�RB|Q�B}G�B~ffB�
B���B�
=B��B�ffB���B��B�=qB��RB�p�B�(�B���B�33B�  B�ffB��B��
B�ffB�
=B��
B�=qB���B��B�=qB���B���B�=qB��RB�\)B�(�B��RB�33B�  B���B���B��B�=qB���B�\)B�B�Q�B���B�\)B��
B�ffB���B�G�B��B�  B�z�B��RB�33B��B��
B�=qB��RB���B�p�B��B�  B���B���B�33B�B��B�ffB��HB��B��B�  B�=qB���B��B�\)B��B�Q�B��\B��B�p�B�B�ffB��\B�
=B���B��
B�=qB���B�
=B��B�{B�Q�B��HB�\)B��B�(�B�Q�B��HB�\)B���B�(�B���B��HB��B��B�=qB��RB�33B��B�{B��\B��RB�\)B���B�  B���B���B�G�B�B�  B��\B�
=B�G�B��B�(�B�z�B���B�\)B��
B�  B�z�B���B�33B�B�(�B�z�B���B�p�B���B�{B���B��HB�G�B��
B�{B�z�B�
=B�\)BÙ�B�(�Bď\B��HB��BŮB�  B�=qBƸRB�
=B�G�BǮB�(�B�ffBȸRB�33BɮB��
B�=qB���B�
=B�G�B��
B�Q�B�z�B���B�\)BͅB�  BΏ\B��HB�
=B�p�B�  B�=qBЏ\B��BхBѮB�  Bҏ\B���B��BӮB�(�B�Q�BԸRB�G�Bՙ�B��B�Q�B��HB�
=BׅB�{B�=qBأ�B�33B�\)B�B�=qBڣ�B��HB�G�B��
B�(�B�ffB��HB�\)Bݙ�B�  B�z�B�
=B�\)B߮B�=qB�RB�
=B�\)B�B�Q�B�RB���B�B�  B�z�B���B��B�B�(�B�ffB���B�\)B��
B�{B�z�B�
=B�p�B�B�=qB�RB��B�p�B�B�Q�B�RB�
=B�B�{B�Q�B��B�
=B�B�  B�=qB���B�G�B�B��
B�ffB���B���B�B�  B�Q�B�\B�
=B�p�B��
B�{B�z�B���B�\)B��B�  B�z�B��HB��B��B�  B�z�B��RB�
=B���B��B�(�B��RB�33B�p�B��
B�ffB���B�
=B���C   C (�C ffC �C �
C  CQ�Cp�C��C�C{C=qC�CC�HC�CffC�\CC{C33Cp�C�RC�
C{CffC�C�RC  C=qCp�C��C�HC�CG�Cz�CC  C(�CQ�C��C�
C	  C	(�C	p�C	�RC	�
C
{C
\)C
z�C
C
��C�CQ�C��C�
C  C=qC�C��C��C(�C\)C��C�HC
=CG�C��C��C��CG�C�C�RC�C(�C\)C�C��C�CQ�C��C�HC
=CG�C��C��C��C33C�CC�C(�Cz�CC�C(�Cp�C�RC�HC{CffC�RC��C(�C\)C��C�C(�C\)C��C��C{C\)C��C�
C
=CG�Cz�CC  C=qC�C�RC��C(�CffC��C�
C�Cp�C�C��C33Cp�C�RC�C�C\)C��C�
C {C \)C ��C �HC!(�C!ffC!��C!�HC"
=C"Q�C"��C"�C#33C#z�C#�C#�C$33C$�C$��C%{C%G�C%z�C%�RC&
=C&Q�C&��C&�HC'{C'G�C'�C'C(
=C(G�C(�C(��C)�C)\)C)��C)�HC*{C*Q�C*�C*��C+{C+\)C+��C+�HC,(�C,p�C,��C,�HC-{C-Q�C-��C-�HC.(�C.ffC.�C.�C/33C/p�C/�C/��C0(�C0ffC0��C0�HC1�C1ffC1��C1�HC2�C2ffC2�C2��C3G�C3�C3C4
=C4G�C4�C4C5
=C5Q�C5�\C5��C6
=C6G�C6�C6��C7
=C7Q�C7�\C7��C8{C8\)C8��C8�HC9(�C9p�C9�RC:
=C:Q�C:�\C:�
C;�C;\)C;�C;��C<33C<ffC<��C<�C=(�C=p�C=�RC>  C>G�C>��C>�
C?�C?p�C?�RC@  C@=qC@�C@CA  CAG�CA�CA��CB{CB\)CB��CB�HCC{CC\)CC��CC�HCD(�CDffCD�CD�CE33CEp�CE�CE��CF=qCF�CF��CG{CG\)CG��CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�9XA�=qA�?}A�C�A�E�A�G�A�Q�A�S�A�S�A�XA�XA�ZA�XA�\)A�ZA�ZA�ZA�\)A�\)A�\)A�^5A�\)A�\)A�`BA�`BA�dZA�ffA�dZA�`BA�`BA�`BA�dZA�jA�p�A�p�A�t�A�t�A�v�A�x�A�r�A�ZA��Aܝ�A��A�^5A��;A�ĜA�dZA�%A�$�A�t�A�^5A�K�A�bA�G�A�bA��
A˼jA�/A�+A���A�
=A���A���AŲ-Ağ�A�JA�1'A�JA��A�M�A���A�1A���A�&�A�G�A�VA�ĜA��A���A�Q�A�O�A�VA�ffA�ZA�A�\)A�ȴA�E�A��9A�ȴA��#A���A���A���A�ĜA�C�A��7A�M�A�1A���A��`A���A�5?A�/A���A��^A��A�1A�A�;dA�$�A��!A���A�{A��A�JA�z�A�jA%A};dAz�AxE�Av��At1'As%Ao?}Ah�RAg"�AfjAe�Ac�^A_��A^�A[dZAX��AU��AQ�AP1'AL�AK`BAIAHz�AFȴAE��A@�A>I�A<~�A;hsA;oA:~�A6�A4�A29XA1O�A/�mA.�A-S�A+�-A*�yA*1A)�A'�#A&��A%��A$��A#oA!�
A��Az�Ax�AbA�A�AO�A��AȴA33A�HA��A\)A�PA�;A%AM�A%A
-A	�;A	"�A�A	/A��A�+A	��A	dZA^5A=qAƨAVA�yA�A�AA��AA+AdZAXAZA(�A�A��A�uAhsA/A�A�A5?A|�A Ĝ@���@�5?@�{@�V@�1@���@�9X@�%@��@�j@�7L@��`@�Z@�M�@��@�9X@�$�@��@�I�@��H@�-@��m@�~�@� �@�J@�&�@�I�@�w@��@�
=@�^@���@��@噚@�p�@�I�@���@�z�@�hs@���@�33@��@�b@�(�@��;@�p�@���@���@���@�33@��H@��@�hs@ԓu@ҸR@��@�ȴ@�~�@�A�@�(�@� �@�
=@Η�@Ο�@�V@���@�G�@̛�@�z�@�Q�@�(�@˾w@�S�@�ȴ@��H@�v�@�X@�V@�C�@��T@ź^@��`@�9X@å�@��y@�M�@��T@�&�@�r�@� �@�  @�1@��m@�l�@�"�@��\@�5?@��@��h@�O�@�?}@���@��@��@���@�~�@�=q@��T@�`B@�Q�@��m@��w@��y@���@�@��#@��#@���@��@��D@�9X@��@�l�@��+@�=q@�{@���@��@��@���@�hs@�`B@�O�@��`@�1'@��w@�"�@�ȴ@�v�@�@��h@�%@���@�bN@��@��P@�o@���@�n�@�$�@�{@��#@��-@�&�@�r�@�1'@��w@�K�@��H@��@���@���@��+@�5?@��T@���@��^@���@�X@���@���@�Q�@���@���@�E�@��#@��-@�G�@��/@��@�j@��u@�z�@��w@�^5@�J@��T@���@��7@�O�@�%@���@��/@��D@�A�@�1@�\)@���@���@�^5@�J@�@���@�`B@��@��j@��@��@��H@�~�@�$�@��@���@�@���@���@��h@�p�@�?}@��@���@��/@���@�r�@�bN@�A�@��
@��@�dZ@�C�@�o@��R@�ff@�E�@�E�@�E�@�-@��-@��@�`B@�/@��@��u@��@�r�@�z�@��@�  @��m@�ƨ@���@�^5@��@���@�`B@�7L@���@���@���@��@���@��j@���@���@��u@�r�@�b@���@��@���@�\)@��P@��@�|�@�l�@�S�@�K�@�S�@�dZ@�o@���@�v�@�x�@�V@��u@�bN@�A�@�1@���@��@���@���@�|�@�S�@�33@�@���@�$�@�J@�5?@�{@��7@���@�b@�b@��F@���@�;d@�o@�o@�
=@��!@�~�@�{@��^@��h@�x�@�?}@�`B@�p�@�V@���@���@��@�(�@�@;d@~�y@~V@}@}�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�A�A�C�A�7LA�33A�?}A�;dA�C�A�=qA�?}A�G�A�A�A�E�A�A�A�I�A�C�A�E�A�K�A�G�A�G�A�O�A�VA�XA�O�A�VA�VA�K�A�XA�VA�ZA�ZA�VA�\)A�XA�ZA�ZA�VA�^5A�XA�ZA�\)A�XA�ZA�S�A�VA�^5A�XA�^5A�ZA�ZA�^5A�XA�ZA�\)A�VA�ZA�ZA�VA�^5A�\)A�ZA�\)A�XA�ZA�^5A�XA�\)A�\)A�XA�\)A�ZA�\)A�\)A�`BA�\)A�\)A�`BA�XA�^5A�\)A�ZA�\)A�^5A�XA�\)A�^5A�\)A�`BA�^5A�^5A�bNA�\)A�`BA�ZA�ZA�^5A�^5A�ZA�`BA�^5A�ZA�`BA�^5A�ZA�`BA�ZA�^5A�^5A�ZA�`BA�\)A�ZA�`BA�^5A�`BA�dZA�bNA�dZA�ffA�\)A�`BA�\)A�^5A�bNA�ffA�`BA�bNA�ffA�`BA�ffA�hsA�ffA�jA�jA�ffA�hsA�jA�`BA�ffA�hsA�bNA�ffA�bNA�\)A�bNA�^5A�\)A�bNA�^5A�ZA�`BA�^5A�ffA�ffA�`BA�dZA�dZA�^5A�`BA�`BA�^5A�^5A�bNA�\)A�`BA�dZA�bNA�`BA�ffA�dZA�bNA�hsA�`BA�ffA�bNA�`BA�n�A�l�A�l�A�r�A�n�A�l�A�p�A�t�A�p�A�n�A�t�A�n�A�n�A�r�A�r�A�p�A�t�A�v�A�p�A�v�A�t�A�t�A�v�A�r�A�v�A�x�A�r�A�t�A�x�A�v�A�r�A�v�A�z�A�t�A�v�A�x�A�t�A�v�A�z�A�v�A�x�A�z�A�r�A�r�A�r�A�n�A�p�A�l�A�^5A�\)A�^5A�XA�VA�S�A�?}A�-A�%A���AݼjAݮAݕ�A�/A�ȴAܗ�A�M�A�%A��mA��;Aۧ�A�|�A�1'Aڧ�A�=qA�oAٺ^Aٟ�AكA�z�A�`BA�\)A�ZA�A�A�A�A�;dA�/A�-A�$�A�oA�
=A���A��TA���A���Aذ!A؛�A�jA�Q�A�O�A�?}A�(�A�  Aװ!A�~�A�;dA���A��yA���A�r�A�7LA�  Aէ�A�ffA���AԃA��A���Aӝ�A�n�A�M�A�&�A�oA��A�AҰ!AғuAҋDA�t�A�jA�dZA�S�A�;dA���A��#Aѩ�Aч+A�|�A�x�A�z�A�x�A�z�A�t�A�n�A�jA�l�A�^5A�dZA�^5A�`BA�bNA�^5A�^5A�\)A�S�A�VA�ZA�M�A�I�A�M�A�M�A�I�A�K�A�M�A�G�A�I�A�/A��A��A��A�oA�{A�oA�1A���A��`A�ƨA�~�A�t�A�dZA�Q�A�C�A�33A��A�VA�bA�JA�%A�
=A�JA�
=A�JA��A�(�A�5?A�1'A�bAϥ�AσA�C�A�9XA��A��A�{A�%AΑhA�oA͛�A�\)A���A�~�A�VA�(�A��A˝�A�-A���Aʙ�A�^5A�;dA���A���AɓuA�=qA�JA���A�ȴAȴ9AȅA�S�A�=qA�1'A�(�A�(�A�+A�$�A�&�A�(�A�&�A�$�A�$�A��A�{A�%A���A��A��TA��/A���A�ffA�t�A�ffA�=qA��A���A���A��A��`A��#A��/A��;A��/A��A��A���A���A���A�ȴA���A�ȴA�A�A�ĜA�ĜA���AžwAžwAżjAŶFAŴ9AŸRAŶFAŲ-AŰ!AŲ-AŲ-AŬAŮAŧ�A�t�A�33A��AĴ9Aď\A�t�A�-A�A��A��
A���A�A�S�A�VA��TA���A���A´9A°!A§�A�A+A�n�A�K�A�(�A��A��A�oA��A��/A�A��uA�O�A�Q�A�7LA��A��`A���A��^A���A�dZA�7LA���A�n�A�\)A�G�A��TA��hA��A��+A��;A��^A���A��A�^5A�K�A�7LA� �A�  A��HA���A�7LA���A�VA��jA�-A�ƨA��DA�ffA�C�A�5?A� �A�VA�A���A��A��`A��HA���A�l�A�bA��A��/A�ȴA��9A���A���A��+A�z�A�l�A�I�A�(�A�oA�A��A��A���A��RA��A���A�x�A�\)A�&�A�A���A�hsA��hA���A�v�A�%A���A�bNA�S�A�/A��A���A��HA���A�ƨA��jA��A���A�x�A�bNA�XA�XA�Q�A�=qA�%A�ƨA���A��A�t�A�jA�Q�A�7LA�VA��PA�9XA��A�
=A���A�ƨA�S�A��A�A��^A���A��7A�z�A�ZA�O�A�?}A�7LA�/A�&�A�$�A�"�A��A��A�{A�JA���A���A��#A�ƨA�ȴA��A��hA�jA�G�A���A���A�jA�(�A��/A���A�
=A��/A��hA�=qA��A��HA���A���A�ȴA���A��A�Q�A�bA��`A�ƨA��A�/A��A���A�M�A��\A�33A��TA��wA��A�^5A��yA���A��PA�ZA�7LA�-A�&�A��A�JA�A���A��yA���A��A��7A�n�A�dZA�M�A� �A���A��A��^A��\A��A�l�A�&�A���A��A��yA���A��wA��RA��9A��!A��A���A���A���A���A��+A�hsA�\)A�=qA���A�A���A��A�v�A�hsA�^5A�I�A��A��/A���A�~�A�XA�G�A�9XA�1'A�1A��-A�v�A�jA�`BA�=qA�%A��
A�l�A�7LA��A��hA�^5A�I�A�5?A�{A���A�ffA��A�ĜA���A�jA�ffA�S�A�K�A�E�A�9XA�/A�$�A��A��A�{A�%A���A��A��HA�ƨA��FA���A��DA��+A��A�|�A�hsA�O�A�C�A���A��9A�l�A�33A�
=A��`A���A��-A��hA�jA�S�A�;dA�&�A���A�dZA�$�A�A��#A��9A��A�;dA��;A��!A�VA��-A�K�A��A��A��7A��9A�t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�9XA�=qA�?}A�C�A�E�A�G�A�Q�A�S�A�S�A�XA�XA�ZA�XA�\)A�ZA�ZA�ZA�\)A�\)A�\)A�^5A�\)A�\)A�`BA�`BA�dZA�ffA�dZA�`BA�`BA�`BA�dZA�jA�p�A�p�A�t�A�t�A�v�A�x�A�r�A�ZA��Aܝ�A��A�^5A��;A�ĜA�dZA�%A�$�A�t�A�^5A�K�A�bA�G�A�bA��
A˼jA�/A�+A���A�
=A���A���AŲ-Ağ�A�JA�1'A�JA��A�M�A���A�1A���A�&�A�G�A�VA�ĜA��A���A�Q�A�O�A�VA�ffA�ZA�A�\)A�ȴA�E�A��9A�ȴA��#A���A���A���A�ĜA�C�A��7A�M�A�1A���A��`A���A�5?A�/A���A��^A��A�1A�A�;dA�$�A��!A���A�{A��A�JA�z�A�jA%A};dAz�AxE�Av��At1'As%Ao?}Ah�RAg"�AfjAe�Ac�^A_��A^�A[dZAX��AU��AQ�AP1'AL�AK`BAIAHz�AFȴAE��A@�A>I�A<~�A;hsA;oA:~�A6�A4�A29XA1O�A/�mA.�A-S�A+�-A*�yA*1A)�A'�#A&��A%��A$��A#oA!�
A��Az�Ax�AbA�A�AO�A��AȴA33A�HA��A\)A�PA�;A%AM�A%A
-A	�;A	"�A�A	/A��A�+A	��A	dZA^5A=qAƨAVA�yA�A�AA��AA+AdZAXAZA(�A�A��A�uAhsA/A�A�A5?A|�A Ĝ@���@�5?@�{@�V@�1@���@�9X@�%@��@�j@�7L@��`@�Z@�M�@��@�9X@�$�@��@�I�@��H@�-@��m@�~�@� �@�J@�&�@�I�@�w@��@�
=@�^@���@��@噚@�p�@�I�@���@�z�@�hs@���@�33@��@�b@�(�@��;@�p�@���@���@���@�33@��H@��@�hs@ԓu@ҸR@��@�ȴ@�~�@�A�@�(�@� �@�
=@Η�@Ο�@�V@���@�G�@̛�@�z�@�Q�@�(�@˾w@�S�@�ȴ@��H@�v�@�X@�V@�C�@��T@ź^@��`@�9X@å�@��y@�M�@��T@�&�@�r�@� �@�  @�1@��m@�l�@�"�@��\@�5?@��@��h@�O�@�?}@���@��@��@���@�~�@�=q@��T@�`B@�Q�@��m@��w@��y@���@�@��#@��#@���@��@��D@�9X@��@�l�@��+@�=q@�{@���@��@��@���@�hs@�`B@�O�@��`@�1'@��w@�"�@�ȴ@�v�@�@��h@�%@���@�bN@��@��P@�o@���@�n�@�$�@�{@��#@��-@�&�@�r�@�1'@��w@�K�@��H@��@���@���@��+@�5?@��T@���@��^@���@�X@���@���@�Q�@���@���@�E�@��#@��-@�G�@��/@��@�j@��u@�z�@��w@�^5@�J@��T@���@��7@�O�@�%@���@��/@��D@�A�@�1@�\)@���@���@�^5@�J@�@���@�`B@��@��j@��@��@��H@�~�@�$�@��@���@�@���@���@��h@�p�@�?}@��@���@��/@���@�r�@�bN@�A�@��
@��@�dZ@�C�@�o@��R@�ff@�E�@�E�@�E�@�-@��-@��@�`B@�/@��@��u@��@�r�@�z�@��@�  @��m@�ƨ@���@�^5@��@���@�`B@�7L@���@���@���@��@���@��j@���@���@��u@�r�@�b@���@��@���@�\)@��P@��@�|�@�l�@�S�@�K�@�S�@�dZ@�o@���@�v�@�x�@�V@��u@�bN@�A�@�1@���@��@���@���@�|�@�S�@�33@�@���@�$�@�J@�5?@�{@��7@���@�b@�b@��F@���@�;d@�o@�o@�
=@��!@�~�@�{@��^@��h@�x�@�?}@�`B@�p�@�V@���@���@��@�(�@�@;d@~�y@~V@}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�A�A�C�A�7LA�33A�?}A�;dA�C�A�=qA�?}A�G�A�A�A�E�A�A�A�I�A�C�A�E�A�K�A�G�A�G�A�O�A�VA�XA�O�A�VA�VA�K�A�XA�VA�ZA�ZA�VA�\)A�XA�ZA�ZA�VA�^5A�XA�ZA�\)A�XA�ZA�S�A�VA�^5A�XA�^5A�ZA�ZA�^5A�XA�ZA�\)A�VA�ZA�ZA�VA�^5A�\)A�ZA�\)A�XA�ZA�^5A�XA�\)A�\)A�XA�\)A�ZA�\)A�\)A�`BA�\)A�\)A�`BA�XA�^5A�\)A�ZA�\)A�^5A�XA�\)A�^5A�\)A�`BA�^5A�^5A�bNA�\)A�`BA�ZA�ZA�^5A�^5A�ZA�`BA�^5A�ZA�`BA�^5A�ZA�`BA�ZA�^5A�^5A�ZA�`BA�\)A�ZA�`BA�^5A�`BA�dZA�bNA�dZA�ffA�\)A�`BA�\)A�^5A�bNA�ffA�`BA�bNA�ffA�`BA�ffA�hsA�ffA�jA�jA�ffA�hsA�jA�`BA�ffA�hsA�bNA�ffA�bNA�\)A�bNA�^5A�\)A�bNA�^5A�ZA�`BA�^5A�ffA�ffA�`BA�dZA�dZA�^5A�`BA�`BA�^5A�^5A�bNA�\)A�`BA�dZA�bNA�`BA�ffA�dZA�bNA�hsA�`BA�ffA�bNA�`BA�n�A�l�A�l�A�r�A�n�A�l�A�p�A�t�A�p�A�n�A�t�A�n�A�n�A�r�A�r�A�p�A�t�A�v�A�p�A�v�A�t�A�t�A�v�A�r�A�v�A�x�A�r�A�t�A�x�A�v�A�r�A�v�A�z�A�t�A�v�A�x�A�t�A�v�A�z�A�v�A�x�A�z�A�r�A�r�A�r�A�n�A�p�A�l�A�^5A�\)A�^5A�XA�VA�S�A�?}A�-A�%A���AݼjAݮAݕ�A�/A�ȴAܗ�A�M�A�%A��mA��;Aۧ�A�|�A�1'Aڧ�A�=qA�oAٺ^Aٟ�AكA�z�A�`BA�\)A�ZA�A�A�A�A�;dA�/A�-A�$�A�oA�
=A���A��TA���A���Aذ!A؛�A�jA�Q�A�O�A�?}A�(�A�  Aװ!A�~�A�;dA���A��yA���A�r�A�7LA�  Aէ�A�ffA���AԃA��A���Aӝ�A�n�A�M�A�&�A�oA��A�AҰ!AғuAҋDA�t�A�jA�dZA�S�A�;dA���A��#Aѩ�Aч+A�|�A�x�A�z�A�x�A�z�A�t�A�n�A�jA�l�A�^5A�dZA�^5A�`BA�bNA�^5A�^5A�\)A�S�A�VA�ZA�M�A�I�A�M�A�M�A�I�A�K�A�M�A�G�A�I�A�/A��A��A��A�oA�{A�oA�1A���A��`A�ƨA�~�A�t�A�dZA�Q�A�C�A�33A��A�VA�bA�JA�%A�
=A�JA�
=A�JA��A�(�A�5?A�1'A�bAϥ�AσA�C�A�9XA��A��A�{A�%AΑhA�oA͛�A�\)A���A�~�A�VA�(�A��A˝�A�-A���Aʙ�A�^5A�;dA���A���AɓuA�=qA�JA���A�ȴAȴ9AȅA�S�A�=qA�1'A�(�A�(�A�+A�$�A�&�A�(�A�&�A�$�A�$�A��A�{A�%A���A��A��TA��/A���A�ffA�t�A�ffA�=qA��A���A���A��A��`A��#A��/A��;A��/A��A��A���A���A���A�ȴA���A�ȴA�A�A�ĜA�ĜA���AžwAžwAżjAŶFAŴ9AŸRAŶFAŲ-AŰ!AŲ-AŲ-AŬAŮAŧ�A�t�A�33A��AĴ9Aď\A�t�A�-A�A��A��
A���A�A�S�A�VA��TA���A���A´9A°!A§�A�A+A�n�A�K�A�(�A��A��A�oA��A��/A�A��uA�O�A�Q�A�7LA��A��`A���A��^A���A�dZA�7LA���A�n�A�\)A�G�A��TA��hA��A��+A��;A��^A���A��A�^5A�K�A�7LA� �A�  A��HA���A�7LA���A�VA��jA�-A�ƨA��DA�ffA�C�A�5?A� �A�VA�A���A��A��`A��HA���A�l�A�bA��A��/A�ȴA��9A���A���A��+A�z�A�l�A�I�A�(�A�oA�A��A��A���A��RA��A���A�x�A�\)A�&�A�A���A�hsA��hA���A�v�A�%A���A�bNA�S�A�/A��A���A��HA���A�ƨA��jA��A���A�x�A�bNA�XA�XA�Q�A�=qA�%A�ƨA���A��A�t�A�jA�Q�A�7LA�VA��PA�9XA��A�
=A���A�ƨA�S�A��A�A��^A���A��7A�z�A�ZA�O�A�?}A�7LA�/A�&�A�$�A�"�A��A��A�{A�JA���A���A��#A�ƨA�ȴA��A��hA�jA�G�A���A���A�jA�(�A��/A���A�
=A��/A��hA�=qA��A��HA���A���A�ȴA���A��A�Q�A�bA��`A�ƨA��A�/A��A���A�M�A��\A�33A��TA��wA��A�^5A��yA���A��PA�ZA�7LA�-A�&�A��A�JA�A���A��yA���A��A��7A�n�A�dZA�M�A� �A���A��A��^A��\A��A�l�A�&�A���A��A��yA���A��wA��RA��9A��!A��A���A���A���A���A��+A�hsA�\)A�=qA���A�A���A��A�v�A�hsA�^5A�I�A��A��/A���A�~�A�XA�G�A�9XA�1'A�1A��-A�v�A�jA�`BA�=qA�%A��
A�l�A�7LA��A��hA�^5A�I�A�5?A�{A���A�ffA��A�ĜA���A�jA�ffA�S�A�K�A�E�A�9XA�/A�$�A��A��A�{A�%A���A��A��HA�ƨA��FA���A��DA��+A��A�|�A�hsA�O�A�C�A���A��9A�l�A�33A�
=A��`A���A��-A��hA�jA�S�A�;dA�&�A���A�dZA�$�A�A��#A��9A��A�;dA��;A��!A�VA��-A�K�A��A��A��7A��9A�t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
	B�B	7B	B	7B	7B1B	B�B�B	7B�B	B�B	B	B	B	B	B	B	B	7B	7B	B	B	B	7B	7B	7B	7B	lB	B	B	7B	7B�B�BfB�B_B
rB'�B�@B�[B��B��B�(BGB��B�B�QB�DB�sB�"B�2B��B�B�B�B�B&B;�B>BBAUBC�BNpBQ�BR�BW�B_pBS�B_BT,BT,BL�BD�B?HB0�B-�B+kB%�BOB7BMB�B�B��B��B��B�vB�BŢB��B��B�LB��B�:B��B�lB��BcBw�Bb�BV�B?}BxB
rB
�"B
��B
�jB
�B
��B
�xB
�bB
�B
zDB
f�B
]�B
TaB
C�B
6�B
)�B
�B
	�B	�	B	�QB	�fB	��B	��B	�nB	�@B	�-B	�B	{�B	qB	VmB	N�B	8RB	1�B	%B	�B	�B	.B	SB	YB	�B��B�ZB��B�B�MB��B�BݘB�B�mB� B�}B�0B�zB��B��B�B�XB�B��B�LB�:B�-B��B�YB��B�uB��B��B��B��B��B�	B��B��B�kB��B�B�=B��B��B�CB��B��B��B��B��B�fB�B�%B��B�>B��B��B��B	.B	%zB	(�B	+�B	5B	?B	L�B	Q�B	QB	O�B	L0B	K�B	F?B	H�B	LdB	J�B	K�B	HB	F�B	D�B	A�B	LdB	V�B	^jB	aB	c B	o B	o5B	o B	z�B	}�B	{JB	xB	s�B	v+B	wfB	sMB	rB	poB	m�B	l"B	h�B	h�B	f�B	cTB	bNB	`�B	_�B	cTB	d�B	d�B	e�B	e�B	e,B	e,B	YB	XyB	T�B	K�B	J#B	[WB	^5B	a�B	jB	aB	h>B	j�B	i�B	h
B	jB	r�B	qvB	o5B	kQB	i�B	s�B	x�B	|�B	{�B	}VB	��B	�GB	��B	��B	��B	�(B	�4B	��B	��B	��B	��B	��B	�SB	��B	��B	�B	��B	��B	��B	��B	�uB	�YB	�B	��B	��B	�@B	�B	�$B	��B	�eB	�6B	��B	��B	��B	�nB	��B	��B	��B	��B	�jB	�B	�wB	�[B	��B	�3B	�tB	ȀB	ȴB	��B	��B	�B	�XB	�B	�2B	�?B	��B	�B	�BB	�B	ߤB	�B	�pB	�B	ޞB	�B	�B	�B	�B	�fB	�B	��B	�8B	�sB	�KB	�B	��B	�B	�GB	�GB	�MB	�B	�B	�ZB	��B	��B	��B	��B	�2B	�B	�	B	��B	�DB	��B	��B	�B	�2B	��B	�fB	��B	��B	�JB	��B	�VB	��B	�]B	��B	��B	��B
  B	�.B
  B
  B	��B
  B
 �B
 �B
�B
�B
�B
+B
	B

�B

�B
�B
fB
	B
	B
	B
	�B

rB

�B
"B
.B
bB
�B
�B
(B
�B
\B
(B
�B
\B
�B
�B
bB
�B
�B
bB
�B
�B
 B
 B
4B
4B
4B
4B
hB
:B
B
:B
�B
@B
B
@B
@B
{B
�B
�B
�B
�B
�B
�B
�B
kB
=B
B
xB
CB
�B
xB
�B
�B
!B
 �B
!-B
!�B
"�B
#�B
$B
"hB
 \B
 �B
!-B
!�B
!�B
"4B
"4B
!�B
!�B
!�B
"hB
"�B
"hB
"�B
#�B
$@B
#�B
$B
$@B
&LB
(�B
+B
+kB
+�B
,qB
,qB
,�B
-wB
.�B
/OB
/OB
,�B
,B
*�B
*�B
,B
,�B
-�B
.B
.�B
.�B
/OB
/OB
/B
/�B
.�B
.B
-�B
/�B
0!B
/�B
-�B
-wB
/B
/�B
0�B
1[B
2aB
2�B
3hB
3hB
2�B
3�B
3�B
5�B
5�B
5�B
6�B
9XB
8�B
8�B
8RB
8�B
9$B
9�B
;�B
=<B
=B
<�B
=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�2B
=B	7B	lB�B�B	B�B	B	�B�B
�B	7B
	BfB	7B
=B�B
	B	lB	7B	lB�BB	BfB
=B�B�B�B�B	�B�B	lB�B	7B	7B�B	7B�B�B	�B	lBfB�B_B	�B1B	B	B�B	�B	7B	B
	B�B�B	�B_B	7B	lBfB	�B	B�B	�B	B1B
=B1B�BfB	7B1B	�B	lB�B
=BfB�B	�B	B1B
=B�B�B	7B�B	7B	�B�B
=B�B	lB
=BfB	B
�B1B�B
=B�BfB
	B1B
=B	BfB	�B1B	�B
=B�B	lB	lB�B
�B	B�B
=B	lB�B
rBfB_B	�B�B	lB	�B�B�B	lB1B	B	lB	7B�BB	B�B
�B	B	�B
rB�B	lB
	B�B	lB
�BfB	lB_B�B	�B�BfB	�B	lB	B
=B	�BfB
=B	�B�B	B	�BfB�B	�B�B
=B�B	�B
	BYB	lB	lB_B	lB
=B	�B�B	lB	�B�B	�B
	BfB�B
	B�B�B
	B�B	B	7B1B
	B�B�B	�B	�B�BfB	lB1B+B	B1B�B�B�B�B1B�BYB	B1BYB�B�B�B
=B
�B	�BB
�B
=B�B�B 'B4B8�B=<BD�Bi�B��B��B�B��B�dB��B�EB��B͟B�WB��B�B�B��B��B�DB�WB�B��B�B�B��B��B�"B�)B�B�B�TB�%B��B�>B��B��B�.B�lB�+B��B�BoB%B 4B�B  B�lB�B
=BB�B1B;BYB�B��B��B�8B�B��B�WB�B�B�B��B�B� B�DB�DB�JB��BMB��B�"B�B��B��B�/B��B�B��B�B�B�B�B��B�
B�KB�yB�B�KB��B�sB�B�B�B�B�B�B�mB�yB��B�fB�>B�sB�/B�B�B�B��B�B�B�WB��B�B��B�DB�B�2B��B�fB�lB�B��B��B��B�>B��B�`B�xB�B��B��BGB�B	�B�B�BB�B 'B7B�B�B+B�B�B@B)�B�B�B�B~BB	lB��B��B�B�B��B��B�fB�B 4BBJB�BBkB�BeB	B	B�B�B	BkB=B�B=B�BOB!bB!�BB!�B �B"hB?BFB7�B8�B;�B=B;dB:�B<jB>BB=B<B=B>BB=�B=�B>�B>�B@�B?B?}BA�BA�B@B@OBA�BA�BA�BA�BC�BC�BB�BB�BD�BEBC�BB�BD�BB�BC�BL0BQ�BW?BK^BOBBM�BS&BMjBJ�BL�BI�BOvB]dBU�BQBPBQ�BQ�BPBOBBOBBU2BV9BUgBVmBRTBP�BQ�BV�BP�BY�B[#BU2BS�BUgBT�BW
BU�BW
BU2BS�BY�BrBj�BQNBRTBYKBZ�B\)Bg8B^�BS&BR�BU�BQ�BR�BR BS[BP�BS�BYB^�B_pBb�Be�Bf�Bd�BXBZBVmBS�BR�BU�BT�BTaBS�BR�BQ�BS�Bi�B[�BR�BP�BP�BR BP}BNBM�BN�BNBOBO�BL�BK�BK�BJXBGEBFBGEBC-BDgBD�BE�B;0BF�BF�BYBOB@�BA�B>B5tB33B4�B2-B4nB1�B0�B.�B/OB/�B.IB33B/B-�B*�B*eB-B2aB3�B+6B+�B*eB(XB)�B)*B/B7B-�B$�B#�B#B($B1�B,qB"4B�B#:B�B�B$�BB�B=BB�B�B�B�B1BB7B�BYBB�B:B�B�B�BVB	B�B\B�BB
�B�B�DB �B�xB��B�>B��B� B��B��B��B��B��B��B�B�	B��B�)B�B�B�AB�&B�B�2B��B� B�/B҉B��BٴB�sB�BѷB�B�vB�pB��B�[B��BӏB�HB͟B��B�B��BɺB�#B� B�XB�XBΥBбB��B��B��BƨB��B�UB�'B�[B��B��B�B��B�BB� B��B�dB� B�B�<B��B�zB�zB��B�tB��B�^B��B�RB�B��B��B�OB�OB��B��B��B��B�qB�B�IB��B��B��B�'B��B��B��B��B�_B�"B�B�%B�hB�{B��B��B�	B�lB��B��B��B��B��B��B�B��B�B��B��B��B��B�iB��B{B{B{B{�BxBv�B~]BxlBu�Bp�Bk�Bh�Be�Bd�BcTBaB^B\�BY�BbNBj�BT,BP}BNpBM�BK�BMjBH�B=qBO�BB[B8B)�B)_B.�B-�B�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                           4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                           4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022102613022920221026130229IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022110509012320221105090123QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022110509012320221105090123QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                