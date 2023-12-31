CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-03-09T15:35:27Z creation; 2021-02-12T22:10:01Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  U�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  [�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  t`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  z�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Р   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ( �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 (  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � &�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ?P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ?�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   E�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   K�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T Q�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   R   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   R   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   R   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   R   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � R$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   R�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   R�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    R�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        R�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        R�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       R�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    S Argo profile    3.1 1.2 19500101000000  20200309153527  20210212221001  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               &   &AA  AOAO7824_008764_038                 7824_008764_038                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @��A5Tv@��A5Tv11  @��q�i�@��q�i�@7.�!�.I@7.�!�.I�d��`�G��d��`�G�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@@  @�G�@�  @�  @�  A ��A��A   A,(�A@  A_\)A�  A��A��A��A�  A�  A�  A�  B   B  B  B  B   B'�
B/�B7�
B@  BG�
BP(�BX  B_�
Bg�Bo�
Bx(�B�=qB�{B�  B�  B�  B�  B�  B�  B�  B�  B�  B��B�  B�  B��B�  B�  B��B�  B��B��B�  B�{B�{B�{B�{B�  B�{B�  B��B�  B�  C   C��C��C��C  C	��C��C��C  C  C��C  C
=C  C  C
=C   C"  C$  C&
=C(
=C*  C,
=C.  C/��C2  C4  C6
=C8  C:  C<
=C>
=C@  CB
=CD  CF  CG��CI��CL
=CN  CP  CR
=CT
=CV
=CX
=CY��C\
=C^{C`
=Ca��Cc�Ce��Ch
=Cj  Ck��Cm��Cp  Cr
=Ct
=Cv
=Cx{Cz
=C{��C~  C�C�C�  C�  C�  C�C�C�  C���C���C�C�
=C�
=C�  C���C���C�  C���C�  C�C���C���C���C���C���C���C�C�\C�
=C�
=C�C�C�C�C�  C���C���C�  C���C���C���C���C���C���C�  C�  C���C���C�  C�C�
=C�C�  C�  C�C�C�C�
=C�C���C�  C�  C�
=C�C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C�  C���C�  C�  C���C���C�C�C�  C���C�  C���C���C���C���C���C�  C�  C���C�  C�C�  C���C�C�
=C�C���C�  C�  C�  C�C���C�  C�  C�  C�  C�  C���C�  C�  C�C�C�  C���C���C�  C�  C���C�  C�  C���D }qD �qD� D  D}qD�qD� D�qD}qD  D� D�qD� D�D� D�qD}qD�qD	� D
  D
}qD
�qD}qD  D� D�D��D�D��D  D� D�D� D�D��D�D�D  D� D�qD� D�D��D  D� D  D� D�D� D  D��D�qD}qD�qD� D  D}qD�qD� D  D� DD� D   D � D!  D!}qD!�qD"}qD"�qD#� D$  D$z�D$�qD%}qD&  D&� D&�qD'}qD(  D(�D)  D)� D*�D*� D+  D+� D+�qD,}qD,�qD-}qD.  D.��D/  D/� D0  D0��D0�qD1}qD2  D2� D3  D3� D3�qD4��D5�D5� D5�qD6� D7  D7� D8  D8� D9  D9� D:  D:� D;�D;��D;�qD<}qD=  D=� D>�D>� D>��D?}qD@�D@��DA  DAz�DA�qDB� DC  DC}qDD�DD��DE  DE}qDF  DF� DG  DG� DG�qDH}qDH�qDI� DJ�DJ� DJ�qDK}qDL  DL}qDL�qDM� DN�DN� DN�qDO��DP�DP}qDQ  DQ��DR  DR� DS�DS� DT  DT� DU  DU}qDU�qDV}qDW  DW}qDX  DX� DX�qDYz�DY�qDZ}qD[  D[� D\�D\� D\�qD]}qD^�D^�D_�D_}qD_�qD`��Da!HDa^�Da�RDb�Dc
DcaHDc�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?B�\?�=q?�{?\?�@�@
=@+�@8Q�@G�@^�R@s33@�  @���@���@�(�@�ff@���@�
=@�  @���@У�@��H@��@�{@�Q�AG�A�A
=qA\)A�
AQ�Ap�A"�\A'�A,(�A1G�A5A:�HA?\)AC�
AG�AL��AQ�AU�AY��A^�RAc�
Ag
=Ak�Ap��Au�Ax��A}p�A���A��HA�z�A�ffA���A�=qA�(�A�A�  A�=qA�(�A�{A�  A��\A�(�A�ffA�Q�A��A��
A�{A�Q�A��A��
A�{A�  A��A�(�A�ffA���A��\A�z�A��RA���A�33A��A�
=Aə�A�33A��A�\)A�G�A�33A��A׮A�G�A�33A��A�\)A�G�A�33A�p�A�A�=qA�(�A�ffA��A�33A��A�\)A���A�(�A�{B (�Bp�B�\B�B�B=qB�B	�B
ffB�
BG�B�RB  Bp�B�RB(�B��B�HBQ�B��B�HBQ�B��B
=B z�B!��B"�HB$(�B%��B&�HB((�B)p�B*�RB,(�B-G�B.�\B0  B1p�B2�\B4  B5p�B6�RB8(�B9��B;33B<��B>{B?�B@��BBffBD  BEp�BF�HBHQ�BIBK
=BL��BM�BO�BP��BRffBS�
BU�BV�\BX  BYp�BZ�HB\Q�B]B_33B`z�Ba�Bc\)Bd��BfffBg�Bi�BjffBl  BmG�Bn�HBpQ�Bq��Bs
=BtQ�BuBw
=Bx��By�B{
=B|z�B}p�B~�\B�
B�Q�B���B�\)B��
B�=qB��\B�
=B�p�B��B�Q�B���B��B��B��B�=qB��RB�33B��B��B�=qB��RB��B�p�B��B�=qB��RB��B��B��B�Q�B���B��B��B��B�Q�B���B��B��B��B�Q�B��RB��B��B��B�Q�B��RB�33B��B��B�Q�B��RB��B�p�B�B�=qB���B��B�\)B�B�=qB���B���B�\)B�B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�{B�z�B���B�33B��B�(�B��\B���B�G�B��B�(�B�z�B��HB�\)B�B�(�B��\B��HB�\)B�B�{B�z�B���B�\)B�B�=qB���B�
=B�p�B��
B�=qB���B��B��B��
B�ffB��RB�33B���B�  B�z�B���B�\)B�B�=qB���B�
=B��B��B�ffB���B�G�B��B�(�B��\B��HB�\)B�B�(�B���B�
=B�p�B��B�Q�B��HB�G�B��B�(�B���B�
=B�p�B��B�Q�B���B�33B��B�(�B��\B���B�\)B��
B�=qB��RB�33B���B�{B\B���B�\)B�B�=qBĸRB��BŅB�  B�z�B��HB�G�B�B�=qBȣ�B��BɅB�  B�z�B���B�G�BˮB�(�B̸RB��BͅB�{B�ffB��HB�\)B�B�=qBиRB��BѮB�  B�z�B���B�p�B��
B�Q�B���B�33B�B�(�B֏\B��BׅB��B�ffB���B�G�B�B�(�Bڣ�B��BۅB�  B�ffB��HB�G�BݮB�(�B޸RB��B߅B�  B�z�B��HB�\)B�B�=qB�RB�33B�B�  B�z�B��HB�G�B�B�(�B��B���B�B��
B�ffB���B�G�B陚B�(�B��B���B�\)B��
B�=qB��B��B홚B�  B�z�B��HB�33B�B�{B�z�B���B�\)B��
B�=qB��B�
=B�p�B��B�Q�B���B�33B���B�  B��\B�
=B�p�B��
B�Q�B���B�G�B��B�(�B���B�
=B���B�  B�z�B���B�\)B�B�Q�B���B�33B��C 
=C G�C z�C �C �C(�CffC��C�HC�CQ�C�C��C  C=qCz�C�RC��C33Cp�C�C�C(�C\)C��C�
C{CQ�C��C�
C{CQ�C�\C��C
=CG�C�CC	  C	G�C	�C	C

=C
G�C
�C
C
=CG�C�\CC
=CG�C�C��C  CG�C�CC  C=qC�CC
=CQ�C�\C��C�CQ�C�\C�
C{C\)C��C�HC�CffC�C�C33Cp�C�C��C33Cz�CC
=CQ�C��C��C(�CffC��C�C=qCp�C�RC
=CG�C�\C�
C�C\)C��C�C(�Cp�CC  C=qC�C�
C
=C\)C��C�
C�CffC�C�C(�Cp�C�RC��C(�Cp�C�RC   C G�C �C ��C!{C!\)C!��C!�HC"(�C"ffC"�C"��C#=qC#z�C#C$  C$G�C$��C$�
C%{C%\)C%��C%�HC&�C&p�C&�RC&��C'33C'�C'C(
=C(G�C(�\C(�
C){C)\)C)�\C)�HC*(�C*p�C*�C*��C+33C+z�C+�RC,  C,=qC,z�C,C-  C-G�C-�\C-�
C.{C.\)C.��C.�HC/33C/ffC/�C/��C0=qC0�C0C1{C1Q�C1��C1�
C2�C2\)C2��C2�C3(�C3p�C3C4  C4G�C4��C4�
C5{C5\)C5�C5�HC633C6p�C6�RC6��C7=qC7�C7��C8
=C8\)C8��C8�HC9(�C9p�C9�RC:  C:G�C:�\C:��C;{C;ffC;��C;�C<(�C<z�C<�RC=  C=G�C=�C=�
C>{C>\)C>��C>�HC?33C?p�C?C@  C@=qC@�C@�
CA{CAffCA��CA�HCB33CBp�CB�RCB��CC=qCC�CC�RCD  CD=qCD�CDCE
=CEQ�CE�\CE�
CF�CF\)CF�CF�CG33CG�CG��CH
=CHQ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                     1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  ?��H@@  @�G�@�  @�  @�  A ��A��A   A,(�A@  A_\)A�  A��A��A��A�  A�  A�  A�  B   B  B  B  B   B'�
B/�B7�
B@  BG�
BP(�BX  B_�
Bg�Bo�
Bx(�B�=qB�{B�  B�  B�  B�  B�  B�  B�  B�  B�  B��B�  B�  B��B�  B�  B��B�  B��B��B�  B�{B�{B�{B�{B�  B�{B�  B��B�  B�  C   C��C��C��C  C	��C��C��C  C  C��C  C
=C  C  C
=C   C"  C$  C&
=C(
=C*  C,
=C.  C/��C2  C4  C6
=C8  C:  C<
=C>
=C@  CB
=CD  CF  CG��CI��CL
=CN  CP  CR
=CT
=CV
=CX
=CY��C\
=C^{C`
=Ca��Cc�Ce��Ch
=Cj  Ck��Cm��Cp  Cr
=Ct
=Cv
=Cx{Cz
=C{��C~  C�C�C�  C�  C�  C�C�C�  C���C���C�C�
=C�
=C�  C���C���C�  C���C�  C�C���C���C���C���C���C���C�C�\C�
=C�
=C�C�C�C�C�  C���C���C�  C���C���C���C���C���C���C�  C�  C���C���C�  C�C�
=C�C�  C�  C�C�C�C�
=C�C���C�  C�  C�
=C�C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C�  C���C�  C�  C���C���C�C�C�  C���C�  C���C���C���C���C���C�  C�  C���C�  C�C�  C���C�C�
=C�C���C�  C�  C�  C�C���C�  C�  C�  C�  C�  C���C�  C�  C�C�C�  C���C���C�  C�  C���C�  C�  C���D }qD �qD� D  D}qD�qD� D�qD}qD  D� D�qD� D�D� D�qD}qD�qD	� D
  D
}qD
�qD}qD  D� D�D��D�D��D  D� D�D� D�D��D�D�D  D� D�qD� D�D��D  D� D  D� D�D� D  D��D�qD}qD�qD� D  D}qD�qD� D  D� DD� D   D � D!  D!}qD!�qD"}qD"�qD#� D$  D$z�D$�qD%}qD&  D&� D&�qD'}qD(  D(�D)  D)� D*�D*� D+  D+� D+�qD,}qD,�qD-}qD.  D.��D/  D/� D0  D0��D0�qD1}qD2  D2� D3  D3� D3�qD4��D5�D5� D5�qD6� D7  D7� D8  D8� D9  D9� D:  D:� D;�D;��D;�qD<}qD=  D=� D>�D>� D>��D?}qD@�D@��DA  DAz�DA�qDB� DC  DC}qDD�DD��DE  DE}qDF  DF� DG  DG� DG�qDH}qDH�qDI� DJ�DJ� DJ�qDK}qDL  DL}qDL�qDM� DN�DN� DN�qDO��DP�DP}qDQ  DQ��DR  DR� DS�DS� DT  DT� DU  DU}qDU�qDV}qDW  DW}qDX  DX� DX�qDYz�DY�qDZ}qD[  D[� D\�D\� D\�qD]}qD^�D^�D_�D_}qD_�qD`��Da!HDa^�Da�RDb�Dc
DcaHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?B�\?�=q?�{?\?�@�@
=@+�@8Q�@G�@^�R@s33@�  @���@���@�(�@�ff@���@�
=@�  @���@У�@��H@��@�{@�Q�AG�A�A
=qA\)A�
AQ�Ap�A"�\A'�A,(�A1G�A5A:�HA?\)AC�
AG�AL��AQ�AU�AY��A^�RAc�
Ag
=Ak�Ap��Au�Ax��A}p�A���A��HA�z�A�ffA���A�=qA�(�A�A�  A�=qA�(�A�{A�  A��\A�(�A�ffA�Q�A��A��
A�{A�Q�A��A��
A�{A�  A��A�(�A�ffA���A��\A�z�A��RA���A�33A��A�
=Aə�A�33A��A�\)A�G�A�33A��A׮A�G�A�33A��A�\)A�G�A�33A�p�A�A�=qA�(�A�ffA��A�33A��A�\)A���A�(�A�{B (�Bp�B�\B�B�B=qB�B	�B
ffB�
BG�B�RB  Bp�B�RB(�B��B�HBQ�B��B�HBQ�B��B
=B z�B!��B"�HB$(�B%��B&�HB((�B)p�B*�RB,(�B-G�B.�\B0  B1p�B2�\B4  B5p�B6�RB8(�B9��B;33B<��B>{B?�B@��BBffBD  BEp�BF�HBHQ�BIBK
=BL��BM�BO�BP��BRffBS�
BU�BV�\BX  BYp�BZ�HB\Q�B]B_33B`z�Ba�Bc\)Bd��BfffBg�Bi�BjffBl  BmG�Bn�HBpQ�Bq��Bs
=BtQ�BuBw
=Bx��By�B{
=B|z�B}p�B~�\B�
B�Q�B���B�\)B��
B�=qB��\B�
=B�p�B��B�Q�B���B��B��B��B�=qB��RB�33B��B��B�=qB��RB��B�p�B��B�=qB��RB��B��B��B�Q�B���B��B��B��B�Q�B���B��B��B��B�Q�B��RB��B��B��B�Q�B��RB�33B��B��B�Q�B��RB��B�p�B�B�=qB���B��B�\)B�B�=qB���B���B�\)B�B�{B�z�B��HB�G�B��B�{B�z�B��HB�G�B��B�{B�z�B���B�33B��B�(�B��\B���B�G�B��B�(�B�z�B��HB�\)B�B�(�B��\B��HB�\)B�B�{B�z�B���B�\)B�B�=qB���B�
=B�p�B��
B�=qB���B��B��B��
B�ffB��RB�33B���B�  B�z�B���B�\)B�B�=qB���B�
=B��B��B�ffB���B�G�B��B�(�B��\B��HB�\)B�B�(�B���B�
=B�p�B��B�Q�B��HB�G�B��B�(�B���B�
=B�p�B��B�Q�B���B�33B��B�(�B��\B���B�\)B��
B�=qB��RB�33B���B�{B\B���B�\)B�B�=qBĸRB��BŅB�  B�z�B��HB�G�B�B�=qBȣ�B��BɅB�  B�z�B���B�G�BˮB�(�B̸RB��BͅB�{B�ffB��HB�\)B�B�=qBиRB��BѮB�  B�z�B���B�p�B��
B�Q�B���B�33B�B�(�B֏\B��BׅB��B�ffB���B�G�B�B�(�Bڣ�B��BۅB�  B�ffB��HB�G�BݮB�(�B޸RB��B߅B�  B�z�B��HB�\)B�B�=qB�RB�33B�B�  B�z�B��HB�G�B�B�(�B��B���B�B��
B�ffB���B�G�B陚B�(�B��B���B�\)B��
B�=qB��B��B홚B�  B�z�B��HB�33B�B�{B�z�B���B�\)B��
B�=qB��B�
=B�p�B��B�Q�B���B�33B���B�  B��\B�
=B�p�B��
B�Q�B���B�G�B��B�(�B���B�
=B���B�  B�z�B���B�\)B�B�Q�B���B�33B��C 
=C G�C z�C �C �C(�CffC��C�HC�CQ�C�C��C  C=qCz�C�RC��C33Cp�C�C�C(�C\)C��C�
C{CQ�C��C�
C{CQ�C�\C��C
=CG�C�CC	  C	G�C	�C	C

=C
G�C
�C
C
=CG�C�\CC
=CG�C�C��C  CG�C�CC  C=qC�CC
=CQ�C�\C��C�CQ�C�\C�
C{C\)C��C�HC�CffC�C�C33Cp�C�C��C33Cz�CC
=CQ�C��C��C(�CffC��C�C=qCp�C�RC
=CG�C�\C�
C�C\)C��C�C(�Cp�CC  C=qC�C�
C
=C\)C��C�
C�CffC�C�C(�Cp�C�RC��C(�Cp�C�RC   C G�C �C ��C!{C!\)C!��C!�HC"(�C"ffC"�C"��C#=qC#z�C#C$  C$G�C$��C$�
C%{C%\)C%��C%�HC&�C&p�C&�RC&��C'33C'�C'C(
=C(G�C(�\C(�
C){C)\)C)�\C)�HC*(�C*p�C*�C*��C+33C+z�C+�RC,  C,=qC,z�C,C-  C-G�C-�\C-�
C.{C.\)C.��C.�HC/33C/ffC/�C/��C0=qC0�C0C1{C1Q�C1��C1�
C2�C2\)C2��C2�C3(�C3p�C3C4  C4G�C4��C4�
C5{C5\)C5�C5�HC633C6p�C6�RC6��C7=qC7�C7��C8
=C8\)C8��C8�HC9(�C9p�C9�RC:  C:G�C:�\C:��C;{C;ffC;��C;�C<(�C<z�C<�RC=  C=G�C=�C=�
C>{C>\)C>��C>�HC?33C?p�C?C@  C@=qC@�C@�
CA{CAffCA��CA�HCB33CBp�CB�RCB��CC=qCC�CC�RCD  CD=qCD�CDCE
=CEQ�CE�\CE�
CF�CF\)CF�CF�CG33CG�CG��CH
=CHQ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                     1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��9A��9A��FA��FA��RA��9A��9A��FA��RA��FA��FA��FA��FA��!A��A��!A��-A��RA��9A��9A��jA�A�ĜA�ĜA�ƨA�ĜA��A�r�A���A�-A��A�ffA�ZA�Q�A�G�A�-A�"�A�{A�A��A��;A���A��FA��PA�33A��#A�;dA�%A��A�t�A��A���A��mA���A��A�\)A�7LA��A���A�hsA��A�VA�oA��PA��A���A��A���A�JA��A���A��HA��/A��A��DA�\)A�%A�v�A��mA�K�A�"�A�l�A�{A�  A��wA�=qA�1A��/A��-A�p�A�C�A��;A�(�A�E�A��HA�S�A�K�A�ȴA�A��A�A�A���A�9XA��hA�^5A�$�A���A��7A�VA�;dA�/A�ƨA�$�A�A���A��7A��`A�ZA�+A���A�Q�A��hA�{A}�A{�hAzZAv�HAsK�Ap��Al��Ahr�Af�yAfr�AeƨAd �Ac`BAbI�A_�
A_oA^�`A]33AZbAX�AU|�AT�yAT�\AS�mAR(�AO�AOXAO�AN�AN�\AN-AM�AL��AK�AKoAJ�HAJ��AJ�AJ�\AJ�DAJM�AJ�AI�mAIO�AH��AH~�AHVAG�AG`BAGO�AG&�AF��AF�RAE�PAD~�AB��A@��A?��A?
=A=��A<�RA;7LA;`BA:�`A:$�A8z�A6�A5�-A5oA3ƨA2A�A1�-A0bNA/+A.��A-�
A,�RA*�A*I�A)dZA(9XA't�A'C�A'�A&M�A#�A"ZA!33A33Ar�A"�A��Ar�Al�AĜAVA�TA�A/A�;A  AVA�9A�A
=A��A	�A�A=qA��A��A�PA|�Ax�AdZA33AĜAI�A�AA1AA�A7L@��m@�-@�&�@�1@�+@��@�J@�V@��;@�ȴ@���@�O�@�7L@�r�@�+@�O�@�G�@��;@��@��@�-@�O�@�?}@�7L@���@㝲@��@ޗ�@��@��T@�7L@�z�@�(�@ۥ�@�ff@؛�@׾w@ְ!@ԋD@�|�@ҏ\@���@�|�@��@̼j@�  @�ff@��T@�p�@ȴ9@�(�@��
@ǥ�@�v�@��@�Ĝ@���@ă@��7@��;@��\@�V@��#@�@���@��-@��@��@�%@��@�z�@�j@��@��u@��D@�ƨ@��
@�1@���@�S�@�{@��j@��@�S�@�V@�@�hs@���@���@�t�@��@�^5@��@�Q�@��@��;@��F@��@�;d@�
=@���@���@���@�V@�7L@�1'@�l�@�\)@�C�@�C�@���@�5?@��u@��u@��u@��u@�1'@�A�@��@���@��w@��@�"�@�ȴ@�$�@��#@�@��^@��-@���@�p�@��@���@�Ĝ@��j@�r�@�z�@���@���@��u@�z�@�bN@� �@��m@�;d@��@�^5@��T@��@�1'@�t�@���@�hs@��@��@��y@���@�1'@���@���@�\)@���@�V@��;@�7L@�hs@��@�n�@�M�@�E�@��@��@���@���@�K�@�^5@��h@���@��R@��\@�^5@���@��^@�@��@�j@{�@z=q@xĜ@t��@r^5@r�@r=q@r^5@o�@m��@m`B@m?}@mV@l�@lj@lj@lj@lj@lj@l9X@l1@k�m@k��@k��@k��@kC�@kS�@kS�@kS�@kdZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��9A��FA��FA��FA��FA��9A��FA��9A��9A��9A��9A��FA��FA��RA��RA��^A��^A��^A��RA��9A��-A��!A��-A��9A��!A��FA��^A��^A��RA��RA��RA��9A��FA��9A��9A��9A��FA��FA��FA��FA��RA��RA��^A��^A��^A��^A��RA��RA��RA��!A��A��!A��!A��9A��-A��9A��RA��A��!A���A���A���A���A���A��A��-A��-A��9A��-A��!A��A��A��A��A���A��A��A��!A��FA��RA��FA��9A��9A��FA��FA��FA��FA��RA��RA��RA��^A��^A��RA��FA��9A��-A��!A��-A��9A��9A��9A��9A��FA��FA��9A��9A��FA��^A��wA��jA��jA��jA��jA��wA�A���A���A�A�ƨA�ƨA�ȴA�A�A�A�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ȴA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�A��RA��jA��wA���A���A���A��DA�~�A�v�A�t�A�jA�`BA�ZA�E�A� �A��A���A��A�n�A�z�A�5?A�A��mA��jA���A�~�A�x�A�r�A�n�A�n�A�l�A�jA�hsA�bNA�^5A�ZA�ZA�ZA�XA�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�M�A�I�A�E�A�C�A�A�A�?}A�7LA�/A�(�A�$�A�"�A�"�A�"�A�$�A�$�A�"�A� �A��A��A��A�{A�oA�bA�VA�JA�JA�
=A�JA�JA�JA�
=A���A���A���A��A��A��A��A��A��A��A��A��A��A��mA��mA��yA��mA��TA��;A��#A��A��
A��
A���A���A���A���A���A���A���A���A���A���A�ȴA���A��wA��wA��^A��9A��-A��!A��!A��A��A��A���A���A���A��\A��+A��A��A��A�|�A�t�A�hsA�S�A�A�A�=qA�/A��A�bA�JA�A�  A���A���A��A��A��mA��A���A��^A���A�z�A�ZA�K�A�C�A�9XA�33A�/A�(�A�"�A��A�oA�bA�VA�
=A�
=A�1A�%A�A�A���A���A��A��A��mA��/A�ȴA�ĜA�A���A��9A��\A��7A�x�A�n�A�jA�ffA�bNA�\)A�G�A�"�A�JA��A��`A��HA��;A��#A��A��A���A���A���A��jA��9A���A�v�A�dZA�C�A�"�A�A��A��A��TA��/A��
A��
A���A���A��RA���A���A���A���A���A���A��hA��PA��7A��+A��A��A�~�A�z�A�v�A�p�A�n�A�hsA�bNA�`BA�^5A�ZA�VA�VA�VA�VA�O�A�M�A�E�A�7LA�1'A�+A�$�A� �A��A�bA�1A�A��A��yA��HA��#A��
A���A���A��jA��-A���A���A���A��hA��+A��A�z�A�t�A�n�A�jA�jA�hsA�VA�G�A�=qA�;dA�1'A��A��A���A�ƨA��FA���A��+A��A�t�A�\)A�Q�A�M�A�=qA�7LA�5?A�-A�"�A��A��A��A�VA�%A�A�  A���A��A��jA���A��7A�hsA�XA�I�A�=qA�5?A�/A�(�A�$�A�$�A��A�JA�A��A��`A��/A�ȴA��jA���A��\A��PA��7A��A��A��A��A��A�x�A�l�A�$�A��A��+A�\)A�"�A���A��FA�9XA��-A��7A�v�A�ZA� �A���A��uA�=qA��A��A��A��wA���A��DA�p�A�ZA�9XA�(�A��A�%A��mA�A���A��A�XA�;dA��A���A�M�A���A�p�A�I�A�?}A�1'A��A�JA��A���A�I�A�"�A�
=A���A��`A��/A��
A���A�ĜA���A��jA��wA��FA��RA��9A���A���A���A���A���A��\A��PA��PA��A��DA�x�A�n�A�n�A�dZA�ffA�dZA�XA�S�A�A�A�5?A�"�A�"�A�oA��A���A��A��TA��HA���A��9A���A��7A�Q�A�bA�JA���A���A���A��yA��/A��
A���A���A���A�ƨA��DA�G�A�%A��HA���A���A��hA�v�A�&�A��A��A�ĜA���A��uA��+A�n�A�n�A�ffA�`BA�A�A�&�A�"�A�VA�bA�JA�VA�JA�JA�JA�
=A�JA�A���A��yA��`A��/A��
A���A�ĜA�ĜA��A���A��uA��PA��A�\)A�9XA�-A�1A��mA�?}A�&�A�%A���A���A���A���A���A���A��A��A��HA���A���A�ĜA��wA��RA��-A��9A��A��-A���A���A���A��A�v�A�hsA�XA�ZA�XA�Q�A�I�A�C�A�?}A�A�A�A�A�=qA�=qA�=qA�7LA�&�A���A�ƨA��uA�z�A�VA�&�A���A��A�  A��uA�O�A�
=A��!A�z�A�5?A�&�A� �A��A�bA�
=A���A��A��;A��/A��#A���A��-A���A���A��hA��7A�33A��A�ĜA��\A�~�A�x�A�ffA�I�A�+A�VA��A��HA��HA��/A���A��RA���A��DA�p�A�E�A��
A���A��A�bNA�33A��A�{A�bA�
=A��A���A���A��PA��DA�~�A�n�A�\)A�E�A�  A�n�A��A�^5A��#A�v�A�5?A��;A�G�A�bA��A�XA�7LA��mA���A�ƨA���A��jA��9A��!A���A��uA��A�7LA�ĜA�C�A��wA�M�A�/A��A��A�Q�A��A�|�A�%A��yA���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                     1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��9A��9A��FA��FA��RA��9A��9A��FA��RA��FA��FA��FA��FA��!A��A��!A��-A��RA��9A��9A��jA�A�ĜA�ĜA�ƨA�ĜA��A�r�A���A�-A��A�ffA�ZA�Q�A�G�A�-A�"�A�{A�A��A��;A���A��FA��PA�33A��#A�;dA�%A��A�t�A��A���A��mA���A��A�\)A�7LA��A���A�hsA��A�VA�oA��PA��A���A��A���A�JA��A���A��HA��/A��A��DA�\)A�%A�v�A��mA�K�A�"�A�l�A�{A�  A��wA�=qA�1A��/A��-A�p�A�C�A��;A�(�A�E�A��HA�S�A�K�A�ȴA�A��A�A�A���A�9XA��hA�^5A�$�A���A��7A�VA�;dA�/A�ƨA�$�A�A���A��7A��`A�ZA�+A���A�Q�A��hA�{A}�A{�hAzZAv�HAsK�Ap��Al��Ahr�Af�yAfr�AeƨAd �Ac`BAbI�A_�
A_oA^�`A]33AZbAX�AU|�AT�yAT�\AS�mAR(�AO�AOXAO�AN�AN�\AN-AM�AL��AK�AKoAJ�HAJ��AJ�AJ�\AJ�DAJM�AJ�AI�mAIO�AH��AH~�AHVAG�AG`BAGO�AG&�AF��AF�RAE�PAD~�AB��A@��A?��A?
=A=��A<�RA;7LA;`BA:�`A:$�A8z�A6�A5�-A5oA3ƨA2A�A1�-A0bNA/+A.��A-�
A,�RA*�A*I�A)dZA(9XA't�A'C�A'�A&M�A#�A"ZA!33A33Ar�A"�A��Ar�Al�AĜAVA�TA�A/A�;A  AVA�9A�A
=A��A	�A�A=qA��A��A�PA|�Ax�AdZA33AĜAI�A�AA1AA�A7L@��m@�-@�&�@�1@�+@��@�J@�V@��;@�ȴ@���@�O�@�7L@�r�@�+@�O�@�G�@��;@��@��@�-@�O�@�?}@�7L@���@㝲@��@ޗ�@��@��T@�7L@�z�@�(�@ۥ�@�ff@؛�@׾w@ְ!@ԋD@�|�@ҏ\@���@�|�@��@̼j@�  @�ff@��T@�p�@ȴ9@�(�@��
@ǥ�@�v�@��@�Ĝ@���@ă@��7@��;@��\@�V@��#@�@���@��-@��@��@�%@��@�z�@�j@��@��u@��D@�ƨ@��
@�1@���@�S�@�{@��j@��@�S�@�V@�@�hs@���@���@�t�@��@�^5@��@�Q�@��@��;@��F@��@�;d@�
=@���@���@���@�V@�7L@�1'@�l�@�\)@�C�@�C�@���@�5?@��u@��u@��u@��u@�1'@�A�@��@���@��w@��@�"�@�ȴ@�$�@��#@�@��^@��-@���@�p�@��@���@�Ĝ@��j@�r�@�z�@���@���@��u@�z�@�bN@� �@��m@�;d@��@�^5@��T@��@�1'@�t�@���@�hs@��@��@��y@���@�1'@���@���@�\)@���@�V@��;@�7L@�hs@��@�n�@�M�@�E�@��@��@���@���@�K�@�^5@��h@���@��R@��\@�^5@���@��^@�@��@�j@{�@z=q@xĜ@t��@r^5@r�@r=q@r^5@o�@m��@m`B@m?}@mV@l�@lj@lj@lj@lj@lj@l9X@l1@k�m@k��@k��@k��@kC�@kS�@kS�@kS�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��9A��FA��FA��FA��FA��9A��FA��9A��9A��9A��9A��FA��FA��RA��RA��^A��^A��^A��RA��9A��-A��!A��-A��9A��!A��FA��^A��^A��RA��RA��RA��9A��FA��9A��9A��9A��FA��FA��FA��FA��RA��RA��^A��^A��^A��^A��RA��RA��RA��!A��A��!A��!A��9A��-A��9A��RA��A��!A���A���A���A���A���A��A��-A��-A��9A��-A��!A��A��A��A��A���A��A��A��!A��FA��RA��FA��9A��9A��FA��FA��FA��FA��RA��RA��RA��^A��^A��RA��FA��9A��-A��!A��-A��9A��9A��9A��9A��FA��FA��9A��9A��FA��^A��wA��jA��jA��jA��jA��wA�A���A���A�A�ƨA�ƨA�ȴA�A�A�A�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ȴA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�ĜA�A��RA��jA��wA���A���A���A��DA�~�A�v�A�t�A�jA�`BA�ZA�E�A� �A��A���A��A�n�A�z�A�5?A�A��mA��jA���A�~�A�x�A�r�A�n�A�n�A�l�A�jA�hsA�bNA�^5A�ZA�ZA�ZA�XA�S�A�S�A�S�A�S�A�S�A�Q�A�O�A�M�A�I�A�E�A�C�A�A�A�?}A�7LA�/A�(�A�$�A�"�A�"�A�"�A�$�A�$�A�"�A� �A��A��A��A�{A�oA�bA�VA�JA�JA�
=A�JA�JA�JA�
=A���A���A���A��A��A��A��A��A��A��A��A��A��A��mA��mA��yA��mA��TA��;A��#A��A��
A��
A���A���A���A���A���A���A���A���A���A���A�ȴA���A��wA��wA��^A��9A��-A��!A��!A��A��A��A���A���A���A��\A��+A��A��A��A�|�A�t�A�hsA�S�A�A�A�=qA�/A��A�bA�JA�A�  A���A���A��A��A��mA��A���A��^A���A�z�A�ZA�K�A�C�A�9XA�33A�/A�(�A�"�A��A�oA�bA�VA�
=A�
=A�1A�%A�A�A���A���A��A��A��mA��/A�ȴA�ĜA�A���A��9A��\A��7A�x�A�n�A�jA�ffA�bNA�\)A�G�A�"�A�JA��A��`A��HA��;A��#A��A��A���A���A���A��jA��9A���A�v�A�dZA�C�A�"�A�A��A��A��TA��/A��
A��
A���A���A��RA���A���A���A���A���A���A��hA��PA��7A��+A��A��A�~�A�z�A�v�A�p�A�n�A�hsA�bNA�`BA�^5A�ZA�VA�VA�VA�VA�O�A�M�A�E�A�7LA�1'A�+A�$�A� �A��A�bA�1A�A��A��yA��HA��#A��
A���A���A��jA��-A���A���A���A��hA��+A��A�z�A�t�A�n�A�jA�jA�hsA�VA�G�A�=qA�;dA�1'A��A��A���A�ƨA��FA���A��+A��A�t�A�\)A�Q�A�M�A�=qA�7LA�5?A�-A�"�A��A��A��A�VA�%A�A�  A���A��A��jA���A��7A�hsA�XA�I�A�=qA�5?A�/A�(�A�$�A�$�A��A�JA�A��A��`A��/A�ȴA��jA���A��\A��PA��7A��A��A��A��A��A�x�A�l�A�$�A��A��+A�\)A�"�A���A��FA�9XA��-A��7A�v�A�ZA� �A���A��uA�=qA��A��A��A��wA���A��DA�p�A�ZA�9XA�(�A��A�%A��mA�A���A��A�XA�;dA��A���A�M�A���A�p�A�I�A�?}A�1'A��A�JA��A���A�I�A�"�A�
=A���A��`A��/A��
A���A�ĜA���A��jA��wA��FA��RA��9A���A���A���A���A���A��\A��PA��PA��A��DA�x�A�n�A�n�A�dZA�ffA�dZA�XA�S�A�A�A�5?A�"�A�"�A�oA��A���A��A��TA��HA���A��9A���A��7A�Q�A�bA�JA���A���A���A��yA��/A��
A���A���A���A�ƨA��DA�G�A�%A��HA���A���A��hA�v�A�&�A��A��A�ĜA���A��uA��+A�n�A�n�A�ffA�`BA�A�A�&�A�"�A�VA�bA�JA�VA�JA�JA�JA�
=A�JA�A���A��yA��`A��/A��
A���A�ĜA�ĜA��A���A��uA��PA��A�\)A�9XA�-A�1A��mA�?}A�&�A�%A���A���A���A���A���A���A��A��A��HA���A���A�ĜA��wA��RA��-A��9A��A��-A���A���A���A��A�v�A�hsA�XA�ZA�XA�Q�A�I�A�C�A�?}A�A�A�A�A�=qA�=qA�=qA�7LA�&�A���A�ƨA��uA�z�A�VA�&�A���A��A�  A��uA�O�A�
=A��!A�z�A�5?A�&�A� �A��A�bA�
=A���A��A��;A��/A��#A���A��-A���A���A��hA��7A�33A��A�ĜA��\A�~�A�x�A�ffA�I�A�+A�VA��A��HA��HA��/A���A��RA���A��DA�p�A�E�A��
A���A��A�bNA�33A��A�{A�bA�
=A��A���A���A��PA��DA�~�A�n�A�\)A�E�A�  A�n�A��A�^5A��#A�v�A�5?A��;A�G�A�bA��A�XA�7LA��mA���A�ƨA���A��jA��9A��!A���A��uA��A�7LA�ĜA�C�A��wA�M�A�/A��A��A�Q�A��A�|�A�%A��yA���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                     1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B��B��B��B��B�bB��B��B��B�bB��B��B��B��B� B�4B� B� B��B� B� B��B��B��B�bB��B��B�uB�=B��B��B
�B�B�B\B�B@B�B@B{BBYB+B�B�B!bB&LB0!B0�B2�B7�B<BA�BIRBL�BNpBP}BYB]�B`�Bb�BhsBi�BiBl"Bk�BlWBm]Bp�BjBb�Ba�BV�BOBM6BK�BJ�BIRBIBC-BB�B;�B4nB0�B/�B-�B,qB$@B �BOB�BeB�B�B��B�ZB�B�8B�B�EB�B��B�!B��Bu%B`vB<jB+�B
	BuB
��B
��B
��B
��B
� B
�;B
�B
ɺB
��B
��B
�tB
�RB
�B
��B
~�B
o B
aB
PB
4�B
"4B
uB	�B	�B	�pB	یB	��B	�^B	�KB	�qB	�nB	��B	��B	�VB	�B	��B	~]B	|�B	y	B	y�B	h�B	e,B	`�B	`BB	^jB	\�B	Z�B	V�B	O�B	M�B	L0B	K)B	K�B	J#B	IRB	I�B	GEB	FtB	E9B	A�B	@�B	@�B	;0B	9�B	9$B	8�B	7B	4�B	2�B	+�B	&B	�B	4B	�B		lB	�B��B�>B��B�B�WB��BںB��B��BȴB�BĜB�jB��B��B��B�B�B�*B�B��B��B��B�OB�B��B�B�	B��B�%B��B��B� B}�B|�Bz�Bx�Bz�Bv+BwfBp;Bm�BrGBi�Bn�BlWBk�BkQBkQBj�BjBjBjKBjBi�BjKBiyBg�BjKBiDBk�Bk�Bm�Bl�Bl�BncBl�Bl�Bm�Bl�Bn/Bn�Bo Bm�Bl�Bm�BncBm�Br�BpBqvBqvBs�BqABqBp�BqBqBt�Br�BrBr|BsMBsMBrBq�BoiBo Bl�Bm]BkQBi�Bi�Bh�Bm]Bj�Bh�Bi�Bh�Bf�Bf�Be�Bd�Bd�Be`BhsBkBl�BiBqBrGBv�BxBwfBy�By	BzxBxlB{�B~]B}�B~�B�GB��B�uB��B�B�B�%B��B�fB��B�B�wB��B�B͟B�HB҉B��B��BیB�
B�B�DB��B�"B	 iB	 �B	B	�B	B	SB	�B	�B		7B	hB	�B	�B	CB	�B	B	B	%B	/�B	.B	-�B	-�B	0�B	/�B	0�B	1�B	33B	3�B	7�B	9�B	=�B	?HB	?�B	?�B	?�B	@B	A�B	C�B	D�B	E�B	GB	H�B	G�B	GEB	GzB	GEB	H�B	H�B	J�B	K�B	OB	PB	S�B	V9B	]�B	_pB	c�B	ncB	o5B	poB	pB	}�B	��B	��B	�B	��B	�@B	�B	�CB	�tB	�mB	�gB	�B	��B	��B	�jB	�3B	ƨB	�^B	��B	̘B	ѷB	��B	�KB	�B	�B	��B	�2B	��B	��B	�B	� B	��B	�>B	�B
+B
	�B
	�B
	7B
_B
�B
bB
�B
:B
�B
oB
B
�B
�B
B
�B
�B
@B
�B
FB
B
B
�B
{B
{B
{B
{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�.B�.B�.B��B��B�bB�.B��B�4B�4B��B� B��B�.B��B�\B��B��B��B��B��B�hB�:B��B��B�(B��B��B��B��B��B�4B�4B��B�4B�4B�.B��B��B��B��B��B��B��B��B��B��B�bB��B��B�B�hB�bB��B�4B��B��B�FB�\B��B�:B�:B�:B��B��B�.B��B��B�.B��B�4B��B��B��B��B��B� B��B��B��B��B�hB�4B��B�hB��B��B� B��B� B��B��B�bB�bB�4B�hB�B��B��B�4B��B� B��B� B� B�bB��B�VB��B� B��B�bB� B�hB�bB�oB��B��B�.B�\B��B��B�hB�4B��B��B��B�\B�\B�\B�(B��B��B��B�bB��B� B��B�bB�bB�bB��B��B��B�B��B��B�$B�MB�B�eB��B�qB�kB�	B��B�B��B��B�*B�$BרB�,B�TB�VB��B�cB�B	7BDBB�BJB�B~B~BPB�B(B(B�B(B�B(B�B�B�B�B�B�B�BbB BhB�B�B@B@B�BBBuB@BoB�B�B�BuB�B�B�B�B�BB�BFB{B�BuBB@BSB�B�B�B�B�BB�B�B�BMB�B�B$B$BSB�B$B�BYB$B�B+B�B�B1B�B�B�B�B�B+B_B�B�B�B+B�B�BB7BkBkB�B�BCB=B�B�BqB	B�B	B	BCB~B�B�B�B!�B#nB$B"hB#�B$B$@B$B$tB"�B#�B&LB'RB&�B*�B.�B1'B/�B/�B0UB0UB0�B1[B1'B1�B0�B0�B0UB0!B/�B/�B/�B/�B0UB0�B0�B1�B1[B1�B33B4nB2�B2-B2�B4B:^B5tB9$B7LB7�B7LB7LB7LB;0B<6B<�B=�B<B<6B;�B<�B<B<6B=�B<jB<�B@OB@�BCaBF�BFtBL�BL�BI�BHKBGzBH�BH�BI�BHBIBH�BM6BM�BMjBL�BL0BLdBMBMjBNpBOBOBOBBOBN�BN<BN<BNpBNpBOvBN�BO�BO�BP�BRTBQ�BQBR BU�BU�BX�BZ�BZ�BZ�B[#BZBZ�B[#B\]B]�B_�B^5B^jB]�B]dB]/B^�BaB_�Ba�Ba�B`�Ba|Ba|BaBbBcTBc Bb�BbBa�Be`Bd&Bc�Bc�Be�Bi�Bm]Bh�Bh
Bj�Bk�Bh
Bh
BjKBkQBi�BhsBj�BjBh�Bh�BjBjKBg�Bh>BjBjBh>Bg�Bh
BjBrBjBm�Bl�BlWBl�Bm)Bk�Bj�Bk�Bk�BjKBkBl�Bk�Bl�Bl�Bk�Bm)Bl"Bm�Bo Bj�BkQBk�Bi�BjBiyBh
Bh�BiBr�Bz�Bl�BpBn/Bm�Br�B��Bm�Bo�BjKBn/Bk�Bt�Bo5BkQBiDBjBf�Bf�Bd�Be,Bd&Bc�Ba�Ba�Bb�BaBb�BcTB`BB^�Bb�B]�Be�B`�Bh>BgBYBX�BT�BUgBW
BUgBWsBT,BT,BS[BPHBP}BOBNBNBQ�BK�BM�BM6BM6BNpBLdBNpBMBN<BI�BJ�BMBK^BJ�BK�BK�BI�BI�BM�BI�BK^BI�BJ�BJXBJ�BH�BL0BJ�BHKBK)BJ�BF�BGEBH�BGEBHBOBBGEBF�BU2BF?BDgBEmBC�BB�BEBE�B@OB@�B@�B?HB?�BJXBF?B?}B@OB?�B=�B;0B<BA�B@�B8�B7B5�B5?B4nB4B1[B3�B2�B6�B1�B0�B33B/�B0�B/OB0!B0�B/�B/�B.�B0�B0�B4nB.IB0!B.}B-�B.�B,qB,�B-CB+�B+kB*0B5?B)�B%�B+kB,BD�B(�B#:B"4B!-B �B �B �BVB"4B!B �B#:B�B \B�BOB �B~B�BB�B�B�B �B�BVB�B�B�BCBB1BeB�B�B+B�B�BB7B�B�B�B B�BBSB@B�B�B	�B��B	�BMB��B�8B�fB��B�8B��B��B�ZB��B�B�B��B�+B�B�GB�5B�B��B��B�B�B��B�B��B�B�B��B��B�|BݘB�;B�B��B�B�BںBݘB��B�
B�TB�TB�aB̘BȀB�B�B�<B�KBĜB��B�B��B�B�jB��B�-B��B�B�B��B�_B��B��B�0B��B�B�=B�uB�rBxB|�Bx�Bu�BxlBtBr|Bu�BsMBwfB~�Be`Bw�BYBU�BUgBP�BI�BN�BR�BK^B6zB2�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                     1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�B��B��B��B��B�'B��B�B�B�B�[B�uB��B�[B�AB�AB�[B�AB�B��B�B��B�UB��B��B��B��B�B��B�.B �BB�B�B�BBYB�B	B
=B0B(B�B=B"�B#�B&�B+�B/�B7B<6B>�B@�BB�BK�BP�BS�BW$B]IB\�B]�B`BB_�Ba�Bk6BmBb4B]dB]dBN"BA�B?}B>�B>wB>B=�B8�B:�B1AB'�B"�B#B"�B$�B�BBNB\B�B&BdB�AB�B��B�CBּB��B� B��B�iB}�BtTB_�B7fB*B
�<B
��B
�B
�GB
��B
�5B
��B
�
B
��B
��B
�B
��B
�B
��B
��B
��B
v�B
eFB
]�B
M6B
.�B
!�B
oB	�B	�B	өB	҉B	�XB	�;B	�oB	��B	�RB	��B	��B	�B	��B	yXB	q�B	qAB	qB	raB	\�B	W�B	TB	R�B	Q�B	Q4B	O�B	LB	CGB	@B	>wB	=VB	=�B	<B	<B	<B	9�B	:DB	8RB	5tB	3�B	5?B	-�B	+�B	+�B	+QB	*B	*�B	(XB	#B	VB	�B	mB	gB��B�JB�B��B�B�"B�B��B�(B�rB˒B�"B�VB��B��B�iB��B��B��B�HB�;B��B��B�uB�FB�1B�.B�B��B.B.B|�B{�BxlBtTBq�Bp�Bn�Br�Bq�Bn�Bl�Bd�BjBf�BaHBg�Ba�B_!B^�B^B\�B\�B\�B\�B\�B]~B^B]B]/B`Ba|Ba�Bb�Bc B`�B`�Ba�B_pB`BBa�BaBb4BbhBa�B`'B`vBbNBd@Bf�Bg�Be�Bg8BgmBfLBc�Bc:Bc�BeFBfLBl�Be�Bd�Be�Bf�Be�BeFBfBd�Bb�B`�Bc:B_VB]�B]dB_!Bc�B]�B\xB^�B[�BY�BZBX�BWsBWYBY�B\�B]�B^jB\xBhsBg�BkBj�BjeBl"BkQBlqBkBn�Bp�Bo�BqvButBu�BtnBt�BvzBz�BxBx�B{0B�OB��B�B��B��B��B��B�mBǔB�lB�TBޞB�mB��B�UB�B�B��B�B�B�LB��B�8B�XB�VB	SB	DB	�B	pB	�B	�B	�B	�B	!|B	 B	�B	 \B	"�B	"4B	# B	$&B	%�B	&LB	*0B	-B	0UB	1[B	1�B	1�B	1�B	2|B	4nB	5�B	6�B	7�B	9�B	:�B	9�B	9>B	9rB	9rB	:�B	;B	<�B	>�B	A�B	C-B	F�B	JXB	P�B	SB	YB	`�B	a�B	b�B	fB	r�B	{dB	��B	�1B	��B	�KB	�B	��B	�"B	��B	��G�O�G�O�B	��B	�oB	�2B	��B	��B	��B	�OB	�9B	ȚB	ΊB	��B	�{B	�
B	��B	ևB	��B	��B	�B	�0B	��B	��B	�JB	��B	�dB	�0B	��B
B
�B
�B
3B
�B
�B
�B
�B
�B
�B
�B
�B
9B
�B
B
B
%B
tB
YB
?B
?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�oB�;B��B��B�AB��B��B�B�uB�B��B�oB��B�;B�;B�;B�GB�AB��B��B�AB�GB��B�oB�;B�oB�;B�B��B��B�GB��B��B��B�B�oB�;B�B�B�B�B�oB�oB�B��B�B�GB�{B��B��B�B��B�;B�oB��B��B�B��B��B��B�B�B��B�B�;B��B�B��B�B�B�B�B�B�uB�AB� B�B�B��B��B�B��B�B�GB�uB�AB�uB�oB�oB��B��B��B��B�{B�B�B��B�AB�uB�oB�uB�uB��B�B�B�;B�uB�B��B�uB��B��B��B�B�oB��B��B�;B�GB��B��B�B�oB�;B��B��B��B��B�;B�B�oB��B�B�uB�AB��B��B��B�B�B�B��B�oB�;B��B��B��B��B�B��B��B�~B�PB��B�B�?B��B�B�BևBԯB�B��B�B�9B��B��B�qB�B��B�<B��B��B��B B �B �B �B �B B �B��B��B��B B B �B�B�B[B�B-B-B�B�BBmBmB�B�B�B-B�B�B�BB9BB3B�BgBB�B�BB�BgB�B�BBKBB�BBzB�B�B�B�BBEB�B�B�B�B�B�B�B�B	RB	�B	�B
#B
�B
#B	�B
XB	�B	RB	�B	�B	�B
XB	�B	�B
�B
�B^B�B�B�B�B�B�B�B6B6B�BdB0BdBdB�B�BNB�BB[B�BgB�B2BgB�BgB�B�B2B�B�BB)B B"�B!B!HB!�B!�B"B"�B"�B"�B!�B!�B!�B!|B!B!HB!B!B!�B"NB!�B"�B"�B#TB$�B%�B$ZB#�B$&B%`B+�B&�B*B(�B(�B(�B(�B(�B,�B-�B./B/ B-]B-�B,�B-�B-]B-�B/ B-�B-�B1�B1�B4�B88B7�B=�B=�B;B9�B8�B:B:B;B9rB:xB:DB>�B?.B>�B=�B=�B=�B>]B>�B?�B@iB@iB@�B@iB@4B?�B?�B?�B?�B@�B@4BABABBBC�BB�BBuBC{BF�BF�BJ	BK�BK�BLJBL~BKxBK�BL~BM�BN�BQ4BO�BO�BO(BN�BN�BP.BRoBQ4BSBSBRBR�BR�BRoBSuBT�BT{BTFBSuBSBV�BU�BT�BUMBWYB[	B^�BZ7BYeB\CB]IBYeBYeB[�B\�B[	BY�B\CB[�BZ7BZ7B[qB[�BY1BY�B[qB[qBY�BX�BYeB[�BcnB[�B_VB^OB]�B^B^�B]IB\CB\�B\�B[�B\xB^B]IB^B]�B\�B^�B]~B_VB`\B\CB\�B\�B[=B[�BZ�BYeBZ7BZkBdBlB^BabB_�B_!Bd@Bs3B_VB`�B[�B_�B]IBfLB`�B\�BZ�B[qBX+BX+BVBV�BU�BUMBS@BS@BTBRoBS�BT�BQ�BO�BS�BN�BV�BR:BY�BX_BJ�BJ	BE�BF�BHfBF�BH�BE�BE�BD�BA�BA�B@iB?cB?cBCB="B>�B>�B>�B?�B=�B?�B>]B?�B;JB<PB>]B<�B<B="B<�B;B;JB?.B:�B<�B:�B<B;�B<B:DB=�B;�B9�B<�B<B88B8�B9�B8�B9rB@�B8�B8BF�B7�B5�B6�B5%B4TB6`B6�B1�B2GB1�B0�B1B;�B7�B0�B1�B1AB/ B,�B-]B3MB1�B*B(sB'8B&�B%�B%`B"�B$�B$&B(
B# B"B$�B!B"B �B!|B!�B!B!HB B"B"B%�B�B!|B�B;B B�BB�B/B�B�B&�B#B?B�BdB6+B�B�B�B�B B BTB�B�B}B�B�BBB�B<B�B B�BHBpBB�B�B�B6B�B�B)B0B�BpB
�B
�B	�B�B	�B�B�BzB�BB)BB[B�B�qB�B�B�B�HB��B�CB��B��B��B�B��B�RB�B�B�B�B�RB��B�tB�B�B��B�B�B�B�B�HB�BچB�KB�yB�KB�BچB�KB�NB��B�BбB�vB�[B҉B˒B�0B�B�gBȀB��B��B��B�B��B��B��B��B��B�B�<B�zB�UB��B��B�CB��B�B��B��B�!B��B�SB�qB��B|B�{B��Bs�B{�BiyBncBjKBgBi�Be�Bc�Bg8Bd�Bh�Bp;BV�BiBJ�BGEBF�BB'B:�B@BC�B<�B'�B$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                     1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<PSi<:��<#�
<#�
<+_1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6V�<8��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�{�<d�#<#�
<cM�<d�!<#�
<S~�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<8!a<?�0<#�
<f�:<[IQ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OW V1.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0139(+/-0.003)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       NO correction for Conductivity Thermal Mass (CTM) is applied;    OW V1.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0139(+/-0.003)                                                                                                                       SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:8/4,y:4/2.0]; Fit T>2.1C; max_breaks=1;                                                                    PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:8/4,y:4/2.0]; Fit T>2.1C; max_breaks=1;                                                                    PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202102122209382021021222093820210212220938202102122209382021021222093820210212220938SI  SI  ARFMARFM                                                                                                                                                2020030915352720200309153527IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020031912003020200319120030QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020031912003020200319120030QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020060109060220200601090602IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20200601174619              CF      PSAL                            DK}qG�O�DL  G�O�?�  G�O�Density Inver.                  SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021021222095320210212220953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2021021222095320210212220953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021021222095320210212220953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                