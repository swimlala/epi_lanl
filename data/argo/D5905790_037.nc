CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-02-28T17:45:38Z creation; 2021-02-12T22:10:01Z DMQC;      
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
_FillValue        G�O�     H  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  TP   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     H  Z$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ql   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     H  w@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H  �$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H @   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 1�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   1�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   7�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   =�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T C�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   D<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   DD   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   DL   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   DT   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � D\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   D�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   D�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    E    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        E    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        E(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       E0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    E8Argo profile    3.1 1.2 19500101000000  20200228174538  20210212221001  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               %   %AA  AOAO7824_008764_037                 7824_008764_037                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�h�Q�@�h�Q�11  @�h����@�h����@74�zxl"@74�zxl"�d��W}��d��W}�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@B�\@�  @��R@�  @�  @��RA\)A   A,(�A@��A`��A�Q�A�  A��A�  A�  A�  A�  A�A�\)B�B�
B  B�
B'�
B0  B8  B@  BH(�BP  BW�
B`  Bh  Bp  BxQ�B�  B�  B�{B�(�B�(�B�{B�{B�  B�  B��B��B�  B�(�B�  B�  B�{B��B��B�{B�  B�  B��B��B�  B�{B�  B��B�{B�{B��B�  B�  C   C��C��C��C  C
  C  C
=C  C  C  C��C��C  C  C  C   C"  C#��C&  C(
=C*{C,
=C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA��CD  CF  CH
=CJ  CK��CN  CP
=CR
=CS��CU��CX  CZ
=C\
=C^
=C`  Ca��Cc��Cf  Ch
=Cj
=Ck��Cn  Cp  Cr  Ct  Cv
=Cx
=Cz  C|
=C~  C��C���C�  C�  C�C�  C�C�C�  C���C�  C�  C�  C�C�  C���C�C�C���C���C���C���C���C�  C�  C���C���C�  C�C�  C�  C���C���C���C�  C�  C�  C���C���C�  C�C�C�  C���C���C���C�C�  C�  C�  C�  C�  C�C�  C���C�C�
=C�  C�  C�  C�C�  C���C�  C�
=C�  C�  C�  C�  C�  C�C�
=C�C�  C���C���C���C���C�  C�  C���C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�  C���C���C���C�  C�C�C�C�C�C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C�  C�C���C���D }qD  D��D�D��D  D��D  D� D  D��D  D}qD  D� D  D��D	�D	� D
  D
� D
�qD}qD�qD}qD  D�D  D}qD�qD}qD�D��D  D� D�qD� D  D}qD�D� D�qD� D  D� D�D��D  D}qD  D� D�D��D  D}qD  D��D  D� DD��D�D� D�qD � D!�D!� D!��D"}qD#  D#� D$  D$��D%  D%� D&�D&� D&�qD'}qD'�qD(}qD(�qD)� D*  D*� D+�D+��D,�D,}qD,��D-z�D-�qD.� D/�D/� D/�qD0}qD1  D1��D2�D2��D3�D3��D4  D4��D5  D5� D6  D6}qD7  D7��D8  D8}qD9  D9� D9�qD:}qD;  D;��D<D<��D<�qD=}qD>  D>��D>�qD?}qD@  D@��DA  DA}qDB�DB� DB�qDC}qDC��DD}qDE  DE� DFDF��DG  DG� DH�DH� DI�DI� DI�qDJ� DK�DK��DL  DLz�DL�qDM}qDN  DN��DO�DO� DP  DP}qDP�qDQ� DR�DR��DS  DS� DT�DT� DU  DU� DV  DV��DW  DW� DX  DX}qDY  DY� DZ  DZ� D[  D[� D\D\��D\�qD]� D^�D^��D_  D_� D`  D`��Da�Da��Db  Db}qDb�qDc}qDd  Dd��De  De� Df  Df� Dg�Dg� Dg��Dhz�Dh�qDi� Dj�Dj��Dj�qDk� Dl  Dl� Dm�Dm� Dn  Dn}qDn�qDo}qDp  Dp}qDq  Dq� Dr  Dr� Dr�qDs}qDs�qDt� Du  Du}qDv  Dv��Dw  Dw��Dx�Dx� Dx��Dy}qDz  Dz� Dz��D{}qD{�qD|� D}�D}� D~  D~� D  D�D��D�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D�HD�AHD�~�D���D���D�>�D�� D��HD�  D�@ D�~�D���D�  D�B�D��HD��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�>�D�}qD���D���D�>�D�� D���D���D�@ D�~�D�� D�HD�B�D��HD�� D�  D�AHD��HD��HD�HD�AHD��HD�� D���D�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D��D�AHD��HD�� D�  D�@ D�~�D�� D�HD�=qD�~�D���D���D�@ D�� D���D���D�AHD��HD�� D���D�@ D�� D���D��qD�>�D�~�D���D��qD�@ D�� D���D��qD�@ D���D��HD�HD�@ D�� D�� D���D�>�D�� D��HD�  D�=qD�� D��HD�  D�@ D�~�D���D�HD�B�D�� D���D�  D�B�D��HD�� D���D�@ D��HD�� D�  D�@ D�~�D��HD��D�@ D��HD��HD���D�>�D�~�D�� D�HD�AHD��HD�� D���D�AHD�� D���D���D�@ D�� D���D�  D�@ D�� D��HD���D�>�D�� D���D�  D�AHD�� D��HD�HD�AHD��HD�� D�  D�AHD�� D���D�  D�@ D�� D���D��qD�>�D�� D�� D���D�>�D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D��HD�HD�>�D�~�D���D�  D�AHD��HD�� D���D�AHD�� D���D���D�>�D�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D�� D��H>�Q�>���?#�
?�  ?���?\?�G�@�@�@(��@8Q�@L��@\(�@n{@�G�@�=q@���@���@��\@�{@�@�p�@�ff@�\)@ٙ�@�G�@�@�
=A ��A�A
�HA\)Az�A��A�RA$z�A(��A.�RA3�
A9��A?\)ADz�AJ=qAP  AU�A[�A`��AfffAj�HAqG�AvffAz�HA�  A��HA�p�A�  A�=qA��A�\)A��A�z�A�
=A���A��
A�{A���A�33A�{A�Q�A��HA��A�\)A��A�(�A��RA���A�33A�p�A��A�=qA�z�A�
=Aə�A˅A�ffA�Q�A��HA��A�\)A�=qA�(�A�ffA��A�33A��A�A��A�(�A�RA���A�A�A�  A��HA�p�B   Bp�B�RB  Bp�B�RB  B	p�B
�RB(�Bp�B�RB(�Bp�B
=BQ�B��B
=BQ�B��B�HBQ�B��B�HB Q�B!��B"�HB$(�B%��B&�HB((�B)��B*�RB,Q�B-��B.�HB0Q�B1B2�HB4Q�B5B7
=B8(�B9��B:�HB<Q�B=��B>�HB@Q�BA��BB�HBD(�BEp�BF�RBH(�BIp�BJ�RBL  BMp�BN�RBP  BQG�BR�\BT  BUG�BV�RBW�
BYG�BZ�\B[�
B]G�B^ffB_�B`��BbffBc�Be�Bf=qBg�Bh��BjffBk�Bl��BnffBo�Bq�BrffBs�
BuG�Bv�\Bw�
ByG�Bz�RB|  B}G�B~�RB�  B���B�\)B�  B��RB�G�B�  B���B�\)B��B���B�33B��B���B�\)B��B��\B�G�B��B��RB�G�B��B���B�G�B��B���B�\)B��B���B�G�B��B���B�33B��B��\B�33B��B��\B�33B��
B�z�B�33B��
B��\B��B��
B�z�B�
=B��
B�z�B�33B�B�z�B��B�B�z�B��B��
B�ffB��B�B�z�B�
=B��B�ffB�
=B��B�ffB���B��B�Q�B�
=B���B�Q�B�
=B���B�Q�B���B���B�ffB���B��B�Q�B���B���B�Q�B���B���B�=qB��HB��B�=qB��HB��B�(�B���B�p�B�{B��RB�\)B�  B£�B�G�B��Bď\B�33B��
B�z�B��B�B�ffB�
=BɮB�=qB��HB˅B�(�B���BͅB�(�B���B�p�B�{B���B�p�B�{BҸRB�p�B�{B���B�p�B�{BָRB�p�B�{B���B�\)B�(�B���B�p�B�(�B���B݅B�=qB���Bߙ�B�Q�B���BᙚB�=qB���B�B�ffB�
=B�B�ffB�
=B�B�ffB�
=B��
B�z�B��B�B�ffB�
=B홚B�=qB���B�\)B��B�ffB���B�\)B��
B�=qB�RB��B�B��B�Q�B���B�33B���B�  B�z�B���B�\)B�B�(�B���B�
=B��B��B�Q�B��RB��B��B�  B�z�B��HB�\)B�B�=qB���B���B�p�B��C (�C \)C �\C ��C
=C33Cz�C��C�HC{CQ�Cz�C�RC��C�CQ�C�\CC  C=qCp�C�C�HC(�C\)C�\C�
C  CG�C�C�C�C(�CffC��C��C
=C=qCz�C�RC��C	(�C	ffC	��C	�
C
{C
Q�C
�\C
C
��C33CffC��C�HC{CG�C�C�RC��C�CffC��C��C  C33Cz�C�C�C{CQ�C�CC�C(�C\)C��C��C  C33CffC��C�
C
=C=qCz�C�C�HC{CQ�C�\C�RC��C(�C\)C��C��C  C=qCp�C�C�
C{C=qCz�C�C�HC�CQ�C�CC��C(�C\)C�\C��C
=C=qCp�C��C�HC�CG�Cz�C�RC�C(�C\)C�\C�
C  C33Cp�C��C�
C
=CG�Cz�C�C�HC�CQ�C�CC��C33C\)C�\CC��C 33C p�C ��C ��C!{C!G�C!�C!�RC!��C"=qC"z�C"�RC"�C#(�C#p�C#��C#�HC$(�C$ffC$��C$�C%(�C%\)C%��C%�HC&�C&\)C&��C&�
C'�C'Q�C'�\C'��C({C(Q�C(�C(��C)  C)G�C)�C)C*  C*=qC*�C*C+  C+=qC+�\C+C,
=C,G�C,�C,��C-
=C-Q�C-�C-��C.
=C.G�C.�C.�
C/
=C/Q�C/�\C/�
C0{C0Q�C0��C0�
C1{C1\)C1��C1�
C2�C2ffC2��C2�HC3�C3ffC3�C3��C4(�C4p�C4�RC4��C5=qC5�C5��C6
=C6\)C6��C6��C733C7�C7C8{C8\)C8�C8�C9=qC9�C9��C:{C:\)C:��C:�C;33C;z�C;��C<{C<\)C<��C<�C=33C=�C=C>�C>\)C>�C>��C?G�C?�\C?�
C@�C@p�C@�CA
=CAG�CA��CA�CB33CBz�CB��CC{CCffCC�CC��CDG�CD��CD�
CE33CEp�CE��CF
=CF\)CF�CF��CG=qCG�\CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                       ?u?��H@B�\@�  @��R@�  @�  @��RA\)A   A,(�A@��A`��A�Q�A�  A��A�  A�  A�  A�  A�A�\)B�B�
B  B�
B'�
B0  B8  B@  BH(�BP  BW�
B`  Bh  Bp  BxQ�B�  B�  B�{B�(�B�(�B�{B�{B�  B�  B��B��B�  B�(�B�  B�  B�{B��B��B�{B�  B�  B��B��B�  B�{B�  B��B�{B�{B��B�  B�  C   C��C��C��C  C
  C  C
=C  C  C  C��C��C  C  C  C   C"  C#��C&  C(
=C*{C,
=C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA��CD  CF  CH
=CJ  CK��CN  CP
=CR
=CS��CU��CX  CZ
=C\
=C^
=C`  Ca��Cc��Cf  Ch
=Cj
=Ck��Cn  Cp  Cr  Ct  Cv
=Cx
=Cz  C|
=C~  C��C���C�  C�  C�C�  C�C�C�  C���C�  C�  C�  C�C�  C���C�C�C���C���C���C���C���C�  C�  C���C���C�  C�C�  C�  C���C���C���C�  C�  C�  C���C���C�  C�C�C�  C���C���C���C�C�  C�  C�  C�  C�  C�C�  C���C�C�
=C�  C�  C�  C�C�  C���C�  C�
=C�  C�  C�  C�  C�  C�C�
=C�C�  C���C���C���C���C�  C�  C���C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�  C���C���C���C�  C�C�C�C�C�C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C�  C�C���C���D }qD  D��D�D��D  D��D  D� D  D��D  D}qD  D� D  D��D	�D	� D
  D
� D
�qD}qD�qD}qD  D�D  D}qD�qD}qD�D��D  D� D�qD� D  D}qD�D� D�qD� D  D� D�D��D  D}qD  D� D�D��D  D}qD  D��D  D� DD��D�D� D�qD � D!�D!� D!��D"}qD#  D#� D$  D$��D%  D%� D&�D&� D&�qD'}qD'�qD(}qD(�qD)� D*  D*� D+�D+��D,�D,}qD,��D-z�D-�qD.� D/�D/� D/�qD0}qD1  D1��D2�D2��D3�D3��D4  D4��D5  D5� D6  D6}qD7  D7��D8  D8}qD9  D9� D9�qD:}qD;  D;��D<D<��D<�qD=}qD>  D>��D>�qD?}qD@  D@��DA  DA}qDB�DB� DB�qDC}qDC��DD}qDE  DE� DFDF��DG  DG� DH�DH� DI�DI� DI�qDJ� DK�DK��DL  DLz�DL�qDM}qDN  DN��DO�DO� DP  DP}qDP�qDQ� DR�DR��DS  DS� DT�DT� DU  DU� DV  DV��DW  DW� DX  DX}qDY  DY� DZ  DZ� D[  D[� D\D\��D\�qD]� D^�D^��D_  D_� D`  D`��Da�Da��Db  Db}qDb�qDc}qDd  Dd��De  De� Df  Df� Dg�Dg� Dg��Dhz�Dh�qDi� Dj�Dj��Dj�qDk� Dl  Dl� Dm�Dm� Dn  Dn}qDn�qDo}qDp  Dp}qDq  Dq� Dr  Dr� Dr�qDs}qDs�qDt� Du  Du}qDv  Dv��Dw  Dw��Dx�Dx� Dx��Dy}qDz  Dz� Dz��D{}qD{�qD|� D}�D}� D~  D~� D  D�D��D�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D�HD�AHD�~�D���D���D�>�D�� D��HD�  D�@ D�~�D���D�  D�B�D��HD��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�>�D�}qD���D���D�>�D�� D���D���D�@ D�~�D�� D�HD�B�D��HD�� D�  D�AHD��HD��HD�HD�AHD��HD�� D���D�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�~�D�� D��D�AHD��HD�� D�  D�@ D�~�D�� D�HD�=qD�~�D���D���D�@ D�� D���D���D�AHD��HD�� D���D�@ D�� D���D��qD�>�D�~�D���D��qD�@ D�� D���D��qD�@ D���D��HD�HD�@ D�� D�� D���D�>�D�� D��HD�  D�=qD�� D��HD�  D�@ D�~�D���D�HD�B�D�� D���D�  D�B�D��HD�� D���D�@ D��HD�� D�  D�@ D�~�D��HD��D�@ D��HD��HD���D�>�D�~�D�� D�HD�AHD��HD�� D���D�AHD�� D���D���D�@ D�� D���D�  D�@ D�� D��HD���D�>�D�� D���D�  D�AHD�� D��HD�HD�AHD��HD�� D�  D�AHD�� D���D�  D�@ D�� D���D��qD�>�D�� D�� D���D�>�D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D��HD�HD�>�D�~�D���D�  D�AHD��HD�� D���D�AHD�� D���D���D�>�D�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D�� G�O�>�Q�>���?#�
?�  ?���?\?�G�@�@�@(��@8Q�@L��@\(�@n{@�G�@�=q@���@���@��\@�{@�@�p�@�ff@�\)@ٙ�@�G�@�@�
=A ��A�A
�HA\)Az�A��A�RA$z�A(��A.�RA3�
A9��A?\)ADz�AJ=qAP  AU�A[�A`��AfffAj�HAqG�AvffAz�HA�  A��HA�p�A�  A�=qA��A�\)A��A�z�A�
=A���A��
A�{A���A�33A�{A�Q�A��HA��A�\)A��A�(�A��RA���A�33A�p�A��A�=qA�z�A�
=Aə�A˅A�ffA�Q�A��HA��A�\)A�=qA�(�A�ffA��A�33A��A�A��A�(�A�RA���A�A�A�  A��HA�p�B   Bp�B�RB  Bp�B�RB  B	p�B
�RB(�Bp�B�RB(�Bp�B
=BQ�B��B
=BQ�B��B�HBQ�B��B�HB Q�B!��B"�HB$(�B%��B&�HB((�B)��B*�RB,Q�B-��B.�HB0Q�B1B2�HB4Q�B5B7
=B8(�B9��B:�HB<Q�B=��B>�HB@Q�BA��BB�HBD(�BEp�BF�RBH(�BIp�BJ�RBL  BMp�BN�RBP  BQG�BR�\BT  BUG�BV�RBW�
BYG�BZ�\B[�
B]G�B^ffB_�B`��BbffBc�Be�Bf=qBg�Bh��BjffBk�Bl��BnffBo�Bq�BrffBs�
BuG�Bv�\Bw�
ByG�Bz�RB|  B}G�B~�RB�  B���B�\)B�  B��RB�G�B�  B���B�\)B��B���B�33B��B���B�\)B��B��\B�G�B��B��RB�G�B��B���B�G�B��B���B�\)B��B���B�G�B��B���B�33B��B��\B�33B��B��\B�33B��
B�z�B�33B��
B��\B��B��
B�z�B�
=B��
B�z�B�33B�B�z�B��B�B�z�B��B��
B�ffB��B�B�z�B�
=B��B�ffB�
=B��B�ffB���B��B�Q�B�
=B���B�Q�B�
=B���B�Q�B���B���B�ffB���B��B�Q�B���B���B�Q�B���B���B�=qB��HB��B�=qB��HB��B�(�B���B�p�B�{B��RB�\)B�  B£�B�G�B��Bď\B�33B��
B�z�B��B�B�ffB�
=BɮB�=qB��HB˅B�(�B���BͅB�(�B���B�p�B�{B���B�p�B�{BҸRB�p�B�{B���B�p�B�{BָRB�p�B�{B���B�\)B�(�B���B�p�B�(�B���B݅B�=qB���Bߙ�B�Q�B���BᙚB�=qB���B�B�ffB�
=B�B�ffB�
=B�B�ffB�
=B��
B�z�B��B�B�ffB�
=B홚B�=qB���B�\)B��B�ffB���B�\)B��
B�=qB�RB��B�B��B�Q�B���B�33B���B�  B�z�B���B�\)B�B�(�B���B�
=B��B��B�Q�B��RB��B��B�  B�z�B��HB�\)B�B�=qB���B���B�p�B��C (�C \)C �\C ��C
=C33Cz�C��C�HC{CQ�Cz�C�RC��C�CQ�C�\CC  C=qCp�C�C�HC(�C\)C�\C�
C  CG�C�C�C�C(�CffC��C��C
=C=qCz�C�RC��C	(�C	ffC	��C	�
C
{C
Q�C
�\C
C
��C33CffC��C�HC{CG�C�C�RC��C�CffC��C��C  C33Cz�C�C�C{CQ�C�CC�C(�C\)C��C��C  C33CffC��C�
C
=C=qCz�C�C�HC{CQ�C�\C�RC��C(�C\)C��C��C  C=qCp�C�C�
C{C=qCz�C�C�HC�CQ�C�CC��C(�C\)C�\C��C
=C=qCp�C��C�HC�CG�Cz�C�RC�C(�C\)C�\C�
C  C33Cp�C��C�
C
=CG�Cz�C�C�HC�CQ�C�CC��C33C\)C�\CC��C 33C p�C ��C ��C!{C!G�C!�C!�RC!��C"=qC"z�C"�RC"�C#(�C#p�C#��C#�HC$(�C$ffC$��C$�C%(�C%\)C%��C%�HC&�C&\)C&��C&�
C'�C'Q�C'�\C'��C({C(Q�C(�C(��C)  C)G�C)�C)C*  C*=qC*�C*C+  C+=qC+�\C+C,
=C,G�C,�C,��C-
=C-Q�C-�C-��C.
=C.G�C.�C.�
C/
=C/Q�C/�\C/�
C0{C0Q�C0��C0�
C1{C1\)C1��C1�
C2�C2ffC2��C2�HC3�C3ffC3�C3��C4(�C4p�C4�RC4��C5=qC5�C5��C6
=C6\)C6��C6��C733C7�C7C8{C8\)C8�C8�C9=qC9�C9��C:{C:\)C:��C:�C;33C;z�C;��C<{C<\)C<��C<�C=33C=�C=C>�C>\)C>�C>��C?G�C?�\C?�
C@�C@p�C@�CA
=CAG�CA��CA�CB33CBz�CB��CC{CCffCC�CC��CDG�CD��CD�
CE33CEp�CE��CF
=CF\)CF�CF��CG=qCG�\CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�  A�A�A�A�A�A�A�A�
=A�
=A�JA�JA�VA�bA�VA�VA�VA�bA�bA�bA��A��A��A��A��A��A��A��A� �A� �A��A�{A�  A�VA�VA���A��A��A��A��yA��`A��TA��TA��#A���A��A��A���A���A��FA���A��PA�hsA���A��A��HA��!A�ȴA�t�A��\A��RA� �A���A�7LA�hsA�7LA�M�A�"�A�XA���A���A�I�A��yA���A��A���A�S�A��A���A�7LA��FA���A�z�A�I�A��wA�A�A�E�A���A��FA�A�v�A��RA�E�A�G�A���A�ĜA���A�
=A��
A�A�A�XA�1'A�hsA��yA�?}A���A�~�A��wA�%A���A��uA�%A�C�A�?}A��DA�Q�A���A�v�A���A���A�hsA�I�A�7LA��mA�A��A�v�A�;A%A~VA{��Ax�Av{Ao�Al��Ak�TAh�yAe��AeVAdv�Ac��Aa��A_�-A^�A]�A\�AZE�AVI�ASVAR�AQ�wAQC�AQ�AP1'AN�AM��AMAM|�AM\)AM"�AL�AL  AK7LAI��AH$�AF��AE�hAD�yAD��AD1AC�-AC�-AC�-AC�FAC�-AAdZA?`BA<bNA:n�A9K�A8Q�A7�A6~�A2jA0�/A/hsA.5?A-K�A,�9A,^5A,-A+��A+&�A*�uA*A�A(��A&{A%G�A#�A"{A!t�A �/A �A ȴA �A �+A r�A ^5A �A\)A��A�AXAO�A�wAVA��A�TA?}A$�A�A-A�hA��A��AA�A;dA	�
A��A\)A~�AA33Az�A�A�hA��A33A �/A A�@�33@��h@���@�r�@�;d@�n�@���@��-@�X@�G�@��@�%@��j@�7@���@�=q@�@웦@��m@�@�V@��@��@�;d@柾@�h@�7L@�u@�t�@�\@�!@�R@���@���@��@�I�@�  @߮@�\)@�"�@އ+@�Ĝ@٩�@ם�@�ȴ@���@� �@Ӯ@Ұ!@�E�@��`@ͺ^@�"�@�?}@�33@Ə\@ŉ7@�(�@��#@���@��@�(�@���@��@�ȴ@��^@���@���@��7@�X@�?}@�/@�%@��u@���@��@��+@�-@���@��P@�t�@���@���@�J@�`B@�dZ@�v�@�E�@�5?@�J@���@�1@���@�G�@�7L@�&�@�&�@��@��@��/@���@��u@�1'@�  @��F@�o@��@�?}@���@��j@�j@� �@��w@��@�-@��h@�p�@�p�@�hs@�hs@�X@���@�r�@��F@�"�@�
=@��R@�@���@��@���@��9@�z�@�  @���@���@��@���@�5?@��@��@�7L@�I�@�@�^5@��@���@��h@�?}@���@��9@�A�@��m@�t�@���@�=q@�@��T@��^@��7@��`@���@�Ĝ@��9@��@�j@�(�@��@�ƨ@��@���@�5?@�@�x�@�G�@�%@���@���@��@�bN@��@�b@��w@���@��P@��@��@�dZ@�
=@��+@��@��@�O�@�O�@�7L@��@��@���@�(�@�ƨ@�o@��y@���@��+@�^5@�5?@��@��-@�x�@�X@�O�@�G�@�?}@�/@���@��@���@���@��D@�r�@�9X@��w@�\)@�K�@�33@�@��@��H@��R@�^5@���@�@��^@���@��h@�`B@�/@��@��`@��@���@��@�A�@�1'@�b@�|�@�+@���@���@���@��\@�M�@���@���@�x�@�7L@��@�%@���@��j@��u@�Z@�b@l�@~ȴ@~V@~{@}�@|��@|��@|�@{�@{33@z�@z��@z^5@y��@yG�@y%@x�u@v�R@uO�@t�@tj@tI�@t(�@s��@s�m@s�F@s�F@s��@s�F@s��@s�F@sƨ@sS�@st�@sS�@s33@sdZ@r��@rJ@q�7@pĜ@pbN@o|�@m@mp�@m`B@l�/@l��@l��@l��@l�D@l9X@k��@k��@k�@j^5@jJ@j-@j�@i��@i��@iX@f��@f$�@e��@e@eV@d�/@d�D@dZ@d9X@cƨ@cC�@b�\@b�\@b�\@b�\@bn�@`��@_l�@_+@^�y@_�@_;d@_;d@_;d@_
=@^V@^@]��@]O�@\�@[�
@[t�@[S�@Z�@Zn�@Z��@Zn�@Z-@ZJ@Y��@Y�^@Y��@Yx�@YX@YX@YG�@YX@YX@Y7L@Y%@XĜ@X�u@X  @W�;@W\)@V��@Vȴ@Vȴ@V�R@VV@U/@Tj@T1@S�m@Sƨ@S��@SC�@S33@S"�@S@S@R�@R�H@R�H@R��@R�H@R�@R�@R��@R=q@Q�^@Q��@Q%@P�9@P �@O|�@OK�@O�@N�y@N��@N��@N�+@NV@NV@NV@N$�@N@M�@M�@L��@L�D@L�D@L�D@L�@L1@K��@KC�@J=q@Ix�@I&�@I%@HĜ@Hr�@H �@G|�@F5?@E��@E�-@E�-@D9X@Cƨ@B^5@AX@@�`@@�9@@�@@Q�@@ �@?\)@>�R@>�R@=/@=�@=V@=V@=V@<��@<��@<�/@=V@<�@<z�@<j@<I�@<9X@<1@;��@;��@;t�@;�@;t�@:��@9��@9��@9hs@8�9@81'@7�;@7�@7�P@7;d@6�@6E�@5O�@4�@4��@4�D@4�@4z�@4j@4Z@4(�@3�
@3�
@3�
@3ƨ@3�m@3ƨA��yA��A��`A��A��A���A�  A���A���A���A���A�  A�  A�A�A�A���A�A�A���A�  A�A�A�A�A�A�A�A�A�A�A�A�%A�A�A�
=A�
=A�1A�1A�1A�1A�JA�
=A�
=A�%A�
=A�bA�bA�JA�
=A�
=A�VA�JA�JA�
=A�
=A�VA�bA�VA�bA�VA�{A��A�VA�VA�VA�bA�bA�VA�JA�JA�VA�bA�bA�JA�
=A�JA�VA�bA�oA�bA�bA�bA�VA�VA�bA�bA�VA�JA�VA�bA�oA�VA�VA�JA�VA�oA�oA�oA�bA�bA�VA�VA�VA�VA�VA�bA�bA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A��A��A��A� �A� �A��A��A��A�"�A�"�A��A��A��A� �A� �A��A��A��A��A��A��A��A��A��A�bA�%A�
=A�  A���A�A�A�A���A�1A�bA�bA�VA�JA�VA�bA�bA�JA�bA�VA�
=A�%A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��`A��mA��mA��yA��mA��TA��mA��TA��TA��TA��HA��TA��`A��`A��TA��HA��HA��`A��`A��TA��;A��#A��#A��#A��#A��#A��
A���A���A���A��
A��
A���A���A��
A��#A��#A��A��A��A��
A��
A��A��#A��#A��A���A��
A��
A��
A���A���A���A���A���A�ȴA�ƨA��wA��^A��FA��9A��!A��A���A���A���A���A���A���A���A���A��hA��7A��A��A�~�A�|�A�x�A�r�A�ffA�VA�-A���A��HA��A���A�n�A�C�A���A�p�A��A�ĜA��A�jA�`BA�M�A�$�A��mA��+A�S�A�$�A�A��
A���A�jA�S�A�1'A�(�A��A���A���A�{A��-A���A��uA�hsA�O�A�7LA�$�A���A��\A��A�XA�&�A��A��A���A���A���A��DA�r�A�O�A�A�A��yA���A���A�ȴA���A���A���A���A���A��PA�v�A�VA�1'A�1A�A�VA�$�A�(�A���A��A��A�ZA�5?A���A��A��A���A���A��A�bNA�=qA���A��;A���A���A�ĜA��-A���A���A�~�A�E�A�-A��A���A�ĜA���A��A�bNA�A�A� �A�A��A���A�A���A��7A�~�A�hsA�\)A�dZA�-A�(�A�&�A�"�A��A�oA�VA�  A�  A���A��yA��TA��/A��wA��jA��9A���A��PA���A��hA��+A��A�|�A�dZA�`BA�\)A�=qA�7LA�&�A��A�JA���A���A��yA��mA��`A��HA��/A��A���A�ƨA�A���A���A���A��wA��wA��^A��9A��9A��FA��9A��!A��A��A��A���A���A���A���A���A���A���A��uA��7A��7A�~�A�r�A�t�A�dZA�ffA�M�A�O�A�G�A�?}A�?}A�;dA�;dA�1'A�/A�(�A�$�A��A�oA�A��A��A��;A��/A��/A���A���A�ȴA�ƨA��FA��!A���A���A���A�1'A�/A�A��`A��#A���A���A��jA��jA�A��^A��9A��!A��A��A���A���A��A���A��A���A���A���A���A���A���A��uA��PA��7A�|�A�n�A�n�A�hsA�bNA�dZA�ZA�ZA�ZA�VA�Q�A�I�A�A�A�?}A��A��
A�p�A��`A�v�A���A�\)A�33A��A��9A�t�A�bNA�Q�A�E�A�=qA� �A��A���A���A���A�r�A�G�A�/A���A��yA��mA��`A���A���A���A���A���A���A���A���A���A���A���A�ĜA�A��RA�v�A�jA�C�A�9XA�5?A��A��A���A���A���A���A��\A��\A�z�A�n�A�`BA�E�A���A��A��mA�ȴA���A��hA��7A��DA��+A�t�A�z�A�jA�\)A�/A��A�A��A���A���A�x�A�(�A�A���A��uA�^5A�I�A�+A��yA���A���A�|�A�=qA�  A��#A�A��A�hsA�ZA�ZA�VA�Q�A��A��^A��DA�+A��A�&�A���A��;A���A�ĜA��!A��\A��7A�+A��RA�z�A�+A���A���A�z�A�I�A�"�A�bA��mA��#A��hA���A�A��FA��hA�v�A�p�A�hsA�S�A�/A�VA���A��A��^A��A��DA�I�A�33A� �A� �A��A�bA�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                       A��A���A�  A�A�A�A�A�A�A�A�
=A�
=A�JA�JA�VA�bA�VA�VA�VA�bA�bA�bA��A��A��A��A��A��A��A��A� �A� �A��A�{A�  A�VA�VA���A��A��A��A��yA��`A��TA��TA��#A���A��A��A���A���A��FA���A��PA�hsA���A��A��HA��!A�ȴA�t�A��\A��RA� �A���A�7LA�hsA�7LA�M�A�"�A�XA���A���A�I�A��yA���A��A���A�S�A��A���A�7LA��FA���A�z�A�I�A��wA�A�A�E�A���A��FA�A�v�A��RA�E�A�G�A���A�ĜA���A�
=A��
A�A�A�XA�1'A�hsA��yA�?}A���A�~�A��wA�%A���A��uA�%A�C�A�?}A��DA�Q�A���A�v�A���A���A�hsA�I�A�7LA��mA�A��A�v�A�;A%A~VA{��Ax�Av{Ao�Al��Ak�TAh�yAe��AeVAdv�Ac��Aa��A_�-A^�A]�A\�AZE�AVI�ASVAR�AQ�wAQC�AQ�AP1'AN�AM��AMAM|�AM\)AM"�AL�AL  AK7LAI��AH$�AF��AE�hAD�yAD��AD1AC�-AC�-AC�-AC�FAC�-AAdZA?`BA<bNA:n�A9K�A8Q�A7�A6~�A2jA0�/A/hsA.5?A-K�A,�9A,^5A,-A+��A+&�A*�uA*A�A(��A&{A%G�A#�A"{A!t�A �/A �A ȴA �A �+A r�A ^5A �A\)A��A�AXAO�A�wAVA��A�TA?}A$�A�A-A�hA��A��AA�A;dA	�
A��A\)A~�AA33Az�A�A�hA��A33A �/A A�@�33@��h@���@�r�@�;d@�n�@���@��-@�X@�G�@��@�%@��j@�7@���@�=q@�@웦@��m@�@�V@��@��@�;d@柾@�h@�7L@�u@�t�@�\@�!@�R@���@���@��@�I�@�  @߮@�\)@�"�@އ+@�Ĝ@٩�@ם�@�ȴ@���@� �@Ӯ@Ұ!@�E�@��`@ͺ^@�"�@�?}@�33@Ə\@ŉ7@�(�@��#@���@��@�(�@���@��@�ȴ@��^@���@���@��7@�X@�?}@�/@�%@��u@���@��@��+@�-@���@��P@�t�@���@���@�J@�`B@�dZ@�v�@�E�@�5?@�J@���@�1@���@�G�@�7L@�&�@�&�@��@��@��/@���@��u@�1'@�  @��F@�o@��@�?}@���@��j@�j@� �@��w@��@�-@��h@�p�@�p�@�hs@�hs@�X@���@�r�@��F@�"�@�
=@��R@�@���@��@���@��9@�z�@�  @���@���@��@���@�5?@��@��@�7L@�I�@�@�^5@��@���@��h@�?}@���@��9@�A�@��m@�t�@���@�=q@�@��T@��^@��7@��`@���@�Ĝ@��9@��@�j@�(�@��@�ƨ@��@���@�5?@�@�x�@�G�@�%@���@���@��@�bN@��@�b@��w@���@��P@��@��@�dZ@�
=@��+@��@��@�O�@�O�@�7L@��@��@���@�(�@�ƨ@�o@��y@���@��+@�^5@�5?@��@��-@�x�@�X@�O�@�G�@�?}@�/@���@��@���@���@��D@�r�@�9X@��w@�\)@�K�@�33@�@��@��H@��R@�^5@���@�@��^@���@��h@�`B@�/@��@��`@��@���@��@�A�@�1'@�b@�|�@�+@���@���@���@��\@�M�@���@���@�x�@�7L@��@�%@���@��j@��u@�Z@�b@l�@~ȴ@~V@~{@}�@|��@|��@|�@{�@{33@z�@z��@z^5@y��@yG�@y%@x�u@v�R@uO�@t�@tj@tI�@t(�@s��@s�m@s�F@s�F@s��@s�F@s��@s�F@sƨ@sS�@st�@sS�@s33@sdZ@r��@rJ@q�7@pĜ@pbN@o|�@m@mp�@m`B@l�/@l��@l��@l��@l�D@l9X@k��@k��@k�@j^5@jJ@j-@j�@i��@i��@iX@f��@f$�@e��@e@eV@d�/@d�D@dZ@d9X@cƨ@cC�@b�\@b�\@b�\@b�\@bn�@`��@_l�@_+@^�y@_�@_;d@_;d@_;d@_
=@^V@^@]��@]O�@\�@[�
@[t�@[S�@Z�@Zn�@Z��@Zn�@Z-@ZJ@Y��@Y�^@Y��@Yx�@YX@YX@YG�@YX@YX@Y7L@Y%@XĜ@X�u@X  @W�;@W\)@V��@Vȴ@Vȴ@V�R@VV@U/@Tj@T1@S�m@Sƨ@S��@SC�@S33@S"�@S@S@R�@R�H@R�H@R��@R�H@R�@R�@R��@R=q@Q�^@Q��@Q%@P�9@P �@O|�@OK�@O�@N�y@N��@N��@N�+@NV@NV@NV@N$�@N@M�@M�@L��@L�D@L�D@L�D@L�@L1@K��@KC�@J=q@Ix�@I&�@I%@HĜ@Hr�@H �@G|�@F5?@E��@E�-@E�-@D9X@Cƨ@B^5@AX@@�`@@�9@@�@@Q�@@ �@?\)@>�R@>�R@=/@=�@=V@=V@=V@<��@<��@<�/@=V@<�@<z�@<j@<I�@<9X@<1@;��@;��@;t�@;�@;t�@:��@9��@9��@9hs@8�9@81'@7�;@7�@7�P@7;d@6�@6E�@5O�@4�@4��@4�D@4�@4z�@4j@4Z@4(�@3�
@3�
@3�
@3ƨ@3�mG�O�A��yA��A��`A��A��A���A�  A���A���A���A���A�  A�  A�A�A�A���A�A�A���A�  A�A�A�A�A�A�A�A�A�A�A�A�%A�A�A�
=A�
=A�1A�1A�1A�1A�JA�
=A�
=A�%A�
=A�bA�bA�JA�
=A�
=A�VA�JA�JA�
=A�
=A�VA�bA�VA�bA�VA�{A��A�VA�VA�VA�bA�bA�VA�JA�JA�VA�bA�bA�JA�
=A�JA�VA�bA�oA�bA�bA�bA�VA�VA�bA�bA�VA�JA�VA�bA�oA�VA�VA�JA�VA�oA�oA�oA�bA�bA�VA�VA�VA�VA�VA�bA�bA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A��A��A��A� �A� �A��A��A��A�"�A�"�A��A��A��A� �A� �A��A��A��A��A��A��A��A��A��A�bA�%A�
=A�  A���A�A�A�A���A�1A�bA�bA�VA�JA�VA�bA�bA�JA�bA�VA�
=A�%A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��`A��mA��mA��yA��mA��TA��mA��TA��TA��TA��HA��TA��`A��`A��TA��HA��HA��`A��`A��TA��;A��#A��#A��#A��#A��#A��
A���A���A���A��
A��
A���A���A��
A��#A��#A��A��A��A��
A��
A��A��#A��#A��A���A��
A��
A��
A���A���A���A���A���A�ȴA�ƨA��wA��^A��FA��9A��!A��A���A���A���A���A���A���A���A���A��hA��7A��A��A�~�A�|�A�x�A�r�A�ffA�VA�-A���A��HA��A���A�n�A�C�A���A�p�A��A�ĜA��A�jA�`BA�M�A�$�A��mA��+A�S�A�$�A�A��
A���A�jA�S�A�1'A�(�A��A���A���A�{A��-A���A��uA�hsA�O�A�7LA�$�A���A��\A��A�XA�&�A��A��A���A���A���A��DA�r�A�O�A�A�A��yA���A���A�ȴA���A���A���A���A���A��PA�v�A�VA�1'A�1A�A�VA�$�A�(�A���A��A��A�ZA�5?A���A��A��A���A���A��A�bNA�=qA���A��;A���A���A�ĜA��-A���A���A�~�A�E�A�-A��A���A�ĜA���A��A�bNA�A�A� �A�A��A���A�A���A��7A�~�A�hsA�\)A�dZA�-A�(�A�&�A�"�A��A�oA�VA�  A�  A���A��yA��TA��/A��wA��jA��9A���A��PA���A��hA��+A��A�|�A�dZA�`BA�\)A�=qA�7LA�&�A��A�JA���A���A��yA��mA��`A��HA��/A��A���A�ƨA�A���A���A���A��wA��wA��^A��9A��9A��FA��9A��!A��A��A��A���A���A���A���A���A���A���A��uA��7A��7A�~�A�r�A�t�A�dZA�ffA�M�A�O�A�G�A�?}A�?}A�;dA�;dA�1'A�/A�(�A�$�A��A�oA�A��A��A��;A��/A��/A���A���A�ȴA�ƨA��FA��!A���A���A���A�1'A�/A�A��`A��#A���A���A��jA��jA�A��^A��9A��!A��A��A���A���A��A���A��A���A���A���A���A���A���A��uA��PA��7A�|�A�n�A�n�A�hsA�bNA�dZA�ZA�ZA�ZA�VA�Q�A�I�A�A�A�?}A��A��
A�p�A��`A�v�A���A�\)A�33A��A��9A�t�A�bNA�Q�A�E�A�=qA� �A��A���A���A���A�r�A�G�A�/A���A��yA��mA��`A���A���A���A���A���A���A���A���A���A���A���A�ĜA�A��RA�v�A�jA�C�A�9XA�5?A��A��A���A���A���A���A��\A��\A�z�A�n�A�`BA�E�A���A��A��mA�ȴA���A��hA��7A��DA��+A�t�A�z�A�jA�\)A�/A��A�A��A���A���A�x�A�(�A�A���A��uA�^5A�I�A�+A��yA���A���A�|�A�=qA�  A��#A�A��A�hsA�ZA�ZA�VA�Q�A��A��^A��DA�+A��A�&�A���A��;A���A�ĜA��!A��\A��7A�+A��RA�z�A�+A���A���A�z�A�I�A�"�A�bA��mA��#A��hA���A�A��FA��hA�v�A�p�A�hsA�S�A�/A�VA���A��A��^A��A��DA�I�A�33A� �A� �A��A�bA�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"B�B�B�B�B"BVB�BVB"B�B�B"B"B�B�B�B�B"B"B"BVB�BVB�B"B�BVB�BbB�BhBoBB�BB�B�B�B{B�BBMBMB�B�B�BSB�B�B�B�BMBSB1B&�B=qBC-BQNBW�B^jBl�Bg�BV�BOvBO�BUgB[�B[�BR BM6BK�BJ�BJ�BIBHBG�BG�BF?BD�BA�BA�B?HB>BB<6B7B2�B{B�B
rBBfBSB��B�B�GB�fB��B��B��B��B�.B�Bm�Be�B]dBU2BJ�BF�BAUB2aB-�B(XB �B
�B
�PB
��B
�?B
ȴB
��B
��B
��B
�B
�-B
��B
��B
��B
��B
��B
�B
{�B
t�B
k�B
M6B
CaB
~B
�B	�	B	�B	��B	�B	�mB	�B	��B	�B	��B	�=B	�{B	�B	}�B	cTB	_;B	]�B	XyB	XEB	W?B	S�B	MB	N<B	J�B	K�B	JXB	H�B	C�B	B'B	<6B	5B	.B	(�B	"�B	"4B	�B	IB	B		B	�B	SB	$B	�B��B�B��B�B�|B�B� B��B�B�3B��B��B�6B�*B��B��B�nB�aB��B�0B��B�B�\B��B�IB��B��B�xB��B�=B��B��B�1B��B�B��B��B�B��B�fB�B��B��B�;B~�B}�B�B{JBz�Bv�Bu�Bv�By�Bx�BxBw2Bv�Bu%BtTBt�BsBn�BpoBl�Bn/Bm�Bl�BrBqvBrGBrBrBsMBr|Bq�Bp�BxBx8BxBx�Bx8BzxB{�BzDB{JBy>ByrBu�Bv+Bs�BsBu%Bu�Bt�Bt�By	Bw2Bw�BxlBw�Bw�Bv`BuZBt�Bu%B�iB��B��B��B�B��B�$B�B�_B��B��B�_B�B�B�B��B��B��B�B�UB��BÖB��B��B��BʌB��B��B�0B��B�B��B�&B�aB՛B�gB�/B�jB�5B�vB�B��B�`B��B�B�vB�B��B�2B��B	�B	�B	�B		B	�B		7B		�B		�B	
	B	xB	�B	�B	\B	4B	�B	B	�B	�B	~B	OB	!-B	$@B	*0B	-B	-�B	-wB	-�B	-B	-wB	/�B	2�B	7B	9�B	9�B	<6B	?�B	A�B	EB	F�B	H�B	I�B	MjB	N<B	M�B	M�B	S&B	V�B	VmB	W�B	]dB	b�B	kB	n/B	qB	sB	r�B	u%B	v�B	x�B	{B	}VB	��B	��B	�fB	�	B	��B	��B	�PB	��B	��B	�:B	�oB	�B	��B	��B	��B	�_B	��B	�!B	�hB	�B	�B	��B	��B	�B	�B	�qB	�IB	��B	�UB	�'B	�[B	��B	��B	��B	�-B	��B	�FB	�RB	��B	�dB	�0B	��B	�6B	��B	��B	��B	��B	��B	�zB	ȀB	�RB	�#B	��B	��B	�}B	�B	ѷB	уB	��B	��B	�TB	��B	�&B	�[B	� B	ѷB	ѷB	�&B	�B	רB	��B	�yB	�B	ٴB	��B	�QB	�]B	�jB	�jB	ޞB	�pB	�pB	�vB	��B	�|B	�NB	� B	�TB	�TB	�B	�B	��B	�B	�>B	�DB	�yB	�DB	�KB	�B	�"B	�)B	�B	�B	��B	� B	�B	��B	�B	�B	�|B	��B	��B	��B	��B	�lB	��B	�8B	��B	�B	��B	��B	�B	��B	��B	�(B	�]B	��B
;B
�B
�B
MB
�B
B
B
�B
�B
%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	7B
	�B

	B
B
�B
"B
�B
�B
(B
\B
�B
�B
\B
bB
�B
:B
B
�B
:B
:B
�B
oB
@B
+B
$B
�B
YB
_B
+B
�B
1B
�B
�B
�B
	B
	B
�B
7B
�B
B
�B
OB
�B
 'B
!bB
!-B
 �B
!�B
#:B
"�B
"�B
#:B
#nB
#nB
#:B
"�B
#nB
#nB
"hB
#nB
"�B
"hB
"�B
#nB
$B
$B
$�B
$@B
$tB
$�B
%B
$tB
%FB
%�B
&�B
'RB
'�B
'�B
(�B
(�B
(�B
(XB
)�B
+�B
+�B
,�B
,�B
,�B
,�B
.B
-�B
-�B
.B
-wB
.B
.IB
.IB
.}B
.B
.B
-�B
.IB
/B
/�B
/B
/�B
/�B
0�B
1�B
1'B
1'B
1�B
1�B
1�B
1�B
2�B
2aB
1�B
2�B
2�B
2aB
4�B
4nB
4nB
49B
4nB
5B
4�B
4�B
5�B
6FB
7�B
7LB
7�B
8B
7�B
8B
9XB
;dB
:�B
:�B
:*B
<�B
<�B
>wB
>�B
?}B
?�B
?}B
?}B
@B
AUB
A�B
@�B
D3B
C-B
C�B
C-B
B�B
C-B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
D3B
C�B
D�B
D�B
DgB
C�B
E9B
E�B
E9B
FtB
F�B
GB
GB
GzB
GEB
HKB
J�B
J�B
L0B
LdB
K�B
L0B
K�B
LdB
LdB
LdB
MB
MjB
M6B
MjB
M6B
MB
MjB(B(B4BB�B�BDB�B�B�B�B�BVBPB�B�BVB�B�B�B(B�B"B�BPBPB"B�B�B�B\B�B�B�B�B�BPB�BVB�B"B�BB�B�BVBPBPB�B�B�B�B�B�B�B�B�BB�B(B"B�BPB�B"B�BBPB�BVB�B�B�B�BVB\B�B�BB�BPBPB�B�B�B�B�B�B�B�B�B�BVB�B\B�B�BPB�B"BVB\B�B\B\B�B�B�B�B�B�BPBVB�B�B�B�B�B�BB�B�B"BBVB(B�B�B�B�B�BPBPB�B�B�B�B�B�B�B�B"B�B�BVB�B�B�B�B.BbB.B�B.B�B4B B�B�B�B4B B.B�B�B:B�BoB�BoB�B�BuBB�BFBBB�BhBuB�BuB�B�B�B�B4BB�B�B�BB�B�B�BB�BSBYB�B�B�B�B�B�B{BSB@B@B�B{BB�B�B�B�B�BMB�B�BMBB�BFB�B�B�BFBFB�B�B�BB�BMBB�B{B{BMBSB�B�B�B{B�B�BSBB�B�B�B�B$BBBB�B$B�B�B�B�BSB�BYB�BB�B�B�B�B$B�B�B�B�B+B$B�B�BSB�B+B�B�B$B�B�B�BSB�B�BuBFB�B�B�BYB�B�B�B�BYB7BB$@B$@B!�B&LB&�B'�B-CB<6B2aB>wB<�BC-B=qB:�B:�BAUBA�BJ�BIRBP�BK�BO�BT�BT�BS&BQ�BO�BP�BQ�B]dBa�BY�BV9B]dBbNBc Ba|Ba|BkQBncBjBpoBv�Bq�BjKBh�BgBe�B]�Bd�BZ�BV9B[�BJ�BJ�BJ�BP�BPHBOvBP�BS[BUgBVmBXBVmBH�BOBG�BJ�BT,B^�B]�BPBN�BN�B]�BT�BX�BV9BV�BV�Bc�BZQBcTB\]B[WBZ�B\�B]dB[#B[�B`BB]�BZQBYKB]�BWsBT,BQNBT�BV�BTaBP�BRTBT�BJ�BPBM�BK�BN<BK�BK)BT,BJ#BK^BJ�BMjBLdBJ#BNpBI�BK^BK�BI�BJ�BNBJ�BN<BN�BGBG�BF?BI�BIRBJXBL�BJ�BI�BQ�BI�BK)BJ#BN<BL0BK^BI�BH�BI�BH�BJ#BIBIBJ#BIRBH�BG�BG�BF�BGBG�BHKBHKBG�BGEBH�BG�BHKBGEBG�BGEBHBF�BG�BH�BG�BG�BGEBGBI�BIRBEmBK�BEBF�BD�BG�BH�BD�BD�BC�BEmBA�BEBDgBCaBFtBA�BE�B@�BFBA�BA�BB�BA�BA�BA�BB[B@�BA�B>BB>�B^B=BH�B?}B>wB<�B?�B?}B?B>B@�B?B?}B>BB>BB?�B>BB=�B?B=�B?HB>BB>�B>B>�B>BB>BB=�B=�B<6B?HB9$B:^B9�B8�B:*B8RB6�B8�B5?B7LB4�B2�B:�B9$B:*B7�B3hB*eBA�B7�B�B7BBoB�B�B{B�B@B{B$B�B(B�B{BYB	7B	lB	B.B�B�BJB	�BB
=B~B
rB
�B�B	7BB
	BIBB
	BSB%B	7B�B
�BB�B�BBuBSB�BMB1BuB�]B��B�B �B��B��B��B��B��B�lB�B�rB��BB��B�B�oB�oB��B��B�B�8B�KB�B�2B�B�B�B�NB�B�B��B�BԕBݘB�B��B��B�#B�B�&B�BȀB�B��B�hB�<B�B�!B�IB�UB��B��B��B�B��B��B�VB��B�YB��B�(B��B��B�SB�MB�	B��Bn�BsBkQBm)BlWBp;Bu�Bm]Bh�Bj�BjBd�Bk�BiyB`vB`�B]/B^5B^�B^jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                       B��B�cB��B��B��B B 4B��B 4B��B�}B��B B B��B��B��B��B B B B B��B OB��B B��B OB �BAB�B{B�B_B�BB�BB�BtB�B+B_BEB�B�BfBKB�B	7B	lB	lB�B	�B.B"�B5�B<jBH�BQ4BU�Bc�B]IBK�BDgBG+BNVBS@BT{BH�BA�B?�B>�B>�B<B:�B:�B;dB9�B8�B7�B6�B1�B1vB0�B8RB3B�B?B�qBAB�B��B��B�B�=B��BЗB�lB��B��B�<BzDBe,B[#BS�BJ�B>�B=�B7�B%�B!|B�BIB�B
�>B
ٴB
��B
�qB
��B
�5B
�WB
�
B
�B
�@B
��B
��B
��B
��B
y$B
p�B
o�B
h>B
IB
IB
yB	�B	�tB	�zB	��B	��B	�xB	�XB	�!B	��B	�{B	��B	��B	��B	zB	WYB	T�B	Q�B	K�B	M�B	N�B	G�B	@iB	A�B	=�B	?HB	>�B	=B	9	B	9�B	3�B	,B	$�B	~B	mB	�B	 B	�B	�B	�B	B	�B	�B	�B��B�B�BٴB��B��B�#B�mB��B��B�hB�[B��B�B��B�B�sB��B��B�BB��B�=B�2B�uB��B��B��B��B��B�<B�VB��B�pB��B�=B�dB�RB�B�B~(By>BzxBz�Bw2BtBw2BuZBp;BqvBn�Bl"Bn�BoiBn/Bl�BlqBk�Bi_Bj�BlqBgBc�Be�Bb�BezBd�Ba�BfBeFBe,Be`Bd�BfLBe`BeFBiDBo5Bl"BlBl�Bl=Bn�Bo�BnIBoOBn�BmCBjBi�Bg�Bg�BiDBhsBg�Bj0BlBj0Bj�Bk�BkBj�Bi�Bi_Bj�BmwBv�Bu�BwfB��B��B��B��B��B��B�	B��B��B��B��B�tB�eB�'B��B�hB�TB�tB��B�>B��B�VB�qB��B��B��B��B��B�MB��B��B�B��BуB�NBѷB��B��B��B�qB�4B�B�&B�B�B��B�MB��B�JB�JB��B�B�B��B��B�B��B��B	 �B	-B	�B	�B	B	�B	�B	�B	�B	�B	�B	�B	�B	 BB	 B	 vB	�B	 �B	#TB	&�B	*�B	,WB	,�B	0B	3B	5ZB	7�B	9�B	;B	=VB	@�B	@�B	@iB	A�B	F�B	I�B	IlB	K�B	Q�B	W�B	^�B	a�B	dB	e�B	e�B	h$B	i�B	k�B	nIB	p�B	t�B	y>B	{JB	|�B	}qB	~�B	�B	�MB	�gB	��B	�B	�B	��B	�lB	��B	�xB	��B	��B	��B	�B	�B	��B	�dB	��B	��B	�pB	�HB	�B	�TB	��B	�B	�B	�@B	��B	�`B	�RB	��B	��B	�]B	��B	��B	�IB	� B	��B	��B	��B	�zB	��B	�^B	�0B	�B	��B	��B	�B	�aB	��B	�3B	��B	�gB	āB	�9B	ňB	��B	��B	�B	�gB	ĜB	ƎB	�B	�=B	ʌB	�DB	�B	�JB	̳B	̈́B	��B	� B	��B	�NB	�B	�:B	�@B	ӏB	�FB	�2B	յB	�B	�9B	�$B	�YB	�yB	ڠB	�	B	��B	��B	�CB	�IB	ݘB	�!B	��B	�vB	�-B	�|B	��B	�NB	�B	�B	�B	�B	��B	�B	�B	��B	�B	�KB	�6B	�B	�/B	�cB	�}B	� B	�B	�UB	��B	�[B	�3B	�B	��B	�`B	��B	��B	��B	��B	��B	�B	��B	�RB	�8B	�B	�RB	��B	�$B	�XB	�>B	��B	��B	��B	��B	�jB	��B	�qB
 B
;B
 �B
oB
AB
�B
�B
oB
�B
B
�B
�B
�B
�B
B
�B
�B
mB
SB
1B
B
	�B

=B

rB

�B

�B
^B
�B
�B
�B
�B
�B
jB
PB
�B
�B
�B
�B
 B
B
oB
�B
�B
�B
B
�B
�B
�B
�B
B
9B
�B
�B
SB
�B
�B
B
gB
�B
MB
B
�B
�B

B
�B
�B
?B
�B

B
�B
yB
eB
�B
kB
�B
WB
#B
	B
=B
IB
�B
�B
;B
VB
;B
�B
 �B
 \B
 \B
 vB
�B
 �B
 �B
 �B
 �B
 vB
 vB
 BB
!-B
!�B
"NB
"B
"�B
"�B
#�B
$�B
#�B
#�B
$ZB
$tB
$@B
$ZB
$�B
$�B
$ZB
%FB
%,B
%�B
'�B
'B
&�B
&�B
'8B
'�B
'8B
'�B
)B
)yB
*�B
)�B
*0B
*�B
*B
+6B
,�B
.IB
-]B
-B
-�B
/�B
0UB
1�B
1vB
2B
2GB
2B
2-B
3MB
4TB
4nB
4�B
6�B
5�B
5�B
5�B
5tB
5�B
5tB
5�B
5�B
6`B
6FB
6FB
6FB
6`B
6�B
6zB
7fB
72B
6�B
7B
8�B
8B
8B
9�B
9�B
9�B
9�B
:B
9�B
;B
=�B
>BB
>�B
?B
>wB
>wB
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?}B
?}G�O�B B B-B�B �B��B�<B��B�B �B �B �B OB�HB��B��B OB�}B��B�B B �B B�}B�HB�HB B �B �B �BUB�}B�}B��B��B��B�HB��B OB��B B��B�B �B�B OB�HB�HB �B �B �B��B�}B�}B �B �B�}B�B��B B B�}B�HB �B B��B�B�HB��B OB �B�}B��B��B OBUB �B��B�B��B�HB�HB��B �B �B��B�}B �B �B �B�}B�}B OB �BUB �B��B�HB�}B B OBUB�BUBUB �B �B �B��B��B��B�HB OB �B�}B�}B �B�B�}B�B��B �B B�B OB B��B��B��B �B �B�HB�HB �B��B��B��B�}B �B�B��B B �B�B OB�}B �B �B�B'B[B'B�B'B�B-B�B�B�B�B-B�B'B�B�B3B�BgB�BgB�B�BmBB�B?BBB�BaBmB�BmB�B�B�B�B-BBzB�B�BB�B�B�BB�BKB	RB�B�B�B�B�B�BtBKB9B9B�BtBB�B�B�B�B�BEB�B�BEBBzB?B�B�BzB?B?BzB�B�BB�BEBBzBtBtBEBKB�B�B�BtB�B�BKBB�B�B�B	�B	BBBB�B	B	�B�B�B�BKB�B	RB�BBzBzB�B�B	B	�B�B�B	�B
#B	B	�B�BKB	�B
#B
�B�B	B�B�B�BKB�B�BmB?B�B�B�B	RB	�B�B�B�B	RB0B�B9B9B�BEB�B�B;B./B$ZB0oB.�B5%B/iB,�B,�B3MB3�B<�B;JBB�B=�BA�BF�BF�BEBC�BA�BB�BC{BO\BS�BK�BH1BO\BTFBUBSuBSuB]IB`\B\xBbhBh�Bc�B\CBZ�BX�BW�BO�BV�BL�BH1BM�B<�B<�B<�BB�BBABAoBB�BESBG_BHfBJ	BHfB:�BAB9�B<�BF%BP�BO�BBB@�B@�BO�BF�BJ�BH1BH�BH�BU�BLJBUMBNVBMPBL�BN�BO\BMBM�BR:BO�BLJBKDBO�BIlBF%BCGBF�BH�BFYBB�BDMBF�B<�BBB?�B=�B@4B=�B="BF%B<B=VB<�B?cB>]B<B@iB;�B=VB=�B;�B<�B@ B<�B@4B@�B9	B9�B88B;B;JB<PB>�B<�B;�BC{B;B="B<B@4B>(B=VB;�B:�B;�B:�B<B;B;B<B;JB:�B9�B9�B8�B9	B9�B:DB:DB9�B9>B:�B9�B:DB9>B9�B9>B:B8�B9�B:xB9�B9�B9>B9	B;�B;JB7fB=�B6�B8�B6�B9�B:xB6�B6�B5�B7fB3�B6�B6`B5ZB8lB3�B7�B2�B8B3�B3�B4�B3�B3�B3�B4TB2�B3�B0;B0�BO�B/ B:xB1vB0oB.�B1�B1vB1B0B2|B1B1vB0;B0;B1�B0;B/�B1B/�B1AB0;B0�B0B0�B0;B0;B/�B/�B./B1AB+B,WB+�B*�B,"B*KB(�B*�B'8B)DB&�B$�B,�B+B,"B)�B%`B]B3�B)�B�B0B�BgB�B�BtB�B9BtB	BzB B�BtB	RB�0B�dB��B'B��B��B�BB��B�B�6B�wB�jB��B��B�0B�B�BBB�B�B�LB�B�0B��B��B�B�zB��B�B�nB�LB�zB�FB�*B�nB�UB�B��B�B��B��B��B��B�B�eB�B�kB��B��B��B�B�hB�hB�B�B�B�0B�]B��B�EB�)B�B��B�aB�,B��B��B�)BƨBϫB�'B��B�B�6B�0B�9B�B��B�B��B�zB�OB�'B�4B�\B�hB��B��B��B�B��B��B�hB��B�lB�B�;B|�B}�BwfB�_B�BzB`�Be,B]dB_;B^jBbNBg�B_pBZ�B\�B\�BV�B]�B[�BR�BR�BOBBPHBP�BP}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<{B_<og�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<A��<#�
<)hG<G�u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<.Y�<#�
<4M�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)�<#�
<���<#�
<#�
<*<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<O�<*��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$Rv<#�
<#�
<#�
<#�
<#�
<RV?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OW V1.0: r =0.9997(+/-0.0001), vertically averaged dS =-0.0133(+/-0.0029)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NO correction for Conductivity Thermal Mass (CTM) is applied;    OW V1.0: r =0.9997(+/-0.0001), vertically averaged dS =-0.0133(+/-0.0029)                                                                                                                      SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:8/4,y:4/2.0]; Fit T>2.1C; max_breaks=1;                                                                    PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OW weighted least squares fit is adopted; Map Scales:[x:8/4,y:4/2.0]; Fit T>2.1C; max_breaks=1;                                                                    PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202102122209382021021222093820210212220938202102122209382021021222093820210212220938SI  SI  ARFMARFM                                                                                                                                                2020022817453820200228174538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020030917051120200309170511QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020030917051120200309170511QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020060109060220200601090602IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021021222095220210212220952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2021021222095220210212220952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021021222095220210212220952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                