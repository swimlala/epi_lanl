CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  #   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-12-15T12:02:01Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  V    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       \h   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  u�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       {�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       Ӡ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�         PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H %0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      +x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` D�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   D�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   J�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   P�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T V�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   WD   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   WL   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   WT   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   W\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � Wd   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   W�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   X    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        X(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        X0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       X8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    X@Argo profile    3.1 1.2 19500101000000  20221215120201  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_240                 6810_008521_240                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�����E@�����E11  @��(��@��(��@2m&����@2m&�����d�csl�&�d�csl�&11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?��H@=p�@�G�@�  @�  @�  @��RA��A ��A,(�AAG�AaG�A�  A�\)A�  A���A���AУ�A�  A�  B (�B�
B(�B  B   B(  B0  B8Q�B@Q�BHQ�BP  BX(�B`  Bg�Bp  BxQ�B�
B��
B��B��
B�B��
B�  B�{B�  B�{B�{B�  B�  B�  B��B��B��B��B��B��B��B�  B�  B�  B�  B�{B�  B�  B�{B�  B�  B�{C 
=C
=C��C��C
=C
  C��C�C  C
=C
=C
=C
=C
=C
=C
=C 
=C"  C#��C&
=C(
=C*
=C+��C-�C/��C1��C3��C5��C7��C:  C;��C=��C?��CB  CD
=CF  CG��CI��CK��CN  CP
=CR
=CT
=CV
=CX
=CZ  C\
=C]��C_��Cb  Cc��Ce��Ch  Cj{Cl{Cm��Cp  Cr  Ct  Cv  Cw��Cy��C{��C~  C�  C�  C�C�C�C���C�  C�  C�C�  C�  C�C�  C�  C�C���C�  C�C�
=C�C�  C�  C�  C���C���C�  C���C���C�C�C�  C�  C���C���C�C�C�C�C�C�  C�  C�  C�
=C���C�  C�  C���C���C���C���C�  C�  C�  C�  C�  C���C���C�C�
=C�C�C���C���C�C�C�  C�  C�  C���C���C�  C�C�C�  C�  C�  C���C���C�  C���C�  C�C�C�C���C�  C�  C�C�  C�  C�C�  C���C���C���C���C���C���C�  C�  C���C���C�C�  C�  C�  C���C�  C�C���C�  C�  C�  C���C���C�  C�  C�  C�  C�C�C�C���C�  C���C���C�  C���C���D � D  D� D  D� DD��D�qD��D  D}qD�qD}qD  D� D�D��D	D	��D
  D
��D�D��D  D� DD� D  D� D�qD� D�qD��D�D� D��Dz�D  D}qD  D� D��D� D�D��D�D� D�D� D�D� D�qDz�D  D}qD��Dz�D��D}qDD}qD�qD}qD   D ��D �qD!}qD"�D"��D#�D#� D#�qD$}qD$��D%}qD%�qD&}qD&�qD'� D(�D(�D)�D)� D*  D*� D*�qD+� D,D,��D-  D-� D.  D.� D.�qD/� D0  D0}qD0�qD1}qD1�qD2� D3  D3� D4�D4��D5  D5��D6D6��D7  D7��D8�D8}qD9  D9� D9�qD:� D;  D;}qD<  D<� D=  D=� D=��D>z�D>�qD?��D@  D@}qD@��DA� DB  DBz�DB�qDC� DC�qDD}qDE  DE�DF�DF��DGDG��DH�DH��DI  DI��DJ�DJ� DK  DK�DL�DL}qDM  DM� DN�DN��DN�qDO� DO�qDPz�DQ  DQ}qDR  DR}qDR��DSz�DS��DT� DT�qDU}qDV  DV�DW�DW� DX  DXz�DX�RDY}qDY�qDZz�D[  D[� D[�qD\}qD\�qD]��D^  D^� D_  D_� D`�D`� Da  Da�Db�Db��Dc�Dc��Dd  Dd}qDe�De��Df  Df}qDf�qDgz�Dg�qDh� Di  Di}qDj  Dj� Dj�qDk}qDl�Dl�Dm  Dm��DnDn��Do  Do��DpDp��Dp�qDq� Dq�qDr}qDs  Ds� Ds�qDt��Du�Du� Dv�Dv� Dw  Dw}qDw�qDx}qDx�qDy��Dz�Dz�D{�D{��D{�qD|}qD|��D}� D~  D~}qD�D��D�  D�@ D��HD��HD�HD�AHD�� D�� D�HD�>�D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�AHD�� D��HD�HD�AHD��HD���D���D�@ D�~�D�� D��D�@ D�~�D���D���D�AHD��HD��HD�HD�B�D���D��HD�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?\)?8Q�?�\)?�{?�(�?�@z�@#�
@=p�@G�@c�
@p��@��@���@�@�  @�=q@�z�@�(�@Ǯ@�\)@�p�@��
@��@�Q�A�A�A�AG�AA�A!G�A$z�A+�A0  A4z�A;�A@  AE�AJ�HAP  AUAY��AaG�Ae�Aj�HAo\)As�
Az=qA~{A�=qA��
A�
=A�G�A��
A�{A�Q�A��A��A�Q�A��A��A�{A���A�33A�ffA�  A��HA���A��A���A�(�A��RA���A��A�p�A�Q�A\A���A�
=A�G�A��
A�p�A�Q�A��A��AָRA�G�A��HA��A�
=AᙚA�A�{A�  A�\A�z�A�RA�G�A�33A�ffA��A��HA�(�A��RB (�B��B�\B�
B��B�B\)B(�B	B
�RB  B�B=qB\)B(�B��BffB�BQ�BB�\B�B�B��B33B  BG�B{B�B z�B!��B"�HB#�B%�B%�B'33B(Q�B)�B*�RB+\)B,��B-B/\)B0(�B1��B2�\B3�
B5�B5�B7�B8Q�B9�B:�HB;�
B=p�B>=qB?�B@z�BA�BB�HBC�
BE��BF=qBG�
BH��BI�BK\)BL(�BMp�BN�RBO�BQ�BQ�BS�BTQ�BU�BV�HBXQ�BYp�BZ�RB\  B\��B^ffB_33B`��Ba��Bc33Bd  BeG�Bf�RBg�Bh��BiBk�Bl(�BmBn�RBp(�Bq�Br=qBs�Btz�Bv{Bv�\Bx  By�Bz{B{�B|Q�B}B~�HB�B���B�
=B��B�=qB���B��B��
B��RB�
=B��
B�=qB���B�p�B�  B��RB��B��B�Q�B�
=B���B�(�B���B�33B�  B�ffB��B���B�(�B���B�G�B�  B�ffB�33B���B�Q�B���B�p�B�(�B��\B�\)B��
B�ffB��B���B�Q�B���B�\)B�(�B�z�B�33B��
B�=qB��B��B�(�B���B�G�B�  B�z�B�
=B�B�(�B��HB�\)B�  B���B�
=B��
B�z�B��HB���B�(�B��RB�p�B��
B���B�G�B��B��\B��HB���B�=qB��RB��B��B���B��B��
B�z�B���B��B�=qB���B���B�{B���B�\)B��B��\B�G�B��B�z�B�
=B���B�ffB���B��B�=qB��\B�\)B��B�ffB�33BÙ�B�=qB��HB�\)B�(�Bƣ�B�G�B�  B�ffB��B��B�Q�B�
=B�B�(�B���BͅB�{B��HB�\)B�{BиRB�33B�  B�z�B�33B��
B�Q�B�33BծB�=qB���BׅB�{B��HB�G�B�  Bڏ\B���B�B�Q�BܸRB�p�B��B�ffB��B߅B�{B���B�\)B��
B�RB�33B��B��B�
=B��B��B�
=B��
B�\B���B�B�z�B���B뙚B�ffB���B�B�ffB���B�B�=qB���B�\)B�(�B��HB�G�B�  B���B��B��B�Q�B�
=B��
B�=qB�
=B��B�(�B���B���B�{B���B���B�(�B�
=B��C �C �C C�C�CC{Cz�C�RC{Cp�C�C{C\)C��C
=C=qC��C��C(�C�\C��C(�Cp�C�RC{CG�C�RC�C	=qC	��C	��C
�C
�C
�RC
=Cp�C�C{CQ�C��C  C=qC��C�C(�C��C�C�C�\C�
C�C�CC�CffC�C{CG�C��C��C(�C�C��C  C\)C�C��C{C=qC�\C�RC��C(�CQ�C��CC�HC(�C=qCz�C��C�RC��C
=C=qCp�C�C�RC�C��C33CG�CffC��C�RC�HC{C(�CQ�C�C�\C�
C�C{CG�C\)C�\CC�
C  C33CG�Cp�C�CC�HC�C=qCQ�Cz�CC��C  C(�CG�C�C�\C�
C�HC{CG�CffC��C�RC�
C {C =qC Q�C �C �RC ��C ��C!33C!Q�C!p�C!��C!�
C!�C"{C"G�C"z�C"�\C"C#  C#�C#33C#ffC#��C#�RC#�
C$�C$33C$\)C$��C$�RC$�
C%{C%=qC%Q�C%�\C%C%�
C&  C&=qC&Q�C&p�C&�C&�HC&��C'�C'\)C'�C'��C'C(  C((�C(G�C(ffC(��C(��C(��C)
=C)=qC)p�C)�C)��C)�
C*
=C*33C*G�C*ffC*��C*��C*�
C+
=C+=qC+Q�C+z�C+�C+C+�HC,�C,Q�C,ffC,�C,�RC,�C-{C-(�C-ffC-�C-��C-�C.
=C.�C.\)C.z�C.��C.�
C/
=C/(�C/=qC/ffC/��C/��C/�HC0  C0=qC0ffC0z�C0��C0��C1  C1�C1=qC1\)C1��C1C1�C2  C2(�C2ffC2�C2��C2�
C3{C333C3Q�C3z�C3�RC3�HC4  C4�C4Q�C4�\C4��C4��C5
=C5(�C5G�C5z�C5�RC5�HC6  C6�C6ffC6�C6��C6��C7{C7�C7\)C7�\C7��C7�
C8{C8(�C8G�C8�\C8�RC8�
C9{C9G�C9\)C9�\C9�
C9��C:
=C:Q�C:�C:��C:�
C;{C;33C;Q�C;�\C;��C;�HC<
=C<G�C<z�C<�\C<�RC=  C=�C==qC=ffC=�C=�
C>  C>�C>G�C>p�C>�C>�HC?{C?33C?Q�C?��C?��C@  C@(�C@G�C@p�C@�RC@�CA{CA=qCAffCA��CA�
CB  CB(�CBG�CB�CBCB�HCC{CCG�CCp�CC�\CC�RCD  CD33CDp�CD�\CD�RCD�HCE
=CEG�CEz�CE�RCE�HCF
=CF33CFp�CF�CF�HCG{CG33CGffCG�\CGCH  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?k�?��H@=p�@�G�@�  @�  @�  @��RA��A ��A,(�AAG�AaG�A�  A�\)A�  A���A���AУ�A�  A�  B (�B�
B(�B  B   B(  B0  B8Q�B@Q�BHQ�BP  BX(�B`  Bg�Bp  BxQ�B�
B��
B��B��
B�B��
B�  B�{B�  B�{B�{B�  B�  B�  B��B��B��B��B��B��B��B�  B�  B�  B�  B�{B�  B�  B�{B�  B�  B�{C 
=C
=C��C��C
=C
  C��C�C  C
=C
=C
=C
=C
=C
=C
=C 
=C"  C#��C&
=C(
=C*
=C+��C-�C/��C1��C3��C5��C7��C:  C;��C=��C?��CB  CD
=CF  CG��CI��CK��CN  CP
=CR
=CT
=CV
=CX
=CZ  C\
=C]��C_��Cb  Cc��Ce��Ch  Cj{Cl{Cm��Cp  Cr  Ct  Cv  Cw��Cy��C{��C~  C�  C�  C�C�C�C���C�  C�  C�C�  C�  C�C�  C�  C�C���C�  C�C�
=C�C�  C�  C�  C���C���C�  C���C���C�C�C�  C�  C���C���C�C�C�C�C�C�  C�  C�  C�
=C���C�  C�  C���C���C���C���C�  C�  C�  C�  C�  C���C���C�C�
=C�C�C���C���C�C�C�  C�  C�  C���C���C�  C�C�C�  C�  C�  C���C���C�  C���C�  C�C�C�C���C�  C�  C�C�  C�  C�C�  C���C���C���C���C���C���C�  C�  C���C���C�C�  C�  C�  C���C�  C�C���C�  C�  C�  C���C���C�  C�  C�  C�  C�C�C�C���C�  C���C���C�  C���C���D � D  D� D  D� DD��D�qD��D  D}qD�qD}qD  D� D�D��D	D	��D
  D
��D�D��D  D� DD� D  D� D�qD� D�qD��D�D� D��Dz�D  D}qD  D� D��D� D�D��D�D� D�D� D�D� D�qDz�D  D}qD��Dz�D��D}qDD}qD�qD}qD   D ��D �qD!}qD"�D"��D#�D#� D#�qD$}qD$��D%}qD%�qD&}qD&�qD'� D(�D(�D)�D)� D*  D*� D*�qD+� D,D,��D-  D-� D.  D.� D.�qD/� D0  D0}qD0�qD1}qD1�qD2� D3  D3� D4�D4��D5  D5��D6D6��D7  D7��D8�D8}qD9  D9� D9�qD:� D;  D;}qD<  D<� D=  D=� D=��D>z�D>�qD?��D@  D@}qD@��DA� DB  DBz�DB�qDC� DC�qDD}qDE  DE�DF�DF��DGDG��DH�DH��DI  DI��DJ�DJ� DK  DK�DL�DL}qDM  DM� DN�DN��DN�qDO� DO�qDPz�DQ  DQ}qDR  DR}qDR��DSz�DS��DT� DT�qDU}qDV  DV�DW�DW� DX  DXz�DX�RDY}qDY�qDZz�D[  D[� D[�qD\}qD\�qD]��D^  D^� D_  D_� D`�D`� Da  Da�Db�Db��Dc�Dc��Dd  Dd}qDe�De��Df  Df}qDf�qDgz�Dg�qDh� Di  Di}qDj  Dj� Dj�qDk}qDl�Dl�Dm  Dm��DnDn��Do  Do��DpDp��Dp�qDq� Dq�qDr}qDs  Ds� Ds�qDt��Du�Du� Dv�Dv� Dw  Dw}qDw�qDx}qDx�qDy��Dz�Dz�D{�D{��D{�qD|}qD|��D}� D~  D~}qD�D��D�  D�@ D��HD��HD�HD�AHD�� D�� D�HD�>�D�~�D�� D�HD�AHD�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�AHD�� D��HD�HD�AHD��HD���D���D�@ D�~�D�� D��D�@ D�~�D���D���D�AHD��HD��HD�HD�B�D���D��HD�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?\)?8Q�?�\)?�{?�(�?�@z�@#�
@=p�@G�@c�
@p��@��@���@�@�  @�=q@�z�@�(�@Ǯ@�\)@�p�@��
@��@�Q�A�A�A�AG�AA�A!G�A$z�A+�A0  A4z�A;�A@  AE�AJ�HAP  AUAY��AaG�Ae�Aj�HAo\)As�
Az=qA~{A�=qA��
A�
=A�G�A��
A�{A�Q�A��A��A�Q�A��A��A�{A���A�33A�ffA�  A��HA���A��A���A�(�A��RA���A��A�p�A�Q�A\A���A�
=A�G�A��
A�p�A�Q�A��A��AָRA�G�A��HA��A�
=AᙚA�A�{A�  A�\A�z�A�RA�G�A�33A�ffA��A��HA�(�A��RB (�B��B�\B�
B��B�B\)B(�B	B
�RB  B�B=qB\)B(�B��BffB�BQ�BB�\B�B�B��B33B  BG�B{B�B z�B!��B"�HB#�B%�B%�B'33B(Q�B)�B*�RB+\)B,��B-B/\)B0(�B1��B2�\B3�
B5�B5�B7�B8Q�B9�B:�HB;�
B=p�B>=qB?�B@z�BA�BB�HBC�
BE��BF=qBG�
BH��BI�BK\)BL(�BMp�BN�RBO�BQ�BQ�BS�BTQ�BU�BV�HBXQ�BYp�BZ�RB\  B\��B^ffB_33B`��Ba��Bc33Bd  BeG�Bf�RBg�Bh��BiBk�Bl(�BmBn�RBp(�Bq�Br=qBs�Btz�Bv{Bv�\Bx  By�Bz{B{�B|Q�B}B~�HB�B���B�
=B��B�=qB���B��B��
B��RB�
=B��
B�=qB���B�p�B�  B��RB��B��B�Q�B�
=B���B�(�B���B�33B�  B�ffB��B���B�(�B���B�G�B�  B�ffB�33B���B�Q�B���B�p�B�(�B��\B�\)B��
B�ffB��B���B�Q�B���B�\)B�(�B�z�B�33B��
B�=qB��B��B�(�B���B�G�B�  B�z�B�
=B�B�(�B��HB�\)B�  B���B�
=B��
B�z�B��HB���B�(�B��RB�p�B��
B���B�G�B��B��\B��HB���B�=qB��RB��B��B���B��B��
B�z�B���B��B�=qB���B���B�{B���B�\)B��B��\B�G�B��B�z�B�
=B���B�ffB���B��B�=qB��\B�\)B��B�ffB�33BÙ�B�=qB��HB�\)B�(�Bƣ�B�G�B�  B�ffB��B��B�Q�B�
=B�B�(�B���BͅB�{B��HB�\)B�{BиRB�33B�  B�z�B�33B��
B�Q�B�33BծB�=qB���BׅB�{B��HB�G�B�  Bڏ\B���B�B�Q�BܸRB�p�B��B�ffB��B߅B�{B���B�\)B��
B�RB�33B��B��B�
=B��B��B�
=B��
B�\B���B�B�z�B���B뙚B�ffB���B�B�ffB���B�B�=qB���B�\)B�(�B��HB�G�B�  B���B��B��B�Q�B�
=B��
B�=qB�
=B��B�(�B���B���B�{B���B���B�(�B�
=B��C �C �C C�C�CC{Cz�C�RC{Cp�C�C{C\)C��C
=C=qC��C��C(�C�\C��C(�Cp�C�RC{CG�C�RC�C	=qC	��C	��C
�C
�C
�RC
=Cp�C�C{CQ�C��C  C=qC��C�C(�C��C�C�C�\C�
C�C�CC�CffC�C{CG�C��C��C(�C�C��C  C\)C�C��C{C=qC�\C�RC��C(�CQ�C��CC�HC(�C=qCz�C��C�RC��C
=C=qCp�C�C�RC�C��C33CG�CffC��C�RC�HC{C(�CQ�C�C�\C�
C�C{CG�C\)C�\CC�
C  C33CG�Cp�C�CC�HC�C=qCQ�Cz�CC��C  C(�CG�C�C�\C�
C�HC{CG�CffC��C�RC�
C {C =qC Q�C �C �RC ��C ��C!33C!Q�C!p�C!��C!�
C!�C"{C"G�C"z�C"�\C"C#  C#�C#33C#ffC#��C#�RC#�
C$�C$33C$\)C$��C$�RC$�
C%{C%=qC%Q�C%�\C%C%�
C&  C&=qC&Q�C&p�C&�C&�HC&��C'�C'\)C'�C'��C'C(  C((�C(G�C(ffC(��C(��C(��C)
=C)=qC)p�C)�C)��C)�
C*
=C*33C*G�C*ffC*��C*��C*�
C+
=C+=qC+Q�C+z�C+�C+C+�HC,�C,Q�C,ffC,�C,�RC,�C-{C-(�C-ffC-�C-��C-�C.
=C.�C.\)C.z�C.��C.�
C/
=C/(�C/=qC/ffC/��C/��C/�HC0  C0=qC0ffC0z�C0��C0��C1  C1�C1=qC1\)C1��C1C1�C2  C2(�C2ffC2�C2��C2�
C3{C333C3Q�C3z�C3�RC3�HC4  C4�C4Q�C4�\C4��C4��C5
=C5(�C5G�C5z�C5�RC5�HC6  C6�C6ffC6�C6��C6��C7{C7�C7\)C7�\C7��C7�
C8{C8(�C8G�C8�\C8�RC8�
C9{C9G�C9\)C9�\C9�
C9��C:
=C:Q�C:�C:��C:�
C;{C;33C;Q�C;�\C;��C;�HC<
=C<G�C<z�C<�\C<�RC=  C=�C==qC=ffC=�C=�
C>  C>�C>G�C>p�C>�C>�HC?{C?33C?Q�C?��C?��C@  C@(�C@G�C@p�C@�RC@�CA{CA=qCAffCA��CA�
CB  CB(�CBG�CB�CBCB�HCC{CCG�CCp�CC�\CC�RCD  CD33CDp�CD�\CD�RCD�HCE
=CEG�CEz�CE�RCE�HCF
=CF33CFp�CF�CF�HCG{CG33CGffCG�\CGCH  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�AԸRA�A���AԾwAԾwA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�A�ĜA���A���A���A���A���A���A���A��
A��
A��
A���A��
A���A��A��#A���A���A�A�ĜAԼjAԸRAԸRAԶFAԸRAԼjA���A�A�ĜA�ĜA�ĜA���AԼjA�A�AԾwA���A���AԴ9AԲ-AԲ-AԸRA�ĜA���A���A���A���AԸRAԲ-A�^5A��HAɗ�AȅAǮAƣ�A��A���A���A�bA���A�dZA��A�I�A�XA�S�A���A�t�A�t�A��uA�A�A�A�&�A�bNA�;dA���A�XA���A�JA���A��A�bA�/A�33A���A�-A�r�A��`A��`A��A���A���A�  A�I�A��A�
=A�  A�^5A��A��HA�33A��`A�A��PA��uA�\)A�C�A��!A��7A�1'A���A��A���A�E�A���A�5?A|r�AyXAx��AxjAx{At�+Aq�AoC�Ak�AiXAhjAdn�A`^5A]/A\AY�mATbAR�AQ��AQ�AL �AIAG%AE�AD�A@=qA=��A<-A;`BA;A9�A9�A8�/A8A�A7+A5�PA4r�A2��A1�FA0��A.n�A,ZA+|�A*�`A)ƨA(�A(bA'�PA'x�A&�RA%�A$=qA#��A"�uA!��A �A��A�A5?A&�A?}A�mAoA�AoA��A�yA�Ar�A�jA"�A"�A��A�mA%A��A�RA�hA�DA��A
ffA��AA�A��Al�A�A�AȴA�;A�HAQ�A�mA+A v�A E�@���@��@��;@���@�l�@���@��T@���@�ff@���@�%@�bN@�@�$�@�bN@�E�@�@���@��@���@�@�@��H@��@��T@���@��#@�p�@��@� �@�P@�j@��@�@�&�@�M�@��@�ƨ@�1'@�bN@ܓu@�r�@ܣ�@ܬ@�o@�1@�p�@�hs@�  @ӍP@�dZ@�"�@ҸR@ҧ�@���@��y@�E�@�5?@��@���@�7L@У�@�1'@���@��@�=q@��T@�&�@̛�@�Z@�9X@��@���@�\)@�+@�$�@ə�@��@ț�@�(�@Ǿw@Ə\@Ɨ�@�^5@�E�@�ff@�=q@�^5@�V@Ų-@�7L@�9X@Ý�@�ȴ@���@�x�@���@��@�r�@�Q�@�b@�C�@��y@���@���@�M�@���@�`B@�Ĝ@�G�@���@��;@�{@�@�?}@���@�\)@�C�@��H@���@��+@�E�@�@��@���@�V@��@��@�A�@��@�bN@�Q�@�(�@��@�C�@�~�@�=q@�{@���@�X@�X@�O�@��/@�1@�o@�5?@�-@���@��T@��^@��@���@���@�j@��@���@��@��\@�-@��T@���@��7@��@�O�@���@�Ĝ@��9@��@���@�t�@�33@�~�@�$�@�J@��-@�X@�/@��@��@���@��u@�1'@�9X@��
@���@�\)@�o@�
=@��R@�~�@�v�@�M�@�J@�@���@���@��^@��@��j@�j@�(�@�ƨ@�|�@�t�@�S�@�+@��@�ȴ@��@��/@��@�A�@� �@��@�(�@� �@��@���@��P@�\)@�C�@���@�ȴ@���@�ff@���@�7L@��@��@�%@��`@��9@���@��u@���@��D@�I�@�1'@�  @��
@��w@��F@���@�K�@��H@�M�@���@�X@�X@�G�@�7L@��@��D@�z�@�Q�@�A�@�1@�ƨ@�o@���@��@���@���@���@��\@�V@�$�@��T@��7@�7L@���@��j@���@�j@�|�@��R@���@�~�@�=q@��@�J@��@���@��^@��@�G�@��@�I�@�1'@�j@��@��F@�t�@��@���@��\@��@��#@��^@��-@�`B@���@��@���@��@�z�@�1@��w@��@��F@���@�K�@��@�~�@���@��^@���@��h@�hs@��@��D@�bN@�Z@�Q�@�A�@��
@�;d@�K�@�+@��y@��@�@���@�hs@�/@�Ĝ@�Z@�A�@�A�@
=@~��@~V@}�T@}O�@|�/@|z�@|I�@{�F@z^5@z-@y�#@y�#@{��@{��@{33@z�H@z=q@yX@x��@xr�@xA�@w�@w��@w;d@v�y@vV@u@uO�@u?}@u/@t��@t��@tI�@t1@s��@s"�@r�H@r�!@rM�@q��@q7L@pĜ@pA�@o�w@o;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԟ�Aԡ�AԲ-A�ƨA�ĜA�ƨA�A�A���A�AԼjA�ƨA���AԾwAԸRAԾwAԾwAԼjA���A�A�ƨA�ĜA�ȴA�ĜA���A�ĜA�ƨA�ƨA�ĜA�ȴA�ĜA�ƨA�ƨA�ƨA���A�ĜA�A�ĜA���A���A�A���A�AԼjA�ȴA���A�ĜA���AԾwA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A��A���A��
A���A���A���A���A���A��
A���A��
A��
A��
A��A��
A��#A��
A��#A���A��#A���A��
A���A��
A��
A��
A��A���A��#A���A���A���A���A���A���A���A���A��#A���A��A��
A��A��
A���A��A���A��#A���A��A���A���A���A���A���A���A��;A��HA��/A��#A��A��/A��
A��/A��A��/A��#A��#A��/A��A���A���A���A���A���A���A�ƨA���A�ȴA���A�ȴA�ȴA�A���A�ĜAԾwA�A�ƨA���A�ĜA�ĜA�ĜA�ȴA�A�ĜAԾwA�AԼjAԺ^AԺ^AԺ^AԺ^AԴ9AԺ^AԶFAԺ^AԶFAԺ^AԶFAԸRAԺ^AԶFAԺ^AԴ9AԸRAԴ9AԶFAԲ-AԶFAԴ9AԶFAԸRAԴ9AԸRAԴ9AԸRAԸRAԶFAԾwAԶFAԾwAԼjAԼjA���AԾwA�A���A���A�AԾwA�ĜAԾwA�ĜAԾwA�A�ĜA�A�ĜA�A�ƨA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�A�ƨA�ĜA�ĜA�ĜA�A�ƨA���A�ƨA���A�ĜA�ĜA���A�ĜA���A�ĜA���AԾwA�AԾwAԾwAԼjAԸRAԾwAԸRAԾwA���A���A�A���A���A�ĜA���A�ĜA�ĜA�A�ȴA�ĜA�ĜA���A���A���AԺ^AԾwAԾwAԸRAԾwAԼjAԼjA�AԾwA�A�ĜA�ƨA���A�A�ĜA�AԾwAԸRAԲ-AԸRAԲ-AԴ9AԶFAԲ-AԶFAԴ9A԰!AԴ9A԰!A԰!AԴ9AԲ-AԲ-AԶFA԰!AԴ9AԲ-AԲ-AԶFAԴ9AԸRAԸRAԴ9AԸRAԾwAԾwA�ĜAԾwA�ĜA���A�ƨA���A���A���A���A�ƨA�ȴA���A�ƨA�ĜA���AԺ^A���AԼjAԼjA�AԾwA�ĜA�A���A�ĜAԾwA�A�AԾwA�ĜAԾwAԺ^AԺ^AԸRAԶFAԺ^AԶFAԺ^AԸRAԴ9AԸRAԴ9AԮA԰!Aԡ�A�p�A�S�A��A��A�z�A���A�E�Aϗ�A�I�A��TA�$�A��`A���A���A�ƨAɛ�A�x�A�jA�S�A�
=A�ȴAȋDA�\)A�9XA��A�A��TA���AǺ^AǑhA�S�A�oA��A��#A���Aƣ�A�ZAũ�A��APA��\A�7LA���A���A�v�A�^5A�/A��A�;dA��;A���A��FA���A���A��7A�p�A�^5A�C�A�"�A���A��yA��/A��A���A���A���A���A�ȴA�ĜA��RA���A��A�E�A�"�A���A���A���A�\)A��A���A�1'A�bA���A��9A�z�A�1'A��A���A�n�A�VA��#A�l�A��-A�;dA���A�+A�O�A�
=A��`A��jA�ffA��
A��!A���A��DA�|�A�VA�+A���A��+A�`BA�A�A�$�A���A��RA���A���A��7A�p�A�?}A�%A��mA��HA��`A���A���A�x�A�p�A�hsA�C�A��A��hA�bNA�=qA�&�A�
=A��mA���A��A�x�A�E�A��A���A��/A���A��9A���A�z�A�^5A�C�A�5?A�(�A��A�1A��A��HA���A��jA���A�l�A�\)A�E�A�1A�A���A���A���A���A��A��yA��A���A�A��9A���A��+A�p�A�bNA�M�A�33A�1A��A��RA���A�v�A�dZA�XA�K�A�C�A�?}A�33A� �A��A�bA�A���A���A��A��yA��TA���A���A�~�A�x�A�p�A�`BA�S�A�9XA��A���A��TA�A���A�v�A�Q�A�(�A�A��A���A�p�A�=qA�bA��A��TA��/A���A��FA��A���A���A���A�x�A�S�A�9XA�33A�(�A��A�{A�oA�VA���A��A��A��mA��;A���A���A�ƨA��-A���A���A���A��PA��+A��7A��7A��A�|�A�v�A�l�A�M�A�=qA�A�A�;dA�;dA�7LA�  A��/A��RA���A�ffA�  A��DA�XA�7LA�$�A�%A��;A���A��wA��A���A���A��A�p�A�\)A�O�A�I�A�C�A�;dA�33A�"�A��A�%A�  A��yA���A��RA��A���A�ffA�XA�I�A�/A��A��A���A��FA��PA�n�A�M�A��A�VA���A��A��HA��A���A���A�ƨA��wA��RA��A��DA�~�A�t�A�p�A�XA�K�A��A���A�ffA�G�A�(�A��A�
=A��A���A���A�S�A�-A�JA���A��HA���A���A��PA�|�A�p�A�C�A��;A���A�|�A�ffA�E�A�9XA�+A�{A�A���A��A��`A��/A��
A���A��^A��A���A��\A�v�A�ffA�ZA�E�A�-A�VA��;A��wA���A�ffA�A�A�+A��A���A���A���A�n�A�;dA�bA�A���A��A��A��A��mA��mA��`A��/A��FA��A��A���A��hA�x�A�\)A�9XA��A�JA��A��A��A��\A�|�A�jA�XA�S�A�S�A�M�A�I�A�?}A�?}A�7LA�-A�+A�&�A��A�{A�VA�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AԸRA�A���AԾwAԾwA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�A�ĜA���A���A���A���A���A���A���A��
A��
A��
A���A��
A���A��A��#A���A���A�A�ĜAԼjAԸRAԸRAԶFAԸRAԼjA���A�A�ĜA�ĜA�ĜA���AԼjA�A�AԾwA���A���AԴ9AԲ-AԲ-AԸRA�ĜA���A���A���A���AԸRAԲ-A�^5A��HAɗ�AȅAǮAƣ�A��A���A���A�bA���A�dZA��A�I�A�XA�S�A���A�t�A�t�A��uA�A�A�A�&�A�bNA�;dA���A�XA���A�JA���A��A�bA�/A�33A���A�-A�r�A��`A��`A��A���A���A�  A�I�A��A�
=A�  A�^5A��A��HA�33A��`A�A��PA��uA�\)A�C�A��!A��7A�1'A���A��A���A�E�A���A�5?A|r�AyXAx��AxjAx{At�+Aq�AoC�Ak�AiXAhjAdn�A`^5A]/A\AY�mATbAR�AQ��AQ�AL �AIAG%AE�AD�A@=qA=��A<-A;`BA;A9�A9�A8�/A8A�A7+A5�PA4r�A2��A1�FA0��A.n�A,ZA+|�A*�`A)ƨA(�A(bA'�PA'x�A&�RA%�A$=qA#��A"�uA!��A �A��A�A5?A&�A?}A�mAoA�AoA��A�yA�Ar�A�jA"�A"�A��A�mA%A��A�RA�hA�DA��A
ffA��AA�A��Al�A�A�AȴA�;A�HAQ�A�mA+A v�A E�@���@��@��;@���@�l�@���@��T@���@�ff@���@�%@�bN@�@�$�@�bN@�E�@�@���@��@���@�@�@��H@��@��T@���@��#@�p�@��@� �@�P@�j@��@�@�&�@�M�@��@�ƨ@�1'@�bN@ܓu@�r�@ܣ�@ܬ@�o@�1@�p�@�hs@�  @ӍP@�dZ@�"�@ҸR@ҧ�@���@��y@�E�@�5?@��@���@�7L@У�@�1'@���@��@�=q@��T@�&�@̛�@�Z@�9X@��@���@�\)@�+@�$�@ə�@��@ț�@�(�@Ǿw@Ə\@Ɨ�@�^5@�E�@�ff@�=q@�^5@�V@Ų-@�7L@�9X@Ý�@�ȴ@���@�x�@���@��@�r�@�Q�@�b@�C�@��y@���@���@�M�@���@�`B@�Ĝ@�G�@���@��;@�{@�@�?}@���@�\)@�C�@��H@���@��+@�E�@�@��@���@�V@��@��@�A�@��@�bN@�Q�@�(�@��@�C�@�~�@�=q@�{@���@�X@�X@�O�@��/@�1@�o@�5?@�-@���@��T@��^@��@���@���@�j@��@���@��@��\@�-@��T@���@��7@��@�O�@���@�Ĝ@��9@��@���@�t�@�33@�~�@�$�@�J@��-@�X@�/@��@��@���@��u@�1'@�9X@��
@���@�\)@�o@�
=@��R@�~�@�v�@�M�@�J@�@���@���@��^@��@��j@�j@�(�@�ƨ@�|�@�t�@�S�@�+@��@�ȴ@��@��/@��@�A�@� �@��@�(�@� �@��@���@��P@�\)@�C�@���@�ȴ@���@�ff@���@�7L@��@��@�%@��`@��9@���@��u@���@��D@�I�@�1'@�  @��
@��w@��F@���@�K�@��H@�M�@���@�X@�X@�G�@�7L@��@��D@�z�@�Q�@�A�@�1@�ƨ@�o@���@��@���@���@���@��\@�V@�$�@��T@��7@�7L@���@��j@���@�j@�|�@��R@���@�~�@�=q@��@�J@��@���@��^@��@�G�@��@�I�@�1'@�j@��@��F@�t�@��@���@��\@��@��#@��^@��-@�`B@���@��@���@��@�z�@�1@��w@��@��F@���@�K�@��@�~�@���@��^@���@��h@�hs@��@��D@�bN@�Z@�Q�@�A�@��
@�;d@�K�@�+@��y@��@�@���@�hs@�/@�Ĝ@�Z@�A�@�A�@
=@~��@~V@}�T@}O�@|�/@|z�@|I�@{�F@z^5@z-@y�#@y�#@{��@{��@{33@z�H@z=q@yX@x��@xr�@xA�@w�@w��@w;d@v�y@vV@u@uO�@u?}@u/@t��@t��@tI�@t1@s��@s"�@r�H@r�!@rM�@q��@q7L@pĜ@pA�@o�wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aԟ�Aԡ�AԲ-A�ƨA�ĜA�ƨA�A�A���A�AԼjA�ƨA���AԾwAԸRAԾwAԾwAԼjA���A�A�ƨA�ĜA�ȴA�ĜA���A�ĜA�ƨA�ƨA�ĜA�ȴA�ĜA�ƨA�ƨA�ƨA���A�ĜA�A�ĜA���A���A�A���A�AԼjA�ȴA���A�ĜA���AԾwA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A��A���A��
A���A���A���A���A���A��
A���A��
A��
A��
A��A��
A��#A��
A��#A���A��#A���A��
A���A��
A��
A��
A��A���A��#A���A���A���A���A���A���A���A���A��#A���A��A��
A��A��
A���A��A���A��#A���A��A���A���A���A���A���A���A��;A��HA��/A��#A��A��/A��
A��/A��A��/A��#A��#A��/A��A���A���A���A���A���A���A�ƨA���A�ȴA���A�ȴA�ȴA�A���A�ĜAԾwA�A�ƨA���A�ĜA�ĜA�ĜA�ȴA�A�ĜAԾwA�AԼjAԺ^AԺ^AԺ^AԺ^AԴ9AԺ^AԶFAԺ^AԶFAԺ^AԶFAԸRAԺ^AԶFAԺ^AԴ9AԸRAԴ9AԶFAԲ-AԶFAԴ9AԶFAԸRAԴ9AԸRAԴ9AԸRAԸRAԶFAԾwAԶFAԾwAԼjAԼjA���AԾwA�A���A���A�AԾwA�ĜAԾwA�ĜAԾwA�A�ĜA�A�ĜA�A�ƨA�ĜA�ƨA�ĜA�ĜA�ƨA�ĜA�ƨA�A�ƨA�ĜA�ĜA�ĜA�A�ƨA���A�ƨA���A�ĜA�ĜA���A�ĜA���A�ĜA���AԾwA�AԾwAԾwAԼjAԸRAԾwAԸRAԾwA���A���A�A���A���A�ĜA���A�ĜA�ĜA�A�ȴA�ĜA�ĜA���A���A���AԺ^AԾwAԾwAԸRAԾwAԼjAԼjA�AԾwA�A�ĜA�ƨA���A�A�ĜA�AԾwAԸRAԲ-AԸRAԲ-AԴ9AԶFAԲ-AԶFAԴ9A԰!AԴ9A԰!A԰!AԴ9AԲ-AԲ-AԶFA԰!AԴ9AԲ-AԲ-AԶFAԴ9AԸRAԸRAԴ9AԸRAԾwAԾwA�ĜAԾwA�ĜA���A�ƨA���A���A���A���A�ƨA�ȴA���A�ƨA�ĜA���AԺ^A���AԼjAԼjA�AԾwA�ĜA�A���A�ĜAԾwA�A�AԾwA�ĜAԾwAԺ^AԺ^AԸRAԶFAԺ^AԶFAԺ^AԸRAԴ9AԸRAԴ9AԮA԰!Aԡ�A�p�A�S�A��A��A�z�A���A�E�Aϗ�A�I�A��TA�$�A��`A���A���A�ƨAɛ�A�x�A�jA�S�A�
=A�ȴAȋDA�\)A�9XA��A�A��TA���AǺ^AǑhA�S�A�oA��A��#A���Aƣ�A�ZAũ�A��APA��\A�7LA���A���A�v�A�^5A�/A��A�;dA��;A���A��FA���A���A��7A�p�A�^5A�C�A�"�A���A��yA��/A��A���A���A���A���A�ȴA�ĜA��RA���A��A�E�A�"�A���A���A���A�\)A��A���A�1'A�bA���A��9A�z�A�1'A��A���A�n�A�VA��#A�l�A��-A�;dA���A�+A�O�A�
=A��`A��jA�ffA��
A��!A���A��DA�|�A�VA�+A���A��+A�`BA�A�A�$�A���A��RA���A���A��7A�p�A�?}A�%A��mA��HA��`A���A���A�x�A�p�A�hsA�C�A��A��hA�bNA�=qA�&�A�
=A��mA���A��A�x�A�E�A��A���A��/A���A��9A���A�z�A�^5A�C�A�5?A�(�A��A�1A��A��HA���A��jA���A�l�A�\)A�E�A�1A�A���A���A���A���A��A��yA��A���A�A��9A���A��+A�p�A�bNA�M�A�33A�1A��A��RA���A�v�A�dZA�XA�K�A�C�A�?}A�33A� �A��A�bA�A���A���A��A��yA��TA���A���A�~�A�x�A�p�A�`BA�S�A�9XA��A���A��TA�A���A�v�A�Q�A�(�A�A��A���A�p�A�=qA�bA��A��TA��/A���A��FA��A���A���A���A�x�A�S�A�9XA�33A�(�A��A�{A�oA�VA���A��A��A��mA��;A���A���A�ƨA��-A���A���A���A��PA��+A��7A��7A��A�|�A�v�A�l�A�M�A�=qA�A�A�;dA�;dA�7LA�  A��/A��RA���A�ffA�  A��DA�XA�7LA�$�A�%A��;A���A��wA��A���A���A��A�p�A�\)A�O�A�I�A�C�A�;dA�33A�"�A��A�%A�  A��yA���A��RA��A���A�ffA�XA�I�A�/A��A��A���A��FA��PA�n�A�M�A��A�VA���A��A��HA��A���A���A�ƨA��wA��RA��A��DA�~�A�t�A�p�A�XA�K�A��A���A�ffA�G�A�(�A��A�
=A��A���A���A�S�A�-A�JA���A��HA���A���A��PA�|�A�p�A�C�A��;A���A�|�A�ffA�E�A�9XA�+A�{A�A���A��A��`A��/A��
A���A��^A��A���A��\A�v�A�ffA�ZA�E�A�-A�VA��;A��wA���A�ffA�A�A�+A��A���A���A���A�n�A�;dA�bA�A���A��A��A��A��mA��mA��`A��/A��FA��A��A���A��hA�x�A�\)A�9XA��A�JA��A��A��A��\A�|�A�jA�XA�S�A�S�A�M�A�I�A�?}A�?}A�7LA�-A�+A�&�A��A�{A�VA�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BѷB�TB�TBҽBҽB� B� B�TB҉B�TB҉BҽBҽB�TB҉B҉B�TB�TB��B�TB� B� B� B��B�TBҽB� BѷB�NB�HB��B��B�BBϫBϫB�vB��B�B� B�TB�&B�&BҽB҉B�NB҉B�[B҉BҽB��BбB�}BбB��BӏB��B��B��B҉B��B˒B�B��B��B�B��B��B�_B��B�PB�~B�+B�_B��B}"ByrBncB]�BXEBN�BGEB@B4�B+�B#�B�BoBDB��B��B�TB�oB�DB�vBخB��B� B�qB��B�B��Bv�Bl�Be,BX�BS[BF?B6�B+kB�B
�B
��B
�zB
�B
�DB
�	B
�SB
�{B
{B
r|B
^�B
W�B
R�B
K^B
HB
?�B
9�B
-�B
B
VB
DB
YB	��B	�B	�EB	�9B	�?B	��B	�B	�B	s�B	i�B	b�B	M�B	=B	:�B	5�B	-CB	!B	�B	�B	�B�PB��B��B��B�B�B��B�`B��B�B�BߤB��B�5B�)B�B�2B�B�`B�&B�NB�B�)B�B�KB�B�TB�B�6B�^B�#B�3B��B��B��B�qB�B�#B�B�NB�mB��B�4B�aB�3B�9B��B��B�BB�B�B�B��B�B�;B��BںB�#B�B�B��B��B�pB�#B�WBںB�KBٴB��B��B�NB��B�|B�vB�BB�B�BB�HB�B�2B�
B�>B�B�yB��B�>B�yB��B�cB�B��B�"B��B��B	�B	�B	fB	
�B	�B	bB	B	 'B	)�B	'�B	#�B	 �B	B	�B	!�B	&LB	,=B	0�B	6FB	7�B	4�B	/�B	'B	/OB	.B	.B	/�B	0!B	1'B	0�B	2aB	7�B	>BB	>BB	>�B	A�B	B�B	FB	HB	H�B	MjB	PB	Q�B	T�B	T�B	U�B	VmB	X�B	\�B	b�B	c�B	g8B	gmB	gB	h�B	h�B	kQB	n�B	sMB	zDB	~(B	��B	��B	�~B	�~B	�B	�"B	��B	�B	�"B	�.B	��B	��B	��B	��B	�+B	��B	��B	�1B	�B	��B	�\B	��B	�OB	��B	��B	��B	��B	��B	��B	�0B	��B	�*B	�6B	��B	�=B	�B	�!B	��B	�3B	��B	�B	�LB	��B	��B	��B	�-B	��B	�3B	�tB	�)B	�B	��B	�B	� B	�TB	�&B	�
B	��B	�KB	�#B	�#B	�/B	��B	�B	��B	�B	�HB	�|B	�vB	�B	�B	�B	�B	��B	�&B	�ZB	�B	�ZB	�`B	��B	�fB	�2B	�B	�B	�QB	��B	�/B	�/B	�B	�B	��B	�B	�TB	�+B	��B	��B	�2B	�fB	�lB	��B	�	B	�>B	�B	�B	��B	��B	��B	��B	��B	��B
{B
�B
�B
�B
�B
�B
�B
%B
%B
�B
�B
�B

	B

rB
%B
%B
%B
�B
+B
�B

	B

=B

=B
B
~B
B
�B
�B
"B
�B
"B
PB
�B
�B
\B
(B
�B
�B
�B
4B
�B
�B
�B
@B
�B
�B
�B
uB
�B
@B
�B
bB
bB
4B
oB
�B
�B
�B
�B
�B
�B
{B
�B
B
@B
�B
�B
eB
�B
�B
=B
	B
�B
B
�B
xB
�B
�B
�B
�B
~B
VB
�B
�B
 �B
!-B
!�B
"hB
"hB
"hB
!�B
"hB
!bB
 �B
#:B
$tB
%FB
%B
$tB
#nB
#�B
"�B
!�B
!�B
"4B
"�B
#nB
#�B
&B
&�B
&�B
'�B
'B
(XB
*eB
+�B
,B
,�B
-B
-�B
-�B
.�B
/�B
/�B
/�B
/OB
/�B
0�B
0�B
0�B
1'B
/�B
1[B
2aB
2-B
2-B
1�B
2�B
3hB
3hB
3hB
4B
4nB
4�B
3�B
2aB
2�B
2�B
2�B
3hB
3�B
33B
49B
5?B
5�B
7B
7�B
5�B
5tB
5�B
5�B
6FB
6�B
7�B
8�B
9$B
9XB
9�B
9�B
9�B
:*B
:�B
:�B
;0B
;�B
<B
<jB
=B
=qB
=�B
>B
>�B
?}B
@�B
AUB
A�B
B[B
B�B
C-B
B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�&B�[BӏBуB҉BѷBҽBѷB��BуB��B�B�&BуB��B҉B҉BҽBѷB҉BуBҽB�NBӏBѷBӏB�TB� B��B�NBҽB�TBѷB�&B�NB��B�&B�TB��B҉BҽB�[BҽB�,B��B��B��B��B�&B�vB��BѷB�[BуBҽB��B�&B��B��B�&B��B�[BѷB�[BуB�&BѷB�[B�NB�[BуBҽB�TB��B��B�B�&B� B�TB��B��B� B��BҽB�B�&B�B��BуBҽB��B�,BбB��B�NB��BѷB��B��BуB��B�NB�[B�B�[BуBҽB�NB�TBѷBуB�TB�HB��B�B�&B��B�TB��B��BҽBѷB��B�HB�,B��BѷB�TBѷB��B�HB�,BуBӏBѷB�[BҽB�TBԕB� B��B��BбB� B�BҽB��B��B�B҉B�NB��B��BуB��B��B�NBϫB�B�NBϫB�TB�BуB��B�B�}B�BBуBΥB��B��B�BбB�vB�vB�BB�B��B��BѷB�BбBΥB�BB�BB�B�BΥB�}B�pB�HBϫB�}BϫBΥB�HB��B��BΥBбB��B�HBΥBϫB�vB�B�B��BбB�BϫB��B�vB�B�B��B��B�B��BѷB�TB҉BуBҽB�B�[BуB�[B�NB�TB� B� BӏBѷBӏB��BӏB�[B�&B��B�TB�aB�TB��B�&B�&BӏB��B��BѷB��B�TB�TB�&B� B�,BуB� B� B��BӏB�NBбB� B�HBҽB��BуB�&BуBӏBҽBуB�[BҽB��B�,B�NB��B�&BӏB�[BҽB��B� BуBӏB�NB�TB�TB��BҽB҉B҉BԕB��B�B�TB��B՛B�,B��B�HBѷBбBϫBуB�BбB��B��B�B��B�vB��B�B�vBѷB��B��B��BϫB��BѷB�}B� BбBбB҉B�TB��BҽB�[B�mB�aB�aB�2B��B��B�2BӏB՛B�2B��B��BѷB�TB҉BуB�aB�TBҽB��BѷB��B� B�TB��B�BҽB��B�B�B�NB��B��B�pB�pB�<B��B�)B��B��B�KBуB��B҉B��B��B.�B;0B+�B��B˒B��B�VB��B��B��B�{B��B�B��B��B��B��B�oB�hB��B� B�4B�bB��B�@B��B��B�~B�B��B�B��B��B�HB�/B�IB��B�SB��B��B�(B��B��B��B��B�.B��B��B��B��B��B�PB��B��B��B�	B��B�1B��B��B��B�_B�_B�YB�SB��B�_B�JB�_B�YB��B��B��B��B�lB�B~�B{BzB|B|�B�;By�By>BxlBr�B{JB��Bk�BqAB�Bx8Bc B]dBd�BiyBk�B\)BYKBY�BW�BY�B]�BaHB]/BU�BS�BS�BS�BX�BMBJ�BLdBOvBM�BP}BIBFtBDgBI�BS�BE�BCaBA�BF?BK�BN�BD�B@�B?}BA B>BB>B>�B@B?}B=B9�B8�B7�B8�B8�B7�B6�B7�B1[B33B1�B1�B2aB.}B0�B/�B/�B5?B)�B3�B.B(XB)�B)*B(XB'RB(�B'B&�B+�B%FB%�B&�B&�B!-B!bB!�B"�B#�B 'B�BOB�B�B�B�B�B�B�B�BBBB�BoBuBB�B:BBMB�B(B�B�B�B�B�B"BBBB
	B	B�B1BYB�B�BAB �B�B�xB�]B��B�B�DB�JB��B��B�]B�JB�lB��B��B��B��B��B��B�	B�fB��B�`B��B�`B��B��B��B�B��B�B�MB�B�B��B�GB��B�|B�B�B�)B�B�/B��B��B�oB��B�WB�B��B�lB�B�sB�DB�B�B�`B�&B�TB��B��B�B�NB�B�B�;B�5B�B�BߤBޞB�pBޞB�pB�jB�WB��B��BޞB�B�?B�B��BӏB�NB͟BѷB�B�0BҽB̘B��BɆB��B��B�zB�?B�KB�zB��B��B�?B�B��B�qB��B�B�mB�KB�}B��B�B��B��B��B�-B�nB��B��B�$B�B�B��B�B��B��B��B��B��B��B� B��B�B��B�"B��B�VB�~B�B�B��B�	B�=B�xB��B�B��B��B�DB�B��B�B�=B��B�1B�;B�AB|BzDBy�By	Bu�B{�Bv�Bv�BtTBo�Bn�Bm�BncBpoBm�Bl"BlWBncBv`BiyBgmBiBjBh>BiBg�Bg�Bb�Bb�Bc�Bd�B_�B]/BbB[�B[�BW�BXyBXyB\)BW
BZBT�BT�BU2BWsBS�BT�BT�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                    44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                    44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022121512020120221215120201IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022121615012120221216150121QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022121615012120221216150121QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                