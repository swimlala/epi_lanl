CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  "   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-12-05T16:03:27Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue                    W�Argo profile    3.1 1.2 19500101000000  20221205160327  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_239                 6810_008521_239                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�'��o@�'��o11  @�'���@�'���@2z�3�	@2z�3�	�d��+�ʬ�d��+�ʬ11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?�@:�H@�  @�  @�  @޸RA   A��A ��A+�A@  A`  A\)A�  A�  A�Q�A�Q�A�Q�A�Q�A�  B   B  B�
B  B (�B(  B/�
B8  B?�
BH  BP(�BX(�B`  Bh  Bo�
Bw�B�  B��B��B�  B�  B�  B�  B�  B�  B�{B�  B��
B��
B��B��B�  B��B�  B�  B�{B�  B��
B�{B�  B��B��B�  B�{B�(�B�(�B�{B�  B�C��C��C��C  C
  C��C
=C  C  C
=C
=C  C��C
=C  C   C"  C$
=C&  C'��C*
=C,  C-��C0
=C2
=C3��C5��C7��C:  C;��C=��C@
=CB  CD  CF
=CH  CJ  CK��CM��CP
=CR  CT  CV  CX  CZ  C[�C]��C_��Ca��Cc��Ce��Ch  Cj
=Cl  Cn  Cp  Cr  Ct
=Cv  Cx  Cz  C|  C~
=C�  C�C���C���C�  C�
=C�  C�C�
=C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�  C���C���C���C�C�C�C�C�  C���C���C�  C�  C�C�C���C���C�  C�C���C���C���C�  C�C�  C�C�  C���C�  C�C���C���C���C���C���C�  C���C���C���C���C�  C�  C�  C�  C�  C�  C���C�  C�  C�C���C���C�  C���C�  C�  C�  C�C�  C�  C�  C�
=C�C�C�C�C�  C���C���C�C�
=C�
=C�C�  C�C�  C�  C�C�  C�C�
=C�  C���C���C�  C�  C�  C�  C�C�C�  C�C���C���C�  C�  C�  C���C�  C�  C���C���C�  C���C�  C�  C�C�D   D }qD ��D}qD  D��D�D� D  D}qD  D��D  D��D�D}qD�qD}qD	  D	� D	�qD
}qD  D� D  D}qD  D��D  D� D�qD� D�D��DD�D�D� D�qDz�D  D��D  D}qD�qD� D  D� D  D� D  D}qD�qD}qD��D� D�D��D�D��D�D� D  D� D   D � D!  D!}qD!��D"}qD"�qD#��D$�D$}qD$�qD%}qD%�qD&z�D&��D'��D(�D(� D)  D)}qD*�D*}qD*�qD+� D,�D,��D-  D-� D-�qD.z�D/  D/}qD/�qD0��D1�D1��D2�D2}qD3  D3��D4  D4}qD4��D5}qD6  D6}qD6�qD7��D7�qD8}qD9�D9��D9�qD:}qD;  D;}qD<  D<� D<�qD=}qD=�qD>}qD>�qD?z�D?��D@� DADA� DA��DB}qDB�qDCz�DD  DD��DE  DE� DF�DF��DG�DG� DH�DH}qDH�qDI� DJ  DJ}qDK  DK��DL�DL��DM�DM� DM�qDN� DO  DO}qDO�qDP� DQ  DQ� DR  DR� DS�DS��DT�DT� DU�DU��DV  DV}qDV��DW}qDX  DX� DY  DY� DZ  DZ��D[�D[��D\  D\� D]  D]��D^D^��D_  D_��D`D`� D`�qDaz�Da�qDb}qDb�qDcz�Dc�qDd� Dd��Dez�De�qDfz�Dg�Dg}qDg�qDh�DiDi��Di�qDj� Dj�qDk}qDl�Dl�Dm�Dm}qDm�qDn� Do�Do��Dp  Dp}qDq  Dq� Dq�qDr}qDr�qDs}qDs�qDtz�Dt�RDuz�Dv  Dv��DwDw� Dx�Dx��Dx�qDy}qDz  Dz��D{  D{��D|  D|}qD}  D}�D~D~�D  D}qD�qD�@ D���D�D�  D�=qD��HD��HD���D�@ D�� D�� D�HD�>�D�~�D�D��D�AHD�� D�� D��D�AHD�� D�� D�HD�@ D�� D��HD�  D�@ D�� D�� D�HD�AHD��HD�� D���D�AHD�~�D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�G�D�� D�� D�  D�>�D�~�D��HD�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D���D�@ D��HD�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D��D�!HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�?�?aG�?�=q?�Q�?�G�?�@�@(��@333@L��@W
=@p��@�  @��@�z�@��H@��
@�\)@�@�  @���@��@޸R@��@��@��HAG�A�A�A��AA��A   A$z�A(��A.�RA1�A8��A;�A@��AG
=AJ=qAO\)ATz�AW�A]p�Ab�\AfffAl��Ap��AuA{�A~�RA��A���A��RA���A��
A�A���A�=qA��A��A�G�A�(�A�ffA�  A�33A��A��A�=qA�(�A�ffA�G�A�33A���A�  A�=qA��
A�
=A���AÅA�AǮA��HA�z�AϮA�G�A�(�A�ffA�  AۅA��A�  A��A�z�A�\)A���A�(�A�A���A��HA��A�Q�A��A��A�
=B ��B{B
=B��B��B�HB(�B��B
�\B�Bz�B{B�HBQ�BG�BffB  B��B=qB\)BQ�B�B�RBQ�B��BffB   B ��B"=qB#�B$z�B&{B&�HB(Q�B)��B*�\B,(�B-�B.ffB/�B0z�B2=qB333B4Q�B5B6�\B8  B9�B:=qB;�
B<��B=�B?�B@Q�BA�BB�HBD  BE��BFffBH  BIG�BJ=qBK�
BM�BM�BO�BP��BQ�BS\)BT(�BU��BW
=BX  BYp�BZ�\B[�B]G�B^{B_�B`��Ba��Bc
=Bdz�BeG�Bf�HBh  Bi�Bj�RBk�BmG�Bn=qBo�Bp��Bq�Bs\)Btz�BuBw\)Bx(�ByBz�HB|(�B}��B~�\B�
B��RB��B��
B��\B�
=B��
B�ffB���B�B�Q�B��HB��B�=qB���B���B�  B���B�p�B��B���B�G�B��
B��\B��B���B�ffB�
=B��B�{B��HB�\)B��B��RB�G�B�B�z�B���B�p�B�=qB��RB�33B��B�ffB���B�B�(�B��HB���B�  B��\B�p�B�B�z�B��B���B�Q�B���B�\)B�{B��RB��B�B�z�B���B�p�B�=qB���B�33B��B�z�B��HB���B�(�B���B�G�B�  B��\B�
=B��B�ffB���B�p�B�=qB��RB�33B�  B�ffB��B��
B�=qB��HB���B�  B���B�p�B��B�ffB��B��
B�=qB���B���B�  B��\B�33B��B�Q�B��HB���B�{B�z�B�33B��
B�=qB���BŅB��
B�Q�B��BǙ�B�{B���B�\)B��
Bʏ\B���B˅B�=qB��HB�\)B��BΏ\B��HBϮB�=qBЏ\B�33B��
B�Q�BҸRB�33B��B�z�B��HB�p�B�{B�z�B���Bי�B�{B�z�B��BٮB�  Bڣ�B�\)B��
B�Q�B�
=B�B�=qB޸RB߅B�(�B��B�33B��B�z�B���B�B�Q�B���B�p�B�{B�\B�\)B��
B�z�B�33B陚B�(�B���B�p�B�  B���B�\)B��
B��B�33BB�Q�B�
=B�p�B��B��B�\)B��
B�Q�B�
=B��B�(�B��HB���B�  B���B��B�  B���B�p�B��B��\B�\)B�  B�z�B�G�C   C G�C �\C
=CQ�C�\C  CG�C�\C  CG�C�\C  CG�C�\C  C\)C��C  CffCC
=CffC��C�CffC�RC	�C	p�C	�C	��C
G�C
��C
�HC�CffCC
=C=qCp�C�RC  CG�CffC��C�HC{C33Cp�C��C�RC��C=qCG�Cz�C�RC�
C��C=qCQ�Cz�C�RC�
C
=CG�C\)C�CC�C
=C33Cz�C��C�RC��C�C=qCffC��C��C�HC(�CQ�CffC��C��C�HC{CQ�CffC��CC  C{C33CffC��C��C�HC
=C=qCp�C�C�C�C
=C�CQ�C�\C��CC  C(�C=qCz�C�C�RC�
C
=C=qCp�C��C�C�HC�C=qC\)Cz�C�RC�HC
=C�CG�C�C�RC�
C�C(�C\)CffC��C�
C��C
=C=qCz�C�\C�C�HC{C(�CG�Cz�C�C�
C��C 
=C =qC z�C �\C ��C �HC!{C!(�C!G�C!p�C!�C!��C!��C"{C"(�C"ffC"�\C"�C"��C"�C#(�C#Q�C#�C#��C#C#�HC$  C$�C$\)C$�C$��C$�RC$�HC%
=C%33C%p�C%��C%�C%��C&
=C&33C&Q�C&p�C&�\C&C&��C'(�C'=qC'\)C'�C'C'�C(
=C((�C(Q�C(�C(C(�
C)  C)=qC)p�C)�\C)�C)��C*�C*=qC*p�C*�RC*�
C+  C+=qC+p�C+��C+C+�HC,
=C,33C,ffC,��C,�
C-
=C-33C-G�C-z�C-�RC-�C.�C.=qC.ffC.�\C.�RC.��C/(�C/\)C/�\C/C/�C0{C0G�C0�\C0C0�HC1
=C1G�C1�C1C1�C2�C2G�C2ffC2��C2�
C3{C3G�C3z�C3��C3C3��C4G�C4p�C4�\C4C5{C5=qC5\)C5��C5�
C6{C633C6ffC6�\C6C7
=C7G�C7z�C7��C7��C7��C8(�C8ffC8��C8�HC9{C9=qC9ffC9�\C9�
C:�C:G�C:�C:��C:�
C;{C;\)C;�C;�C;�HC<{C<Q�C<��C<��C=  C=33C=p�C=��C=��C>  C>(�C>ffC>��C>�HC?�C?Q�C?�C?�C?�
C@{C@Q�C@�\C@��CA  CA33CA\)CA�CA�RCA�CB{CB\)CB�CB��CC  CC(�CCQ�CCz�CC�CC�HCD�CD\)CD�\CD�CD�
CE  CE33CEffCE��CE�HCE��CF�CF\)CF��CF�RCF�
CG
=CGQ�CG�CG��CG��CG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  ?�@:�H@�  @�  @�  @޸RA   A��A ��A+�A@  A`  A\)A�  A�  A�Q�A�Q�A�Q�A�Q�A�  B   B  B�
B  B (�B(  B/�
B8  B?�
BH  BP(�BX(�B`  Bh  Bo�
Bw�B�  B��B��B�  B�  B�  B�  B�  B�  B�{B�  B��
B��
B��B��B�  B��B�  B�  B�{B�  B��
B�{B�  B��B��B�  B�{B�(�B�(�B�{B�  B�C��C��C��C  C
  C��C
=C  C  C
=C
=C  C��C
=C  C   C"  C$
=C&  C'��C*
=C,  C-��C0
=C2
=C3��C5��C7��C:  C;��C=��C@
=CB  CD  CF
=CH  CJ  CK��CM��CP
=CR  CT  CV  CX  CZ  C[�C]��C_��Ca��Cc��Ce��Ch  Cj
=Cl  Cn  Cp  Cr  Ct
=Cv  Cx  Cz  C|  C~
=C�  C�C���C���C�  C�
=C�  C�C�
=C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�  C���C���C���C�C�C�C�C�  C���C���C�  C�  C�C�C���C���C�  C�C���C���C���C�  C�C�  C�C�  C���C�  C�C���C���C���C���C���C�  C���C���C���C���C�  C�  C�  C�  C�  C�  C���C�  C�  C�C���C���C�  C���C�  C�  C�  C�C�  C�  C�  C�
=C�C�C�C�C�  C���C���C�C�
=C�
=C�C�  C�C�  C�  C�C�  C�C�
=C�  C���C���C�  C�  C�  C�  C�C�C�  C�C���C���C�  C�  C�  C���C�  C�  C���C���C�  C���C�  C�  C�C�D   D }qD ��D}qD  D��D�D� D  D}qD  D��D  D��D�D}qD�qD}qD	  D	� D	�qD
}qD  D� D  D}qD  D��D  D� D�qD� D�D��DD�D�D� D�qDz�D  D��D  D}qD�qD� D  D� D  D� D  D}qD�qD}qD��D� D�D��D�D��D�D� D  D� D   D � D!  D!}qD!��D"}qD"�qD#��D$�D$}qD$�qD%}qD%�qD&z�D&��D'��D(�D(� D)  D)}qD*�D*}qD*�qD+� D,�D,��D-  D-� D-�qD.z�D/  D/}qD/�qD0��D1�D1��D2�D2}qD3  D3��D4  D4}qD4��D5}qD6  D6}qD6�qD7��D7�qD8}qD9�D9��D9�qD:}qD;  D;}qD<  D<� D<�qD=}qD=�qD>}qD>�qD?z�D?��D@� DADA� DA��DB}qDB�qDCz�DD  DD��DE  DE� DF�DF��DG�DG� DH�DH}qDH�qDI� DJ  DJ}qDK  DK��DL�DL��DM�DM� DM�qDN� DO  DO}qDO�qDP� DQ  DQ� DR  DR� DS�DS��DT�DT� DU�DU��DV  DV}qDV��DW}qDX  DX� DY  DY� DZ  DZ��D[�D[��D\  D\� D]  D]��D^D^��D_  D_��D`D`� D`�qDaz�Da�qDb}qDb�qDcz�Dc�qDd� Dd��Dez�De�qDfz�Dg�Dg}qDg�qDh�DiDi��Di�qDj� Dj�qDk}qDl�Dl�Dm�Dm}qDm�qDn� Do�Do��Dp  Dp}qDq  Dq� Dq�qDr}qDr�qDs}qDs�qDtz�Dt�RDuz�Dv  Dv��DwDw� Dx�Dx��Dx�qDy}qDz  Dz��D{  D{��D|  D|}qD}  D}�D~D~�D  D}qD�qD�@ D���D�D�  D�=qD��HD��HD���D�@ D�� D�� D�HD�>�D�~�D�D��D�AHD�� D�� D��D�AHD�� D�� D�HD�@ D�� D��HD�  D�@ D�� D�� D�HD�AHD��HD�� D���D�AHD�~�D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�G�D�� D�� D�  D�>�D�~�D��HD�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D���D�@ D��HD�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�Q�?�?aG�?�=q?�Q�?�G�?�@�@(��@333@L��@W
=@p��@�  @��@�z�@��H@��
@�\)@�@�  @���@��@޸R@��@��@��HAG�A�A�A��AA��A   A$z�A(��A.�RA1�A8��A;�A@��AG
=AJ=qAO\)ATz�AW�A]p�Ab�\AfffAl��Ap��AuA{�A~�RA��A���A��RA���A��
A�A���A�=qA��A��A�G�A�(�A�ffA�  A�33A��A��A�=qA�(�A�ffA�G�A�33A���A�  A�=qA��
A�
=A���AÅA�AǮA��HA�z�AϮA�G�A�(�A�ffA�  AۅA��A�  A��A�z�A�\)A���A�(�A�A���A��HA��A�Q�A��A��A�
=B ��B{B
=B��B��B�HB(�B��B
�\B�Bz�B{B�HBQ�BG�BffB  B��B=qB\)BQ�B�B�RBQ�B��BffB   B ��B"=qB#�B$z�B&{B&�HB(Q�B)��B*�\B,(�B-�B.ffB/�B0z�B2=qB333B4Q�B5B6�\B8  B9�B:=qB;�
B<��B=�B?�B@Q�BA�BB�HBD  BE��BFffBH  BIG�BJ=qBK�
BM�BM�BO�BP��BQ�BS\)BT(�BU��BW
=BX  BYp�BZ�\B[�B]G�B^{B_�B`��Ba��Bc
=Bdz�BeG�Bf�HBh  Bi�Bj�RBk�BmG�Bn=qBo�Bp��Bq�Bs\)Btz�BuBw\)Bx(�ByBz�HB|(�B}��B~�\B�
B��RB��B��
B��\B�
=B��
B�ffB���B�B�Q�B��HB��B�=qB���B���B�  B���B�p�B��B���B�G�B��
B��\B��B���B�ffB�
=B��B�{B��HB�\)B��B��RB�G�B�B�z�B���B�p�B�=qB��RB�33B��B�ffB���B�B�(�B��HB���B�  B��\B�p�B�B�z�B��B���B�Q�B���B�\)B�{B��RB��B�B�z�B���B�p�B�=qB���B�33B��B�z�B��HB���B�(�B���B�G�B�  B��\B�
=B��B�ffB���B�p�B�=qB��RB�33B�  B�ffB��B��
B�=qB��HB���B�  B���B�p�B��B�ffB��B��
B�=qB���B���B�  B��\B�33B��B�Q�B��HB���B�{B�z�B�33B��
B�=qB���BŅB��
B�Q�B��BǙ�B�{B���B�\)B��
Bʏ\B���B˅B�=qB��HB�\)B��BΏ\B��HBϮB�=qBЏ\B�33B��
B�Q�BҸRB�33B��B�z�B��HB�p�B�{B�z�B���Bי�B�{B�z�B��BٮB�  Bڣ�B�\)B��
B�Q�B�
=B�B�=qB޸RB߅B�(�B��B�33B��B�z�B���B�B�Q�B���B�p�B�{B�\B�\)B��
B�z�B�33B陚B�(�B���B�p�B�  B���B�\)B��
B��B�33BB�Q�B�
=B�p�B��B��B�\)B��
B�Q�B�
=B��B�(�B��HB���B�  B���B��B�  B���B�p�B��B��\B�\)B�  B�z�B�G�C   C G�C �\C
=CQ�C�\C  CG�C�\C  CG�C�\C  CG�C�\C  C\)C��C  CffCC
=CffC��C�CffC�RC	�C	p�C	�C	��C
G�C
��C
�HC�CffCC
=C=qCp�C�RC  CG�CffC��C�HC{C33Cp�C��C�RC��C=qCG�Cz�C�RC�
C��C=qCQ�Cz�C�RC�
C
=CG�C\)C�CC�C
=C33Cz�C��C�RC��C�C=qCffC��C��C�HC(�CQ�CffC��C��C�HC{CQ�CffC��CC  C{C33CffC��C��C�HC
=C=qCp�C�C�C�C
=C�CQ�C�\C��CC  C(�C=qCz�C�C�RC�
C
=C=qCp�C��C�C�HC�C=qC\)Cz�C�RC�HC
=C�CG�C�C�RC�
C�C(�C\)CffC��C�
C��C
=C=qCz�C�\C�C�HC{C(�CG�Cz�C�C�
C��C 
=C =qC z�C �\C ��C �HC!{C!(�C!G�C!p�C!�C!��C!��C"{C"(�C"ffC"�\C"�C"��C"�C#(�C#Q�C#�C#��C#C#�HC$  C$�C$\)C$�C$��C$�RC$�HC%
=C%33C%p�C%��C%�C%��C&
=C&33C&Q�C&p�C&�\C&C&��C'(�C'=qC'\)C'�C'C'�C(
=C((�C(Q�C(�C(C(�
C)  C)=qC)p�C)�\C)�C)��C*�C*=qC*p�C*�RC*�
C+  C+=qC+p�C+��C+C+�HC,
=C,33C,ffC,��C,�
C-
=C-33C-G�C-z�C-�RC-�C.�C.=qC.ffC.�\C.�RC.��C/(�C/\)C/�\C/C/�C0{C0G�C0�\C0C0�HC1
=C1G�C1�C1C1�C2�C2G�C2ffC2��C2�
C3{C3G�C3z�C3��C3C3��C4G�C4p�C4�\C4C5{C5=qC5\)C5��C5�
C6{C633C6ffC6�\C6C7
=C7G�C7z�C7��C7��C7��C8(�C8ffC8��C8�HC9{C9=qC9ffC9�\C9�
C:�C:G�C:�C:��C:�
C;{C;\)C;�C;�C;�HC<{C<Q�C<��C<��C=  C=33C=p�C=��C=��C>  C>(�C>ffC>��C>�HC?�C?Q�C?�C?�C?�
C@{C@Q�C@�\C@��CA  CA33CA\)CA�CA�RCA�CB{CB\)CB�CB��CC  CC(�CCQ�CCz�CC�CC�HCD�CD\)CD�\CD�CD�
CE  CE33CEffCE��CE�HCE��CF�CF\)CF��CF�RCF�
CG
=CGQ�CG�CG��CG��CG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A���Aֺ^Aֺ^AּjA־wAּjA־wA���A־wA���A־wAֺ^Aִ9A֬A֣�A֕�A֏\AօAցA�z�A�v�A�p�A�l�A�ffA�^5A�ZA�ZA�XA�VA�S�A�S�A�Q�A�O�A�M�A�M�A�M�A�I�A�I�A�G�A�C�A�7LA�-A���AՕ�A�hsAӗ�A�I�A҇+A�n�A�oAκ^A�O�A˙�AʼjA�1'A�l�A�$�AǅA��A�AčPA���A�;dA�ƨA�33A��A�r�A�I�A�/A�JA���A��FA�ĜA���A��A�ZA�
=A�`BA��TA���A��A�{A��A���A�\)A��A���A�%A��A���A�$�A�A�?}A�(�A��A�K�A�=qA�%A���A���A��#A��A�I�A�ƨA�n�A��wA�A�O�A�A�A�`BA�^5A�ȴA�A�A���A�1'A��A�jA��jA���A��A�p�A
=A|��A{��Az{Ay+Av��At�uAr�DAp�Ao"�Ah�uAe%Ac�Aa�#A`n�A_�7A^I�A]G�A\z�A\1A[�#A[dZAZE�AU�^AQ��AM�FAIK�AE�ABA?oA<�`A:�jA9�FA7K�A4��A2~�A1�A0�!A/t�A-��A+XA)�TA)%A'��A& �A%7LA$$�A#�^A#��A"�\A!�FA �jAC�AbAn�AVA��A�A�#A%AhsA�HA�\AJA��A�AZAK�A��A�-Ar�A(�A-A�;AjA�A�#A
��A
1A�A�PAĜAZA�A��A�TA;dA�/Az�A �yA ^5@��H@�hs@�z�@�1@���@�J@��9@���@�?}@��@�ƨ@��@�t�@��@�7@�G�@�r�@�
=@�E�@�O�@睲@�n�@��@�J@�u@�C�@�
=@�
=@�^5@�O�@���@���@���@�Q�@�b@�S�@�v�@�{@���@�X@���@��@ۍP@�@�X@��@�Ĝ@��@ְ!@��#@Ցh@�O�@��/@Դ9@Դ9@�Q�@�  @�|�@�@ҧ�@҇+@�ff@�V@��@��@�z�@� �@��
@�l�@�@���@�{@�?}@̋D@�|�@��@̋D@���@�dZ@�ȴ@�\)@˝�@�v�@�O�@�%@�r�@�9X@�I�@� �@�A�@���@��@�V@ũ�@���@ļj@ēu@�bN@�(�@�^5@�X@�`B@��h@��7@���@�z�@�A�@�  @�b@��m@�|�@��R@��#@�G�@�p�@�7L@��@�j@���@��y@��@���@��h@�&�@�I�@�(�@��@��F@���@�t�@��@���@��@���@�X@��@��@��/@��u@�(�@��@�|�@�\)@�C�@�\)@�"�@�v�@�=q@���@�x�@�G�@��@���@��j@�r�@��@��
@�o@�~�@�-@���@�/@���@��@�1@��F@�K�@�;d@��@��R@�n�@�M�@�5?@���@���@��7@��T@�`B@�Ĝ@���@�K�@�\)@�33@�~�@�{@��@�7L@��j@��@�j@�Q�@��@��@��w@���@�S�@��@�n�@�$�@���@��7@�O�@�G�@���@�O�@��@��w@�dZ@�
=@���@���@���@��+@�^5@�-@���@���@�&�@��u@��@��w@�|�@�t�@�K�@��@�@��H@���@���@�=q@��@�&�@��@� �@��@�K�@�o@��@���@���@�v�@�n�@�=q@��@���@�hs@�7L@��@���@�Z@���@��F@���@�t�@�
=@���@�v�@�M�@�5?@���@���@�x�@�7L@���@�V@��@��9@��@�9X@��@�ƨ@���@��P@���@���@���@���@���@�;d@��y@��@��^@��7@�7L@�b@��@�K�@�
=@���@��!@�v�@�ff@�=q@��T@���@���@���@��-@���@��h@��7@��@��@��u@�Z@�9X@��@���@�l�@�+@��@��@��\@�^5@�=q@�$�@�{@��^@�G�@�%@���@��`@���@�1'@���@��w@�t�@�33@��@���@�M�@�$�@�@��@��@��^@���@���@���@�x�@�?}@�V@��/@��j@���@�Q�@�A�@�I�@��@�b@�w@K�@
=@~�R@~�+@~5?@}��@|�@|z�@{�m@{S�@z�\@y��@y�^@yx�@y%@xA�@wK�@vv�@u�@u��@u`B@u�@t��@t��@tj@t(�@t1@s��@s�
@s�F@s"�@r~�@r�@q�#@q7L@pQ�@p1'@pA�@pQ�@pQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aְ!AּjA���A�A�A���AֶFAָRAִ9AֶFA�ĜAּjA־wAֺ^Aֺ^A���AּjA־wA־wAֺ^AּjA־wAֺ^A�A־wA���A���A־wA�AּjA���A���A־wA�A���A־wA���AָRA�A־wAּjAָRAִ9Aִ9AָRAֲ-Aֲ-Aֲ-Aְ!Aִ9Aֲ-A֬A֮A֧�A֮Aֲ-A֩�A֥�A֣�A֣�A֣�A֡�A֣�A֝�A֙�A֛�A֛�A֏\A֓uA֏\A֑hA֓uA֑hA֍PA֓uA֑hA։7A֏\A֍PA։7A։7AցAփAցAցAօA�~�AփA�~�AցA�|�A�x�A�|�A�x�A�|�A�x�A�x�A�z�A�t�A�z�A�t�A�x�A�t�A�r�A�v�A�p�A�r�A�n�A�r�A�r�A�n�A�r�A�l�A�jA�l�A�hsA�l�A�hsA�ffA�l�A�ffA�hsA�dZA�`BA�bNA�\)A�`BA�`BA�\)A�`BA�ZA�\)A�^5A�XA�^5A�XA�ZA�ZA�XA�^5A�XA�ZA�XA�XA�\)A�VA�VA�VA�S�A�XA�VA�VA�XA�S�A�VA�S�A�Q�A�XA�Q�A�S�A�VA�O�A�VA�Q�A�Q�A�VA�Q�A�VA�S�A�O�A�VA�S�A�O�A�S�A�O�A�Q�A�Q�A�K�A�O�A�Q�A�K�A�O�A�O�A�K�A�Q�A�K�A�O�A�M�A�K�A�K�A�M�A�I�A�O�A�K�A�M�A�O�A�I�A�O�A�K�A�K�A�M�A�I�A�M�A�K�A�I�A�K�A�E�A�M�A�I�A�G�A�M�A�E�A�G�A�K�A�E�A�I�A�K�A�E�A�I�A�G�A�C�A�I�A�E�A�C�A�G�A�C�A�?}A�?}A�9XA�9XA�;dA�5?A�;dA�5?A�1'A�33A�/A�/A�/A�/A�&�A��A��A�A���A��A��/A���A�ȴAռjAծAՓuA�t�A�hsA�XA�+AԬA�^5A�$�A�%A��A��
A�ƨAӴ9Aӛ�A�~�A�r�A�dZA�`BA�XA�M�A�K�A�?}A�;dA�5?A�5?A�+A���A�A�A�%A��#A�ƨAѧ�AѓuA�t�A�bNA�M�A�9XA�bA�AХ�A�Q�A�A�/A�{A��A��#A�ƨA�Aΰ!AΝ�A�jA��A͙�A�1'A�1'A�(�A���A̟�A�"�A���A˧�A�p�A�=qA�1'A��A��A���AʸRAʝ�AʓuAʓuA�K�A��A�{A�"�A�1'A�E�A�E�A�9XA�(�A�  Aɰ!A�ffA�oAȼjA�p�A�M�A�7LA�/A�+A��A�  A��HA��#AǾwAǟ�A�t�A�O�A�E�A�7LA�$�A��A�{A�oA�%A�A�t�A�ZA�E�A�-A�-A�&�A�%A��AŅA�p�A�p�A���A�dZA�7LA� �AþwA�/A�ƨA¬A�A�AhAhA�x�A�S�A�?}A�;dA�JA�%A��A��;A���A��wA��^A��^A��-A���A���A��hA�;dA�oA���A�n�A�
=A��RA�C�A��/A��^A��A��A�VA���A���A�jA�+A���A��-A�~�A�`BA�I�A�+A��A��HA��A��+A���A��!A�r�A�-A���A�G�A��A���A�=qA�  A��A�A��A�z�A�E�A��A��mA�ƨA��!A���A��A�jA�I�A��A���A�r�A�K�A�"�A�%A��A���A���A���A���A���A��uA��DA��A�t�A�(�A��yA��wA���A�r�A�/A��TA��FA��+A�VA�9XA�-A��A�A��A��`A���A��wA��!A���A��uA��A�z�A�l�A�bNA�ZA�I�A�7LA�-A��A�VA�A��9A�C�A��A��wA���A�|�A�dZA�K�A�1'A�"�A��A�VA�%A�A��A��yA��A��yA��yA��mA��`A��HA��;A��HA��A���A���A���A�ȴA��RA���A�v�A�XA�?}A�9XA�-A�-A�(�A��A�bA�VA�1A�
=A�
=A�%A��A���A�z�A�S�A�O�A�E�A�7LA�$�A�bA���A��+A�\)A�=qA�"�A�  A��A��`A��HA�ƨA��RA���A��\A�x�A�p�A�hsA�hsA�^5A�`BA�`BA�\)A�^5A�`BA�^5A�ZA�\)A�\)A�XA�VA�VA�VA�K�A�I�A�=qA�7LA�+A�1A��
A��^A��-A���A��hA��A�|�A�t�A�l�A�hsA�VA�5?A���A��+A�dZA�XA�K�A�G�A�?}A�=qA�?}A�=qA�9XA�5?A�&�A�{A�1A���A���A��`A���A�A��^A��-A���A��PA�l�A�O�A�1'A��A���A�|�A�O�A�-A�"�A�$�A��A�
=A���A��yA��TA��#A���A���A�t�A�K�A�7LA��A���A��!A��uA��A�ffA�G�A�+A�A��HA���A�dZA�^5A�O�A�M�A�?}A�+A�oA�A��mA���A��A���A���A��\A��7A��A�z�A�jA�S�A��A��TA��/A��#A��A��A��A��A��
A���A�C�A���A�dZA�7LA�VA��A��HA��
A���A�ȴA��^A���A��A�jA�^5A�\)A�ZA�XA�VA�Q�A�M�A�M�A�E�A�7LA�-A�&�A�oA���A��A���A�Q�A���A�n�A�K�A� �A��`A��-A�~�A�S�A�5?A�oA��mA��\A�I�A�?}A�=qA�5?A��A�A�ȴA��-A�z�A�=qA�"�A�JA��A��A�1A��A���A�jA�=qA�"�A��A��A�bA�  A��A��`A���A�ĜA���A��-A��A���A���A���A��uA��A�r�A�n�A�ffA�bNA�^5A�\)A�XA�O�A�I�A�G�A�E�A�E�A�C�A�;dA�1'A�$�A��A���A��
A��wA���A���A���A��DA��A��A�~�A�|�A�~�A�|�A�r�A�l�A�ffA�dZA�\)A�S�A�I�A�5?A��A��mA���A���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���Aֺ^Aֺ^AּjA־wAּjA־wA���A־wA���A־wAֺ^Aִ9A֬A֣�A֕�A֏\AօAցA�z�A�v�A�p�A�l�A�ffA�^5A�ZA�ZA�XA�VA�S�A�S�A�Q�A�O�A�M�A�M�A�M�A�I�A�I�A�G�A�C�A�7LA�-A���AՕ�A�hsAӗ�A�I�A҇+A�n�A�oAκ^A�O�A˙�AʼjA�1'A�l�A�$�AǅA��A�AčPA���A�;dA�ƨA�33A��A�r�A�I�A�/A�JA���A��FA�ĜA���A��A�ZA�
=A�`BA��TA���A��A�{A��A���A�\)A��A���A�%A��A���A�$�A�A�?}A�(�A��A�K�A�=qA�%A���A���A��#A��A�I�A�ƨA�n�A��wA�A�O�A�A�A�`BA�^5A�ȴA�A�A���A�1'A��A�jA��jA���A��A�p�A
=A|��A{��Az{Ay+Av��At�uAr�DAp�Ao"�Ah�uAe%Ac�Aa�#A`n�A_�7A^I�A]G�A\z�A\1A[�#A[dZAZE�AU�^AQ��AM�FAIK�AE�ABA?oA<�`A:�jA9�FA7K�A4��A2~�A1�A0�!A/t�A-��A+XA)�TA)%A'��A& �A%7LA$$�A#�^A#��A"�\A!�FA �jAC�AbAn�AVA��A�A�#A%AhsA�HA�\AJA��A�AZAK�A��A�-Ar�A(�A-A�;AjA�A�#A
��A
1A�A�PAĜAZA�A��A�TA;dA�/Az�A �yA ^5@��H@�hs@�z�@�1@���@�J@��9@���@�?}@��@�ƨ@��@�t�@��@�7@�G�@�r�@�
=@�E�@�O�@睲@�n�@��@�J@�u@�C�@�
=@�
=@�^5@�O�@���@���@���@�Q�@�b@�S�@�v�@�{@���@�X@���@��@ۍP@�@�X@��@�Ĝ@��@ְ!@��#@Ցh@�O�@��/@Դ9@Դ9@�Q�@�  @�|�@�@ҧ�@҇+@�ff@�V@��@��@�z�@� �@��
@�l�@�@���@�{@�?}@̋D@�|�@��@̋D@���@�dZ@�ȴ@�\)@˝�@�v�@�O�@�%@�r�@�9X@�I�@� �@�A�@���@��@�V@ũ�@���@ļj@ēu@�bN@�(�@�^5@�X@�`B@��h@��7@���@�z�@�A�@�  @�b@��m@�|�@��R@��#@�G�@�p�@�7L@��@�j@���@��y@��@���@��h@�&�@�I�@�(�@��@��F@���@�t�@��@���@��@���@�X@��@��@��/@��u@�(�@��@�|�@�\)@�C�@�\)@�"�@�v�@�=q@���@�x�@�G�@��@���@��j@�r�@��@��
@�o@�~�@�-@���@�/@���@��@�1@��F@�K�@�;d@��@��R@�n�@�M�@�5?@���@���@��7@��T@�`B@�Ĝ@���@�K�@�\)@�33@�~�@�{@��@�7L@��j@��@�j@�Q�@��@��@��w@���@�S�@��@�n�@�$�@���@��7@�O�@�G�@���@�O�@��@��w@�dZ@�
=@���@���@���@��+@�^5@�-@���@���@�&�@��u@��@��w@�|�@�t�@�K�@��@�@��H@���@���@�=q@��@�&�@��@� �@��@�K�@�o@��@���@���@�v�@�n�@�=q@��@���@�hs@�7L@��@���@�Z@���@��F@���@�t�@�
=@���@�v�@�M�@�5?@���@���@�x�@�7L@���@�V@��@��9@��@�9X@��@�ƨ@���@��P@���@���@���@���@���@�;d@��y@��@��^@��7@�7L@�b@��@�K�@�
=@���@��!@�v�@�ff@�=q@��T@���@���@���@��-@���@��h@��7@��@��@��u@�Z@�9X@��@���@�l�@�+@��@��@��\@�^5@�=q@�$�@�{@��^@�G�@�%@���@��`@���@�1'@���@��w@�t�@�33@��@���@�M�@�$�@�@��@��@��^@���@���@���@�x�@�?}@�V@��/@��j@���@�Q�@�A�@�I�@��@�b@�w@K�@
=@~�R@~�+@~5?@}��@|�@|z�@{�m@{S�@z�\@y��@y�^@yx�@y%@xA�@wK�@vv�@u�@u��@u`B@u�@t��@t��@tj@t(�@t1@s��@s�
@s�F@s"�@r~�@r�@q�#@q7L@pQ�@p1'@pA�@pQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aְ!AּjA���A�A�A���AֶFAָRAִ9AֶFA�ĜAּjA־wAֺ^Aֺ^A���AּjA־wA־wAֺ^AּjA־wAֺ^A�A־wA���A���A־wA�AּjA���A���A־wA�A���A־wA���AָRA�A־wAּjAָRAִ9Aִ9AָRAֲ-Aֲ-Aֲ-Aְ!Aִ9Aֲ-A֬A֮A֧�A֮Aֲ-A֩�A֥�A֣�A֣�A֣�A֡�A֣�A֝�A֙�A֛�A֛�A֏\A֓uA֏\A֑hA֓uA֑hA֍PA֓uA֑hA։7A֏\A֍PA։7A։7AցAփAցAցAօA�~�AփA�~�AցA�|�A�x�A�|�A�x�A�|�A�x�A�x�A�z�A�t�A�z�A�t�A�x�A�t�A�r�A�v�A�p�A�r�A�n�A�r�A�r�A�n�A�r�A�l�A�jA�l�A�hsA�l�A�hsA�ffA�l�A�ffA�hsA�dZA�`BA�bNA�\)A�`BA�`BA�\)A�`BA�ZA�\)A�^5A�XA�^5A�XA�ZA�ZA�XA�^5A�XA�ZA�XA�XA�\)A�VA�VA�VA�S�A�XA�VA�VA�XA�S�A�VA�S�A�Q�A�XA�Q�A�S�A�VA�O�A�VA�Q�A�Q�A�VA�Q�A�VA�S�A�O�A�VA�S�A�O�A�S�A�O�A�Q�A�Q�A�K�A�O�A�Q�A�K�A�O�A�O�A�K�A�Q�A�K�A�O�A�M�A�K�A�K�A�M�A�I�A�O�A�K�A�M�A�O�A�I�A�O�A�K�A�K�A�M�A�I�A�M�A�K�A�I�A�K�A�E�A�M�A�I�A�G�A�M�A�E�A�G�A�K�A�E�A�I�A�K�A�E�A�I�A�G�A�C�A�I�A�E�A�C�A�G�A�C�A�?}A�?}A�9XA�9XA�;dA�5?A�;dA�5?A�1'A�33A�/A�/A�/A�/A�&�A��A��A�A���A��A��/A���A�ȴAռjAծAՓuA�t�A�hsA�XA�+AԬA�^5A�$�A�%A��A��
A�ƨAӴ9Aӛ�A�~�A�r�A�dZA�`BA�XA�M�A�K�A�?}A�;dA�5?A�5?A�+A���A�A�A�%A��#A�ƨAѧ�AѓuA�t�A�bNA�M�A�9XA�bA�AХ�A�Q�A�A�/A�{A��A��#A�ƨA�Aΰ!AΝ�A�jA��A͙�A�1'A�1'A�(�A���A̟�A�"�A���A˧�A�p�A�=qA�1'A��A��A���AʸRAʝ�AʓuAʓuA�K�A��A�{A�"�A�1'A�E�A�E�A�9XA�(�A�  Aɰ!A�ffA�oAȼjA�p�A�M�A�7LA�/A�+A��A�  A��HA��#AǾwAǟ�A�t�A�O�A�E�A�7LA�$�A��A�{A�oA�%A�A�t�A�ZA�E�A�-A�-A�&�A�%A��AŅA�p�A�p�A���A�dZA�7LA� �AþwA�/A�ƨA¬A�A�AhAhA�x�A�S�A�?}A�;dA�JA�%A��A��;A���A��wA��^A��^A��-A���A���A��hA�;dA�oA���A�n�A�
=A��RA�C�A��/A��^A��A��A�VA���A���A�jA�+A���A��-A�~�A�`BA�I�A�+A��A��HA��A��+A���A��!A�r�A�-A���A�G�A��A���A�=qA�  A��A�A��A�z�A�E�A��A��mA�ƨA��!A���A��A�jA�I�A��A���A�r�A�K�A�"�A�%A��A���A���A���A���A���A��uA��DA��A�t�A�(�A��yA��wA���A�r�A�/A��TA��FA��+A�VA�9XA�-A��A�A��A��`A���A��wA��!A���A��uA��A�z�A�l�A�bNA�ZA�I�A�7LA�-A��A�VA�A��9A�C�A��A��wA���A�|�A�dZA�K�A�1'A�"�A��A�VA�%A�A��A��yA��A��yA��yA��mA��`A��HA��;A��HA��A���A���A���A�ȴA��RA���A�v�A�XA�?}A�9XA�-A�-A�(�A��A�bA�VA�1A�
=A�
=A�%A��A���A�z�A�S�A�O�A�E�A�7LA�$�A�bA���A��+A�\)A�=qA�"�A�  A��A��`A��HA�ƨA��RA���A��\A�x�A�p�A�hsA�hsA�^5A�`BA�`BA�\)A�^5A�`BA�^5A�ZA�\)A�\)A�XA�VA�VA�VA�K�A�I�A�=qA�7LA�+A�1A��
A��^A��-A���A��hA��A�|�A�t�A�l�A�hsA�VA�5?A���A��+A�dZA�XA�K�A�G�A�?}A�=qA�?}A�=qA�9XA�5?A�&�A�{A�1A���A���A��`A���A�A��^A��-A���A��PA�l�A�O�A�1'A��A���A�|�A�O�A�-A�"�A�$�A��A�
=A���A��yA��TA��#A���A���A�t�A�K�A�7LA��A���A��!A��uA��A�ffA�G�A�+A�A��HA���A�dZA�^5A�O�A�M�A�?}A�+A�oA�A��mA���A��A���A���A��\A��7A��A�z�A�jA�S�A��A��TA��/A��#A��A��A��A��A��
A���A�C�A���A�dZA�7LA�VA��A��HA��
A���A�ȴA��^A���A��A�jA�^5A�\)A�ZA�XA�VA�Q�A�M�A�M�A�E�A�7LA�-A�&�A�oA���A��A���A�Q�A���A�n�A�K�A� �A��`A��-A�~�A�S�A�5?A�oA��mA��\A�I�A�?}A�=qA�5?A��A�A�ȴA��-A�z�A�=qA�"�A�JA��A��A�1A��A���A�jA�=qA�"�A��A��A�bA�  A��A��`A���A�ĜA���A��-A��A���A���A���A��uA��A�r�A�n�A�ffA�bNA�^5A�\)A�XA�O�A�I�A�G�A�E�A�E�A�C�A�;dA�1'A�$�A��A���A��
A��wA���A���A���A��DA��A��A�~�A�|�A�~�A�|�A�r�A�l�A�ffA�dZA�\)A�S�A�I�A�5?A��A��mA���A���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B��B�hB��B�3B��B�hB�3B�-B��B��B��B��B�'B�'B��B�UB�!B��B�!B��B��B�!B��B�!B��B��B��B��B��B�!B�!B�!B�!B�!B��B��B��B�!B��B��B�!B��B��B�B��B�B�B �B2�BcTBm)B`BBRTB<6BqB��B��B�bB��B�	B��B��B��B��B��B�	B~�BtTB|�BsBe`B_�B`BX�BTaBT�BJ#BA�B:*B/�B.B)_B%�BeB{BBDBB�B�DB��B��B�B�,BѷB��B��B��B�+B�{BtBm�BjBd�B\]BQ�BEB($BxB
� B
�}B
�3B
�*B
��B
�4B
}"B
o�B
e`B
\�B
F�B
:�B
4�B
#�B
�B
�B
�B
%B	��B	��B	��B	��B	��B	��B	��B	�VB	��B	��B	{�B	s�B	qB	k�B	j�B	f2B	_pB	N�B	4�B	#�B	�B	�B��B��B�)B��B� B��B��B�B�NB�pB�B�TB�B�
B�8B�B�B�B�>B��B��B��B�QB��B�/B��B��B�B�LB��B�^B�3BĜB��B�&B�B��B�
B��B��BӏB�KBΥB�sB�?B��B�B�WB�yB��B��BںB�/B��B��B֡BޞBݘB�jBݘB�B�pB�5B�5BܒBیB�#B�B�`B��B� B�BB�vB�B�dB�B�mB��B�B��B�B�B�GB�;B�B�iB��B�B�B��B�	B�cB	MB	�B	�B	YB	�B	fB	
�B	�B	VB	�B	\B	 B	�B	$B		B	7B	7B	�B	B	VB	!�B	"hB	"hB	#:B	#B	%�B	'RB	($B	)�B	-�B	2aB	33B	5�B	:�B	?�B	A�B	E�B	E�B	F?B	HB	K�B	P�B	Y�B	^�B	a|B	d�B	k�B	u�B	y�B	zB	v�B	}�B	��B	��B	�MB	��B	��B	��B	�"B	��B	�hB	��B	�B	��B	��B	��B	�B	��B	��B	��B	�$B	�7B	��B	�!B	�\B	�bB	��B	�B	��B	��B	��B	�IB	�CB	��B	��B	�-B	��B	�?B	�B	��B	�B	��B	��B	�nB	�nB	��B	��B	�^B	��B	��B	�B	��B	�B	�3B	�tB	��B	�EB	�zB	ǮB	�B	��B	ȴB	�B	�B	�RB	��B	�B	�B	ϫB	��B	�&B	�&B	��B	�?B	�B	�KB	��B	�5B	�B	�HB	�B	�vB	�B	��B	�NB	�B	�B	�B	��B	� B	�B	�&B	��B	��B	�B	��B	�mB	�cB	�B	�;B	��B	�B	�B	��B	�+B	�ZB	�%B	��B	��B	�fB	�8B	�	B	��B	�xB	�JB	��B	��B	��B	��B	��B	��B	��B	�(B	��B
uB
{B
�B
B	�.B
 iB
 4B
 iB
 �B
;B
oB
oB
SB
�B
�B
�B
�B
	lB
	�B

rB
B
�B
�B
VB
�B
�B
"B
�B
�B
"B
\B
�B
4B
B
oB
�B
�B
�B
B
FB
�B
�B
�B
�B
�B
�B
�B
{B
FB
FB
B
@B
B
B
B
�B
�B
�B
�B
�B
�B
1B
	B
�B
�B
1B
1B
eB
B
	B
�B
B
�B
�B
 �B
!�B
"hB
#�B
#:B
#:B
"�B
!�B
�B
 'B
 �B
 'B
!�B
!bB
!bB
#:B
"�B
"4B
"hB
"4B
#�B
$tB
%FB
&�B
($B
'�B
'�B
&�B
&�B
&�B
'RB
)_B
(�B
(�B
(�B
(�B
(XB
(XB
(XB
*eB
*eB
*eB
)�B
)�B
)*B
)�B
*eB
*0B
*�B
+6B
+�B
,�B
,�B
,�B
,�B
-B
-CB
-CB
.IB
.}B
.�B
.�B
/�B
/�B
0!B
/�B
/�B
/�B
/�B
0�B
2�B
3�B
3�B
4B
4nB
4nB
4nB
4�B
5B
5B
5�B
5�B
6�B
6�B
7�B
8RB
8�B
9$B
9�B
:*B
:�B
<�B
>B
>BB
>wB
>wB
>�B
>�B
?B
?}B
?}B
?�B
?�B
?�B
@OB
?}B
?HB
?B
?HB
@�B
@�B
A�B
A�B
A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�҉B�aB��B�hB�-B��B�hB��B��B��B�aB�nB��B��B�nB��B�nB��B�-B��B�hB��B��B�[B��B�aB��B��B�[B��B��B�3B�hB��B��B��B��B�hB�'B��B��B�3B��B��B��B�3B��B��B��B�UB��B�nB��B�UB��B��B��B�OB��B�'B��B��B��B��B��B��B��B��B�B�[B��B�B�!B��B��B��B��B��B��B�'B��B��B��B��B��B�OB�[B��B�'B��B��B��B�OB��B�B��B�!B�B��B�}B��B�B��B��B�B�'B�B�'B�OB��B��B�B��B��B�B��B��B��B��B��B��B��B��B��B��B�UB�OB��B��B��B��B��B�IB�'B��B��B��B��B��B�}B��B��B��B�UB��B��B��B��B��B�B�!B��B��B��B��B�UB��B��B��B�UB�OB�'B�B��B��B��B��B�OB��B��B�B��B�'B��B��B��B�OB��B��B�B�'B��B��B�'B��B�'B�!B��B��B�UB��B�[B�B��B��B�B��B�B��B�UB��B��B�B�UB�!B��B�[B��B�UB�UB��B�[B�!B�OB�[B��B�B�'B�B��B��B��B��B�'B��B�UB�[B��B��B��B��B�UB�}B�'B�!B��B��B�UB��B��B�[B��B�B��B��B��B��B�hB��B�3B�FB�B��B��B��B�BߤB�B�B��B�mB� B�B�B�B�;B��B�/B�WB�B�B�WB�mB�DB�
B��B�8B�TB�BMB�B1B�B%�B2aB8RB<jBC�BC�BE9BZ�B\]B{�BtBv�Bo�Bp;Bn/Bj�Bo�Bh�Bl"BxlBbNB]dBS&B\�B`�Bf�BbNBVBQ�BQ�BL0BA�BD�BDgB=<B8RB8RB6FB5�BK�B[WB[�BkBu%B�rB��B��B��B��B�B�_B��B��B��B�bB�:B�bB�\B�B�hB�hB�B��B�B�bB��B��B��B��B��B�VB��B�B�B��B�B�B�7B�+B��B�rB��B�B�B�B�eB�;B�	B�MB��B��B�(B��B��B�SB�B�B�_B��B�B�SB��B�%B��B�+B��B�B�{B��B�GB�GB� B��B��B��B�DB�VB�1B�lB��B��B�B��B��Br|B��By	B�7B��B~�Bs�B~�Bt�BqvBq�Bn�Bv�B�=BsB}�By	Br|Br�B�Bn�BtTBj�Bu%Be�Bb�BhsBcTBf�Be�Bb�Be,Ba�B^B]�B_�B^�B]dBd&BbNBZ�B]dB[#B[#BXyBY�BV�BU�BS�BR�BS[BT�BRTBS&B[�BUgBTaBR�BQNBZ�BS�BS&BQ�BOBK�BJXBJ#BJXBH�BG�BF�BHBEmBCaBB[BD�BA BCaBB'B?�BA�B@�B=�B>�B=�B:*BHKBF�B<�B7�B8�B5?B4�B5B4�B1'B0�B1[B1'B/�B5B/�B/B.�B/OB/�B.B0!B/�B-�B/�B.IB-B*0B-B0�B,�B.B.�B+6B-B(�B'�B)�B*0B*eB(XB)�B&�B%�B'�B)�B/�B+B%zB$B#B$tB#nB&�B)�B'�B!�B#B 'B!B�B�BqB�B�B	BeBkB�BBBSBBFB�B{B�B�B�BFBuB�B{B�B�B@B�BBhB�B�B�B�B(B�B�B�B�B�B�BPB
�B�B�BxB	7B�B�B�B�B�B�B�B{B�B%BGB�B�B�BGB�BB�B�BBGB�B�B�B��B
rB�B�cB  B�B�	B�xB��B�B�>B�8B��B�>B�PB�lB�lB��B�B��B��B�B�B��B��B�rB��B��B��B��B��B�cB�B��B�GB�B�B�B�B��B��B��B��B��B�B�KB��B��B��B��B�
B��B�mB��B�B�B�B�B{B�B��B��B�yB�[B��B�B�6B̘B�vB��B�mB�BуB�<B�6B��B�B͟B�dBɺB�^B�)B�XB�B�BɆBĜB��B�2B�0B�B�tB�RB�FB�UB��B�kB��B�$B�XB��B��B�	B�eB��B�VB�B��B��B�	B��B��B�.B��B�fB�B�MB�fB��B~�BzxBv�BuZBu�BxBu%BtB{Br|Br�BqBpBo Bn�Bo BncBsBo5Bm)Bm)Bj�Bj�BjBkQBkQBj�BjBjKBiyBi�BhsBg�Bg�Bg�BjBi�BffBb�BaBaB`vB_�B^�B_pB^5B\]B[�B]�B]/B[�BYBZBZBX�BXyBWsBZQBR�BP�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022120516032720221205160327IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022121513010620221215130106QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022121513010620221215130106QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                