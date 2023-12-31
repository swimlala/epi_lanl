CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-11-15T00:02:22Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    U�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  [�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  z8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x %�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` >    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   >`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   D`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   J`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T P`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   P�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   P�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   P�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   P�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � P�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   QT   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Qp   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Qx   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Q�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Q�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       Q�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    Q�Argo profile    3.1 1.2 19500101000000  20221115000222  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_236                 6810_008521_236                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @����{�@����{�11  @���͞��@���͞��@2Wc^t)�@2Wc^t)��d������d�����11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @�  @��R@�  @�  A ��A  A   A+�A@  A_\)A~�RA�\)A��A�Q�A���A�  A߮A�  B (�BQ�BQ�B  B   B(  B0(�B8(�B@  BG�BO�BW�
B_�
Bg�
Bo�
Bx(�B�{B�{B�  B�  B��B��B�{B�  B��B�  B��B��B�  B�  B��B��B��B�  B��
B��
B��B�{B�  B�  B�(�B�{B�  B�{B�  B�  B��B�B��
C  C
=C��C��C	��C  C
=C{C  C��C  C  C��C��C  C   C!�C#�C%�HC'��C*
=C+��C-��C/��C1��C3��C6  C7�C:  C<  C>
=C@
=CB  CD  CE��CG��CJ  CL  CN  CP  CQ��CT  CV  CX  CY��C\
=C^  C`
=Cb
=Cd{Cf{Ch  Cj  Cl  Cn
=Cp  Cr  Cs��Cu�Cx  Cz
=C{�C}�C��C�C�  C�  C�C�C�C�
=C�
=C�  C�  C�C���C�  C�  C���C���C�  C�  C���C���C�  C�C�
=C�  C�  C���C���C�  C�  C�  C�
=C�
=C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C�  C���C�  C�
=C�
=C�C�  C�C�C�  C�  C�  C���C���C���C���C�  C�C�  C�C�C�  C���C�  C�C�  C�C�  C�  C�C���C���C�C�  C���C�  C�  C�  C���C���C���C�  C�C�  C�
=C�\C�  C���C���C���C�C�C�C�  C�  C�  C���C���C�  C�  C���C���C�
=C�C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C�  C�  C�  C�
=C�C���D � D  D}qD  D}qD�RD}qD  D��D�D��DD��D  D}qD  D��D��D	z�D
  D
��D�D��D�D� D  D}qD�qD� D  D}qD�qD}qD��D}qD  D� D�D�D�D� D  D� D�D��D  D� D  D� D�D� D  D� D�D� D  D��D  D� D  D}qD�qD}qD   D }qD �qD!}qD!�qD"��D#�D#}qD#��D$� D$�qD%z�D&  D&� D'  D'��D(  D(}qD)  D)� D)�qD*� D+�D+� D,  D,�D-  D-� D.�D.� D/  D/��D0  D0� D1�D1��D2  D2z�D3  D3� D3�qD4� D5  D5� D6  D6� D6��D7z�D8  D8}qD8�qD9��D:  D:� D;�D;� D<�D<��D=  D=}qD=�qD>}qD>�qD?}qD@  D@� D@��DA}qDA�qDB��DC  DC� DD�DD� DE  DE� DF  DF� DF�qDGz�DG�qDH��DIDI� DI�qDJ� DK  DKz�DK�qDL}qDL�qDM�DN  DN� DO�DO}qDO��DPz�DP�qDQ}qDR�DR�DS�DS��DS�qDT}qDU  DU��DV  DV}qDW  DW� DX�DX�DY�DY}qDY��DZ}qD[  D[� D[��D\� D]  D]}qD]�qD^� D_  D_� D`�D`��DaDa� Da�qDb� Dc  Dc��Dc�qDd� DeDe��Df  Df��Dg  Dg}qDh  Dh}qDi  Di��Dj  Dj}qDj�qDk� Dl  Dl� DmDm�Dn�Dn}qDn�qDo� Dp�Dp� Dq�Dq�Dr�Drz�Dr��Ds� Ds�qDtz�Dt�qDu��Dv  Dv}qDw  Dw}qDw�qDx}qDy  Dy��Dy�qDzz�Dz�qD{��D{�qD|z�D|�qD}}qD~  D~}qD~�qD��D��D�AHD��HD�� D�HD�@ D�� D���D���D�>�D�� D���D��qD�>�D�� D��HD��D�B�D�� D��qD���D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D��HD�� D�  D�@ D�~�D��HD��D�AHD�~�D���D���D�AHD��HD���D�  D�@ D�~�D��qD�  D�AHD��HD�� D���D�@ D�~�D���D���D�>�D�� D���D�  D�B�D���D��HD�HD�@ D�~�D���D�  D�AHD���D�� D���D�AHD��HD�� D�  D�@ D�~�D�� D���D�>�D�~�D���D�  D�AHD�� D���D���D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?��?8Q�?��?��
?���?�ff@�\@�@!G�@0��@@  @Tz�@aG�@xQ�@��
@���@�33@�(�@�ff@�{@���@��R@���@��@�Q�@��
@���@�z�@�(�A�\A�A
=qA�RAz�A�Ap�A!G�A%�A*�HA.{A3�
A6ffA<(�A@��AC33AH��AL(�AQ�AU�A[�A_\)Ae�Ah��An{Ar�\Aw�A|(�A���A�33A��A�Q�A��A��A�
=A��A��
A��RA�G�A��HA�ffA�  A��\A���A�\)A�=qA��
A�
=A���A��A�p�A�  A��HA�z�A�\)A���A��
AƸRAȣ�A�33A�{AϮA��HA�z�A�\)Aٙ�AۅA�ffA߮A�33A���A�\)A��A�A�RA�  A�33A���A��A�=qA�(�A�
=B Q�BB
=B  Bp�B=qB�B��B	�B�BQ�B{B
=B  B��B�RB�
Bp�BffB�B�B{B�B��B{B�B z�B!�B#33B$Q�B%B&�HB(z�B)p�B*�RB,(�B-�B.ffB0  B0��B2ffB3�B4��B6=qB733B8z�B:{B:�HB<Q�B=�B>�HB@z�BAp�BB�RBDQ�BEG�BF�\BH(�BI�BJ�\BL  BL��BN�\BO�BP��BR�\BS\)BU�BU�BW�BX��BZ{B[�B\��B]�B_�Ba�Bb{Bc\)Be�Bf{Bg�Bh��Bi�Bk�Blz�Bm�Bo�BpQ�BqBs33Bt(�BuBw
=Bx  ByG�Bz�RB|  B|��B~�\B�B�ffB�33B���B�Q�B���B�p�B�Q�B��RB��B�(�B���B�\)B�{B��\B�\)B�  B�ffB��B��
B�Q�B���B���B�=qB���B�G�B�  B�ffB�
=B��
B�=qB���B���B�  B���B�\)B��
B�z�B��B��B�(�B��HB�\)B��B���B��B���B�Q�B��HB�G�B�  B��\B���B��B�Q�B��RB�33B��B�Q�B���B��B�  B�Q�B���B��B��
B�z�B�
=B�\)B��
B�z�B���B�p�B�{B��RB�
=B���B�Q�B���B��B��
B�ffB���B�G�B�  B���B�33B���B�Q�B���B�\)B�  B���B�
=B�B�Q�B���B�G�B�{B�z�B���B��B�(�B���B�33B��
B�ffB���B�\)B�(�B��RB��B�B�z�B���B�p�B�(�B���B�33B��
B�z�B��HBŅB�(�BƸRB�
=BǮB�ffBȸRB�p�B�{B�z�B�
=B�B�=qB̸RB�p�B�{BΏ\B�
=B�B�z�B���BхB�=qB���B�p�B��Bԣ�B�\)B��
B�Q�B��B��
B�=qB���Bٙ�B�  Bڣ�BۅB�{B�z�B�\)B��B�ffB��B��
B�=qB��HB�B��B��B�G�B��
B�=qB��HB噚B�{B�\B�G�B��B�Q�B��B�B�Q�B��HB�B�=qB���B�B�Q�B���B�p�B�=qB��B�p�B�(�B��B�G�B�{B��RB��B��B���B�
=B�B�ffB��HB��B�Q�B���B�p�B�=qB��\B�G�B�{B�z�B�33B��C 33C �\C �C=qCz�C�HC(�Cp�C��C(�Cp�C�RC{Cz�CC  CffCC  CQ�C�RC  C=qC��C  C=qC��C	  C	=qC	�C	��C
G�C
�C
�
C=qC��C��C(�C�C��C
=C\)C�RC
=CQ�C�\C�
C{CffC�C�HC{C\)C��C�
C  C�CQ�C��C�RC��C{CG�CffCz�C�RC�C{C(�CffC��CC�
C  C33C\)C��CC�HC
=C33CffC��C��C�C
=C33C\)C��C��C��C{C=qCz�C�C��C�C{CQ�Cz�C��C�RC  C(�CG�CffC��C�
C
=C�CG�Cp�C��C�HC  C(�CG�C�C�C�C�C=qC\)C�\C��C  C{CG�C�C�C�
C  C(�Cp�C��C��C��C�CG�Cz�C�RC��C �C =qC ffC ��C �HC!
=C!=qC!\)C!z�C!�C!�HC"�C"G�C"ffC"�\C"C#  C#�C#=qC#�C#�C#�HC$
=C$�C$G�C$�C$C$�C%
=C%33C%\)C%�\C%C%��C&(�C&Q�C&p�C&��C&�C'{C'33C'\)C'��C'��C'�C(�C(\)C(�C(��C(�
C){C)G�C)z�C)�\C)�RC)��C*(�C*ffC*�\C*�RC*�
C+
=C+33C+\)C+��C+C+��C,{C,=qC,ffC,��C,�
C-
=C-=qC-\)C-�C-�C-�
C.{C.Q�C.�C.�C.��C/  C/33C/ffC/��C/�
C0  C0�C0G�C0z�C0�RC0�C1(�C1\)C1�C1�RC1�C2{C2=qC2ffC2��C2�HC3�C3G�C3p�C3��C3�
C4{C4Q�C4p�C4��C4��C5{C5G�C5�C5�RC5�HC6
=C6=qC6z�C6�RC6�C7�C7G�C7p�C7��C7�HC8�C8\)C8�\C8��C9  C933C9p�C9�\C9�RC9�C:(�C:ffC:��C:C:��C;�C;ffC;�C;��C;��C<33C<p�C<��C<�
C={C==qC=z�C=��C=C=��C>33C>p�C>�C>�
C?  C?(�C?\)C?�\C?C?��C@(�C@p�C@��C@��CA  CA(�CA\)CA�CA�RCA�CB�CB\)CB��CB��CC  CC33CCffCC��CC��CD  CD(�CDQ�CD�CD�RCD��CE�CEQ�CE�CE�RCE�CF�CFQ�CF�CFCG  CG33CGQ�CGz�CG�CG�HCH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333334                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @@  @�  @��R@�  @�  A ��A  A   A+�A@  A_\)A~�RA�\)A��A�Q�A���A�  A߮A�  B (�BQ�BQ�B  B   B(  B0(�B8(�B@  BG�BO�BW�
B_�
Bg�
Bo�
Bx(�B�{B�{B�  B�  B��B��B�{B�  B��B�  B��B��B�  B�  B��B��B��B�  B��
B��
B��B�{B�  B�  B�(�B�{B�  B�{B�  B�  B��B�B��
C  C
=C��C��C	��C  C
=C{C  C��C  C  C��C��C  C   C!�C#�C%�HC'��C*
=C+��C-��C/��C1��C3��C6  C7�C:  C<  C>
=C@
=CB  CD  CE��CG��CJ  CL  CN  CP  CQ��CT  CV  CX  CY��C\
=C^  C`
=Cb
=Cd{Cf{Ch  Cj  Cl  Cn
=Cp  Cr  Cs��Cu�Cx  Cz
=C{�C}�C��C�C�  C�  C�C�C�C�
=C�
=C�  C�  C�C���C�  C�  C���C���C�  C�  C���C���C�  C�C�
=C�  C�  C���C���C�  C�  C�  C�
=C�
=C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C�  C���C�  C�
=C�
=C�C�  C�C�C�  C�  C�  C���C���C���C���C�  C�C�  C�C�C�  C���C�  C�C�  C�C�  C�  C�C���C���C�C�  C���C�  C�  C�  C���C���C���C�  C�C�  C�
=C�\C�  C���C���C���C�C�C�C�  C�  C�  C���C���C�  C�  C���C���C�
=C�C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C�  C�  C�  C�
=C�C���D � D  D}qD  D}qD�RD}qD  D��D�D��DD��D  D}qD  D��D��D	z�D
  D
��D�D��D�D� D  D}qD�qD� D  D}qD�qD}qD��D}qD  D� D�D�D�D� D  D� D�D��D  D� D  D� D�D� D  D� D�D� D  D��D  D� D  D}qD�qD}qD   D }qD �qD!}qD!�qD"��D#�D#}qD#��D$� D$�qD%z�D&  D&� D'  D'��D(  D(}qD)  D)� D)�qD*� D+�D+� D,  D,�D-  D-� D.�D.� D/  D/��D0  D0� D1�D1��D2  D2z�D3  D3� D3�qD4� D5  D5� D6  D6� D6��D7z�D8  D8}qD8�qD9��D:  D:� D;�D;� D<�D<��D=  D=}qD=�qD>}qD>�qD?}qD@  D@� D@��DA}qDA�qDB��DC  DC� DD�DD� DE  DE� DF  DF� DF�qDGz�DG�qDH��DIDI� DI�qDJ� DK  DKz�DK�qDL}qDL�qDM�DN  DN� DO�DO}qDO��DPz�DP�qDQ}qDR�DR�DS�DS��DS�qDT}qDU  DU��DV  DV}qDW  DW� DX�DX�DY�DY}qDY��DZ}qD[  D[� D[��D\� D]  D]}qD]�qD^� D_  D_� D`�D`��DaDa� Da�qDb� Dc  Dc��Dc�qDd� DeDe��Df  Df��Dg  Dg}qDh  Dh}qDi  Di��Dj  Dj}qDj�qDk� Dl  Dl� DmDm�Dn�Dn}qDn�qDo� Dp�Dp� Dq�Dq�Dr�Drz�Dr��Ds� Ds�qDtz�Dt�qDu��Dv  Dv}qDw  Dw}qDw�qDx}qDy  Dy��Dy�qDzz�Dz�qD{��D{�qD|z�D|�qD}}qD~  D~}qD~�qD��D��D�AHD��HD�� D�HD�@ D�� D���D���D�>�D�� D���D��qD�>�D�� D��HD��D�B�D�� D��qD���D�>�D�~�D�� D�  D�@ D��HD��HD�HD�@ D��HD�� D�  D�@ D�~�D��HD��D�AHD�~�D���D���D�AHD��HD���D�  D�@ D�~�D��qD�  D�AHD��HD�� D���D�@ D�~�D���D���D�>�D�� D���D�  D�B�D���D��HD�HD�@ D�~�D���D�  D�AHD���D�� D���D�AHD��HD�� D�  D�@ D�~�D�� D���D�>�D�~�D���D�  D�AHD�� D���D���D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?\)?��?8Q�?��?��
?���?�ff@�\@�@!G�@0��@@  @Tz�@aG�@xQ�@��
@���@�33@�(�@�ff@�{@���@��R@���@��@�Q�@��
@���@�z�@�(�A�\A�A
=qA�RAz�A�Ap�A!G�A%�A*�HA.{A3�
A6ffA<(�A@��AC33AH��AL(�AQ�AU�A[�A_\)Ae�Ah��An{Ar�\Aw�A|(�A���A�33A��A�Q�A��A��A�
=A��A��
A��RA�G�A��HA�ffA�  A��\A���A�\)A�=qA��
A�
=A���A��A�p�A�  A��HA�z�A�\)A���A��
AƸRAȣ�A�33A�{AϮA��HA�z�A�\)Aٙ�AۅA�ffA߮A�33A���A�\)A��A�A�RA�  A�33A���A��A�=qA�(�A�
=B Q�BB
=B  Bp�B=qB�B��B	�B�BQ�B{B
=B  B��B�RB�
Bp�BffB�B�B{B�B��B{B�B z�B!�B#33B$Q�B%B&�HB(z�B)p�B*�RB,(�B-�B.ffB0  B0��B2ffB3�B4��B6=qB733B8z�B:{B:�HB<Q�B=�B>�HB@z�BAp�BB�RBDQ�BEG�BF�\BH(�BI�BJ�\BL  BL��BN�\BO�BP��BR�\BS\)BU�BU�BW�BX��BZ{B[�B\��B]�B_�Ba�Bb{Bc\)Be�Bf{Bg�Bh��Bi�Bk�Blz�Bm�Bo�BpQ�BqBs33Bt(�BuBw
=Bx  ByG�Bz�RB|  B|��B~�\B�B�ffB�33B���B�Q�B���B�p�B�Q�B��RB��B�(�B���B�\)B�{B��\B�\)B�  B�ffB��B��
B�Q�B���B���B�=qB���B�G�B�  B�ffB�
=B��
B�=qB���B���B�  B���B�\)B��
B�z�B��B��B�(�B��HB�\)B��B���B��B���B�Q�B��HB�G�B�  B��\B���B��B�Q�B��RB�33B��B�Q�B���B��B�  B�Q�B���B��B��
B�z�B�
=B�\)B��
B�z�B���B�p�B�{B��RB�
=B���B�Q�B���B��B��
B�ffB���B�G�B�  B���B�33B���B�Q�B���B�\)B�  B���B�
=B�B�Q�B���B�G�B�{B�z�B���B��B�(�B���B�33B��
B�ffB���B�\)B�(�B��RB��B�B�z�B���B�p�B�(�B���B�33B��
B�z�B��HBŅB�(�BƸRB�
=BǮB�ffBȸRB�p�B�{B�z�B�
=B�B�=qB̸RB�p�B�{BΏ\B�
=B�B�z�B���BхB�=qB���B�p�B��Bԣ�B�\)B��
B�Q�B��B��
B�=qB���Bٙ�B�  Bڣ�BۅB�{B�z�B�\)B��B�ffB��B��
B�=qB��HB�B��B��B�G�B��
B�=qB��HB噚B�{B�\B�G�B��B�Q�B��B�B�Q�B��HB�B�=qB���B�B�Q�B���B�p�B�=qB��B�p�B�(�B��B�G�B�{B��RB��B��B���B�
=B�B�ffB��HB��B�Q�B���B�p�B�=qB��\B�G�B�{B�z�B�33B��C 33C �\C �C=qCz�C�HC(�Cp�C��C(�Cp�C�RC{Cz�CC  CffCC  CQ�C�RC  C=qC��C  C=qC��C	  C	=qC	�C	��C
G�C
�C
�
C=qC��C��C(�C�C��C
=C\)C�RC
=CQ�C�\C�
C{CffC�C�HC{C\)C��C�
C  C�CQ�C��C�RC��C{CG�CffCz�C�RC�C{C(�CffC��CC�
C  C33C\)C��CC�HC
=C33CffC��C��C�C
=C33C\)C��C��C��C{C=qCz�C�C��C�C{CQ�Cz�C��C�RC  C(�CG�CffC��C�
C
=C�CG�Cp�C��C�HC  C(�CG�C�C�C�C�C=qC\)C�\C��C  C{CG�C�C�C�
C  C(�Cp�C��C��C��C�CG�Cz�C�RC��C �C =qC ffC ��C �HC!
=C!=qC!\)C!z�C!�C!�HC"�C"G�C"ffC"�\C"C#  C#�C#=qC#�C#�C#�HC$
=C$�C$G�C$�C$C$�C%
=C%33C%\)C%�\C%C%��C&(�C&Q�C&p�C&��C&�C'{C'33C'\)C'��C'��C'�C(�C(\)C(�C(��C(�
C){C)G�C)z�C)�\C)�RC)��C*(�C*ffC*�\C*�RC*�
C+
=C+33C+\)C+��C+C+��C,{C,=qC,ffC,��C,�
C-
=C-=qC-\)C-�C-�C-�
C.{C.Q�C.�C.�C.��C/  C/33C/ffC/��C/�
C0  C0�C0G�C0z�C0�RC0�C1(�C1\)C1�C1�RC1�C2{C2=qC2ffC2��C2�HC3�C3G�C3p�C3��C3�
C4{C4Q�C4p�C4��C4��C5{C5G�C5�C5�RC5�HC6
=C6=qC6z�C6�RC6�C7�C7G�C7p�C7��C7�HC8�C8\)C8�\C8��C9  C933C9p�C9�\C9�RC9�C:(�C:ffC:��C:C:��C;�C;ffC;�C;��C;��C<33C<p�C<��C<�
C={C==qC=z�C=��C=C=��C>33C>p�C>�C>�
C?  C?(�C?\)C?�\C?C?��C@(�C@p�C@��C@��CA  CA(�CA\)CA�CA�RCA�CB�CB\)CB��CB��CC  CC33CCffCC��CC��CD  CD(�CDQ�CD�CD�RCD��CE�CEQ�CE�CE�RCE�CF�CFQ�CF�CFCG  CG33CGQ�CGz�CG�CG�HCH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333334                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A���A���A���A���A���A���A�A���A�ȴA���A���A�ĜA�AۼjAۺ^Aۺ^A۲-Aۧ�Aۡ�Aۗ�AۓuAۓuAە�Aە�Aۗ�Aە�Aۗ�Aۗ�Aۙ�Aۙ�Aۗ�Aۗ�Aۙ�Aۛ�Aۛ�Aۛ�Aۛ�A۝�A۝�A۝�A۝�A۟�A۟�Aۡ�A۬Aۧ�Aۡ�A�v�A�A���A�ffA�bA�C�A˛�A��;A�v�A��A��HA�%A��A��A���A�^5A���A��A�l�A�?}A�+A��\A�S�A�=qA���A�~�A��FA��A�\)A�A���A�x�A�n�A���A�33A��A��yA�JA��A���A�A�n�A��A��\A�^5A�E�A�hsA��A��A�{A��A�`BA���A�S�A��HA�I�A�`BA�n�A��TA�dZA��mA�G�A�
=A���A�M�A�  A�"�A}��A|  Ay�7Au�ArbNAp�9Ao?}Al��Ah�9Af$�Ae?}Ac�A_t�A]��A\(�AY�AWS�AS&�AQ��AO�AL$�AKVAJ=qAF{AA%A??}A=��A;�
A:=qA8�/A7?}A4��A3�A1��A1oA/S�A,��A*�A)ƨA(bA'ƨA'O�A&�A&�A&VA%��A#�#A"�yA"�\A!ƨA �yA �AhsA��A�A�A�TA�hA�A �A�-A1A�AQ�AK�A��A\)A�A�AffA�mA��A;dA�AffA�A?}A
v�A
(�A
  A	oA�A��An�A�AQ�A�AG�A33A�9AC�A ��A 9X@��@�"�@�v�@�9X@�Q�@��\@�@���@�&�@�@���@�Z@�K�@�bN@�Ĝ@�z�@���@�v�@�hs@�?}@���@��@���@���@�@�P@�|�@���@���@���@�9X@��;@�$�@�@�7@�+@�{@�
=@��@�P@�@���@ᙚ@�V@���@�9X@�-@܋D@�dZ@���@ج@�`B@�p�@���@���@�dZ@ج@�1'@���@�-@�-@��@Չ7@��@��/@Դ9@�1'@�o@�%@�z�@�1'@��
@�dZ@�+@�M�@�V@��;@�b@̛�@�bN@�~�@�7L@�t�@�S�@Ƈ+@��@ģ�@�x�@�@��/@�A�@�5?@�X@��@���@��D@�9X@���@�"�@�@�x�@�X@�O�@�Ĝ@�Ĝ@��^@�v�@��@��7@�7L@���@��j@��@��@���@���@�@��^@�{@��#@�J@�{@���@�x�@�/@�1'@�1@� �@�1'@�9X@�A�@�1@��@���@�~�@���@���@�j@��@�ƨ@�+@�ȴ@�n�@�$�@��@�7L@�7L@�V@�%@��@�  @�  @�  @�  @��;@�ƨ@��w@�\)@�ȴ@��#@���@�X@�7L@�&�@��@��`@��@�Q�@��m@�  @�ƨ@��P@�;d@�o@���@��+@�n�@�ff@�J@�X@��@��D@�1'@��;@��P@�+@���@���@�~�@��@��@�G�@��@�O�@��@�V@��`@�r�@�  @���@�dZ@�S�@�K�@��@���@��!@��\@�v�@�^5@�@�@�@���@��-@��@�?}@��@�%@���@�z�@�A�@���@�33@���@��!@��!@�V@���@���@�p�@�/@���@���@��@��@�Ĝ@�r�@�A�@�1'@�1@��@�"�@��@�~�@�J@���@�&�@���@���@��u@�Z@�1'@�b@��;@���@�C�@��@�v�@��@��^@��^@��@�Ĝ@�z�@�(�@��@�r�@�Q�@��w@�|�@�dZ@�;d@���@��+@�v�@��+@�n�@�=q@�-@�J@���@��@�Ĝ@���@�A�@��m@��;@�1@��
@�ƨ@���@���@�33@��y@��!@�v�@�M�@�^5@��@�Ĝ@��m@�|�@�S�@�C�@�K�@�C�@���@���@�~�@�M�@��@��@�hs@�X@�%@���@��D@��D@��D@�j@�b@���@���@��@�l�@�\)@�o@���@��R@���@�n�@�{@��-@��-@��7@���@��T@���@���@�hs@�G�@���@���@��u@�j@�(�@��w@��@�t�@�+@��!@�n�@�v�@�v�@�n�@�^5@�E�@�{@��^@��7@���@��^@�x�@���@���@�Z@�@�P@~E�@}�h@}/@|��@{��@yX@x��@xbN@x  @w�P@w+@v�R@v�+@vv�@v{@u@u�@uO�@u/@tz�@t9X@sƨ@sC�@r�\@r-@q�^@qX@q%@p��@pbN@o��@o|�@o+@n�@nv�@n{@m�@l�@lj@k�m@kt�@k"�@j�H@j~�@j�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A�A���AۼjA���A۾wA�A�ƨA�ĜA���A�ƨA�ȴA���A���A���A���A���A���A�ĜA�ȴA�AۼjA���A۾wA�A۾wA�ƨA���A�ȴA�A�ĜA�ĜAۼjA۸RA۸RAۼjA۶FAۺ^A۸RAۼjA۶FAۼjA۶FAۺ^Aۺ^A۸RA۾wA۸RA۸RA۸RAۺ^A۴9A۲-A۶FA۰!A۲-A۰!A۰!A۶FAۮA۰!Aۡ�A۟�Aۣ�Aۡ�Aۣ�Aۣ�A۟�Aۡ�Aۛ�Aۥ�A۝�Aۗ�Aۙ�AۓuAۙ�AۓuAە�Aە�AۓuAۗ�Aۏ\Aۗ�AۑhAە�Aە�AۑhAە�AۑhAە�Aە�AۑhAۗ�AۑhAە�Aە�AۓuAۗ�AۓuAۙ�Aە�AۓuAۙ�Aۗ�Aە�Aۙ�Aە�Aە�Aۙ�AۓuAۙ�Aە�Aۗ�Aۗ�AۓuAۗ�Aە�Aۗ�Aۗ�Aە�Aۛ�Aە�Aە�Aۛ�Aۗ�Aۗ�Aۙ�Aە�Aۛ�Aۗ�Aۗ�Aۛ�Aە�Aۙ�Aۛ�Aە�Aۗ�Aۙ�Aۗ�Aۛ�Aە�Aۗ�Aۛ�Aۗ�Aۗ�Aۛ�Aە�Aۙ�Aۙ�Aە�Aۛ�Aۗ�Aۗ�Aۛ�Aە�Aۛ�Aە�Aۛ�Aۙ�Aۗ�Aۛ�Aۙ�Aۗ�Aۛ�A۝�Aۗ�Aۙ�A۝�Aۙ�Aۛ�A۝�Aۗ�A۝�Aۗ�Aۛ�A۝�Aۙ�Aۛ�A۝�Aۙ�A۝�A۝�Aۙ�Aۛ�A۟�A۝�Aۙ�A۟�Aۛ�Aۛ�A۟�Aۛ�A۝�A۝�Aۙ�Aۡ�Aۛ�A۟�A۟�Aۛ�A۟�A۟�Aۛ�A۟�Aۡ�Aۛ�A۝�Aۡ�A۟�Aۛ�A۟�Aۡ�Aۛ�A۟�Aۡ�Aۛ�A۝�Aۡ�A۟�A۟�Aۣ�A۟�A۝�Aۣ�A۟�A۟�Aۣ�Aۣ�A۝�Aۣ�Aۡ�Aۡ�AۮA۬A۩�AۮA۬Aۧ�A۬A۬Aۥ�A۩�A۬Aۥ�Aۥ�Aۧ�Aۡ�A۟�Aۥ�Aۣ�A۟�Aۣ�A۟�AۑhAۉ7Aۇ+A�~�A�~�AہA�v�A�A�A��Aک�A�"�A���AٓuA�33A���Aا�A�S�A���A�bNA�A��/A�O�A�`BA�|�A���AхA�/A�1A��A��#AиRAЗ�A�z�A�5?A�ƨA�p�A�C�A��AΥ�A�p�A�`BA�A�A���AͅA�VA�\)A���A�n�A�K�A�
=A���A�~�A�VA�+A��/A���A�r�A�\)A�I�A�oA��;AȑhA�Q�A��mAǲ-A�bNA��A�AƃAś�A�/A���A�M�A� �Aú^A�$�AA�ȴA�hsA��RA�t�A�G�A�
=A���A��A�S�A�oA��A���A�ĜA���A��9A��+A�^5A�5?A�VA��mA���A�A��9A���A���A���A���A��hA��DA��PA��DA�~�A�hsA�=qA�
=A��/A��RA�v�A�A��hA�=qA���A��9A�1A��A��;A���A���A��hA�~�A�r�A�hsA�ZA�ZA�VA�E�A�=qA�=qA�=qA�9XA�9XA�7LA�/A�/A�&�A��A��A�1A��yA���A���A�O�A��HA�VA�  A���A�{A��#A���A�ĜA��DA�bNA�S�A�E�A���A��mA��FA�A�A��FA�K�A�-A�1A��A��TA��!A�ZA�5?A� �A�{A���A��#A��^A��PA�x�A�=qA��`A���A��\A�^5A�C�A� �A��A���A�ffA�K�A� �A��
A���A�|�A�^5A�1'A�A��A�n�A�5?A�$�A�VA�A��A��TA��A���A���A��9A���A���A���A��+A�z�A�n�A�^5A�O�A�5?A�VA�A��HA��wA���A��PA�|�A�\)A�I�A�+A�JA�  A���A��A��TA��/A��A���A���A�ĜA��wA��9A���A��A�t�A�ffA�`BA�VA�K�A�=qA�+A�"�A� �A��A�bA�
=A�%A�  A��yA���A���A��\A�hsA�A�~�A�5?A�(�A��A��A�ffA�M�A���A��RA��DA�I�A�E�A�7LA�
=A�A���A��`A��;A���A���A��DA�E�A�1A���A���A���A�ZA�?}A�;dA�5?A�1'A�+A��A���A��A��`A��mA��;A���A�ĜA��RA��A���A��DA�|�A�r�A�hsA�^5A�I�A�5?A�"�A�bA�A��A��-A���A��A�VA�9XA��A��A�VA���A���A���A���A�ZA���A��jA���A��A�l�A�`BA�?}A�oA���A��A��#A��A��hA��A�|�A�x�A�r�A�;dA��A���A�\)A�A�A�(�A�JA���A���A��A��A���A��hA�v�A�`BA�?}A��A�A���A��HA��HA��HA��#A��wA�t�A�jA�C�A�A���A���A�r�A�E�A��A��/A���A���A��A�ZA��A���A���A���A��A�n�A�O�A�9XA�"�A�1A���A��TA��wA��PA�dZA�?}A�
=A���A��+A�hsA�E�A�bA��wA��PA��DA��DA��A�jA�=qA�JA���A��9A��+A�^5A�5?A���A���A���A�v�A�G�A�+A�bA���A��TA���A��FA��7A�ZA�;dA�33A�&�A� �A��A�
=A�A���A���A��A��HA��FA���A��PA��A�|�A�v�A�r�A�n�A�^5A�VA�K�A�?}A�9XA�&�A��A�
=A���A���A��A��
A���A��
A���A�ĜA��^A���A��DA�x�A�jA�VA�?}A�+A�JA��A��/A���A��A���A�|�A�n�A�\)A�?}A��A���A���A��uA�v�A�hsA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�`BA�33A�
=A��yA���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A�A���A�ȴA���A���A�ĜA�AۼjAۺ^Aۺ^A۲-Aۧ�Aۡ�Aۗ�AۓuAۓuAە�Aە�Aۗ�Aە�Aۗ�Aۗ�Aۙ�Aۙ�Aۗ�Aۗ�Aۙ�Aۛ�Aۛ�Aۛ�Aۛ�A۝�A۝�A۝�A۝�A۟�A۟�Aۡ�A۬Aۧ�Aۡ�A�v�A�A���A�ffA�bA�C�A˛�A��;A�v�A��A��HA�%A��A��A���A�^5A���A��A�l�A�?}A�+A��\A�S�A�=qA���A�~�A��FA��A�\)A�A���A�x�A�n�A���A�33A��A��yA�JA��A���A�A�n�A��A��\A�^5A�E�A�hsA��A��A�{A��A�`BA���A�S�A��HA�I�A�`BA�n�A��TA�dZA��mA�G�A�
=A���A�M�A�  A�"�A}��A|  Ay�7Au�ArbNAp�9Ao?}Al��Ah�9Af$�Ae?}Ac�A_t�A]��A\(�AY�AWS�AS&�AQ��AO�AL$�AKVAJ=qAF{AA%A??}A=��A;�
A:=qA8�/A7?}A4��A3�A1��A1oA/S�A,��A*�A)ƨA(bA'ƨA'O�A&�A&�A&VA%��A#�#A"�yA"�\A!ƨA �yA �AhsA��A�A�A�TA�hA�A �A�-A1A�AQ�AK�A��A\)A�A�AffA�mA��A;dA�AffA�A?}A
v�A
(�A
  A	oA�A��An�A�AQ�A�AG�A33A�9AC�A ��A 9X@��@�"�@�v�@�9X@�Q�@��\@�@���@�&�@�@���@�Z@�K�@�bN@�Ĝ@�z�@���@�v�@�hs@�?}@���@��@���@���@�@�P@�|�@���@���@���@�9X@��;@�$�@�@�7@�+@�{@�
=@��@�P@�@���@ᙚ@�V@���@�9X@�-@܋D@�dZ@���@ج@�`B@�p�@���@���@�dZ@ج@�1'@���@�-@�-@��@Չ7@��@��/@Դ9@�1'@�o@�%@�z�@�1'@��
@�dZ@�+@�M�@�V@��;@�b@̛�@�bN@�~�@�7L@�t�@�S�@Ƈ+@��@ģ�@�x�@�@��/@�A�@�5?@�X@��@���@��D@�9X@���@�"�@�@�x�@�X@�O�@�Ĝ@�Ĝ@��^@�v�@��@��7@�7L@���@��j@��@��@���@���@�@��^@�{@��#@�J@�{@���@�x�@�/@�1'@�1@� �@�1'@�9X@�A�@�1@��@���@�~�@���@���@�j@��@�ƨ@�+@�ȴ@�n�@�$�@��@�7L@�7L@�V@�%@��@�  @�  @�  @�  @��;@�ƨ@��w@�\)@�ȴ@��#@���@�X@�7L@�&�@��@��`@��@�Q�@��m@�  @�ƨ@��P@�;d@�o@���@��+@�n�@�ff@�J@�X@��@��D@�1'@��;@��P@�+@���@���@�~�@��@��@�G�@��@�O�@��@�V@��`@�r�@�  @���@�dZ@�S�@�K�@��@���@��!@��\@�v�@�^5@�@�@�@���@��-@��@�?}@��@�%@���@�z�@�A�@���@�33@���@��!@��!@�V@���@���@�p�@�/@���@���@��@��@�Ĝ@�r�@�A�@�1'@�1@��@�"�@��@�~�@�J@���@�&�@���@���@��u@�Z@�1'@�b@��;@���@�C�@��@�v�@��@��^@��^@��@�Ĝ@�z�@�(�@��@�r�@�Q�@��w@�|�@�dZ@�;d@���@��+@�v�@��+@�n�@�=q@�-@�J@���@��@�Ĝ@���@�A�@��m@��;@�1@��
@�ƨ@���@���@�33@��y@��!@�v�@�M�@�^5@��@�Ĝ@��m@�|�@�S�@�C�@�K�@�C�@���@���@�~�@�M�@��@��@�hs@�X@�%@���@��D@��D@��D@�j@�b@���@���@��@�l�@�\)@�o@���@��R@���@�n�@�{@��-@��-@��7@���@��T@���@���@�hs@�G�@���@���@��u@�j@�(�@��w@��@�t�@�+@��!@�n�@�v�@�v�@�n�@�^5@�E�@�{@��^@��7@���@��^@�x�@���@���@�Z@�@�P@~E�@}�h@}/@|��@{��@yX@x��@xbN@x  @w�P@w+@v�R@v�+@vv�@v{@u@u�@uO�@u/@tz�@t9X@sƨ@sC�@r�\@r-@q�^@qX@q%@p��@pbN@o��@o|�@o+@n�@nv�@n{@m�@l�@lj@k�m@kt�@k"�@j�H@j~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A�A���AۼjA���A۾wA�A�ƨA�ĜA���A�ƨA�ȴA���A���A���A���A���A���A�ĜA�ȴA�AۼjA���A۾wA�A۾wA�ƨA���A�ȴA�A�ĜA�ĜAۼjA۸RA۸RAۼjA۶FAۺ^A۸RAۼjA۶FAۼjA۶FAۺ^Aۺ^A۸RA۾wA۸RA۸RA۸RAۺ^A۴9A۲-A۶FA۰!A۲-A۰!A۰!A۶FAۮA۰!Aۡ�A۟�Aۣ�Aۡ�Aۣ�Aۣ�A۟�Aۡ�Aۛ�Aۥ�A۝�Aۗ�Aۙ�AۓuAۙ�AۓuAە�Aە�AۓuAۗ�Aۏ\Aۗ�AۑhAە�Aە�AۑhAە�AۑhAە�Aە�AۑhAۗ�AۑhAە�Aە�AۓuAۗ�AۓuAۙ�Aە�AۓuAۙ�Aۗ�Aە�Aۙ�Aە�Aە�Aۙ�AۓuAۙ�Aە�Aۗ�Aۗ�AۓuAۗ�Aە�Aۗ�Aۗ�Aە�Aۛ�Aە�Aە�Aۛ�Aۗ�Aۗ�Aۙ�Aە�Aۛ�Aۗ�Aۗ�Aۛ�Aە�Aۙ�Aۛ�Aە�Aۗ�Aۙ�Aۗ�Aۛ�Aە�Aۗ�Aۛ�Aۗ�Aۗ�Aۛ�Aە�Aۙ�Aۙ�Aە�Aۛ�Aۗ�Aۗ�Aۛ�Aە�Aۛ�Aە�Aۛ�Aۙ�Aۗ�Aۛ�Aۙ�Aۗ�Aۛ�A۝�Aۗ�Aۙ�A۝�Aۙ�Aۛ�A۝�Aۗ�A۝�Aۗ�Aۛ�A۝�Aۙ�Aۛ�A۝�Aۙ�A۝�A۝�Aۙ�Aۛ�A۟�A۝�Aۙ�A۟�Aۛ�Aۛ�A۟�Aۛ�A۝�A۝�Aۙ�Aۡ�Aۛ�A۟�A۟�Aۛ�A۟�A۟�Aۛ�A۟�Aۡ�Aۛ�A۝�Aۡ�A۟�Aۛ�A۟�Aۡ�Aۛ�A۟�Aۡ�Aۛ�A۝�Aۡ�A۟�A۟�Aۣ�A۟�A۝�Aۣ�A۟�A۟�Aۣ�Aۣ�A۝�Aۣ�Aۡ�Aۡ�AۮA۬A۩�AۮA۬Aۧ�A۬A۬Aۥ�A۩�A۬Aۥ�Aۥ�Aۧ�Aۡ�A۟�Aۥ�Aۣ�A۟�Aۣ�A۟�AۑhAۉ7Aۇ+A�~�A�~�AہA�v�A�A�A��Aک�A�"�A���AٓuA�33A���Aا�A�S�A���A�bNA�A��/A�O�A�`BA�|�A���AхA�/A�1A��A��#AиRAЗ�A�z�A�5?A�ƨA�p�A�C�A��AΥ�A�p�A�`BA�A�A���AͅA�VA�\)A���A�n�A�K�A�
=A���A�~�A�VA�+A��/A���A�r�A�\)A�I�A�oA��;AȑhA�Q�A��mAǲ-A�bNA��A�AƃAś�A�/A���A�M�A� �Aú^A�$�AA�ȴA�hsA��RA�t�A�G�A�
=A���A��A�S�A�oA��A���A�ĜA���A��9A��+A�^5A�5?A�VA��mA���A�A��9A���A���A���A���A��hA��DA��PA��DA�~�A�hsA�=qA�
=A��/A��RA�v�A�A��hA�=qA���A��9A�1A��A��;A���A���A��hA�~�A�r�A�hsA�ZA�ZA�VA�E�A�=qA�=qA�=qA�9XA�9XA�7LA�/A�/A�&�A��A��A�1A��yA���A���A�O�A��HA�VA�  A���A�{A��#A���A�ĜA��DA�bNA�S�A�E�A���A��mA��FA�A�A��FA�K�A�-A�1A��A��TA��!A�ZA�5?A� �A�{A���A��#A��^A��PA�x�A�=qA��`A���A��\A�^5A�C�A� �A��A���A�ffA�K�A� �A��
A���A�|�A�^5A�1'A�A��A�n�A�5?A�$�A�VA�A��A��TA��A���A���A��9A���A���A���A��+A�z�A�n�A�^5A�O�A�5?A�VA�A��HA��wA���A��PA�|�A�\)A�I�A�+A�JA�  A���A��A��TA��/A��A���A���A�ĜA��wA��9A���A��A�t�A�ffA�`BA�VA�K�A�=qA�+A�"�A� �A��A�bA�
=A�%A�  A��yA���A���A��\A�hsA�A�~�A�5?A�(�A��A��A�ffA�M�A���A��RA��DA�I�A�E�A�7LA�
=A�A���A��`A��;A���A���A��DA�E�A�1A���A���A���A�ZA�?}A�;dA�5?A�1'A�+A��A���A��A��`A��mA��;A���A�ĜA��RA��A���A��DA�|�A�r�A�hsA�^5A�I�A�5?A�"�A�bA�A��A��-A���A��A�VA�9XA��A��A�VA���A���A���A���A�ZA���A��jA���A��A�l�A�`BA�?}A�oA���A��A��#A��A��hA��A�|�A�x�A�r�A�;dA��A���A�\)A�A�A�(�A�JA���A���A��A��A���A��hA�v�A�`BA�?}A��A�A���A��HA��HA��HA��#A��wA�t�A�jA�C�A�A���A���A�r�A�E�A��A��/A���A���A��A�ZA��A���A���A���A��A�n�A�O�A�9XA�"�A�1A���A��TA��wA��PA�dZA�?}A�
=A���A��+A�hsA�E�A�bA��wA��PA��DA��DA��A�jA�=qA�JA���A��9A��+A�^5A�5?A���A���A���A�v�A�G�A�+A�bA���A��TA���A��FA��7A�ZA�;dA�33A�&�A� �A��A�
=A�A���A���A��A��HA��FA���A��PA��A�|�A�v�A�r�A�n�A�^5A�VA�K�A�?}A�9XA�&�A��A�
=A���A���A��A��
A���A��
A���A�ĜA��^A���A��DA�x�A�jA�VA�?}A�+A�JA��A��/A���A��A���A�|�A�n�A�\)A�?}A��A���A���A��uA�v�A�hsA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�`BA�33A�
=A��yA���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                   111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B��B��B��B��B��B�$B��B�B�$B�XB�XB�XB��B��B��B��B��B�RB�B��B��B�RB�RB�XB�XB��B��B��B��B��B�*B��B��B��B��B��B��B�0B��B��B��B�B�B��B�HB�OB��B��B)�Bm�B��B�B��B��B��B��B�B�xB�bB�_B�_B��B�4B�MBx8Bl"BiBe,Be�Bc�B[WB]/BS[BN�BJ�BF?B?�B5?B1[B+�B%zB�B�B�B�GB�]B�sB�B�gB��B�$B�	B�.B�iBp�BbNBOBB>�B,�B"�B�B�B�B
��B
�B
�>B
�
B
�XB
��B
��B
��B
r|B
\�B
QB
@B
6zB
,qB
@B
 4B	��B	��B	�#B	�?B	��B	��B	��B	�uB	�GB	|B	p�B	gmB	S�B	E9B	A�B	.IB	(�B	 �B	+B	�B��B�TB�B�B�jB��B� B��BȀB��B�UB��B��B��B��B�kB�0B�*B�$B�LB�LB�FB��B��B�!B��B��B�B��B��B�YB�MB��B��B�B��B�FB�=B�B��B�B��B��B�(B��B��B�(B�\B��B��B� B�B�B�B��B��B��B��B��B�B�	B��B�kB�eB��B�B��B�=B�B��B��B�-B��B��B�B�B�*B�RB�B�B��B�QB��B�^B̘B�XB�3B̘B��B�2B��B�dB��B	uB	�B	 B	�B	"4B	0�B	6B	7�B	1[B	,�B	#�B	&�B	5tB	?�B	@�B	D�B	L0B	J#B	PB	S&B	U2B	V�B	QNB	PHB	K�B	M�B	?}B	@B	O�B	V�B	S�B	ZQB	a�B	bNB	cTB	d&B	g8B	iDB	iyB	iDB	l"B	n�B	o�B	o�B	v�B	v+B	v`B	u�B	u�B	tTB	rB	n�B	v�B	� B	��B	~�B	|�B	w2B	y	B	}�B	{B	~]B	�JB	��B	�{B	�B	��B	�VB	��B	�bB	�bB	� B	�oB	��B	��B	�B	�=B	�B	��B	�bB	��B	��B	�B	�<B	�B	��B	��B	��B	�B	��B	�[B	B	��B	�B	�EB	�B	�)B	̘B	�B	ΥB	��B	ϫB	�B	�}B	бB	бB	�NB	�NB	� B	�NB	� B	��B	��B	��B	҉B	уB	ӏB	ӏB	ҽB	�,B	՛B	��B	��B	��B	�|B	�|B	�B	��B	�B	��B	�B	��B	�&B	��B	�B	�fB	�B	�
B	�B	��B	�>B	�DB	�B	�WB	��B	�iB	�iB	��B	�vB	��B	�B	��B	��B	�fB	�fB	�fB	��B	�8B	��B	�lB	�rB	��B	�rB	�rB	�xB	�rB	��B	�xB	�]B	��B	��B
 iB
 �B
�B
B
GB
GB
MB
SB
�B
�B
�B
�B
�B
	�B
	�B

	B

rB
xB
xB
DB
DB
xB
~B
~B
�B
�B
�B
�B
(B
�B
�B
�B
�B
�B
4B
�B
�B
 B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
B
B
$B
�B
�B
�B
_B
�B
+B
YB
B
�B
�B
�B
@B
B
�B
�B
eB
_B
�B
�B
B
7B
�B
1B
qB
CB
B
�B
~B
~B
B
qB
qB
xB
xB
xB
 �B
!bB
!�B
#�B
#:B
"hB
!�B
 �B
!�B
 �B
"�B
"4B
!�B
 �B
 \B
 �B
 �B
 �B
!�B
"4B
"�B
#�B
$@B
$B
$B
#nB
$B
%B
%�B
&LB
&LB
&LB
&B
&�B
&B
&B
%�B
&�B
'�B
($B
'�B
'�B
'�B
)_B
)_B
)_B
)�B
,B
+�B
.�B
/�B
1[B
1'B
1[B
0�B
0�B
0�B
0UB
1[B
1�B
0UB
0�B
/�B
/�B
/�B
1[B
1�B
1�B
1�B
2-B
3hB
3�B
2�B
49B
5?B
6B
5tB
5?B
6FB
6�B
6�B
5�B
5�B
6�B
6�B
6zB
7�B
7�B
7�B
7�B
8RB
8�B
9$B
9�B
:�B
<B
<�B
=B
=qB
?B
@OB
@�B
A B
AUB
B[B
B[B
C-B
CaB
C�B
C�B
DgB
EB
EB
FB
FB
F�B
GB
GB
GzB
G�B
HB
HKB
H�B
H�B
H�B
H�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�$B�XB�0B��B��B�XB��B��B��B��B��B�^B�XB��B��B��B��B�^B��B�XB�*B��B��B�B��B��B�B��B�B��B��B�B�^B��B��B��B��B��B�XB�RB��B��B��B�XB��B�*B��B��B�RB��B�$B��B�B��B��B��B�6B�RB��B�B�XB�LB��B�LB��B�RB��B�B�RB��B�LB��B�RB�$B��B�B��B��B�*B��B��B��B�zB��B�B�B�$B�B��B��B�B��B��B�B��B�XB��B�B�$B��B��B��B�B��B�B�XB��B��B��B�LB��B�LB��B�B��B�$B�B��B�RB�RB�$B��B��B��B�XB��B��B�XB��B�RB��B��B�B��B��B��B��B��B��B��B��B�XB�$B��B�RB�^B�^B��B��B��B�$B��B��B�*B��B��B��B��B��B�0B��B�XB�^B�RB��B��B��B��B��B�$B��B��B�*B��B�XB��B�0B��B�B��B�B��B��B�dB��B��B��B�*B��B��B��B��B��B�^B�*B��B��B�B��B��B��B��B��B�B��B�^B��B�0B�*B��B�B��B��B�dB�*B�B��B��B�6B��B�6B��B��B��B��B��B�B�0B�dB��B�jB��B�6B�BB�jB�jB��B��B�6B�B��B�dB�B��B�B��B�B��B��B��B�B�6B�B�B��B��B�B��B��B��B��B��B��B�B��B�B��B��B��B�}B��B�'B�B��B� B��B�'B�dBΥB��B�NB҉B� B��B�`B�B�B'�B/OB1'B:�B?HBC-BIBQBj�B^5BXBw2B�B�mB��B�kB��B��B�B��B��B�:B�@B��B�CB�=B�@B�_B�hB��B�bB�FB��B�3B��B��B��B��B��B�B��B��B��B��B�B��B�B��B�B��B��B�XB��B�B��B�:B�4B��B��B�6B��B��B�	B��B��B�\B��B�XB�MB��B��B��B�B�.B�VB�JB�B��B�SB�SB�GB��B�B�lB�1B�%B�fB��B��B�{B��B�iB�AB�iB� B�iB~]B}�B~�B��B��B�iB}"B� B�oB�rB�MB�uB��B�$Bv�BpBpBrBp;Bl�Bm�Bl�Bj�Bl�BjBi�Bk�Bi�Bg�Bg8Bh�BffBe`Bg8Bd�Bc�BffBaBdZBd&Bd�Bd�Bd�Bp;Bs�Bc BjKBn/B[�BZBYBa�B\�BW�BXBbNBV9B[#Bf�BiDB`�BS[BR�BS�BP}BS[BYBPBL�BNBOvBRTBMBNBIRBYBPHBIBH�BD�BIRBHKBL�BJXBEmBA�BE9BD3BB�BB[B<B@OBGzB<jB8RB>B7LB6�B5?B5�B4B4B2-B2�B3�B2aB/�B1[B2�B0�B0�B.�B1�B1�B2�B+6B7LB/�B,�B*�B+B-CB+kB+B)�B&�B'�B&LB'�B&�B$�B$@B$B%B%�B&B'�B%B#:B"�B!�B!-B!-B \B!�B�B~B�B�B~B	BBOB�B�BMBkB$�B"4BBDB"B�B
=B%B%�BqB�lB��B��B�GB��B�|B�B��B��B�vB�	B�/B��B�B�B��B��B�yB�5B��B��B��B��B�;B��BچB�KBخB�KB��B��B�B�EB��BרB��B��B��B�,B�aB��B�,B��B�B��B�gB̘B��B�pBɺB�mB�?B�9B�KB�EB��B��B�#B�-B�mB��B�B��B�zB��B��B��B�3B�B��B��B�}B��B��B��B�B��B�B��B��B��B��B�OB�OB��B��B�B��B�B��B�B�xB��B��B�{B�4B��B� B�$B�FB�PB��B�\B�VB�DB��B��B��B�fB��B{�B}�B�B��B��Bv�Bu�Bs�BsMBtTBp�Bq�Bm�Bm�Bn�Bp�Bm�Bh�BiyBj�BjBcTB\�B]/B^jBc BS�BP�BOBOvBRTBR�BUgBQBK�BI�BFBGEBE�BB�B?�B@OB=�B9�B7LB6�B2�B2�B1�B5?B1'B+B(�B'�B&B&�B%�B%FB#�B"hB#:B$�B)�B \B�B�B�BCB	B=BxB�BxB+B�BkB�B�BYBB$B�BuB�B�B�BoB�BoB�B�B"BBB	�B�B_B�B �B�B
��B
��B
��B
�VB
��B
�"B
��B
��B
�MB
�B
�5B
�/B
��B
�]B
��B
�"B
��B
�B
��B
�B
��B
�WB
�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                   444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                   444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022111500022220221115000222IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022111605014220221116050142QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022111605014220221116050142QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      TEMP                            D�� G�O�D���G�O�@�  G�O�Valu passes DMQC                SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                