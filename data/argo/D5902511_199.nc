CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-02-04T14:44:07Z creation; 2023-02-10T23:09:43Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   U   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       [   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   s,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       y0   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �x   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      !�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 9�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   :   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   @   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   F   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Ld   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Ll   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Lt   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � L�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   M   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   M    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    M(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        MH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        MP   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       MX   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    M`Argo profile    3.1 1.2 19500101000000  20220204144407  20230210230943  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_199                 6810_008521_199                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٷ!�<��@ٷ!�<��11  @ٷ"Ov@ٷ"Ov@0LA5Tu�@0LA5Tu��d��cI{t�d��cI{t11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@=p�@z�H@�  @�G�@�G�A ��A  A{A+�A@��A`��A\)A�  A�Q�A��A�  A�Q�A�\)A�Q�B Q�B�B�
B(�B�
B(  B0  B8(�B@  BH  BP  BXQ�B`Q�Bg�
Bo�
Bx  B�  B�  B�  B��B�  B�  B�  B�{B�{B��B�  B�{B��B��B�  B��B��
B�  B�  B�  B�  B��
B��
B��B�{B�{B�  B�  B��
B�{B�  B�  B��C  C
=C  C  C
  C�C  C  C  C{C{C
=C
=C
=C  C��C"
=C$  C&  C(  C*
=C,  C.
=C0
=C2
=C4
=C6�C8
=C9��C<{C>
=C@  CA�CD  CF  CG��CJ{CL  CN  CP{CR
=CT  CV  CW�CZ  C\  C^
=C`  Cb  Cd{Cf  Ch  Cj
=Cl  Cm��Cp
=Cr  Ct
=Cu��Cx  Cz  C|{C}�C�C�  C���C���C�
=C�C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C�  C�  C�  C���C���C���C�  C�  C���C���C���C�  C�C�  C�C�  C�  C�C�  C�  C���C�  C�C�C�C���C�  C�  C���C�  C�  C���C���C���C���C�  C���C�C�  C���C�  C�C�C�  C�  C���C���C�C�  C�  C���C�  C�C�C�C�  C���C�C�  C���C�  C�C�C�C���C�  C�  C���C�C�  C���C���C���C�  C�  C���C���C���C�
=C�C�  C���C���C�C�C�  C�C�
=C�C�
=C�
=C���C��C���C���C�  C�  C�C�  C���C�  C�C�  C�  C���C�C���C�  C�C�  C���D }qD  D� D  D��D�D� D  D� D�qDz�D�qD� D  D}qD  D}qD��D	}qD	��D
z�D  D�D�D��D�D}qD�qD}qD��DxRD  D� D�qDz�D  D}qD�qD�D  Dz�D�qD� D�qDz�D�qD��DD�D�qD}qD�qD}qD�D��D�qD��DD�D  D��D�D}qD��D � D �qD!}qD"�D"��D#  D#z�D#��D$��D%�D%}qD%�qD&}qD&�qD'� D'��D(� D)�D)� D)�qD*� D+  D+� D,  D,z�D,��D-� D.D.� D/  D/� D0  D0��D1D1� D1�qD2}qD2�qD3��D4�D4��D5�D5� D6  D6� D6�qD7z�D7�qD8� D8�qD9� D:�D:� D;  D;�D<  D<}qD=  D=� D>  D>� D?  D?}qD@�D@��DA  DA� DB�DB��DC  DC� DD  DD��DE�DE� DE�qDF}qDF�qDG� DH  DH}qDI  DI��DJ�DJ� DK  DK� DL�DL��DM  DM� DN  DN� DO  DO}qDO�qDP}qDQ  DQ}qDR  DR��DS  DS}qDS�qDT}qDT��DUz�DV  DV}qDV�qDW}qDW�qDX}qDY�DY��DZ  DZ}qD[  D[� D\  D\� D]  D]��D^�D^� D_  D_��D`�D`� Da  Da� Db  Db� Dc  Dc��Dd�Dd� Dd�qDe� Df�Df� Dg  Dg� Dg�qDh}qDh�qDi� Dj�Dj� Dj�qDk� Dl  Dl}qDm�Dm��Dn  Dn� Do�Do� Dp  Dp��Dp�qDq� DrDr�Ds  Ds� Dt�Dt��Du�Du��Du��Dv� Dw�Dw� Dx  Dx}qDy  Dy��Dy�qDz}qD{�D{��D{�qD|}qD}  D}� D}��D~z�D~�qD� D�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D��HD��HD�  D�AHD��HD���D���D�@ D�}qD���D�  D�AHD��HD�� D�  D�>�D�� D�� D�HD�@ D�~�D��qD��qD�@ D��HD�� D�  D�B�D�� D���D�  D�@ D�� D�� D�HD�B�D��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?aG�?�=q?�Q�?�@�\@�R@333@E�@aG�@u@��
@�33@�p�@���@�@�G�@˅@��H@�@��@��RAA
=qA�\AQ�Ap�A$z�A+�A0  A7�A>{AB�\AJ�HAO\)AUA\��AaG�Ag�An�RAs�
Ay��A�Q�A��HA�p�A���A�(�A�{A���A���A�
=A��\A�A�  A�33A��RA���A��
A�\)A���A�z�A�  A��\A���A�  A�33A�A�  A˅AθRA���A��
A�\)Aٙ�A�(�A߮A��HA���A��A�A�p�A���A�(�A��RA�G�A���A�\)B ��B=qB  B��B=qB  B	�B
ffB(�Bp�B�\B(�B�B
=Bz�B=qB�B��B=qB�
B�B=qB�
B!p�B"ffB$(�B%��B&�RB((�B)�B+\)B,��B-B/�B0��B2{B3�B5G�B6ffB7�B9p�B:�RB<  B=p�B?33B@Q�BA��BC33BD��BF{BG\)BH��BJ�RBK�BM�BN�\BPQ�BQG�BR�HBT��BUBW
=BX��BZ=qB[\)B\��B^=qB_�B`��Bb�\Bc�
Bd��Bf�\Bh(�BiG�Bj�\BlQ�Bmp�Bn�RBpz�Bqp�Br�HBtz�Bup�Bv�HBxz�ByBz�RB|(�B}B
=B�{B���B��B�=qB���B���B�=qB��HB�p�B�=qB���B�p�B�{B���B���B�(�B��RB�\)B�(�B���B�\)B�(�B��HB�p�B�  B���B���B�{B���B���B�(�B��RB���B�=qB��RB��B�Q�B���B�\)B�(�B��HB�\)B�{B���B�G�B��B��RB�\)B�B�z�B�G�B��B�ffB���B�B�z�B���B���B�ffB��B��B�ffB�
=B��B�Q�B�
=B��B�(�B���B��B�(�B��HB��B�=qB���B��B�Q�B���B�p�B�=qB��RB�G�B�B��\B���B�\)B�  B���B���B�p�B�{B�ffB��RB�G�B��B�  B�=qB��RB�33B�\)B�B�(�B�ffB��\B�
=B�p�B�B��B�ffB���B���B�G�B��B�(�B�z�B���B�
=B�G�B��
B�(�B�Q�BĸRB�33BŅBŮB�{BƏ\B��HB��B�\)B��
B�(�B�ffBȸRB��BɅB�B�  B�z�B��HB�
=B�\)B�  B�=qB�ffB��HB�\)BͮB��
B�Q�B���B���B�33BϮB�(�B�ffBЏ\B�
=B�\)BхB�  B�Q�Bҏ\B���B�\)BӮB��
B�=qBԸRB�
=B�33Bՙ�B�{B�=qB֏\B��HB�p�B׮B��
B�=qBظRB���B�33BٮB�{B�ffBڣ�B���B�p�B��B�(�B�ffB���B�\)Bݙ�B��
B�Q�B޸RB���B�G�B��
B�(�B�ffB���B�G�BᙚB��
B�=qB���B��B�\)B�B�Q�B�\B���B�p�B�  B�Q�B��B�
=B癚B�{B�=qB�RB�G�B�B��B�z�B��HB�33B�B�(�B�z�B���B�\)B��
B�{B�z�B��B�p�B�B�=qB��HB�33B�B�{B��B���B�\)B��B�=qB���B�33B���B��B�Q�B���B�G�B���B��
B�z�B���B�33B��B�(�B�ffB��HB�\)B��B��B�z�B��HB��B��B�(�B�Q�B���B�\)B��B��C 33C z�C ��C C  CG�CffC�\C�
C{C33Cp�C�C��C
=CQ�Cp�C��C�C�C=qCz�CC�HC{CG�C��C�RC�C33CffC�CC
=C33C\)C��C�HC  C33Cp�C�C��C	  C	G�C	z�C	��C	��C
{C
G�C
p�C
��C
�HC�C=qCp�C�C�C{C=qCp�C�RC  C�CG�C�\C��C  C�CffC�C�HC
=C=qC�C�RC�C�C\)C��C�RC�C(�CffC�\C�RC��C=qCz�C�C�
C{C\)C��CC��CG�C�C�RC�HC{CffC��CC  CG�C�C�RC�HC{C\)C��CC��C33Cz�C��C�HC�C\)C��CC  CQ�C�\CC�C33C�C�RC�HC33Cz�C�C�
C(�CffC��C��C  CG�C�\C��C{C=qCz�CC 
=C =qC ffC ��C ��C!=qC!p�C!��C!�HC"�C"ffC"�C"�HC#{C#G�C#�C#�
C${C$=qC$p�C$�RC$��C%G�C%p�C%��C%�HC&=qC&z�C&�RC&�C'�C'ffC'�C'��C(33C(z�C(�RC)  C)=qC)ffC)�C*  C*G�C*p�C*�C*��C+=qC+�C+�RC+�C,(�C,ffC,�C,�C-=qC-�C-C.
=C.G�C.p�C.�C.��C/G�C/�\C/��C0  C0=qC0�C0�
C1�C1Q�C1�\C1��C2{C2\)C2��C2�
C3
=C3\)C3��C3�C4(�C4\)C4��C4�
C5(�C5p�C5�C5��C6=qC6z�C6�C6�HC7�C7ffC7��C7�C8=qC8z�C8�RC8�C9�C9\)C9�\C9��C:
=C:Q�C:�\C:��C;{C;ffC;��C;�HC<�C<Q�C<�C<C<��C=33C=p�C=�RC>  C>=qC>�C>C>��C?33C?\)C?��C?�
C@{C@\)C@��C@�
CA{CAG�CAp�CA��CA�HCB(�CBffCB��CB�CC(�CCQ�CC�CCCD
=CDG�CD�\CDCD��CE�CE\)CE��CE�HCF�CF\)CF��CF�
CG
=CG=qCGp�CG�CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�\@=p�@z�H@�  @�G�@�G�A ��A  A{A+�A@��A`��A\)A�  A�Q�A��A�  A�Q�A�\)A�Q�B Q�B�B�
B(�B�
B(  B0  B8(�B@  BH  BP  BXQ�B`Q�Bg�
Bo�
Bx  B�  B�  B�  B��B�  B�  B�  B�{B�{B��B�  B�{B��B��B�  B��B��
B�  B�  B�  B�  B��
B��
B��B�{B�{B�  B�  B��
B�{B�  B�  B��C  C
=C  C  C
  C�C  C  C  C{C{C
=C
=C
=C  C��C"
=C$  C&  C(  C*
=C,  C.
=C0
=C2
=C4
=C6�C8
=C9��C<{C>
=C@  CA�CD  CF  CG��CJ{CL  CN  CP{CR
=CT  CV  CW�CZ  C\  C^
=C`  Cb  Cd{Cf  Ch  Cj
=Cl  Cm��Cp
=Cr  Ct
=Cu��Cx  Cz  C|{C}�C�C�  C���C���C�
=C�C�  C�  C�  C���C���C���C���C�  C���C���C�  C�C�  C�  C�  C���C���C���C�  C�  C���C���C���C�  C�C�  C�C�  C�  C�C�  C�  C���C�  C�C�C�C���C�  C�  C���C�  C�  C���C���C���C���C�  C���C�C�  C���C�  C�C�C�  C�  C���C���C�C�  C�  C���C�  C�C�C�C�  C���C�C�  C���C�  C�C�C�C���C�  C�  C���C�C�  C���C���C���C�  C�  C���C���C���C�
=C�C�  C���C���C�C�C�  C�C�
=C�C�
=C�
=C���C��C���C���C�  C�  C�C�  C���C�  C�C�  C�  C���C�C���C�  C�C�  C���D }qD  D� D  D��D�D� D  D� D�qDz�D�qD� D  D}qD  D}qD��D	}qD	��D
z�D  D�D�D��D�D}qD�qD}qD��DxRD  D� D�qDz�D  D}qD�qD�D  Dz�D�qD� D�qDz�D�qD��DD�D�qD}qD�qD}qD�D��D�qD��DD�D  D��D�D}qD��D � D �qD!}qD"�D"��D#  D#z�D#��D$��D%�D%}qD%�qD&}qD&�qD'� D'��D(� D)�D)� D)�qD*� D+  D+� D,  D,z�D,��D-� D.D.� D/  D/� D0  D0��D1D1� D1�qD2}qD2�qD3��D4�D4��D5�D5� D6  D6� D6�qD7z�D7�qD8� D8�qD9� D:�D:� D;  D;�D<  D<}qD=  D=� D>  D>� D?  D?}qD@�D@��DA  DA� DB�DB��DC  DC� DD  DD��DE�DE� DE�qDF}qDF�qDG� DH  DH}qDI  DI��DJ�DJ� DK  DK� DL�DL��DM  DM� DN  DN� DO  DO}qDO�qDP}qDQ  DQ}qDR  DR��DS  DS}qDS�qDT}qDT��DUz�DV  DV}qDV�qDW}qDW�qDX}qDY�DY��DZ  DZ}qD[  D[� D\  D\� D]  D]��D^�D^� D_  D_��D`�D`� Da  Da� Db  Db� Dc  Dc��Dd�Dd� Dd�qDe� Df�Df� Dg  Dg� Dg�qDh}qDh�qDi� Dj�Dj� Dj�qDk� Dl  Dl}qDm�Dm��Dn  Dn� Do�Do� Dp  Dp��Dp�qDq� DrDr�Ds  Ds� Dt�Dt��Du�Du��Du��Dv� Dw�Dw� Dx  Dx}qDy  Dy��Dy�qDz}qD{�D{��D{�qD|}qD}  D}� D}��D~z�D~�qD� D�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D��HD��HD�  D�AHD��HD���D���D�@ D�}qD���D�  D�AHD��HD�� D�  D�>�D�� D�� D�HD�@ D�~�D��qD��qD�@ D��HD�� D�  D�B�D�� D���D�  D�@ D�� D�� D�HD�B�D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?aG�?�=q?�Q�?�@�\@�R@333@E�@aG�@u@��
@�33@�p�@���@�@�G�@˅@��H@�@��@��RAA
=qA�\AQ�Ap�A$z�A+�A0  A7�A>{AB�\AJ�HAO\)AUA\��AaG�Ag�An�RAs�
Ay��A�Q�A��HA�p�A���A�(�A�{A���A���A�
=A��\A�A�  A�33A��RA���A��
A�\)A���A�z�A�  A��\A���A�  A�33A�A�  A˅AθRA���A��
A�\)Aٙ�A�(�A߮A��HA���A��A�A�p�A���A�(�A��RA�G�A���A�\)B ��B=qB  B��B=qB  B	�B
ffB(�Bp�B�\B(�B�B
=Bz�B=qB�B��B=qB�
B�B=qB�
B!p�B"ffB$(�B%��B&�RB((�B)�B+\)B,��B-B/�B0��B2{B3�B5G�B6ffB7�B9p�B:�RB<  B=p�B?33B@Q�BA��BC33BD��BF{BG\)BH��BJ�RBK�BM�BN�\BPQ�BQG�BR�HBT��BUBW
=BX��BZ=qB[\)B\��B^=qB_�B`��Bb�\Bc�
Bd��Bf�\Bh(�BiG�Bj�\BlQ�Bmp�Bn�RBpz�Bqp�Br�HBtz�Bup�Bv�HBxz�ByBz�RB|(�B}B
=B�{B���B��B�=qB���B���B�=qB��HB�p�B�=qB���B�p�B�{B���B���B�(�B��RB�\)B�(�B���B�\)B�(�B��HB�p�B�  B���B���B�{B���B���B�(�B��RB���B�=qB��RB��B�Q�B���B�\)B�(�B��HB�\)B�{B���B�G�B��B��RB�\)B�B�z�B�G�B��B�ffB���B�B�z�B���B���B�ffB��B��B�ffB�
=B��B�Q�B�
=B��B�(�B���B��B�(�B��HB��B�=qB���B��B�Q�B���B�p�B�=qB��RB�G�B�B��\B���B�\)B�  B���B���B�p�B�{B�ffB��RB�G�B��B�  B�=qB��RB�33B�\)B�B�(�B�ffB��\B�
=B�p�B�B��B�ffB���B���B�G�B��B�(�B�z�B���B�
=B�G�B��
B�(�B�Q�BĸRB�33BŅBŮB�{BƏ\B��HB��B�\)B��
B�(�B�ffBȸRB��BɅB�B�  B�z�B��HB�
=B�\)B�  B�=qB�ffB��HB�\)BͮB��
B�Q�B���B���B�33BϮB�(�B�ffBЏ\B�
=B�\)BхB�  B�Q�Bҏ\B���B�\)BӮB��
B�=qBԸRB�
=B�33Bՙ�B�{B�=qB֏\B��HB�p�B׮B��
B�=qBظRB���B�33BٮB�{B�ffBڣ�B���B�p�B��B�(�B�ffB���B�\)Bݙ�B��
B�Q�B޸RB���B�G�B��
B�(�B�ffB���B�G�BᙚB��
B�=qB���B��B�\)B�B�Q�B�\B���B�p�B�  B�Q�B��B�
=B癚B�{B�=qB�RB�G�B�B��B�z�B��HB�33B�B�(�B�z�B���B�\)B��
B�{B�z�B��B�p�B�B�=qB��HB�33B�B�{B��B���B�\)B��B�=qB���B�33B���B��B�Q�B���B�G�B���B��
B�z�B���B�33B��B�(�B�ffB��HB�\)B��B��B�z�B��HB��B��B�(�B�Q�B���B�\)B��B��C 33C z�C ��C C  CG�CffC�\C�
C{C33Cp�C�C��C
=CQ�Cp�C��C�C�C=qCz�CC�HC{CG�C��C�RC�C33CffC�CC
=C33C\)C��C�HC  C33Cp�C�C��C	  C	G�C	z�C	��C	��C
{C
G�C
p�C
��C
�HC�C=qCp�C�C�C{C=qCp�C�RC  C�CG�C�\C��C  C�CffC�C�HC
=C=qC�C�RC�C�C\)C��C�RC�C(�CffC�\C�RC��C=qCz�C�C�
C{C\)C��CC��CG�C�C�RC�HC{CffC��CC  CG�C�C�RC�HC{C\)C��CC��C33Cz�C��C�HC�C\)C��CC  CQ�C�\CC�C33C�C�RC�HC33Cz�C�C�
C(�CffC��C��C  CG�C�\C��C{C=qCz�CC 
=C =qC ffC ��C ��C!=qC!p�C!��C!�HC"�C"ffC"�C"�HC#{C#G�C#�C#�
C${C$=qC$p�C$�RC$��C%G�C%p�C%��C%�HC&=qC&z�C&�RC&�C'�C'ffC'�C'��C(33C(z�C(�RC)  C)=qC)ffC)�C*  C*G�C*p�C*�C*��C+=qC+�C+�RC+�C,(�C,ffC,�C,�C-=qC-�C-C.
=C.G�C.p�C.�C.��C/G�C/�\C/��C0  C0=qC0�C0�
C1�C1Q�C1�\C1��C2{C2\)C2��C2�
C3
=C3\)C3��C3�C4(�C4\)C4��C4�
C5(�C5p�C5�C5��C6=qC6z�C6�C6�HC7�C7ffC7��C7�C8=qC8z�C8�RC8�C9�C9\)C9�\C9��C:
=C:Q�C:�\C:��C;{C;ffC;��C;�HC<�C<Q�C<�C<C<��C=33C=p�C=�RC>  C>=qC>�C>C>��C?33C?\)C?��C?�
C@{C@\)C@��C@�
CA{CAG�CAp�CA��CA�HCB(�CBffCB��CB�CC(�CCQ�CC�CCCD
=CDG�CD�\CDCD��CE�CE\)CE��CE�HCF�CF\)CF��CF�
CG
=CG=qCGp�CG�CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�l�A�l�A�l�A�jA�ffA�l�A�n�A�n�A�p�A�n�A�p�A�p�A�r�A�t�A�v�A�t�A�l�A�l�A�z�A�z�A̅Ả7A̍PA̗�A̡�A̰!A̶FA���A��#A���A��A�/A�+A��A�VA�VA�(�A��A��A�oA�{A�"�A�9XA�5?A�9XA�XA�ZA�\)A�O�A�A�A�33A��A��`A�dZA�ffA�O�A��mA���A�
=AÅA���A�-A���A��FA�ffA��A��A��
A�1A�z�A��A��A��yA�oA�O�A�ZA���A�|�A�`BA���A��!A���A�A�^5A��/A���A���A�A�A��A��A���A�G�A���A��!A���A�/A��A��\A��HA���A��jA���A�9XA���A���A�t�A�\)A��
A�%A}S�Ay�
Aw�Avn�Au�PAq�TAodZAm33Ak�AhZAe�A`z�A^-AZQ�AU`BAR��AQ33AN�AN1AK�wAIp�AH=qAE��AC�
AC�A??}A=A<{A;`BA:��A8�yA6ȴA5�A4jA0�A.�`A-�#A-oA+�TA*�9A*n�A)�wA'�7A&ZA%`BA$1'A#�A!��A �A�PAA�AQ�AƨA��A�Az�A��A�FA�A��A�A�A�TAƨAXA7LA
=A��Al�A�^A1A��AȴA~�A$�AK�A1Ax�A�AE�A�AA��AS�A��AZA�hAS�A
��A
�A	��A	��A	&�A��AbNA=qAƨAp�A�AbNA-A�TA�
A��A7LA�`A�A��AffA�7A1'AXA �+@�l�@���@���@��@�9X@��@�V@�~�@��R@�5?@���@���@���@�Z@���@�@�hs@�?}@�J@��@��T@�V@�dZ@�V@�@��#@��@�1@�@�l�@�S�@�33@��@�R@�=q@�Q�@�@�Ĝ@��@�~�@��@߮@��m@�ƨ@�o@ޟ�@�$�@���@�|�@�n�@�5?@��@ف@�&�@׮@��H@�@���@ՙ�@�G�@���@ԓu@ӍP@�33@ӍP@ӍP@�@��@�~�@��#@щ7@���@��;@�l�@�33@�
=@���@�M�@���@���@�bN@�1'@��@˶F@�S�@�+@��@�$�@�@�?}@���@�  @�ƨ@ǶF@ǝ�@�t�@�K�@��@���@ř�@�%@�Ĝ@ēu@�r�@�I�@��m@��y@�$�@��^@�x�@���@��m@�33@��H@�n�@��@��T@�@�x�@�X@���@��w@�"�@�-@���@�p�@���@��j@�9X@��m@��w@�t�@��@�n�@���@�p�@�`B@�?}@��j@�Z@�(�@�  @�t�@���@�J@�x�@��@���@��^@���@��@��H@���@��\@�n�@�@��#@�`B@���@�Ĝ@��j@�r�@�1@�ƨ@�|�@�"�@���@�^5@�{@��T@��#@���@��7@�`B@�?}@���@��u@�Q�@���@�@���@�^5@�=q@�7L@��u@���@�S�@�S�@�C�@��@��@��!@�~�@�@��^@�hs@�V@�Ĝ@�j@��@�@��@��R@�v�@�^5@��@�O�@��`@���@���@��9@��@���@�|�@�|�@�|�@�dZ@�+@�@���@��y@�J@�V@�Ĝ@�j@�Q�@�A�@� �@�|�@�;d@�@���@�ȴ@���@���@�~�@�v�@�n�@�ff@�V@�E�@�$�@��#@�x�@�O�@�%@��D@�  @��
@��@�A�@��@���@��
@��w@�|�@�33@��y@�ff@�=q@��@��^@�G�@���@�r�@�(�@��@��F@���@��P@�33@���@��@���@��R@���@�~�@�V@�$�@��T@���@�p�@�X@�V@���@���@��@�bN@�Q�@�I�@�A�@�(�@�1@�ƨ@�|�@�dZ@�"�@��@��R@���@�E�@��@��T@���@��@�O�@�V@��u@�I�@��@��m@��w@���@�|�@�
=@��\@�V@�5?@�J@��-@��h@�`B@�X@�X@��@��9@���@��u@�z�@�9X@��m@�dZ@�
=@���@��\@��+@�v�@��@��@��@�`B@�/@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�n�A�jA�n�A�l�A�jA�n�A�l�A�jA�n�A�jA�hsA�l�A�bNA�dZA�l�A�l�A�jA�n�A�n�A�jA�n�A�n�A�l�A�p�A�n�A�l�A�n�A�r�A�l�A�p�A�p�A�l�A�r�A�n�A�p�A�r�A�p�A�p�A�t�A�r�A�r�A�x�A�t�A�t�A�v�A�z�A�t�A�t�A�v�A�r�A�v�A�x�A�t�A�r�A�t�A�jA�l�A�n�A�jA�hsA�jA�l�A�hsA�l�A�n�A�jA�t�A�|�A�|�A�x�A�x�A�|�A�z�A�x�A�z�A�z�A�|�A̅Ȧ+ÃÃȦ+Ȧ+A̅A̋DẢ7A̅A̋DȀ\A̋DA̍PȀ\A̍PA̋DȂhȂhA̕�A̙�A̙�A̝�A̛�A̡�Ạ�A̡�Ạ�A̩�A̩�A̩�A̮A̶FA̲-A̴9A̲-A̴9A̰!A̸RA̼jA̾wA̾wA�ƨA���A��A��#A��;A��#A��
A��/A��#A��A��HA��A���A�1A�bA��A��A�oA��A� �A�"�A�&�A�+A�1'A�1'A�33A�5?A�1'A�-A�+A�$�A� �A��A�"�A� �A��A� �A��A��A��A�{A���A�  A�A���A���A�VA��A� �A�+A�-A�(�A�+A�&�A�"�A��A��A��A��A�{A��A��A��A��A� �A��A��A��A�{A�VA�JA�bA�{A�oA�oA�oA��A��A�{A��A��A��A��A�/A�33A�33A�9XA�=qA�;dA�7LA�;dA�5?A�1'A�5?A�9XA�5?A�33A�7LA�9XA�5?A�9XA�;dA�9XA�E�A�K�A�Q�A�XA�ZA�bNA�bNA�ZA�XA�\)A�^5A�XA�VA�^5A�^5A�XA�^5A�^5A�ZA�\)A�\)A�K�A�E�A�K�A�K�A�G�A�E�A�G�A�=qA�9XA�;dA�9XA�33A�33A�7LA�1'A�+A�(�A�+A�$�A� �A��A��A��A�oA�oA�1A���A��A��A��`A��;A��;A��
A�ƨA̸RA̮A̛�Ả7A�~�A�|�A�t�A�n�A�l�A�VA�&�A�bA�A���A��mA��
A�ĜA˾wAˮA�|�A�;dA��A��AʾwAʮAʧ�Aʣ�Aʝ�AʋDA�v�A�ffA�=qA�$�A� �A��A�A���A��A��`A���A�v�A�=qA��yAȮAȁA�Q�A�$�A�bA���A���A��A��#A���A���A�ƨA�AǾwA�AǾwAǺ^AǼjAǴ9A�"�A�dZA��A��A��
AŰ!A�v�A�O�A�1A��Aģ�A�S�A���Aô9Aã�AÅA�l�A�M�A�=qA�"�A�  A��/A¾wADA�33A��`A��wA���A�z�A�A�A��A�VA��TA�ȴA��-A��!A���A��A�`BA�;dA��A��A�A���A��A�I�A�9XA�(�A��A���A���A���A�z�A�?}A��A���A�O�A���A���A� �A��yA���A���A�ĜA��wA��wA��RA��!A���A���A�z�A�S�A���A�1A�33A���A��DA�v�A�ffA�`BA�^5A�VA�33A�oA���A���A��hA��A�jA�\)A�G�A�5?A��A�
=A�%A���A��A��HA��#A�A��uA��A�p�A�O�A�9XA�+A��A�A���A��
A��jA��!A���A�t�A�bNA�;dA���A�n�A��yA��FA�ffA�E�A�7LA��A���A��A��A��#A��jA���A���A��A�p�A�`BA�O�A�C�A�9XA�/A�{A��A���A���A�l�A�G�A�(�A�  A��HA���A��FA���A��+A�t�A�`BA�E�A�7LA�(�A�VA���A��A��/A���A��jA��RA���A�|�A�n�A�I�A�$�A�JA��A��;A�A���A��uA��DA�r�A�K�A�&�A�{A���A��RA�=qA��A���A���A��A�p�A�Q�A�7LA�$�A�%A��A��TA�ȴA���A�hsA�;dA��A�  A��;A��^A���A��hA�x�A�dZA�I�A�(�A�VA�JA��A��A�
=A��A���A�ȴA��-A��+A�dZA�33A�A��
A��RA���A���A��DA�z�A�hsA�\)A�O�A�?}A��A��#A���A�p�A�S�A�A�A�$�A��A���A�&�A���A��!A�hsA�5?A��A�ĜA���A�z�A�XA�?}A�1'A�$�A��A�1A���A��A��HA���A���A���A�ĜA���A�jA��A���A�`BA�M�A�9XA��A�bA�
=A���A��mA��
A���A��hA��7A�x�A�l�A�`BA�XA�S�A�O�A�C�A�&�A�oA�VA��A���A�=qA�A���A���A��uA�n�A�`BA�O�A��A��HA�x�A�&�A���A���A���A�{A�ȴA���A��hA��A�x�A�n�A�hsA�bNA�O�A�?}A�;dA�9XA�33A�-A�(�A�{A�VA�A�ȴA��+A�bNA�^5A�ZA�A�A��A��9A�^5A��A��A���A��^A��!A���A���A���A���A��+A�|�A�p�A�VA�C�A�/A�bA���A��TA��wA�&�A��DA�ĜA��uA�dZA�(�A���A��;A���A��-A���A��7A�n�A�S�A�9XA�A���A�C�A�JA��TA��!A�/A�~�A���A��RA�ffA���A��jA��A�+A��A�%A��A��;A���A��9A���A���A���A���A���A��7A�x�A�M�A�+A��/A�5?A���A��RA���A��+A�bNA�33A�{A��A��
A��!A��PA�dZA�9XA�bA�ĜA�jA�/A�A���A�(�A��HA��PA��A�A�M�A���A��A��A���A���A��9A��71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�l�A�l�A�l�A�jA�ffA�l�A�n�A�n�A�p�A�n�A�p�A�p�A�r�A�t�A�v�A�t�A�l�A�l�A�z�A�z�A̅Ả7A̍PA̗�A̡�A̰!A̶FA���A��#A���A��A�/A�+A��A�VA�VA�(�A��A��A�oA�{A�"�A�9XA�5?A�9XA�XA�ZA�\)A�O�A�A�A�33A��A��`A�dZA�ffA�O�A��mA���A�
=AÅA���A�-A���A��FA�ffA��A��A��
A�1A�z�A��A��A��yA�oA�O�A�ZA���A�|�A�`BA���A��!A���A�A�^5A��/A���A���A�A�A��A��A���A�G�A���A��!A���A�/A��A��\A��HA���A��jA���A�9XA���A���A�t�A�\)A��
A�%A}S�Ay�
Aw�Avn�Au�PAq�TAodZAm33Ak�AhZAe�A`z�A^-AZQ�AU`BAR��AQ33AN�AN1AK�wAIp�AH=qAE��AC�
AC�A??}A=A<{A;`BA:��A8�yA6ȴA5�A4jA0�A.�`A-�#A-oA+�TA*�9A*n�A)�wA'�7A&ZA%`BA$1'A#�A!��A �A�PAA�AQ�AƨA��A�Az�A��A�FA�A��A�A�A�TAƨAXA7LA
=A��Al�A�^A1A��AȴA~�A$�AK�A1Ax�A�AE�A�AA��AS�A��AZA�hAS�A
��A
�A	��A	��A	&�A��AbNA=qAƨAp�A�AbNA-A�TA�
A��A7LA�`A�A��AffA�7A1'AXA �+@�l�@���@���@��@�9X@��@�V@�~�@��R@�5?@���@���@���@�Z@���@�@�hs@�?}@�J@��@��T@�V@�dZ@�V@�@��#@��@�1@�@�l�@�S�@�33@��@�R@�=q@�Q�@�@�Ĝ@��@�~�@��@߮@��m@�ƨ@�o@ޟ�@�$�@���@�|�@�n�@�5?@��@ف@�&�@׮@��H@�@���@ՙ�@�G�@���@ԓu@ӍP@�33@ӍP@ӍP@�@��@�~�@��#@щ7@���@��;@�l�@�33@�
=@���@�M�@���@���@�bN@�1'@��@˶F@�S�@�+@��@�$�@�@�?}@���@�  @�ƨ@ǶF@ǝ�@�t�@�K�@��@���@ř�@�%@�Ĝ@ēu@�r�@�I�@��m@��y@�$�@��^@�x�@���@��m@�33@��H@�n�@��@��T@�@�x�@�X@���@��w@�"�@�-@���@�p�@���@��j@�9X@��m@��w@�t�@��@�n�@���@�p�@�`B@�?}@��j@�Z@�(�@�  @�t�@���@�J@�x�@��@���@��^@���@��@��H@���@��\@�n�@�@��#@�`B@���@�Ĝ@��j@�r�@�1@�ƨ@�|�@�"�@���@�^5@�{@��T@��#@���@��7@�`B@�?}@���@��u@�Q�@���@�@���@�^5@�=q@�7L@��u@���@�S�@�S�@�C�@��@��@��!@�~�@�@��^@�hs@�V@�Ĝ@�j@��@�@��@��R@�v�@�^5@��@�O�@��`@���@���@��9@��@���@�|�@�|�@�|�@�dZ@�+@�@���@��y@�J@�V@�Ĝ@�j@�Q�@�A�@� �@�|�@�;d@�@���@�ȴ@���@���@�~�@�v�@�n�@�ff@�V@�E�@�$�@��#@�x�@�O�@�%@��D@�  @��
@��@�A�@��@���@��
@��w@�|�@�33@��y@�ff@�=q@��@��^@�G�@���@�r�@�(�@��@��F@���@��P@�33@���@��@���@��R@���@�~�@�V@�$�@��T@���@�p�@�X@�V@���@���@��@�bN@�Q�@�I�@�A�@�(�@�1@�ƨ@�|�@�dZ@�"�@��@��R@���@�E�@��@��T@���@��@�O�@�V@��u@�I�@��@��m@��w@���@�|�@�
=@��\@�V@�5?@�J@��-@��h@�`B@�X@�X@��@��9@���@��u@�z�@�9X@��m@�dZ@�
=@���@��\@��+@�v�@��@��@��@�`B@�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jA�n�A�jA�n�A�l�A�jA�n�A�l�A�jA�n�A�jA�hsA�l�A�bNA�dZA�l�A�l�A�jA�n�A�n�A�jA�n�A�n�A�l�A�p�A�n�A�l�A�n�A�r�A�l�A�p�A�p�A�l�A�r�A�n�A�p�A�r�A�p�A�p�A�t�A�r�A�r�A�x�A�t�A�t�A�v�A�z�A�t�A�t�A�v�A�r�A�v�A�x�A�t�A�r�A�t�A�jA�l�A�n�A�jA�hsA�jA�l�A�hsA�l�A�n�A�jA�t�A�|�A�|�A�x�A�x�A�|�A�z�A�x�A�z�A�z�A�|�A̅Ȧ+ÃÃȦ+Ȧ+A̅A̋DẢ7A̅A̋DȀ\A̋DA̍PȀ\A̍PA̋DȂhȂhA̕�A̙�A̙�A̝�A̛�A̡�Ạ�A̡�Ạ�A̩�A̩�A̩�A̮A̶FA̲-A̴9A̲-A̴9A̰!A̸RA̼jA̾wA̾wA�ƨA���A��A��#A��;A��#A��
A��/A��#A��A��HA��A���A�1A�bA��A��A�oA��A� �A�"�A�&�A�+A�1'A�1'A�33A�5?A�1'A�-A�+A�$�A� �A��A�"�A� �A��A� �A��A��A��A�{A���A�  A�A���A���A�VA��A� �A�+A�-A�(�A�+A�&�A�"�A��A��A��A��A�{A��A��A��A��A� �A��A��A��A�{A�VA�JA�bA�{A�oA�oA�oA��A��A�{A��A��A��A��A�/A�33A�33A�9XA�=qA�;dA�7LA�;dA�5?A�1'A�5?A�9XA�5?A�33A�7LA�9XA�5?A�9XA�;dA�9XA�E�A�K�A�Q�A�XA�ZA�bNA�bNA�ZA�XA�\)A�^5A�XA�VA�^5A�^5A�XA�^5A�^5A�ZA�\)A�\)A�K�A�E�A�K�A�K�A�G�A�E�A�G�A�=qA�9XA�;dA�9XA�33A�33A�7LA�1'A�+A�(�A�+A�$�A� �A��A��A��A�oA�oA�1A���A��A��A��`A��;A��;A��
A�ƨA̸RA̮A̛�Ả7A�~�A�|�A�t�A�n�A�l�A�VA�&�A�bA�A���A��mA��
A�ĜA˾wAˮA�|�A�;dA��A��AʾwAʮAʧ�Aʣ�Aʝ�AʋDA�v�A�ffA�=qA�$�A� �A��A�A���A��A��`A���A�v�A�=qA��yAȮAȁA�Q�A�$�A�bA���A���A��A��#A���A���A�ƨA�AǾwA�AǾwAǺ^AǼjAǴ9A�"�A�dZA��A��A��
AŰ!A�v�A�O�A�1A��Aģ�A�S�A���Aô9Aã�AÅA�l�A�M�A�=qA�"�A�  A��/A¾wADA�33A��`A��wA���A�z�A�A�A��A�VA��TA�ȴA��-A��!A���A��A�`BA�;dA��A��A�A���A��A�I�A�9XA�(�A��A���A���A���A�z�A�?}A��A���A�O�A���A���A� �A��yA���A���A�ĜA��wA��wA��RA��!A���A���A�z�A�S�A���A�1A�33A���A��DA�v�A�ffA�`BA�^5A�VA�33A�oA���A���A��hA��A�jA�\)A�G�A�5?A��A�
=A�%A���A��A��HA��#A�A��uA��A�p�A�O�A�9XA�+A��A�A���A��
A��jA��!A���A�t�A�bNA�;dA���A�n�A��yA��FA�ffA�E�A�7LA��A���A��A��A��#A��jA���A���A��A�p�A�`BA�O�A�C�A�9XA�/A�{A��A���A���A�l�A�G�A�(�A�  A��HA���A��FA���A��+A�t�A�`BA�E�A�7LA�(�A�VA���A��A��/A���A��jA��RA���A�|�A�n�A�I�A�$�A�JA��A��;A�A���A��uA��DA�r�A�K�A�&�A�{A���A��RA�=qA��A���A���A��A�p�A�Q�A�7LA�$�A�%A��A��TA�ȴA���A�hsA�;dA��A�  A��;A��^A���A��hA�x�A�dZA�I�A�(�A�VA�JA��A��A�
=A��A���A�ȴA��-A��+A�dZA�33A�A��
A��RA���A���A��DA�z�A�hsA�\)A�O�A�?}A��A��#A���A�p�A�S�A�A�A�$�A��A���A�&�A���A��!A�hsA�5?A��A�ĜA���A�z�A�XA�?}A�1'A�$�A��A�1A���A��A��HA���A���A���A�ĜA���A�jA��A���A�`BA�M�A�9XA��A�bA�
=A���A��mA��
A���A��hA��7A�x�A�l�A�`BA�XA�S�A�O�A�C�A�&�A�oA�VA��A���A�=qA�A���A���A��uA�n�A�`BA�O�A��A��HA�x�A�&�A���A���A���A�{A�ȴA���A��hA��A�x�A�n�A�hsA�bNA�O�A�?}A�;dA�9XA�33A�-A�(�A�{A�VA�A�ȴA��+A�bNA�^5A�ZA�A�A��A��9A�^5A��A��A���A��^A��!A���A���A���A���A��+A�|�A�p�A�VA�C�A�/A�bA���A��TA��wA�&�A��DA�ĜA��uA�dZA�(�A���A��;A���A��-A���A��7A�n�A�S�A�9XA�A���A�C�A�JA��TA��!A�/A�~�A���A��RA�ffA���A��jA��A�+A��A�%A��A��;A���A��9A���A���A���A���A���A��7A�x�A�M�A�+A��/A�5?A���A��RA���A��+A�bNA�33A�{A��A��
A��!A��PA�dZA�9XA�bA�ĜA�jA�/A�A���A�(�A��HA��PA��A�A�M�A���A��A��A���A���A��9A��71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                             11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
�FB
��B
�B
�B
��B
��B
�FB
��B
��B
��B
�zB
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
ŢB
��B
�[B
��B
��B
�fB�B~BSB�B~B�B�B_B �B 'B!bB!-B#B%zB,�B,�B-�B6B:�B=<B@�BFBH�BOvBd&B�IB��B��B  B�B�BGB�B_B"hB;�B<�BN�BO�BO�BPHBk�By�BzDBx�Bl�B`�BS�BF�B:�B1'BW?BFtB:�B(�B�B�B�B�`B��B�B҉B�qB��B�kB��B��B��Bz�B[�BM�B%�BB�B
�sB
��B
��B
��B
�%B
}�B
kB
QB
@�B
)�B
#nB
B
�B	�"B	�B	�B	�B	�3B	�B	�kB	��B	wfB	k�B	`�B	ZQB	N�B	PHB	@�B	?�B	<�B	0�B	0!B	*�B	#:B	 \B	�B	�B	CB	�B	�B	FB	B	�B	xB	�B	&LB	*eB	)�B	,�B	9�B	8�B	:*B	B�B	J�B	OBB	L0B	G�B	G�B	?�B	I�B	W?B	bNB	lWB	{B	�B	��B	�:B	�bB	��B	��B	�MB	�kB	�kB	��B	��B	�hB	��B	��B	�B	��B	��B	�B	�?B	��B	��B	�'B	�B	��B	�B	��B	��B	�'B	�nB	��B	�RB	��B	�LB	�$B	�XB	�B	�RB	�XB	��B	��B	��B	�NB	��B	��B	��B	��B	�#B	�9B	�B	��B	ɺB	�6B	�HB	�aB	�gB	�KB	�&B	ԕB	ϫB	�B	�3B	��B	��B	�jB	�B	�#B	��B	��B	� B	��B	��B	�EB	�]B	�WB	�fB	�DB	�yB	�QB	��B	��B	��B	�"B	�B	��B	��B	�cB	�cB	�/B	�cB	��B	�/B	�B	�B	�
B	�B	�B	�B	� B	�B	�B	�B	�|B	�B	��B	��B	�DB	��B	��B	� B	��B	�B	�"B	��B	��B	�"B	�WB	��B	�B	�iB	�iB	�B	��B	��B	��B	�"B	��B	��B	�.B	��B	�.B
 4B
 �B
B
�B
B
MB
B
SB
B
�B
%B
�B
fB
�B
	B

rB
	�B
�B
DB
B
B
xB
xB
�B
�B
bB
 B
4B
4B
bB
bB
�B
�B
�B
�B
DB

�B
	�B
�B
	B
	�B
fB
1B
�B
�B
+B
�B
	B
�B
�B
%B
�B
+B
	�B
�B
+B
+B
�B
�B
	lB

	B

�B

=B

�B
�B
�B
~B
~B
�B
�B
bB
:B
:B
�B
+B
kB
�B
oB
hB
hB
�B
:B
:B
�B
�B
B
$B
$B
+B
�B
eB
�B
B
�B
�B
�B
 �B
$B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
'B
'�B
(XB
($B
(�B
($B
)�B
-B
.IB
.�B
/B
/OB
/�B
/�B
1[B
1[B
1'B
1[B
1'B
1�B
2�B
33B
2�B
3�B
4�B
5B
5?B
6zB
5tB
5�B
5�B
5�B
6�B
6B
6B
5�B
5�B
5�B
6B
5�B
5�B
5?B
8�B
9�B
:�B
:�B
:�B
:�B
;0B
<�B
=<B
=�B
>B
>B
>BB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?HB
?�B
@�B
AUB
@OB
>�B
>wB
?�B
B�B
C-B
C-B
CaB
C�B
D3B
D�B
E9B
FB
E�B
FB
F�B
HB
HKB
IB
I�B
I�B
I�B
I�B
I�B
J�B
J#B
JXB
J�B
J�B
J�B
J�B
J�B
K)B
K^B
LdB
L0B
L�B
M6B
MjB
M�B
M�B
NB
NB
NB
NB
N<B
N<B
N�B
OB
N�B
PB
O�B
PB
PB
P�B
P�B
QB
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
S&B
S�B
T,B
T�B
T�B
T�B
U2B
VB
U�B
VmB
V9B
VB
V�B
W�B
WsB
W�B
WsB
W�B
W�B
X�B
Y�B
ZQB
ZQB
ZB
ZB
Z�B
ZQB
[#B
Z�B
[WB
[�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
��B
��B
��B
�zB
��B
�B
�LB
��B
��B
�LB
�B
�FB
�B
�B
�FB
��B
��B
��B
��B
��B
�FB
��B
��B
�FB
��B
��B
��B
��B
�B
�FB
�zB
��B
�B
��B
��B
��B
��B
��B
��B
�zB
�LB
�tB
��B
�RB
��B
�LB
�RB
��B
�LB
��B
��B
�LB
��B
��B
��B
��B
��B
��B
��B
�RB
��B
�FB
��B
�LB
�zB
�LB
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�RB
�B
��B
�B
��B
��B
��B
�[B
�B
��B
�-B
��B
��B
�B
�B
�mB
�9B
��B
�-B
��B
��B
�jB
��B
�^B
�HB
�HB
�B
�9B
ԕB
��B
خB
ݘB
ںB
��B
��B
�ZB
ߤB
�|B
��B
�B
�
B
�]B
�TB
��B
��B
�(B �B �B�B�BBB�B�B�BfBuB�B�BMBYBBB�B�B	BB�B�B�B�B�BOB�B�BB=BBxBB�BB�BB�B�BMB�B�BoBeBBB!�B"hB 'B �B!�B!�B�B�B!-B �B!B�B �B 'B!-B"hB$B"4B!bB!-B"�B \B �B!�B"�B#B!�B#�B$�B#�B#:B#�B$@B#B)�B+kB,B,B,�B.B,=B-wB,=B+kB+kB-CB.IB-B,=B.B.IB,�B/�B/�B1�B0!B9�B9�B8RB9XB;0B:�B:*B:*B<B<6B9XB<B>�B=B=�B?�B>�B=�BC�BA�B@�BB�BEBE9BEBF�BHBF?BFtBHKBH�BG�BIRBJ�BK�BJ�BL�BM�BOBBPBR BU2BU�BX�B^jB_�Ba|BffBgmBf�Bh
Bo Br�BuZB}�B��B��B��B�BȴBȀB�pB�BߤB�B�B��B�KB�B�QB�cB�;B��B�B�	B�B��B��B��B�B��B�	B�fB��B��B��B�8B�%B�%B�`B�B�>B�xB��B  B;B�BYB�BYB	7BYB+B�B�B�B�B�BYB�B�BB�B�B+kB($B�B B�B�B
�BfBJB
�BMB�B
�BGB�cBB�BoB�(B 4BuB�B �B�B"B�B�B�BB1BoB:BeB�BbBbBuB�BBYBkB!BIB�B�B%FB�B�B�B!�B�B#nB"�B&�B,qB)_B2�BE�BP�B>wB>�B;�B7B:^B8�B6�B7LB7�B5tB4�B9XB9�BOvBYKBdZBPHBGzBI�BJXBH�BGzBH�BM6BK�BL0BP�BL�BJ�BNBT�BQ�BR BT�BQ�BP�BOBBPBS[BMjBL�BS�BI�BLdBMBMBM6BM�BQBO�BU�BS�BT�BXyBZ�B\�B\�Be,B{�B�Bv`ByrBuZBpoBncB��B��BzDBv�ByrBxlBx8B|B{�BzxByrBy>Bx8Bw2B|PB�ByrBx�By�BzDBv�B{ByrBxlBxByrBwfBt�Br�Br|BpoBl"Bm�Bl"Bh>Bf�Bf2Bd�BbBb�BffBb�Bd�B_pB]dB^5BZ�BZ�BV�BUgBS�BT�BT�BR�BOBBNBVBXEBNBH�BC�BE9BB�BB�B@OB=qB>�B=qB:�B<jB>�B<6B:*B6FB6zB8B5?B0�B1�B2�B0�B1�B/�B.�B-�BOBBZBYB\�BX�BWsBV�BV�BUgBUgBN�BJXBGzBE9BB�B@�B@�B@�B?}B=qB<jB?HBB[B>BB7�B4B0UB1�B1�B2aB8�B 'B($B�B~B!�BqB�BSB�B�B�B:B�BhB�BVB�BVB~B
�B�B"B(BFB�B�B��B�]B��B�B�fB��B�	B�	B  B�GB�B�AB�B�;B�5B�"B�QB�"B� B��B�B�yB�AB�B�"B�"B�B�gB�QB��B��B�yB�TB҉B�B�B�EB�?B҉B�BB��B��B�B�nB��B��B�[B�3B��B��B�OB��B��B��B�IB�XB��B�RB�FB�*B�B�:B��B��B��B��B��B��B��B��B��B��B��B�oB�B�B��B��B��B�B�VB�JB�=B�=B�_B�eB�JB��Bk�Bl�Bl�Bd�BbNB`vB_�B\�BY�BY�BU�BV9BWsBaHBPBI�B@�BG�BI�BI�B4�B/B2�B*�B"�B"hB�B�BSB�B\B�B�BBfB�B_B�B�B1B_BB
rBB
�ZB
�B
�]B
��B
�B
�)B
�B
�ZB
�B
�pB
�/B
��B
چB
֡B
�B
�B
ʌB
ƨB
�dB
��B
��B
�wB
��B
�!B
�B
��B
�RB
�B
�_B
��B
�1B
�14444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                             44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                             44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022020414440720220204144407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022021410010920220214100109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022021410010920220214100109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194320230210131943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                