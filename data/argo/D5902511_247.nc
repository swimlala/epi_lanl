CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2023-02-04T12:02:46Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  Z`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  w�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � (   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 3�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   3�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   9�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ?�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T E�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   F4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   F<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   FD   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   FL   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � FT   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   F�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   F�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    F�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        G   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        G    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       G(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    G0Argo profile    3.1 1.2 19500101000000  20230204120246  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_247                 6810_008521_247                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�]�E��@�]�E��11  @�^
�L0@�^
�L0@2IGNΚ@2IGNΚ�d�3���z�d�3���z11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@�  @�G�@\@�G�AG�AG�A ��A-p�A@��AaG�A\)A��A��A�  A���AУ�A�Q�A�A��B  B(�B(�B Q�B(Q�B0Q�B8(�B@(�BH  BP  BW�
B_�
Bh  BpQ�BxQ�B�  B��B�  B�(�B�{B�  B�  B��B��B��B�  B��
B��
B��B��B��B��B��B��B�{B�=qB�{B��
B��
B�  B�  B�  B�(�B�  B��
B�  B�{C 
=C  C  C  C��C	�C  C��C�C��C  C  C  C
=C  C  C �C"{C$
=C&  C(
=C*  C,  C.  C0  C2  C4  C6  C8  C:
=C<
=C>
=C@  CA�CC��CF
=CH
=CJ  CK��CN  CP  CR  CT
=CU��CX  CZ  C\  C^  C`  Ca��Cd
=Cf  Ch  Cj  Cl  Cm��Cp  Cr  Ct  Cv  Cx
=Cy��C{��C~  C�  C�C�C���C���C���C���C���C�  C���C���C���C�  C���C�  C�  C�  C�C�C�C���C���C�  C�  C�  C�  C�  C�C�  C���C�C�  C�  C���C���C���C�  C�  C�  C�C�  C�  C���C���C�  C�  C�C�  C���C���C���C���C�  C���C�  C�
=C�C���C���C�C�C�  C�C�  C�  C�  C�  C�  C���C���C���C���C�  C�C�C�  C�C�C���C���C�  C�C�C���C���C�  C�C�C�
=C�
=C�C�
=C�C�  C�C�  C���C�  C�C�  C���C�  C���C�C�  C���C�C�  C�  C�  C���C�  C�C�
=C�
=C�  C�  C���C���C�C�  C���C�
=C�C�  C�C�C�  D   D � D�D� D�qD��D�D��D  D� D  D� D�D� D  D}qD�RD� D	�D	��D
�D
� D  D�D�D}qD�D��D�qD� D�D� D  D��D�qD��D�D�D  D� DD��D  D��DD��D  D�D�D��D  D��DD� D  D� D  D� D�qD}qD�qD� D�D��D�qD }qD!�D!��D"  D"� D#  D#�D$�D$� D%�D%��D&�D&��D'�D'��D(�D(}qD)  D)� D)�qD*}qD*��D+}qD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1z�D1�qD2� D3  D3}qD4  D4� D4��D5z�D6  D6�D7  D7}qD7�qD8}qD9�D9��D:  D:}qD:�qD;}qD<�D<� D<�qD=�D>�D>� D?  D?}qD@  D@��DA  DA��DB�DB��DB�qDC}qDD�DD�DE�DE� DE�qDF}qDG  DG}qDG�qDH}qDH�qDI� DI�qDJ}qDK�DK�DL  DL}qDL�qDM� DN�DN��DN�qDO� DO�qDP}qDQ  DQ}qDQ�qDR� DS�DS�DT�DT� DT�qDU� DV  DV� DV�qDW� DX�DX� DY  DY}qDZ  DZ��DZ�qD[z�D\  D\��D\�qD]}qD^  D^}qD^�RD_z�D_��D`}qDa  Da� Db  Db��Dc  Dc� Dd  Dd��De  De� DfDf�Df�qDgxRDg�qDh}qDi  Di}qDi��Dj}qDk  Dk� Dl  Dl� Dm�Dm��Dm��Dnz�Dn�qDo� Do�qDp}qDq�Dq�Dr  Dr� Ds  Ds� Ds��Dtz�Du�Du��DvDv��Dw  Dw� Dw�qDxz�Dx��Dy� Dz�Dz�D{�D{� D|�D|� D|�qD}� D~�D~�D�D� D�  D�@ D�� D���D�HD�B�D�� D���D�HD�AHD�� D�D�  D�>�D�� D���D���D�@ D�~�D��qD���D�@ D�~�D��HD�  D�>�D��HD�D�HD�@ D�~�D���D���D�>�D�� D�� D��D�B�D�� D���D�HD�>�D�� D�� D�  D�>�D�~�D��HD��D�@ D�� D�� D�  D�@ D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�� D���D���D�AHD��HD�� D���D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�0�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?#�
?aG�?�\)?�33?�(�?��@z�@#�
@0��@B�\@\(�@h��@�G�@�=q@���@�p�@��@�{@�p�@�ff@�{@ٙ�@��@���@�
=A�\AA
=qA  AA=qA ��A$z�A*�HA/\)A3�
A8��A>�RAB�\AH��AMp�AQ�AXQ�A]p�Aa�AhQ�Al��Ap��Aw
=A|��A�Q�A�33A��A�  A��HA���A��A��A�(�A��A���A�(�A�\)A���A��
A�\)A�G�A�(�A�
=A���A�z�A��RA���A�(�A�
=A���A�z�A�ffA�G�A�z�A�ffAљ�A�z�A�ffA�G�A�z�A޸RAᙚA�z�A�ffA��A�z�A�ffA��A�(�A�ffA��A�(�A��RB�B{B\)B��B{B
=B��B	�B
=B��B�B�HBQ�B�B�HB  B��B
=B  Bp�B�RB�Bp�BffB�B ��B"ffB#33B$��B&{B&�HB((�B)B*ffB,  B-G�B.=qB/�B1�B2{B333B4��B5�B6�RB8Q�B9p�B:ffB<  B<��B>=qB?\)B@(�BA��BB�HBC�BD��BF�\BG�BHz�BJ{BJ�HBLQ�BMp�BNffBP  BP��BRffBS�BTz�BV{BW\)BXQ�BY��B[
=B\(�B]�B^�\B`  B`��Bb{Bc�Bd��Be�Bg\)Bh(�Bi��Bk
=Bk�
BmG�Bn�RBo�Bp��Br�\Bs�Btz�Bu�Bw\)Bx  By��Bz�HB{�
B}�B~�RB�B�ffB��B�B�=qB���B��B�=qB���B��B�(�B��RB��B��B���B�p�B��
B��\B�G�B��
B�ffB�33B�B�=qB�
=B��B�(�B���B��B�(�B���B�G�B�  B��RB��B��
B�z�B���B�B�=qB���B��B�  B���B�p�B��B��\B�G�B��B�z�B��B��B�Q�B��HB�\)B�(�B���B�G�B��B���B��B��
B�z�B��HB��B�Q�B���B���B�=qB���B�p�B�{B��\B�\)B�  B�z�B��B��B�z�B���B�B�ffB��HB���B�Q�B��RB�\)B�=qB���B�33B��B���B�
=B�B�z�B���B���B�Q�B���B�p�B�(�B���B�G�B�  B�z�B��B��
B�Q�B��HBîB�(�B���Bř�B�{BƏ\B�\)B��B�ffB�33B�B�Q�B�
=BˮB�{B��HBͅB��B���B�G�B�BЏ\B�33B�B�Q�B�
=BӮB�(�BԸRB�p�B�(�B֣�B�33B��B�z�B���BٮB�ffB���B�p�B�=qB��HB�\)B�{B��HB�\)B��B��B�G�B�B�z�B�33B�B�=qB��HB�B�{B��B�p�B�{B�\B��B��
B��B�33B�B�ffB�33B홚B�=qB���B�B�(�B��B�p�B�{B�z�B�33B�  B�z�B���B�B�z�B���B��B�Q�B���B��B�(�B���B��B�  B��RB�p�B��B��\B�G�C   C =qC �\C �CG�Cz�C�
C=qCffCC�C\)C��C��C\)C��C�HCQ�C��C�HCG�C��C�HC33C��C�C(�Cz�C�HC	�C	ffC	��C
�C
\)C
�RC�CQ�C��C  C\)C��C�
C=qC��C�
C�C�C�HC�Cz�C�HC(�Cz�C�
C33Cz�C�
CG�C��C�HC33C��C  C=qC�\C��CQ�C�\C�
C=qC��C�C33C�C�CQ�C��C�HCQ�C��C�C=qC��C  CQ�C�C�
C=qC��C�
C(�Cz�C�HC33C�C��C33C�\C�C =qC z�C ��C!(�C!�C!�HC"(�C"p�C"�RC#(�C#z�C#�C$  C$Q�C$��C$�C%{C%\)C%��C%�HC&(�C&Q�C&z�C&C'  C'33C'\)C'z�C'��C'�
C(  C(33C(\)C(ffC(�\C(C(��C)
=C)(�C)G�C)z�C)�C)�
C)��C*{C*(�C*Q�C*�\C*�C*��C*�HC+
=C+=qC+p�C+�\C+�C+��C+�C,�C,G�C,z�C,��C,�RC,�
C-
=C-=qC-\)C-p�C-��C-��C.  C.�C.=qC.\)C.�C.�RC.�C/{C/33C/Q�C/p�C/�\C/��C0  C0(�C0G�C0\)C0z�C0��C0��C1  C1(�C1Q�C1ffC1�C1�C1�C2
=C2�C2=qC2\)C2�C2�C2�HC3
=C3(�C3Q�C3ffC3�\C3�RC3�C4�C4G�C4p�C4�C4�C4��C4��C5{C5=qC5ffC5��C5��C5��C6{C633C6Q�C6p�C6��C6C7  C7�C7=qC7ffC7�C7��C7��C8  C833C8\)C8�C8��C8C8�C9  C9(�C9\)C9�\C9�RC9�HC9�C:
=C:33C:ffC:�\C:��C:��C:�HC;{C;G�C;p�C;�\C;��C;C;�C<{C<=qC<p�C<��C<C<�HC=  C=�C==qC=\)C=�\C=C=�C>
=C>(�C>G�C>ffC>�\C>�C>�
C?  C?33C?\)C?�C?��C?C?�
C?��C@�C@G�C@z�C@��C@��C@�CA{CA=qCAffCA�CA�CA��CA��CB�CB=qCB\)CB�\CB�RCB�CC{CCG�CCp�CC��CC�RCC�
CD  CD33CD\)CD�CD�CD�CE{CE=qCEp�CE��CECE�CF{CF33CF\)CF�CF��CF�
CG  CG(�CGQ�CGz�CG�CG��CH
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @=p�@�  @�G�@\@�G�AG�AG�A ��A-p�A@��AaG�A\)A��A��A�  A���AУ�A�Q�A�A��B  B(�B(�B Q�B(Q�B0Q�B8(�B@(�BH  BP  BW�
B_�
Bh  BpQ�BxQ�B�  B��B�  B�(�B�{B�  B�  B��B��B��B�  B��
B��
B��B��B��B��B��B��B�{B�=qB�{B��
B��
B�  B�  B�  B�(�B�  B��
B�  B�{C 
=C  C  C  C��C	�C  C��C�C��C  C  C  C
=C  C  C �C"{C$
=C&  C(
=C*  C,  C.  C0  C2  C4  C6  C8  C:
=C<
=C>
=C@  CA�CC��CF
=CH
=CJ  CK��CN  CP  CR  CT
=CU��CX  CZ  C\  C^  C`  Ca��Cd
=Cf  Ch  Cj  Cl  Cm��Cp  Cr  Ct  Cv  Cx
=Cy��C{��C~  C�  C�C�C���C���C���C���C���C�  C���C���C���C�  C���C�  C�  C�  C�C�C�C���C���C�  C�  C�  C�  C�  C�C�  C���C�C�  C�  C���C���C���C�  C�  C�  C�C�  C�  C���C���C�  C�  C�C�  C���C���C���C���C�  C���C�  C�
=C�C���C���C�C�C�  C�C�  C�  C�  C�  C�  C���C���C���C���C�  C�C�C�  C�C�C���C���C�  C�C�C���C���C�  C�C�C�
=C�
=C�C�
=C�C�  C�C�  C���C�  C�C�  C���C�  C���C�C�  C���C�C�  C�  C�  C���C�  C�C�
=C�
=C�  C�  C���C���C�C�  C���C�
=C�C�  C�C�C�  D   D � D�D� D�qD��D�D��D  D� D  D� D�D� D  D}qD�RD� D	�D	��D
�D
� D  D�D�D}qD�D��D�qD� D�D� D  D��D�qD��D�D�D  D� DD��D  D��DD��D  D�D�D��D  D��DD� D  D� D  D� D�qD}qD�qD� D�D��D�qD }qD!�D!��D"  D"� D#  D#�D$�D$� D%�D%��D&�D&��D'�D'��D(�D(}qD)  D)� D)�qD*}qD*��D+}qD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1z�D1�qD2� D3  D3}qD4  D4� D4��D5z�D6  D6�D7  D7}qD7�qD8}qD9�D9��D:  D:}qD:�qD;}qD<�D<� D<�qD=�D>�D>� D?  D?}qD@  D@��DA  DA��DB�DB��DB�qDC}qDD�DD�DE�DE� DE�qDF}qDG  DG}qDG�qDH}qDH�qDI� DI�qDJ}qDK�DK�DL  DL}qDL�qDM� DN�DN��DN�qDO� DO�qDP}qDQ  DQ}qDQ�qDR� DS�DS�DT�DT� DT�qDU� DV  DV� DV�qDW� DX�DX� DY  DY}qDZ  DZ��DZ�qD[z�D\  D\��D\�qD]}qD^  D^}qD^�RD_z�D_��D`}qDa  Da� Db  Db��Dc  Dc� Dd  Dd��De  De� DfDf�Df�qDgxRDg�qDh}qDi  Di}qDi��Dj}qDk  Dk� Dl  Dl� Dm�Dm��Dm��Dnz�Dn�qDo� Do�qDp}qDq�Dq�Dr  Dr� Ds  Ds� Ds��Dtz�Du�Du��DvDv��Dw  Dw� Dw�qDxz�Dx��Dy� Dz�Dz�D{�D{� D|�D|� D|�qD}� D~�D~�D�D� D�  D�@ D�� D���D�HD�B�D�� D���D�HD�AHD�� D�D�  D�>�D�� D���D���D�@ D�~�D��qD���D�@ D�~�D��HD�  D�>�D��HD�D�HD�@ D�~�D���D���D�>�D�� D�� D��D�B�D�� D���D�HD�>�D�� D�� D�  D�>�D�~�D��HD��D�@ D�� D�� D�  D�@ D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�� D���D���D�AHD��HD�� D���D�@ D�� D�� D���D�>�D�� D�� D�  D�@ D�� D�� D�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?#�
?aG�?�\)?�33?�(�?��@z�@#�
@0��@B�\@\(�@h��@�G�@�=q@���@�p�@��@�{@�p�@�ff@�{@ٙ�@��@���@�
=A�\AA
=qA  AA=qA ��A$z�A*�HA/\)A3�
A8��A>�RAB�\AH��AMp�AQ�AXQ�A]p�Aa�AhQ�Al��Ap��Aw
=A|��A�Q�A�33A��A�  A��HA���A��A��A�(�A��A���A�(�A�\)A���A��
A�\)A�G�A�(�A�
=A���A�z�A��RA���A�(�A�
=A���A�z�A�ffA�G�A�z�A�ffAљ�A�z�A�ffA�G�A�z�A޸RAᙚA�z�A�ffA��A�z�A�ffA��A�(�A�ffA��A�(�A��RB�B{B\)B��B{B
=B��B	�B
=B��B�B�HBQ�B�B�HB  B��B
=B  Bp�B�RB�Bp�BffB�B ��B"ffB#33B$��B&{B&�HB((�B)B*ffB,  B-G�B.=qB/�B1�B2{B333B4��B5�B6�RB8Q�B9p�B:ffB<  B<��B>=qB?\)B@(�BA��BB�HBC�BD��BF�\BG�BHz�BJ{BJ�HBLQ�BMp�BNffBP  BP��BRffBS�BTz�BV{BW\)BXQ�BY��B[
=B\(�B]�B^�\B`  B`��Bb{Bc�Bd��Be�Bg\)Bh(�Bi��Bk
=Bk�
BmG�Bn�RBo�Bp��Br�\Bs�Btz�Bu�Bw\)Bx  By��Bz�HB{�
B}�B~�RB�B�ffB��B�B�=qB���B��B�=qB���B��B�(�B��RB��B��B���B�p�B��
B��\B�G�B��
B�ffB�33B�B�=qB�
=B��B�(�B���B��B�(�B���B�G�B�  B��RB��B��
B�z�B���B�B�=qB���B��B�  B���B�p�B��B��\B�G�B��B�z�B��B��B�Q�B��HB�\)B�(�B���B�G�B��B���B��B��
B�z�B��HB��B�Q�B���B���B�=qB���B�p�B�{B��\B�\)B�  B�z�B��B��B�z�B���B�B�ffB��HB���B�Q�B��RB�\)B�=qB���B�33B��B���B�
=B�B�z�B���B���B�Q�B���B�p�B�(�B���B�G�B�  B�z�B��B��
B�Q�B��HBîB�(�B���Bř�B�{BƏ\B�\)B��B�ffB�33B�B�Q�B�
=BˮB�{B��HBͅB��B���B�G�B�BЏ\B�33B�B�Q�B�
=BӮB�(�BԸRB�p�B�(�B֣�B�33B��B�z�B���BٮB�ffB���B�p�B�=qB��HB�\)B�{B��HB�\)B��B��B�G�B�B�z�B�33B�B�=qB��HB�B�{B��B�p�B�{B�\B��B��
B��B�33B�B�ffB�33B홚B�=qB���B�B�(�B��B�p�B�{B�z�B�33B�  B�z�B���B�B�z�B���B��B�Q�B���B��B�(�B���B��B�  B��RB�p�B��B��\B�G�C   C =qC �\C �CG�Cz�C�
C=qCffCC�C\)C��C��C\)C��C�HCQ�C��C�HCG�C��C�HC33C��C�C(�Cz�C�HC	�C	ffC	��C
�C
\)C
�RC�CQ�C��C  C\)C��C�
C=qC��C�
C�C�C�HC�Cz�C�HC(�Cz�C�
C33Cz�C�
CG�C��C�HC33C��C  C=qC�\C��CQ�C�\C�
C=qC��C�C33C�C�CQ�C��C�HCQ�C��C�C=qC��C  CQ�C�C�
C=qC��C�
C(�Cz�C�HC33C�C��C33C�\C�C =qC z�C ��C!(�C!�C!�HC"(�C"p�C"�RC#(�C#z�C#�C$  C$Q�C$��C$�C%{C%\)C%��C%�HC&(�C&Q�C&z�C&C'  C'33C'\)C'z�C'��C'�
C(  C(33C(\)C(ffC(�\C(C(��C)
=C)(�C)G�C)z�C)�C)�
C)��C*{C*(�C*Q�C*�\C*�C*��C*�HC+
=C+=qC+p�C+�\C+�C+��C+�C,�C,G�C,z�C,��C,�RC,�
C-
=C-=qC-\)C-p�C-��C-��C.  C.�C.=qC.\)C.�C.�RC.�C/{C/33C/Q�C/p�C/�\C/��C0  C0(�C0G�C0\)C0z�C0��C0��C1  C1(�C1Q�C1ffC1�C1�C1�C2
=C2�C2=qC2\)C2�C2�C2�HC3
=C3(�C3Q�C3ffC3�\C3�RC3�C4�C4G�C4p�C4�C4�C4��C4��C5{C5=qC5ffC5��C5��C5��C6{C633C6Q�C6p�C6��C6C7  C7�C7=qC7ffC7�C7��C7��C8  C833C8\)C8�C8��C8C8�C9  C9(�C9\)C9�\C9�RC9�HC9�C:
=C:33C:ffC:�\C:��C:��C:�HC;{C;G�C;p�C;�\C;��C;C;�C<{C<=qC<p�C<��C<C<�HC=  C=�C==qC=\)C=�\C=C=�C>
=C>(�C>G�C>ffC>�\C>�C>�
C?  C?33C?\)C?�C?��C?C?�
C?��C@�C@G�C@z�C@��C@��C@�CA{CA=qCAffCA�CA�CA��CA��CB�CB=qCB\)CB�\CB�RCB�CC{CCG�CCp�CC��CC�RCC�
CD  CD33CD\)CD�CD�CD�CE{CE=qCEp�CE��CECE�CF{CF33CF\)CF�CF��CF�
CG  CG(�CGQ�CGz�CG�CG��CH
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�AȅAȗ�Aȝ�Aȟ�AȮAȮAȮAȮAȰ!AȲ-AȲ-AȰ!AȲ-AȲ-Aȴ9Aȴ9Aȴ9Aȴ9AȶFAȼjAȺ^AȼjAȼjAȾwAȼjAȶFAȶFAȰ!Aȴ9AȲ-Aȥ�Aȥ�Aș�Aș�AȓuAȉ7A�t�A�l�A�hsA�VA�Q�A�Q�A�S�A�Q�A�G�A�33A�"�A�VA���A���A���A���A��mAǣ�AǏ\AǅA�|�A�p�A�^5A���AƑhA�-A��A���AÑhA��#A��FA���A��uA�-A��hA�ZA��A��A��;A��A�~�A�$�A�&�A���A�z�A��wA�`BA�ȴA�ZA�bA��RA���A�ȴA�%A�?}A���A��A���A��yA���A�`BA�?}A���A��7A��A�ZA�hsA��9A�VA�\)A�\)A�^5A���A�?}A�-A���A~��Az=qAuG�Ar�9Ap�An�jAm�AmK�Al��Aj�/Ag�;Afz�Ae�7Ac��Aa/A_��A]�-A\JAYt�AV�9ATAQx�AQ�APȴANĜAL�AIK�AFn�AD�jAC�ABz�AA7LA?��A>M�A<�+A;;dA:ZA9"�A7�#A7%A5x�A2��A0��A/�A.{A-7LA++A*ZA(bA%�A$bA"bA $�A�hA��A��A��A�uAJA%A��A �A��A��A�7AoA�A~�A�Ap�AhsA
=A��AQ�A��A�A��AXA��A��A�A�RA�AG�A�`Av�A�^A
�RA	�A~�AQ�A��A�A�7A`BA�A��Ax�AM�A�AoAI�A�wA��A%A1'A�AdZA ��@�33@�J@��7@���@��@��@�+@���@���@��w@�l�@��y@�M�@��@��`@���@��@�C�@�7@�  @�~�@�n�@�^5@�E�@���@�$�@��;@�$�@�7@�O�@陚@�@�p�@��@�z�@�D@�A�@��@噚@�v�@ᙚ@�p�@�?}@�?}@���@�\)@��H@ް!@ާ�@ߥ�@�@���@۶F@�33@�C�@�\)@�$�@���@���@ׅ@�
=@��#@�1'@��@�E�@��@��@щ7@��;@�S�@�33@�+@�{@�"�@�"�@��y@�E�@Ɂ@�A�@�33@Ƈ+@�n�@�ff@�M�@Ł@ċD@�Z@�9X@�1@���@�dZ@�@�E�@��@�X@���@��9@�1'@���@�l�@�
=@��+@���@�r�@��@�t�@��y@���@�=q@�@��#@���@�p�@�&�@���@�bN@��m@���@���@�v�@�^5@�{@��^@�O�@��`@��j@���@��u@� �@��@�ƨ@�dZ@��H@���@�E�@�{@���@�x�@�O�@���@��D@�A�@�ƨ@�@�~�@���@���@�$�@��@�G�@�&�@���@�j@�Q�@� �@��
@�33@��@�+@�\)@��y@�ȴ@���@�$�@���@�/@���@�(�@��m@��
@�ƨ@��@�+@��H@�~�@�$�@�J@���@��h@��@�`B@�`B@�x�@��@��/@���@�Z@�"�@���@���@��+@�5?@��@��@�J@��@��T@��#@���@�`B@�&�@��@�j@�I�@��@��w@��P@�+@���@�~�@�$�@���@�7L@�/@�/@�%@�9X@��@��F@��@�v�@�=q@���@���@�O�@��`@���@��u@�j@�Z@�A�@�(�@�  @��@�S�@�
=@���@�5?@���@�/@�V@�V@�%@��@��m@��@��@�S�@�o@��@���@�5?@�x�@�?}@�?}@�7L@��`@��@���@��D@�bN@��@��w@�t�@�C�@�33@�o@���@���@��R@���@��+@�n�@�E�@���@��@�%@���@��9@�z�@�r�@�j@�Q�@�A�@�1'@��@���@��
@��F@���@�|�@�C�@��@�v�@�$�@��@��@���@�7L@���@���@��D@�r�@�bN@�1'@�ƨ@�t�@�33@��!@���@���@���@���@�^5@���@��^@���@�hs@�V@��u@� �@��m@��w@�dZ@�K�@��@���@�=q@�$�@��@�J@��@���@�7L@�V@��/@��j@��@��@�bN@�I�@�9X@�@��@|�@��@l�@
=@~5?@}�@}�@|��@|Z@{�F@{o@z��@z-@y�#@yx�@yG�@y%@x��@x��@xbN@xb@w�w@w��@wl�@w+@v��@u�@u�@u/@t��@t��@t�D@t(�@st�@r��@r-@rJ@q��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�AȅAȉ7AȅAȇ+AȍPAȏ\Aȥ�Aȟ�AȓuAȡ�Aȡ�AȑhAȟ�AȮAȬAȲ-AȮAȬAȰ!AȰ!Aȩ�AȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȲ-AȰ!AȰ!Aȴ9AȰ!AȰ!AȲ-AȰ!AȰ!AȲ-AȮAȴ9AȰ!AȰ!Aȴ9AȲ-AȰ!Aȴ9AȲ-AȰ!Aȴ9Aȴ9AȮAȴ9AȰ!Aȴ9Aȴ9AȲ-AȶFAȲ-AȲ-AȶFAȴ9AȲ-AȸRAȴ9AȲ-Aȴ9AȰ!Aȴ9AȶFAȲ-AȶFAȴ9AȰ!AȶFAȶFAȲ-AȶFAȲ-AȲ-AȸRAȲ-AȶFAȺ^AȸRAȺ^AȾwAȺ^AȼjAȼjAȺ^AȼjAȺ^AȸRA���AȼjAȸRAȾwAȺ^AȺ^AȾwAȺ^AȺ^AȾwAȾwAȺ^A���AȾwAȼjA���A���AȼjAȼjA�AȾwAȺ^A���AȾwAȼjAȺ^AȼjAȴ9AȺ^AȶFAȲ-AȶFAȸRAȴ9AȸRAȺ^Aȴ9AȶFAȸRAȰ!AȲ-AȲ-AȬAȰ!Aȴ9AȲ-AȮAȴ9Aȴ9AȰ!Aȴ9Aȴ9AȲ-Aȴ9AȲ-Aȴ9Aȴ9AȮAȶFAȴ9AȰ!Aȧ�Aȩ�Aȥ�Aȣ�Aȧ�Aȣ�Aȥ�Aȣ�Aȣ�Aȩ�Aȡ�Aȧ�Aȣ�Aș�Aș�Aș�Aȕ�Aș�Aȝ�Aț�Aȗ�Aȕ�Aș�Aș�Aș�Aȝ�AȑhAȕ�Aȗ�AȑhAȑhAȑhAȉ7Aȏ\AȑhAȉ7AȅAȋDAȅA�t�A�v�A�x�A�r�A�t�A�v�A�r�A�p�A�r�A�n�A�ffA�jA�jA�hsA�hsA�l�A�jA�ffA�ffA�`BA�^5A�ZA�O�A�O�A�Q�A�M�A�O�A�S�A�O�A�O�A�S�A�Q�A�O�A�S�A�Q�A�O�A�VA�Q�A�S�A�S�A�O�A�S�A�S�A�O�A�S�A�Q�A�O�A�S�A�Q�A�O�A�S�A�K�A�I�A�K�A�E�A�G�A�E�A�=qA�?}A�;dA�33A�33A�+A�(�A�+A�+A�"�A�$�A�"�A��A��A��A�VA�bA�VA�1A�
=A�A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A���A���A���A��A���A���A���A���A���A���A���A��A���A���A���AǾwAǣ�Aǝ�AǏ\AǏ\AǓuAǏ\AǏ\AǓuAǍPAǋDAǍPAǋDAǇ+Aǉ7AǅAǁAǅAǃA�~�AǃA�~�A�v�A�z�A�z�A�v�A�x�A�p�A�l�A�p�A�n�A�jA�hsA�jA�ffA�^5A�ZA�M�A�(�A�oA���A���A��mA��HA��;A���A�ƨAƇ+A�t�A�p�A�^5A�M�A�7LA�+A� �A�&�A�&�A��A�oA�1A���A�ȴAŬAś�Aŉ7A�bNA���Aĕ�A�t�A�n�A�hsA�VA�ƨA�dZA�O�A�C�A�33A�1A�  A���A��`A���A£�AA�`BA�$�A���A�G�A��A�1A��A�ĜA�|�A��A��/A�A��A��/A���A�~�A�bNA�bNA�XA�I�A�9XA�(�A�&�A��A�
=A��A�r�A��uA��mA���A���A��7A�t�A�hsA�^5A�XA�?}A��A��A�jA��A���A�JA��mA��-A���A��\A�n�A�XA�K�A�1'A�bA���A���A���A��\A�t�A�\)A��A��yA�A��A�ffA�7LA�{A���A�hsA��\A�-A���A�|�A�^5A��A�ZA�Q�A�I�A�5?A�"�A��A�bA�A��TA���A�A��^A���A���A���A���A���A�l�A�S�A�$�A��A�5?A���A�O�A��A���A��/A���A�ZA��A�VA�  A��TA���A�ĜA��jA��^A��-A���A���A���A���A�\)A�n�A���A�-A�%A���A��mA��;A���A���A�A��FA��A���A�t�A�=qA��yA��!A���A�l�A�M�A�;dA��A���A��TA���A���A��A�dZA�K�A�?}A�7LA�$�A� �A��A�JA��A��mA��HA��;A��;A��/A��
A�ƨA��-A���A��hA�t�A�?}A�A��A��A��A��HA���A��^A��A�l�A�^5A�O�A�(�A�ĜA���A�C�A�/A�$�A��A�JA�JA�A��A���A���A�jA�1'A�A��!A�p�A�;dA�bA���A���A��RA���A�l�A�K�A�1'A�VA��A��A�A��9A���A��+A�v�A�jA�^5A�M�A�33A�$�A��A�bA���A��A���A��!A���A�p�A�;dA�1A���A�t�A�1'A���A��A�A���A�hsA�5?A��A���A���A���A��uA��PA��A�|�A�r�A�l�A�hsA�bNA�K�A�E�A�1'A��A��A�JA���A��/A���A��^A���A�bNA�VA��A��;A���A�\)A��A��!A�E�A�
=A�A�  A��A��`A��
A�ȴA��jA���A�jA�E�A�-A�(�A�oA���A���A���A�p�A�XA�&�A�JA�  A��A��#A��FA���A���A���A���A�p�A�bNA�S�A�(�A�%A���A��yA���A��9A�z�A�(�A�{A���A��!A�VA���A�Q�A�+A��`A�x�A�+A�A��A��
A��9A�t�A���A��-A�O�A�(�A� �A�bA�JA�1A���A��wA�x�A�VA�E�A��A��A��
A�A��FA���A���A���A�"�A��7A�t�A�l�A�ffA�O�A�"�A�oA���A��A��HA��A���A�A���A��RA��-A��A���A��uA�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AȅAȗ�Aȝ�Aȟ�AȮAȮAȮAȮAȰ!AȲ-AȲ-AȰ!AȲ-AȲ-Aȴ9Aȴ9Aȴ9Aȴ9AȶFAȼjAȺ^AȼjAȼjAȾwAȼjAȶFAȶFAȰ!Aȴ9AȲ-Aȥ�Aȥ�Aș�Aș�AȓuAȉ7A�t�A�l�A�hsA�VA�Q�A�Q�A�S�A�Q�A�G�A�33A�"�A�VA���A���A���A���A��mAǣ�AǏ\AǅA�|�A�p�A�^5A���AƑhA�-A��A���AÑhA��#A��FA���A��uA�-A��hA�ZA��A��A��;A��A�~�A�$�A�&�A���A�z�A��wA�`BA�ȴA�ZA�bA��RA���A�ȴA�%A�?}A���A��A���A��yA���A�`BA�?}A���A��7A��A�ZA�hsA��9A�VA�\)A�\)A�^5A���A�?}A�-A���A~��Az=qAuG�Ar�9Ap�An�jAm�AmK�Al��Aj�/Ag�;Afz�Ae�7Ac��Aa/A_��A]�-A\JAYt�AV�9ATAQx�AQ�APȴANĜAL�AIK�AFn�AD�jAC�ABz�AA7LA?��A>M�A<�+A;;dA:ZA9"�A7�#A7%A5x�A2��A0��A/�A.{A-7LA++A*ZA(bA%�A$bA"bA $�A�hA��A��A��A�uAJA%A��A �A��A��A�7AoA�A~�A�Ap�AhsA
=A��AQ�A��A�A��AXA��A��A�A�RA�AG�A�`Av�A�^A
�RA	�A~�AQ�A��A�A�7A`BA�A��Ax�AM�A�AoAI�A�wA��A%A1'A�AdZA ��@�33@�J@��7@���@��@��@�+@���@���@��w@�l�@��y@�M�@��@��`@���@��@�C�@�7@�  @�~�@�n�@�^5@�E�@���@�$�@��;@�$�@�7@�O�@陚@�@�p�@��@�z�@�D@�A�@��@噚@�v�@ᙚ@�p�@�?}@�?}@���@�\)@��H@ް!@ާ�@ߥ�@�@���@۶F@�33@�C�@�\)@�$�@���@���@ׅ@�
=@��#@�1'@��@�E�@��@��@щ7@��;@�S�@�33@�+@�{@�"�@�"�@��y@�E�@Ɂ@�A�@�33@Ƈ+@�n�@�ff@�M�@Ł@ċD@�Z@�9X@�1@���@�dZ@�@�E�@��@�X@���@��9@�1'@���@�l�@�
=@��+@���@�r�@��@�t�@��y@���@�=q@�@��#@���@�p�@�&�@���@�bN@��m@���@���@�v�@�^5@�{@��^@�O�@��`@��j@���@��u@� �@��@�ƨ@�dZ@��H@���@�E�@�{@���@�x�@�O�@���@��D@�A�@�ƨ@�@�~�@���@���@�$�@��@�G�@�&�@���@�j@�Q�@� �@��
@�33@��@�+@�\)@��y@�ȴ@���@�$�@���@�/@���@�(�@��m@��
@�ƨ@��@�+@��H@�~�@�$�@�J@���@��h@��@�`B@�`B@�x�@��@��/@���@�Z@�"�@���@���@��+@�5?@��@��@�J@��@��T@��#@���@�`B@�&�@��@�j@�I�@��@��w@��P@�+@���@�~�@�$�@���@�7L@�/@�/@�%@�9X@��@��F@��@�v�@�=q@���@���@�O�@��`@���@��u@�j@�Z@�A�@�(�@�  @��@�S�@�
=@���@�5?@���@�/@�V@�V@�%@��@��m@��@��@�S�@�o@��@���@�5?@�x�@�?}@�?}@�7L@��`@��@���@��D@�bN@��@��w@�t�@�C�@�33@�o@���@���@��R@���@��+@�n�@�E�@���@��@�%@���@��9@�z�@�r�@�j@�Q�@�A�@�1'@��@���@��
@��F@���@�|�@�C�@��@�v�@�$�@��@��@���@�7L@���@���@��D@�r�@�bN@�1'@�ƨ@�t�@�33@��!@���@���@���@���@�^5@���@��^@���@�hs@�V@��u@� �@��m@��w@�dZ@�K�@��@���@�=q@�$�@��@�J@��@���@�7L@�V@��/@��j@��@��@�bN@�I�@�9X@�@��@|�@��@l�@
=@~5?@}�@}�@|��@|Z@{�F@{o@z��@z-@y�#@yx�@yG�@y%@x��@x��@xbN@xb@w�w@w��@wl�@w+@v��@u�@u�@u/@t��@t��@t�D@t(�@st�@r��@r-@rJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�AȅAȉ7AȅAȇ+AȍPAȏ\Aȥ�Aȟ�AȓuAȡ�Aȡ�AȑhAȟ�AȮAȬAȲ-AȮAȬAȰ!AȰ!Aȩ�AȰ!AȰ!AȮAȰ!AȰ!AȮAȮAȲ-AȰ!AȰ!Aȴ9AȰ!AȰ!AȲ-AȰ!AȰ!AȲ-AȮAȴ9AȰ!AȰ!Aȴ9AȲ-AȰ!Aȴ9AȲ-AȰ!Aȴ9Aȴ9AȮAȴ9AȰ!Aȴ9Aȴ9AȲ-AȶFAȲ-AȲ-AȶFAȴ9AȲ-AȸRAȴ9AȲ-Aȴ9AȰ!Aȴ9AȶFAȲ-AȶFAȴ9AȰ!AȶFAȶFAȲ-AȶFAȲ-AȲ-AȸRAȲ-AȶFAȺ^AȸRAȺ^AȾwAȺ^AȼjAȼjAȺ^AȼjAȺ^AȸRA���AȼjAȸRAȾwAȺ^AȺ^AȾwAȺ^AȺ^AȾwAȾwAȺ^A���AȾwAȼjA���A���AȼjAȼjA�AȾwAȺ^A���AȾwAȼjAȺ^AȼjAȴ9AȺ^AȶFAȲ-AȶFAȸRAȴ9AȸRAȺ^Aȴ9AȶFAȸRAȰ!AȲ-AȲ-AȬAȰ!Aȴ9AȲ-AȮAȴ9Aȴ9AȰ!Aȴ9Aȴ9AȲ-Aȴ9AȲ-Aȴ9Aȴ9AȮAȶFAȴ9AȰ!Aȧ�Aȩ�Aȥ�Aȣ�Aȧ�Aȣ�Aȥ�Aȣ�Aȣ�Aȩ�Aȡ�Aȧ�Aȣ�Aș�Aș�Aș�Aȕ�Aș�Aȝ�Aț�Aȗ�Aȕ�Aș�Aș�Aș�Aȝ�AȑhAȕ�Aȗ�AȑhAȑhAȑhAȉ7Aȏ\AȑhAȉ7AȅAȋDAȅA�t�A�v�A�x�A�r�A�t�A�v�A�r�A�p�A�r�A�n�A�ffA�jA�jA�hsA�hsA�l�A�jA�ffA�ffA�`BA�^5A�ZA�O�A�O�A�Q�A�M�A�O�A�S�A�O�A�O�A�S�A�Q�A�O�A�S�A�Q�A�O�A�VA�Q�A�S�A�S�A�O�A�S�A�S�A�O�A�S�A�Q�A�O�A�S�A�Q�A�O�A�S�A�K�A�I�A�K�A�E�A�G�A�E�A�=qA�?}A�;dA�33A�33A�+A�(�A�+A�+A�"�A�$�A�"�A��A��A��A�VA�bA�VA�1A�
=A�A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A���A���A���A��A���A���A���A���A���A���A���A��A���A���A���AǾwAǣ�Aǝ�AǏ\AǏ\AǓuAǏ\AǏ\AǓuAǍPAǋDAǍPAǋDAǇ+Aǉ7AǅAǁAǅAǃA�~�AǃA�~�A�v�A�z�A�z�A�v�A�x�A�p�A�l�A�p�A�n�A�jA�hsA�jA�ffA�^5A�ZA�M�A�(�A�oA���A���A��mA��HA��;A���A�ƨAƇ+A�t�A�p�A�^5A�M�A�7LA�+A� �A�&�A�&�A��A�oA�1A���A�ȴAŬAś�Aŉ7A�bNA���Aĕ�A�t�A�n�A�hsA�VA�ƨA�dZA�O�A�C�A�33A�1A�  A���A��`A���A£�AA�`BA�$�A���A�G�A��A�1A��A�ĜA�|�A��A��/A�A��A��/A���A�~�A�bNA�bNA�XA�I�A�9XA�(�A�&�A��A�
=A��A�r�A��uA��mA���A���A��7A�t�A�hsA�^5A�XA�?}A��A��A�jA��A���A�JA��mA��-A���A��\A�n�A�XA�K�A�1'A�bA���A���A���A��\A�t�A�\)A��A��yA�A��A�ffA�7LA�{A���A�hsA��\A�-A���A�|�A�^5A��A�ZA�Q�A�I�A�5?A�"�A��A�bA�A��TA���A�A��^A���A���A���A���A���A�l�A�S�A�$�A��A�5?A���A�O�A��A���A��/A���A�ZA��A�VA�  A��TA���A�ĜA��jA��^A��-A���A���A���A���A�\)A�n�A���A�-A�%A���A��mA��;A���A���A�A��FA��A���A�t�A�=qA��yA��!A���A�l�A�M�A�;dA��A���A��TA���A���A��A�dZA�K�A�?}A�7LA�$�A� �A��A�JA��A��mA��HA��;A��;A��/A��
A�ƨA��-A���A��hA�t�A�?}A�A��A��A��A��HA���A��^A��A�l�A�^5A�O�A�(�A�ĜA���A�C�A�/A�$�A��A�JA�JA�A��A���A���A�jA�1'A�A��!A�p�A�;dA�bA���A���A��RA���A�l�A�K�A�1'A�VA��A��A�A��9A���A��+A�v�A�jA�^5A�M�A�33A�$�A��A�bA���A��A���A��!A���A�p�A�;dA�1A���A�t�A�1'A���A��A�A���A�hsA�5?A��A���A���A���A��uA��PA��A�|�A�r�A�l�A�hsA�bNA�K�A�E�A�1'A��A��A�JA���A��/A���A��^A���A�bNA�VA��A��;A���A�\)A��A��!A�E�A�
=A�A�  A��A��`A��
A�ȴA��jA���A�jA�E�A�-A�(�A�oA���A���A���A�p�A�XA�&�A�JA�  A��A��#A��FA���A���A���A���A�p�A�bNA�S�A�(�A�%A���A��yA���A��9A�z�A�(�A�{A���A��!A�VA���A�Q�A�+A��`A�x�A�+A�A��A��
A��9A�t�A���A��-A�O�A�(�A� �A�bA�JA�1A���A��wA�x�A�VA�E�A��A��A��
A�A��FA���A���A���A�"�A��7A�t�A�l�A�ffA�O�A�"�A�oA���A��A��HA��A���A�A���A��RA��-A��A���A��uA�z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B��B��B�dB�B�B�B��B�NB�B�NB�B�NB�B�NB��B�NB�B�NB��B�B�NB�NB�NB�B�NB��B��B��B�B�B�|B�B�B�vB�B��B�BB�B�B�ZB��B��B�B�B�B�B�mB�DB��B�WB��B�)B�B�/B��B��B��B�"B�B��B�
B�B�BB��B�yB��B��B��B�RBÖB�aB�B��B��B��B��B�CB��B��B��B�	B�PBi�BYB_pB>wB7�B<B9�B6FB5B#�B
	B�B��B˒B�0B��B�VBjBK)B+kB~B
��B
�B
یB
�EB
��B
��B
��B
��B
g8B
Q�B
<jB
B
%B	��B	�oB	��B	�B	�B	��B	˒B	�[B	�6B	�hB	��B	�B	�"B	�MB	{JB	jB	c�B	RTB	N�B	J�B	D�B	1�B	(�B	�B	{B	bB	�B	%B	AB�xB�TB��B�B��B�BB��B֡BѷBȴB�9B��B��B��B��B��B�^B��B��B��B�BÖB��B�UB�B��B�'B�UB�9B��B�}B�[B�9B�BܒB�B�B�B	;B	 �B		lB	�B	B	uB	�B		B	�B	OB	�B	%B	$�B	&B	%zB	(XB	%FB	&LB	�B	�B	"�B	'B	)�B	*eB	-CB	/�B	.�B	)�B	$tB	"�B	(�B	,�B	1�B	5?B	2�B	1�B	2-B	5�B	:�B	<6B	<jB	=�B	:�B	5?B	.�B	,=B	/�B	B�B	I�B	G�B	F�B	C�B	H�B	T�B	XEB	ZB	W
B	U2B	U2B	VB	V9B	V9B	W?B	W�B	\�B	V�B	R�B	[WB	g�B	h
B	h�B	jB	j�B	k�B	p�B	tB	rGB	g�B	b�B	bNB	c B	d�B	g�B	g8B	h
B	h>B	iB	sMB	w2B	t�B	t�B	u�B	{�B	�B	�+B	��B	��B	�B	��B	�1B	�1B	��B	��B	��B	��B	�MB	�1B	��B	��B	�DB	��B	�~B	�DB	�\B	��B	�\B	��B	��B	� B	��B	�{B	�B	�kB	��B	��B	��B	��B	�CB	��B	�bB	��B	�B	��B	��B	��B	��B	�B	�6B	�kB	��B	�IB	��B	�IB	�OB	��B	��B	�B	�tB	��B	�FB	�LB	��B	��B	�^B	�B	�HB	�OB	��B	��B	�UB	��B	ÖB	��B	�9B	�mB	�9B	��B	�B	�EB	ȀB	��B	ȴB	�B	�B	��B	�RB	˒B	�dB	�^B	�dB	��B	�dB	�B	ΥB	�B	��B	уB	�NB	уB	��B	��B	�,B	��B	��B	��B	ܒB	�jB	�B	�B	��B	�B	�B	��B	��B	�B	��B	�DB	�B	�B	��B	�B	�B	��B	�WB	�/B	�B	��B	�B	�AB	�B	��B	��B	�TB	�B	��B	�B	�B	�B	�B	�%B	��B	��B	��B	�%B	�ZB	��B	�%B	��B	�2B	�B	��B	��B	�B	��B	��B	��B	�]B	��B	�cB	��B
  B
  B	��B
 �B
�B
{B
�B
B
�B
SB
�B
�B
+B
�B
�B
	B
	lB
	�B

�B
JB
�B
"B
VB
�B
�B
�B
VB
�B
�B
�B
�B
4B
�B
hB
�B
B
B
@B
uB
MB
�B
{B
MB
�B
1B
1B
1B
1B
�B
�B
B
=B
�B
xB
B
�B
�B
�B
�B
�B
�B
!B
�B
B
�B
IB
�B
B
�B
VB
�B
�B
�B
�B
�B
 \B
 \B
 �B
 �B
 \B
!-B
"hB
"hB
#:B
#�B
$B
$tB
$tB
$�B
%B
$�B
$�B
%FB
&�B
'�B
(�B
*0B
)�B
)�B
)_B
)*B
)�B
+B
*�B
*�B
*0B
*0B
)_B
)�B
)_B
)�B
*0B
*0B
*0B
+kB
,qB
-B
-CB
-CB
-wB
-�B
.IB
.�B
.�B
.}B
.IB
.}B
.�B
/B
/OB
/�B
0UB
0�B
0�B
1'B
1�B
1�B
2aB
2�B
2�B
33B
3hB
3�B
4B
49B
4nB
4�B
5B
5tB
5�B
5�B
6B
6FB
6zB
6�B
6�B
6�B
7B
6�B
6�B
7B
7�B
8B
8�B
9�B
:^B
:�B
9�B
9�B
9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�pB��B�#B��B�/B�5B��BޞB�;B�]B�BB�jB��B��B��B�B�B�NB� B�HB�HB�TB�B�|B�B�B�B� B��B�HB�B�B�HB��B�B�B�B��B�|B� B�HB�NB��B�|B�NB�B�HB�B� B�|B�B� B�HB�TB�B�B��B�|B�B�B�B�B� B�|B�B�B�|B�TB�NB�B�B��B�NB�TB�HB�B�TB�B�B�B�B� B��B��B�B�B�HB��B��B��B� B�B�B� B�B�B�TB�HB�B�B�HB� B� B�HB�B��B�|B�B�B�|B�HB��B�B�HB�B�TB�|B�B�B�B��B�B�vB�B� B�B�|B�B�|B��B�B�B�HB�B�HB��B� B�B�HB�B� B��B�B� B�|B�HB�B��B�B��B�B�TB�BB�|B� B��B�vB��B��B�B�NB�B��B��B�B�B�jB�|B�vB�BߤB�BߤB�jBߤB��B�BߤB�B�HBߤB�B�pB��B�vB�BB�dB�BߤB�5B�vB�B�dB�BB��B��B�;B�HB�B�pB�HB�B�pB�|B�B��B�vB�B��B�BB�B�NB�B�B�B�TB�2B��B�&B��B�`B�B�,B�`B�B�`B��B��B�B��B�B�,B�ZB�ZB�B�&B�&B�B�&B�B�`B�B�B�`B�B�2B��B�&B�fB�,B��B�B�,B��B��B��B�mB�mB�2B�2B�>B��B�B�yB�mB�B�B��B�B�B��B��B��B�B�QB�)B�B�QB��B��B�B�WB��B�B�B��B��B�B��B�B��B�]B�/B��B��B��B�B�B�]B��B��B��B��B�WB��B�5B�cB�/B�]B��B�B�B�B�B��B��B��B�cB�"B�B��B��B�B��B�"B��B��B�WB�"B��B��B�]B��B�B�B�"B��B�B�KB�B��B�"B�B�B�B�B�DB�DB�>B�B�KB�ZB�2B�B�2B�B�vB�vB�HB�;B�pB�HB�|B�5B�NB��B�B�#B�B��B�B�pB��B�B��B��B��B��BуB� B�TB�[B��B̘B�BBϫB�BB�dB��B��BҽB�aB�pBȀB�XB��BϫB�vBB�KB�B�XBϫB�XB�B�3BƨB�B�B��B��B�-B�OB�-B�9B�BĜB�B��B��B��B��B��B�=B�B��B�B��B��B�B�OB��B�LB��B��B�[B��B�IB�B��B��B��B�UB��B��B�qB�B��B��B��B�B�RB��B��B��B��B�!B��B��B�-B��B��B��B��B�B��B��B��B��B��B�YB��B�VB�B��B�kB��B�CB��B�1B�xB�:B�xB��B.By�Bt�Bq�Bu�Bo�Ba�B\�B^B\)BZ�BZ�BW�BW
BXEBX�BXEBW?BXyBaHB|�BaHB~B<�B:^B:*B:�B9�B6�B7LB6FB5B7B7�B;0BEB9�B8�B>�B:�B:*B>BB9XB7LB6�B<jB:*B;0B9XB4�B6�B6B6zB5tB6�B7B4�B5�B5�B4�B4B5�B8B8RB7LB7B8�B<�B8B2�B1�B/B/OB1'B2-B-�B2�B)�B*eB.�B(XB+B%�B�B_B�B�BMB�BhB�B#B�B�B�B	7BAB 4B��B�	B��B��B��B�+B��B�B�;B�B��B�KB�>B�KB�B��B� B�B�NB��B�B�B�B�5BݘB��B��B��B�B�#B��B�mBרB�&B��BʌBɆB�tB��B�RBɆB�aB��B�}B��B�BB�wB�<B��B�0B��B�dB�B��B��B��B�B��B�FB�3B��B�tB��B�tB��B��B��B��B��B�RB��B��B��B�PB�VB��B�B�~B�B��B��B�"B�+B�uB|PB{�B�{B��Bu�Bd�Bd�Bg�Bk�BZ�BZB\�BVmBO�BPBK�BO�BM�BI�BJ�BN<BF�BEBHKBEmBE�BHKBB�B:�B3�B?B49BA�B/�B!�B,=B+kB�B�B�BMBYBuB�BoB�B�B�BB�BB%B�BfB
��B
�B{B
��B
��B
��B
�`B
��B
�B
�B�B
�5B
�DB
�8B
�2B
�B
�B
�ZB
�B
�B
�ZB
�|B
�B
�NB
�jB
�jB
�dB
ݘB
��B
�;B
��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                    4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                    4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2023020412024620230204120246IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023020413020820230204130208QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023020413020820230204130208QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194920230210131949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                