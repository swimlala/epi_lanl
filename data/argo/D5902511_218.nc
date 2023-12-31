CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-07-16T09:04:03Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  U�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  \   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  t�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  {(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 
   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8 "�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � )   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` A�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   BP   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   HP   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   NP   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T TP   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   T�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   T�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   T�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   T�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   UD   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   U`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Uh   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        U�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        U�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       U�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    U�Argo profile    3.1 1.2 19500101000000  20220716090403  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_218                 6810_008521_218                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�ڰ�ud@�ڰ�ud11  @�ڱ��v@�ڱ��v@0|NP��1@0|NP��1�d����n�d����n11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @�  @�  @�G�@�p�A\)A�RA+�A?\)A_\)A�Q�A�Q�A��A�  A�  AУ�A�Q�A�Q�A��B  BQ�B(�B�B'�B/�
B7�
B?�
BH  BP  BW�
B_�
Bh  Bp(�BxQ�B�{B��B��B��B�  B�{B�  B�{B�{B�  B�{B�  B��B��B�  B�(�B�{B��B�  B�(�B�  B�  B�{B�  B�  B��B�  B�{B�  B�{B�{B��B��C��C  C
=C  C	��C{C  C��C��C
=C
=C  C  C
=C  C {C"{C$
=C&  C(  C*  C+��C.{C0  C2  C3��C6
=C8
=C:  C<  C>  C@  CB  CD
=CF
=CH
=CJ
=CL  CN
=CO��CQ��CS��CU��CX  CZ{C\
=C^  C`
=Cb  Cd  Cf  Ch  Cj  Cl
=Cm��Cp  Cr  Ct
=Cv{Cw��Cy��C|
=C}��C��C�C�C�C�C�  C�C�
=C�C�C�C�  C�  C���C���C���C�  C�C�
=C�  C���C�  C���C���C�  C�  C�C�  C���C���C��C���C�  C���C���C�
=C�\C�
=C�C�C�C���C�  C�  C���C���C���C���C���C���C���C���C���C�
=C�C�  C�C�C�  C��C�  C�
=C�C�C�  C���C���C�C�  C���C�  C�C�  C���C�  C�
=C�C�  C���C���C�  C�C�C�C�
=C�  C�  C�C�C�  C���C�  C�C�C���C���C���C�  C���C���C���C�  C�  C�  C�  C���C�  C�  C���C�C�C���C���C�  C���C���C�  C���C���C�  C�C�
=C�C���C�  C���C���C�D �D ��DD� D  D� D  D��D�D}qD�D� D��Dz�D  D� D  D��D	�D	��D
  D
� D�D� D  D� D�qD}qD��D� D  D��D�D��D  D� D�D��DD�D  D� D�qD� D  D� D�qD� D  D� D  D� D�D��DD�D�D}qD  D��D  D��D�D� D�qD z�D!  D!}qD!�qD"� D"�qD#}qD#�qD$��D%�D%��D&D&� D&�qD'��D(�D(� D)  D)��D*D*��D*�qD+}qD,  D,� D,�qD-z�D-�qD.� D/  D/��D0D0�D1D1� D2  D2��D3  D3}qD4  D4��D5�D5� D5�qD6� D7�D7� D7�qD8� D9  D9� D9�qD:� D;�D;� D<  D<� D=  D=� D>  D>}qD?  D?��D@  D@��DA�DA}qDA�qDB}qDB�qDC��DD�DD��DE�DE��DFDF� DF�qDG}qDH  DH}qDH�qDI� DI�qDJ� DK�DK��DL  DL� DL�qDM}qDN�DN��DN�qDO}qDO�qDP� DQ  DQ� DR  DR� DS  DS� DS�qDT��DUDU� DV�DV��DV�qDW}qDX�DX�DY�DY� DZ  DZ��D[�D[}qD\�D\��D]  D]� D^�D^�D_�D_� D_�qD`� Da�Da��Db  Db}qDb�qDc� Dd�Dd�De  De}qDe��Df}qDg  Dg��Dh  Dh}qDi  Di� Dj  Dj��DkDk�Dl�Dl��Dm  Dm}qDn  Dn� Do�Do�Do�qDpz�Dp��Dq}qDrDr��Ds�Ds��Dt�Dt��Du  Du��Du�qDv}qDw  Dw}qDx�Dx��Dx�qDy��Dz  Dzz�D{  D{��D|  D|}qD}  D}�D~�D~� D~�qD� D�HD�AHD��HD�� D���D�@ D�� D�� D�HD�AHD�� D��HD�  D�>�D�� D���D�  D�AHD�~�D�� D�HD�@ D��HD�D��D�AHD�� D���D��qD�>�D�~�D���D�HD�@ D��HD��HD���D�>�D�� D�� D�  D�>�D�xRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
>�?W
=?��R?Ǯ?��@
=@&ff@@  @W
=@h��@��
@�{@�Q�@�ff@���@�p�@˅@�33@�G�@�{@�AG�AQ�A��A�
A=qA{A%�A+�A0  A5A=p�AB�\AG�AN�RATz�AX��A_\)AeAi��Ap��Aw
=A{�A���A�(�A��RA�G�A���A��A��A�p�A���A��HA�A�G�A�(�A�ffA�=qA��A�\)A��HA�{A���A�33A��RA���A�(�A�  A��HA��A�Q�A��
AָRA�G�A���A�  A�=qA�p�A�G�A�A�{A��A��A��A�=qA�B Q�B��B33B��B�B33B��B
{B
=Bz�B�B�RB�
Bp�BffB
=Bz�B��B=qB33Bz�B�BB
=B�
BQ�Bp�BffB�HB�
B ��B!B"=qB#\)B$Q�B$��B&{B'
=B'�B(z�B)��B*{B*�HB,(�B,��B-p�B.�RB/\)B0  B1�B2{B2�RB3\)B4��B5G�B6{B7\)B7�B8��B9B:�\B;
=B<(�B=G�B=�B>�\B?�B@��BAp�BB{BC
=BDQ�BD��BE��BF�\BG�BH(�BH��BI�BJ�HBK�BLQ�BMp�BN{BN�HBO�BP��BQ��BR=qBS\)BTQ�BU�BU��BV�RBW�
BXz�BYG�BZ=qB[\)B\(�B\��B]B^�RB_\)B`Q�BaG�Ba�Bb�\Bc�Bd��BeG�BeBf�HBg�
Bh(�Bi�Bj=qBj�HBk\)Blz�BmG�BmBn�\Bo�
BpQ�Bp��Br{Bs
=Bs�BtQ�Bup�BvffBw\)Bw�
Bxz�ByBz�RB{33B|  B}�B~{B~�RB\)B�(�B��RB�
=B�\)B��B�ffB��RB�
=B��B�(�B�z�B��HB�\)B��B�=qB��\B�
=B���B�  B�=qB��RB�33B��B�{B�ffB���B�G�B��
B�(�B�ffB���B��B��B�(�B��\B��B���B��B�=qB��RB�33B��B��
B�=qB��RB�33B���B��
B�=qB��RB�33B��B�B�(�B��RB��B�p�B��B�Q�B��RB���B�\)B��
B�=qB�z�B���B�\)B��
B�  B�ffB��RB�33B���B��
B�=qB��RB�33B��B��
B�=qB��RB�G�B��B�  B�Q�B��HB�\)B�B�  B��\B�
=B�\)B��B�=qB��RB��B�\)B��
B�Q�B���B�
=B�\)B��B�z�B��RB���B��B�{B�Q�B���B�
=B��B�  B�=qB��\B���B�p�B��B�=qB�z�B���B��B��B�=qB��\B�
=B���B�{B�Q�B���B�33B�B�(�B�z�B���B�G�B�B�=qB���B���B�G�B��B�Q�B���B��B��B��
B�=qB��RB�G�B���B��B�Q�B��HB�\)B�B�  B�ffB���B�p�B�B�  B�z�B���B�p�B�B�{B�z�B��HB�p�B��
B�=qB�z�B��HB�p�B��
B�ffB��HB�G�BɅB��B�z�B���B�\)BˮB�{B̏\B�
=B�p�B�B�(�BΣ�B�33Bϙ�B��
B�(�BУ�B�
=Bљ�B�{B�z�B���B��B�p�B��
B�Q�B��HB�G�BՅB��B�ffB��HB�G�Bי�B��B�z�B���B�33Bٙ�B�{Bڣ�B��B�p�B�B�=qB���B�
=B�p�B��
B�ffB���B�
=Bߙ�B�{B�z�B�RB�33B�B�{B�ffB���B�B��B�=qB��B�33B�B�  B�Q�B���B�p�B�B�{B�\B��B陚B�  B�Q�B���B�p�B��
B�=qB��B�G�B�B�{B�z�B�
=B�B��
B�ffB��HB�G�B�B�  B�\B���B�\)B�B�Q�B���B�33B���B�  B��\B���B�33B��B�=qB���B�
=B�p�B��B�z�B���B�33B���B�=qB��RB���B��B�  B���B���B�G�B��
C 33C \)C �\C �
C�CG�Cz�C��C�CG�Cz�CC{CQ�C�C�RC{C\)C�\CC
=CQ�C��C�
C{CffC�C�HC�Cp�C�RC�C�Cp�CC	
=C	G�C	z�C	�RC

=C
\)C
��C
��C{CffC�C��C=qCz�C�C�C=qC�\C�HC�CQ�C��C�C=qC�\C��C  CG�C��C�C=qCp�C�C
=CffC�C�C(�Cz�C�
C(�CffC��C��CQ�C�C�C(�Cz�C�RC
=Cp�C�C��C=qC��C��C=qCz�C�RC
=Cp�C�C��C33C�\C�HC33C�C��C
=Cp�CC
=CG�C�\C�HC=qC�\C�HC 33C p�C �C ��C!G�C!��C!��C"=qC"�\C"�
C#�C#\)C#��C$  C$\)C$�C$��C%=qC%�C%��C&�C&ffC&C'{C'ffC'�C'�C((�C(z�C(C){C)\)C)�RC*  C*Q�C*��C*�HC+(�C+p�C+�C+��C,=qC,z�C,��C-{C-ffC-�RC.  C.Q�C.��C.�
C/�C/ffC/��C/�C033C0p�C0�RC1  C1Q�C1��C1��C2=qC2z�C2C3
=C3G�C3�\C3��C4{C4\)C4��C4�HC5(�C5p�C5C6
=C6G�C6��C6�HC7(�C7p�C7�C7��C8=qC8�C8C9{C9\)C9��C9��C:=qC:�C:C;{C;\)C;��C;�
C<{C<\)C<��C<�HC=33C=z�C=��C>{C>\)C>��C>�C?33C?z�C?�RC?��C@=qC@�C@CA
=CAG�CA�\CA�
CB�CB\)CB��CB��CC{CC\)CC��CC�CD33CDp�CD�CD��CE=qCE�CECF
=CFQ�CF��CF�HCG�CGffCG��CG�HCH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@   @@  @�  @�  @�  @�G�@�p�A\)A�RA+�A?\)A_\)A�Q�A�Q�A��A�  A�  AУ�A�Q�A�Q�A��B  BQ�B(�B�B'�B/�
B7�
B?�
BH  BP  BW�
B_�
Bh  Bp(�BxQ�B�{B��B��B��B�  B�{B�  B�{B�{B�  B�{B�  B��B��B�  B�(�B�{B��B�  B�(�B�  B�  B�{B�  B�  B��B�  B�{B�  B�{B�{B��B��C��C  C
=C  C	��C{C  C��C��C
=C
=C  C  C
=C  C {C"{C$
=C&  C(  C*  C+��C.{C0  C2  C3��C6
=C8
=C:  C<  C>  C@  CB  CD
=CF
=CH
=CJ
=CL  CN
=CO��CQ��CS��CU��CX  CZ{C\
=C^  C`
=Cb  Cd  Cf  Ch  Cj  Cl
=Cm��Cp  Cr  Ct
=Cv{Cw��Cy��C|
=C}��C��C�C�C�C�C�  C�C�
=C�C�C�C�  C�  C���C���C���C�  C�C�
=C�  C���C�  C���C���C�  C�  C�C�  C���C���C��C���C�  C���C���C�
=C�\C�
=C�C�C�C���C�  C�  C���C���C���C���C���C���C���C���C���C�
=C�C�  C�C�C�  C��C�  C�
=C�C�C�  C���C���C�C�  C���C�  C�C�  C���C�  C�
=C�C�  C���C���C�  C�C�C�C�
=C�  C�  C�C�C�  C���C�  C�C�C���C���C���C�  C���C���C���C�  C�  C�  C�  C���C�  C�  C���C�C�C���C���C�  C���C���C�  C���C���C�  C�C�
=C�C���C�  C���C���C�D �D ��DD� D  D� D  D��D�D}qD�D� D��Dz�D  D� D  D��D	�D	��D
  D
� D�D� D  D� D�qD}qD��D� D  D��D�D��D  D� D�D��DD�D  D� D�qD� D  D� D�qD� D  D� D  D� D�D��DD�D�D}qD  D��D  D��D�D� D�qD z�D!  D!}qD!�qD"� D"�qD#}qD#�qD$��D%�D%��D&D&� D&�qD'��D(�D(� D)  D)��D*D*��D*�qD+}qD,  D,� D,�qD-z�D-�qD.� D/  D/��D0D0�D1D1� D2  D2��D3  D3}qD4  D4��D5�D5� D5�qD6� D7�D7� D7�qD8� D9  D9� D9�qD:� D;�D;� D<  D<� D=  D=� D>  D>}qD?  D?��D@  D@��DA�DA}qDA�qDB}qDB�qDC��DD�DD��DE�DE��DFDF� DF�qDG}qDH  DH}qDH�qDI� DI�qDJ� DK�DK��DL  DL� DL�qDM}qDN�DN��DN�qDO}qDO�qDP� DQ  DQ� DR  DR� DS  DS� DS�qDT��DUDU� DV�DV��DV�qDW}qDX�DX�DY�DY� DZ  DZ��D[�D[}qD\�D\��D]  D]� D^�D^�D_�D_� D_�qD`� Da�Da��Db  Db}qDb�qDc� Dd�Dd�De  De}qDe��Df}qDg  Dg��Dh  Dh}qDi  Di� Dj  Dj��DkDk�Dl�Dl��Dm  Dm}qDn  Dn� Do�Do�Do�qDpz�Dp��Dq}qDrDr��Ds�Ds��Dt�Dt��Du  Du��Du�qDv}qDw  Dw}qDx�Dx��Dx�qDy��Dz  Dzz�D{  D{��D|  D|}qD}  D}�D~�D~� D~�qD� D�HD�AHD��HD�� D���D�@ D�� D�� D�HD�AHD�� D��HD�  D�>�D�� D���D�  D�AHD�~�D�� D�HD�@ D��HD�D��D�AHD�� D���D��qD�>�D�~�D���D�HD�@ D��HD��HD���D�>�D�� D�� D�  D�>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
>�?W
=?��R?Ǯ?��@
=@&ff@@  @W
=@h��@��
@�{@�Q�@�ff@���@�p�@˅@�33@�G�@�{@�AG�AQ�A��A�
A=qA{A%�A+�A0  A5A=p�AB�\AG�AN�RATz�AX��A_\)AeAi��Ap��Aw
=A{�A���A�(�A��RA�G�A���A��A��A�p�A���A��HA�A�G�A�(�A�ffA�=qA��A�\)A��HA�{A���A�33A��RA���A�(�A�  A��HA��A�Q�A��
AָRA�G�A���A�  A�=qA�p�A�G�A�A�{A��A��A��A�=qA�B Q�B��B33B��B�B33B��B
{B
=Bz�B�B�RB�
Bp�BffB
=Bz�B��B=qB33Bz�B�BB
=B�
BQ�Bp�BffB�HB�
B ��B!B"=qB#\)B$Q�B$��B&{B'
=B'�B(z�B)��B*{B*�HB,(�B,��B-p�B.�RB/\)B0  B1�B2{B2�RB3\)B4��B5G�B6{B7\)B7�B8��B9B:�\B;
=B<(�B=G�B=�B>�\B?�B@��BAp�BB{BC
=BDQ�BD��BE��BF�\BG�BH(�BH��BI�BJ�HBK�BLQ�BMp�BN{BN�HBO�BP��BQ��BR=qBS\)BTQ�BU�BU��BV�RBW�
BXz�BYG�BZ=qB[\)B\(�B\��B]B^�RB_\)B`Q�BaG�Ba�Bb�\Bc�Bd��BeG�BeBf�HBg�
Bh(�Bi�Bj=qBj�HBk\)Blz�BmG�BmBn�\Bo�
BpQ�Bp��Br{Bs
=Bs�BtQ�Bup�BvffBw\)Bw�
Bxz�ByBz�RB{33B|  B}�B~{B~�RB\)B�(�B��RB�
=B�\)B��B�ffB��RB�
=B��B�(�B�z�B��HB�\)B��B�=qB��\B�
=B���B�  B�=qB��RB�33B��B�{B�ffB���B�G�B��
B�(�B�ffB���B��B��B�(�B��\B��B���B��B�=qB��RB�33B��B��
B�=qB��RB�33B���B��
B�=qB��RB�33B��B�B�(�B��RB��B�p�B��B�Q�B��RB���B�\)B��
B�=qB�z�B���B�\)B��
B�  B�ffB��RB�33B���B��
B�=qB��RB�33B��B��
B�=qB��RB�G�B��B�  B�Q�B��HB�\)B�B�  B��\B�
=B�\)B��B�=qB��RB��B�\)B��
B�Q�B���B�
=B�\)B��B�z�B��RB���B��B�{B�Q�B���B�
=B��B�  B�=qB��\B���B�p�B��B�=qB�z�B���B��B��B�=qB��\B�
=B���B�{B�Q�B���B�33B�B�(�B�z�B���B�G�B�B�=qB���B���B�G�B��B�Q�B���B��B��B��
B�=qB��RB�G�B���B��B�Q�B��HB�\)B�B�  B�ffB���B�p�B�B�  B�z�B���B�p�B�B�{B�z�B��HB�p�B��
B�=qB�z�B��HB�p�B��
B�ffB��HB�G�BɅB��B�z�B���B�\)BˮB�{B̏\B�
=B�p�B�B�(�BΣ�B�33Bϙ�B��
B�(�BУ�B�
=Bљ�B�{B�z�B���B��B�p�B��
B�Q�B��HB�G�BՅB��B�ffB��HB�G�Bי�B��B�z�B���B�33Bٙ�B�{Bڣ�B��B�p�B�B�=qB���B�
=B�p�B��
B�ffB���B�
=Bߙ�B�{B�z�B�RB�33B�B�{B�ffB���B�B��B�=qB��B�33B�B�  B�Q�B���B�p�B�B�{B�\B��B陚B�  B�Q�B���B�p�B��
B�=qB��B�G�B�B�{B�z�B�
=B�B��
B�ffB��HB�G�B�B�  B�\B���B�\)B�B�Q�B���B�33B���B�  B��\B���B�33B��B�=qB���B�
=B�p�B��B�z�B���B�33B���B�=qB��RB���B��B�  B���B���B�G�B��
C 33C \)C �\C �
C�CG�Cz�C��C�CG�Cz�CC{CQ�C�C�RC{C\)C�\CC
=CQ�C��C�
C{CffC�C�HC�Cp�C�RC�C�Cp�CC	
=C	G�C	z�C	�RC

=C
\)C
��C
��C{CffC�C��C=qCz�C�C�C=qC�\C�HC�CQ�C��C�C=qC�\C��C  CG�C��C�C=qCp�C�C
=CffC�C�C(�Cz�C�
C(�CffC��C��CQ�C�C�C(�Cz�C�RC
=Cp�C�C��C=qC��C��C=qCz�C�RC
=Cp�C�C��C33C�\C�HC33C�C��C
=Cp�CC
=CG�C�\C�HC=qC�\C�HC 33C p�C �C ��C!G�C!��C!��C"=qC"�\C"�
C#�C#\)C#��C$  C$\)C$�C$��C%=qC%�C%��C&�C&ffC&C'{C'ffC'�C'�C((�C(z�C(C){C)\)C)�RC*  C*Q�C*��C*�HC+(�C+p�C+�C+��C,=qC,z�C,��C-{C-ffC-�RC.  C.Q�C.��C.�
C/�C/ffC/��C/�C033C0p�C0�RC1  C1Q�C1��C1��C2=qC2z�C2C3
=C3G�C3�\C3��C4{C4\)C4��C4�HC5(�C5p�C5C6
=C6G�C6��C6�HC7(�C7p�C7�C7��C8=qC8�C8C9{C9\)C9��C9��C:=qC:�C:C;{C;\)C;��C;�
C<{C<\)C<��C<�HC=33C=z�C=��C>{C>\)C>��C>�C?33C?z�C?�RC?��C@=qC@�C@CA
=CAG�CA�\CA�
CB�CB\)CB��CB��CC{CC\)CC��CC�CD33CDp�CD�CD��CE=qCE�CECF
=CFQ�CF��CF�HCG�CGffCG��CG�HCH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�;dA�C�A�A�A�C�A�G�A�G�A�I�A�G�A�I�A�G�A�I�A�G�A�I�A�K�A�ffA�l�A�z�AՑhAգ�Aա�A՗�AՉ7AՉ7AՃA�t�A�1'A��yA�A�z�AӰ!A���A��AЍPA�XA�^5A�1'A�z�AС�AБhA�33A�ĜA��A�;dA��Ả7A�+A���A�+A�/AǬA��A���A��yA��`A��A���A���A��9A�x�A��hA�A�VA��#A��A�C�A�K�A��
A�=qA�XA���A��A�33A��mA��!A��+A�1A��
A�-A�;dA�
=A�l�A��A��!A�hsA�{A��9A�JA�z�A���A���A��\A�bA��TA���A���A��uA�|�A�A���A�dZA���A�7LA�-A�ZA�VA�
=A�\)A�/A�7LA��FA��A��A��+A�G�A�S�A��A�~�A��RA~n�A{�^Az��Aw�
Au;dArQ�Ao�Al�jAj �Ai?}Ah��AgƨAcƨA_��A]dZA[AW|�ASS�AO�AM�^AK|�AI/AH1AG�AE��ADĜACA@�yA?oA=�#A<�jA;�A;G�A;%A9hsA6�9A49XA2=qA0��A/�A.��A.�A-C�A*r�A)7LA(��A'��A&�yA%�-A$~�A#�#A"�jA"=qA!��A!%A�TA�yAI�A�;A7LA��AM�A��AK�A�HA�\A��AK�A�mA�A��A
=AXAA�Av�A�A  A�hA&�A�;A+A�+A+A^5AI�A�A�A�+A�FA�RA��A��A��A�A�A�+Ax�@�^5@�hs@�I�@�I�@��j@��@���@��@��\@�V@�hs@��9@���@��T@�h@��#@��@�I�@�(�@���@�x�@�@�5?@�;d@�@�
=@��;@�Z@�@��;@�+@��@�"�@�G�@�Z@�{@��u@�@�\@���@��T@�z�@�1@߾w@ݺ^@�X@ە�@�C�@�dZ@�"�@�$�@ّh@ج@׶F@֧�@�G�@�(�@�b@�  @ӍP@��@�5?@���@с@���@�1@���@�ff@͑h@�7L@̬@�bN@� �@��;@˶F@˶F@�ƨ@�1@���@ʏ\@���@�hs@�7L@�7L@�@ɉ7@�&�@Ȭ@��m@�\)@���@���@��@�"�@öF@�|�@���@�K�@�+@�@�@���@���@�x�@�/@���@�{@��@�9X@��
@��F@��F@�o@�v�@���@���@�x�@�V@��u@�z�@�I�@�  @��m@��@�\)@��H@�@���@��D@��@��@�|�@�\)@�K�@��@�=q@��7@��^@�=q@�J@��-@�x�@�X@�&�@���@���@�l�@�t�@�dZ@��y@��+@�$�@���@�X@�G�@�?}@�/@��9@�Z@��w@�K�@�;d@��@��y@��!@��+@�V@��@�`B@�/@�r�@�(�@��w@��P@�C�@�33@��@���@��R@�ff@�@��T@���@�@�O�@��/@��@��@�Q�@�  @���@��F@���@�t�@�\)@�33@���@�v�@�-@��-@�G�@���@�r�@�b@��@��@�v�@�=q@���@��#@���@�x�@�7L@��@���@��9@���@� �@��@���@�l�@�C�@�33@�33@�"�@�
=@��@��@�ȴ@��R@���@�v�@�M�@���@���@���@��/@�Q�@��w@�33@��@��H@��H@��H@��@���@��!@�E�@�J@��@���@��h@�%@���@��u@�A�@��@��@�\)@��@�ȴ@���@�M�@�=q@�5?@�-@��@���@�@��/@�Q�@��@�ƨ@��@�|�@��@�ȴ@��R@���@�n�@�E�@�$�@��@�`B@��@���@��j@��@��@�1@�\)@�33@���@��\@�n�@�5?@���@��@���@��/@���@��@�bN@�A�@��@��F@�l�@�o@���@��\@�v�@�V@��@�@���@��h@�G�@�%@��@�ƨ@��y@��R@���@�ff@��@���@�/@���@��D@�j@�9X@�@\)@~ȴ@~�+@}�@|��@|�j@|1@{dZ@z�\@yX@x��@xb@w�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�5?A�9XA�?}A�A�A�C�A�G�A�A�A�A�A�C�A�?}A�G�A�G�A�E�A�I�A�E�A�E�A�I�A�E�A�I�A�K�A�E�A�G�A�K�A�G�A�I�A�I�A�E�A�I�A�K�A�E�A�G�A�I�A�E�A�C�A�I�A�I�A�E�A�I�A�K�A�G�A�G�A�K�A�G�A�G�A�K�A�O�A�^5A�ffA�hsA�bNA�ffA�n�A�n�A�jA�l�A�jA�hsA�x�A�t�A�p�A�~�AՇ+A�~�AՉ7AՕ�A՛�A՗�A՝�Aա�Aա�Aե�Aթ�Aե�Aա�Aե�Aե�A՟�A՛�A՛�A՗�AՕ�Aՙ�Aՙ�AՏ\AՉ7AՋDAՉ7AՅAՉ7AՋDAՇ+AՅAՋDAՋDAՅAՉ7AՋDAՅAՅAՇ+AՁA�x�A�z�A�|�A�x�A�x�A�x�A�p�A�l�A�p�A�n�A�hsA�ffA�\)A�1'A��A�{A�JA�A���A��A��A��A��A��mA��mA��yA��`A��/A��/A���A�ȴA���A�ĜAԺ^AԼjAԼjAԴ9AԮAԬAԣ�Aԝ�Aԛ�Aԇ+A�r�A�S�A�K�A�=qA�-A�"�A���A��HA���AӉ7A�t�A�=qA�7LA��A�VA�A�1A�A��A��A��yA��mA��/A�ȴAҧ�A҉7A�dZA�5?A�oA��/Aѡ�A�\)A��A�ĜAЛ�AЉ7AЁA�l�A�dZA�l�A�jA�bNA�\)A�\)A�\)A�XA�XA�XA�M�A�Q�A�`BAЃA�|�A�jA�^5A�Q�A�M�A�M�A�S�A�M�A�K�A�M�A�E�A�%A���A�A�JA�5?A�M�A�`BA�^5A�bNA�jA�r�A�x�A�x�A�~�AЇ+AЉ7AЉ7AЋDAБhAЕ�AЗ�AН�AЩ�AЮAЬAЧ�AЩ�AЩ�AХ�AП�AС�AП�AГuAЁA�jA�^5A�ZA�XA�Q�A�Q�A�C�A�-A�JA�A���A��A��yA��`A��`A��mA��A϶FAϑhA�|�A�p�A�XA�A�A�-A�  A��/A�ȴAκ^Aδ9AΧ�A΋DA�n�A�ZA�O�A�A�A�33A�+A�1'A�33A�+A�JAͮA�bA���A̲-A̧�A̾wA��A���A�{A��A�bA���A��A��A���A̋DA�dZA�G�A��A���A��yA���A˸RA˅A�`BA�K�A�"�A���Aʩ�AʁA�Q�A��AɬA�A�t�A�S�A�C�A�A�A�7LA�1'A�5?A�1'A�-A�&�A�(�A�-A�+A�&�A�$�A�(�A�(�A�$�A�"�A�$�A�(�A�+A�+A�33A�A�A�S�A�l�Aȗ�A�AȶFA��AǴ9A�?}AƏ\A��A���AœuA�VA��A��A��mA�ƨAġ�AđhA�r�A�E�A�33A�$�A��A��AöFA×�A�x�A�n�A�dZA�`BA�XA�"�A�JA��A��yA��/A�A�r�A�`BA�G�A�-A�"�A��A�A���A���A��A�z�A�hsA�?}A�(�A���A��`A���A���A�ȴA���A���A��wA���A���A��wA��#A��A��`A��TA��#A���A���A��jA��!A���A���A���A��hA�z�A�n�A�ffA�XA�;dA�A��+A�l�A�M�A�9XA�/A��A�VA��HA��A�~�A�ffA�?}A�-A��A�bA�%A�  A���A��mA���A���A�z�A�S�A�(�A���A���A�A�A�A���A���A���A���A��A���A��uA�VA�Q�A�K�A�G�A�-A�A��#A��^A��uA�r�A�A�A�JA��A��A��RA���A��uA�r�A�S�A�7LA�-A�oA��`A��-A��PA��DA��+A��A�r�A�^5A���A��A���A���A��DA�|�A�p�A�l�A�jA�bNA�XA�K�A�=qA�(�A�VA��A��A�^5A���A���A��wA���A�t�A�A���A��uA�S�A�;dA�/A�&�A�"�A�"�A�"�A�$�A��A���A�A��DA�VA��A���A�x�A�Q�A�$�A���A���A�z�A�l�A�\)A�I�A�9XA�(�A��yA�=qA�hsA�{A��
A���A�z�A�n�A�XA�K�A�;dA�1'A�$�A�{A�%A���A���A���A��A��mA��mA��A�ȴA�ĜA���A��RA���A���A���A���A���A���A��\A��7A��7A��A�|�A�n�A�`BA�?}A�-A�$�A�
=A��;A�ĜA��uA�v�A�ffA�5?A��HA�t�A��A���A��
A��-A�hsA�VA���A���A�hsA���A�v�A�9XA�  A���A��yA���A�n�A�E�A�-A�
=A���A��!A���A��DA�|�A�t�A�n�A�`BA�A�A��A�%A���A���A��DA�l�A�=qA��A��A���A��-A��uA�S�A�"�A���A���A��+A�M�A�$�A��/A���A�K�A�{A�A��A��;A��A��A���A���A�ĜA���A��DA�VA�A�A�1'A��A�A��A���A��FA���A���A��hA�l�A�A�A�/A�&�A�$�A�
=A��;A���A���A��wA��^A��FA��-A��A���A��hA��\A��\A��\A��hA��\A��\A��PA��DA�x�A�`BA�$�A��A���A�p�A�C�A�&�A�A���A��jA��FA���A���A���A���A���A��uA��hA��\A��\A��PA�`BA���A�n�A�/A�oA���A��#A���A���A�z�A�l�A�E�A�{A��A���A���A��^A�^5A�VA��;A���A�O�A�JA�ĜA��A�S�A�-A�  A�ĜA�n�A��A���A�l�A��A��#A��RA��hA�dZA�C�A�1'A��A�VA�A���A��A���A��jA���A��\A�z�A�hsA�Q�A�7LA�"�A�JA��mA�ĜA���A�XA�A�A�oA��A��HA���A��^A���A���A��\A�v�A�C�A��yA�z�A�?}A�/A�(�A��A���A��A���A���A�z�A�Q�A�"�A��`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�;dA�C�A�A�A�C�A�G�A�G�A�I�A�G�A�I�A�G�A�I�A�G�A�I�A�K�A�ffA�l�A�z�AՑhAգ�Aա�A՗�AՉ7AՉ7AՃA�t�A�1'A��yA�A�z�AӰ!A���A��AЍPA�XA�^5A�1'A�z�AС�AБhA�33A�ĜA��A�;dA��Ả7A�+A���A�+A�/AǬA��A���A��yA��`A��A���A���A��9A�x�A��hA�A�VA��#A��A�C�A�K�A��
A�=qA�XA���A��A�33A��mA��!A��+A�1A��
A�-A�;dA�
=A�l�A��A��!A�hsA�{A��9A�JA�z�A���A���A��\A�bA��TA���A���A��uA�|�A�A���A�dZA���A�7LA�-A�ZA�VA�
=A�\)A�/A�7LA��FA��A��A��+A�G�A�S�A��A�~�A��RA~n�A{�^Az��Aw�
Au;dArQ�Ao�Al�jAj �Ai?}Ah��AgƨAcƨA_��A]dZA[AW|�ASS�AO�AM�^AK|�AI/AH1AG�AE��ADĜACA@�yA?oA=�#A<�jA;�A;G�A;%A9hsA6�9A49XA2=qA0��A/�A.��A.�A-C�A*r�A)7LA(��A'��A&�yA%�-A$~�A#�#A"�jA"=qA!��A!%A�TA�yAI�A�;A7LA��AM�A��AK�A�HA�\A��AK�A�mA�A��A
=AXAA�Av�A�A  A�hA&�A�;A+A�+A+A^5AI�A�A�A�+A�FA�RA��A��A��A�A�A�+Ax�@�^5@�hs@�I�@�I�@��j@��@���@��@��\@�V@�hs@��9@���@��T@�h@��#@��@�I�@�(�@���@�x�@�@�5?@�;d@�@�
=@��;@�Z@�@��;@�+@��@�"�@�G�@�Z@�{@��u@�@�\@���@��T@�z�@�1@߾w@ݺ^@�X@ە�@�C�@�dZ@�"�@�$�@ّh@ج@׶F@֧�@�G�@�(�@�b@�  @ӍP@��@�5?@���@с@���@�1@���@�ff@͑h@�7L@̬@�bN@� �@��;@˶F@˶F@�ƨ@�1@���@ʏ\@���@�hs@�7L@�7L@�@ɉ7@�&�@Ȭ@��m@�\)@���@���@��@�"�@öF@�|�@���@�K�@�+@�@�@���@���@�x�@�/@���@�{@��@�9X@��
@��F@��F@�o@�v�@���@���@�x�@�V@��u@�z�@�I�@�  @��m@��@�\)@��H@�@���@��D@��@��@�|�@�\)@�K�@��@�=q@��7@��^@�=q@�J@��-@�x�@�X@�&�@���@���@�l�@�t�@�dZ@��y@��+@�$�@���@�X@�G�@�?}@�/@��9@�Z@��w@�K�@�;d@��@��y@��!@��+@�V@��@�`B@�/@�r�@�(�@��w@��P@�C�@�33@��@���@��R@�ff@�@��T@���@�@�O�@��/@��@��@�Q�@�  @���@��F@���@�t�@�\)@�33@���@�v�@�-@��-@�G�@���@�r�@�b@��@��@�v�@�=q@���@��#@���@�x�@�7L@��@���@��9@���@� �@��@���@�l�@�C�@�33@�33@�"�@�
=@��@��@�ȴ@��R@���@�v�@�M�@���@���@���@��/@�Q�@��w@�33@��@��H@��H@��H@��@���@��!@�E�@�J@��@���@��h@�%@���@��u@�A�@��@��@�\)@��@�ȴ@���@�M�@�=q@�5?@�-@��@���@�@��/@�Q�@��@�ƨ@��@�|�@��@�ȴ@��R@���@�n�@�E�@�$�@��@�`B@��@���@��j@��@��@�1@�\)@�33@���@��\@�n�@�5?@���@��@���@��/@���@��@�bN@�A�@��@��F@�l�@�o@���@��\@�v�@�V@��@�@���@��h@�G�@�%@��@�ƨ@��y@��R@���@�ff@��@���@�/@���@��D@�j@�9X@�@\)@~ȴ@~�+@}�@|��@|�j@|1@{dZ@z�\@yX@x��@xbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�5?A�9XA�?}A�A�A�C�A�G�A�A�A�A�A�C�A�?}A�G�A�G�A�E�A�I�A�E�A�E�A�I�A�E�A�I�A�K�A�E�A�G�A�K�A�G�A�I�A�I�A�E�A�I�A�K�A�E�A�G�A�I�A�E�A�C�A�I�A�I�A�E�A�I�A�K�A�G�A�G�A�K�A�G�A�G�A�K�A�O�A�^5A�ffA�hsA�bNA�ffA�n�A�n�A�jA�l�A�jA�hsA�x�A�t�A�p�A�~�AՇ+A�~�AՉ7AՕ�A՛�A՗�A՝�Aա�Aա�Aե�Aթ�Aե�Aա�Aե�Aե�A՟�A՛�A՛�A՗�AՕ�Aՙ�Aՙ�AՏ\AՉ7AՋDAՉ7AՅAՉ7AՋDAՇ+AՅAՋDAՋDAՅAՉ7AՋDAՅAՅAՇ+AՁA�x�A�z�A�|�A�x�A�x�A�x�A�p�A�l�A�p�A�n�A�hsA�ffA�\)A�1'A��A�{A�JA�A���A��A��A��A��A��mA��mA��yA��`A��/A��/A���A�ȴA���A�ĜAԺ^AԼjAԼjAԴ9AԮAԬAԣ�Aԝ�Aԛ�Aԇ+A�r�A�S�A�K�A�=qA�-A�"�A���A��HA���AӉ7A�t�A�=qA�7LA��A�VA�A�1A�A��A��A��yA��mA��/A�ȴAҧ�A҉7A�dZA�5?A�oA��/Aѡ�A�\)A��A�ĜAЛ�AЉ7AЁA�l�A�dZA�l�A�jA�bNA�\)A�\)A�\)A�XA�XA�XA�M�A�Q�A�`BAЃA�|�A�jA�^5A�Q�A�M�A�M�A�S�A�M�A�K�A�M�A�E�A�%A���A�A�JA�5?A�M�A�`BA�^5A�bNA�jA�r�A�x�A�x�A�~�AЇ+AЉ7AЉ7AЋDAБhAЕ�AЗ�AН�AЩ�AЮAЬAЧ�AЩ�AЩ�AХ�AП�AС�AП�AГuAЁA�jA�^5A�ZA�XA�Q�A�Q�A�C�A�-A�JA�A���A��A��yA��`A��`A��mA��A϶FAϑhA�|�A�p�A�XA�A�A�-A�  A��/A�ȴAκ^Aδ9AΧ�A΋DA�n�A�ZA�O�A�A�A�33A�+A�1'A�33A�+A�JAͮA�bA���A̲-A̧�A̾wA��A���A�{A��A�bA���A��A��A���A̋DA�dZA�G�A��A���A��yA���A˸RA˅A�`BA�K�A�"�A���Aʩ�AʁA�Q�A��AɬA�A�t�A�S�A�C�A�A�A�7LA�1'A�5?A�1'A�-A�&�A�(�A�-A�+A�&�A�$�A�(�A�(�A�$�A�"�A�$�A�(�A�+A�+A�33A�A�A�S�A�l�Aȗ�A�AȶFA��AǴ9A�?}AƏ\A��A���AœuA�VA��A��A��mA�ƨAġ�AđhA�r�A�E�A�33A�$�A��A��AöFA×�A�x�A�n�A�dZA�`BA�XA�"�A�JA��A��yA��/A�A�r�A�`BA�G�A�-A�"�A��A�A���A���A��A�z�A�hsA�?}A�(�A���A��`A���A���A�ȴA���A���A��wA���A���A��wA��#A��A��`A��TA��#A���A���A��jA��!A���A���A���A��hA�z�A�n�A�ffA�XA�;dA�A��+A�l�A�M�A�9XA�/A��A�VA��HA��A�~�A�ffA�?}A�-A��A�bA�%A�  A���A��mA���A���A�z�A�S�A�(�A���A���A�A�A�A���A���A���A���A��A���A��uA�VA�Q�A�K�A�G�A�-A�A��#A��^A��uA�r�A�A�A�JA��A��A��RA���A��uA�r�A�S�A�7LA�-A�oA��`A��-A��PA��DA��+A��A�r�A�^5A���A��A���A���A��DA�|�A�p�A�l�A�jA�bNA�XA�K�A�=qA�(�A�VA��A��A�^5A���A���A��wA���A�t�A�A���A��uA�S�A�;dA�/A�&�A�"�A�"�A�"�A�$�A��A���A�A��DA�VA��A���A�x�A�Q�A�$�A���A���A�z�A�l�A�\)A�I�A�9XA�(�A��yA�=qA�hsA�{A��
A���A�z�A�n�A�XA�K�A�;dA�1'A�$�A�{A�%A���A���A���A��A��mA��mA��A�ȴA�ĜA���A��RA���A���A���A���A���A���A��\A��7A��7A��A�|�A�n�A�`BA�?}A�-A�$�A�
=A��;A�ĜA��uA�v�A�ffA�5?A��HA�t�A��A���A��
A��-A�hsA�VA���A���A�hsA���A�v�A�9XA�  A���A��yA���A�n�A�E�A�-A�
=A���A��!A���A��DA�|�A�t�A�n�A�`BA�A�A��A�%A���A���A��DA�l�A�=qA��A��A���A��-A��uA�S�A�"�A���A���A��+A�M�A�$�A��/A���A�K�A�{A�A��A��;A��A��A���A���A�ĜA���A��DA�VA�A�A�1'A��A�A��A���A��FA���A���A��hA�l�A�A�A�/A�&�A�$�A�
=A��;A���A���A��wA��^A��FA��-A��A���A��hA��\A��\A��\A��hA��\A��\A��PA��DA�x�A�`BA�$�A��A���A�p�A�C�A�&�A�A���A��jA��FA���A���A���A���A���A��uA��hA��\A��\A��PA�`BA���A�n�A�/A�oA���A��#A���A���A�z�A�l�A�E�A�{A��A���A���A��^A�^5A�VA��;A���A�O�A�JA�ĜA��A�S�A�-A�  A�ĜA�n�A��A���A�l�A��A��#A��RA��hA�dZA�C�A�1'A��A�VA�A���A��A���A��jA���A��\A�z�A�hsA�Q�A�7LA�"�A�JA��mA�ĜA���A�XA�A�A�oA��A��HA���A��^A���A���A��\A�v�A�C�A��yA�z�A�?}A�/A�(�A��A���A��A���A���A�z�A�Q�A�"�A��`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
��B
��B
��B
ʌB
��B
��B
ʌB
��B
��B
�^B
�#B
��B
ʌB
�#B
��B
��B
��B
��B
�B
�2B
�2B
�8B
�>B
��BB5�B]/B`�B_pBW
BE9B8B�B�B+BA�B�MB�4B��B�B�B��B�XB�-B��B��B�pB�dB�BAB��B�/BB�B�BHKBd�Be�Bj�Bm�BhsBw�BzBtBp�Bc Bc B^jBp�By	B|�BqBn/Bl�Bj�BjBe,BT�BF�B;0B2�B0!B(�B!-BB�B_BB�"B�	B�B�B�ZB�dB�yB�3B�9B�B�%Bm�Bc�BZQBM�BB'B-�B$@B�B�B
��B
��B
�]B
�NB
�$B
�bB
��B
p�B
l�B
h�B
T,B
8�B
/�B
!B
	7B	�DB	�QB	�WB	��B	B	�qB	�B	�hB	��B	�+B	�B	s�B	iB	V�B	L0B	C�B	8�B	33B	/OB	)�B	(�B	 �B	B	B	�B	�B	1B	�B	B		�B	�B	�B	�B	B	�B	�B	�B	�B	�B	SB	�B	�B	+B	�B	xB	~B	$tB	%FB	&�B	/�B	9XB	<�B	?B	G�B	E�B	PHB	S�B	V�B	K^B	JXB	L�B	PHB	V9B	Q�B	JXB	F�B	@�B	7LB	5?B	<�B	`B	s�B	tTB	sB	rGB	c�B	T�B	G�B	EB	F�B	JXB	C�B	@�B	?�B	5�B	<jB	E9B	:*B	>�B	C�B	Q�B	M6B	C�B	>BB	B�B	G�B	VmB	?}B	<�B	D�B	gmB	kQB	e,B	�+B	�~B	~�B	{�B	�B	��B	}�B	~�B	�B	�GB	rGB	h>B	l�B	r�B	f�B	d�B	��B	�+B	�"B	��B	�rB	{JB	}�B	{�B	��B	��B	��B	��B	��B	�OB	��B	� B	� B	B	�OB	B	�HB	��B	��B	��B	�qB	��B	��B	�B	��B	��B	��B	��B	�B	B	�aB	B	��B	� B	��B	�jB	�jB	�dB	��B	�0B	��B	�B	�B	�qB	�HB	��B	�aB	��B	�'B	�gB	�9B	ƨB	ȀB	ѷB	�B	��B	��B	�^B	˒B	�dB	ѷB	�^B	��B	�B	уB	�TB	�yB	�B	�B	רB	�B	��B	�KB	�yB	چB	�2B	҉B	ӏB	�TB	ѷB	�[B	�aB	�gB	�[B	ҽB	��B	�&B	�[B	�&B	�[B	�&B	�&B	�&B	�&B	�[B	՛B	՛B	�,B	�mB	��B	ߤB	�BB	�BB	��B	�NB	�B	�B	�B	��B	�ZB	�ZB	��B	��B	��B	�ZB	�2B	��B	�VB
GB
GB
�B
B
�B
�B
B
�B
	�B

	B
�B
�B
�B
�B
JB
~B
~B
�B
�B
�B
"B
�B
�B
�B
�B
 B
 B
4B
�B
B
�B
uB
�B
uB
�B
{B
B
�B
B
MB
�B
B
SB
�B
�B
�B
�B
YB
+B
+B
�B
_B
eB
�B
�B
7B
B
~B
�B
B
�B
!B
�B
 \B
 \B
 �B
!-B
!-B
"�B
#nB
#:B
#�B
$B
$B
$B
$@B
$B
$tB
$tB
$tB
$�B
$�B
$�B
$�B
&B
%zB
$�B
'�B
&�B
(XB
)�B
*0B
*�B
*�B
*�B
*�B
*�B
*�B
,B
+�B
+�B
,�B
-�B
.B
.IB
/�B
0!B
0!B
0�B
1�B
1�B
2-B
2aB
2�B
2aB
2aB
2aB
1�B
1�B
1�B
3�B
3hB
3hB
4B
3�B
4�B
5tB
5tB
5�B
5�B
6B
6B
6B
6zB
7LB
7B
6�B
7B
7B
6�B
7�B
8�B
8�B
9�B
9�B
8�B
9XB
:*B
;0B
:�B
:�B
;dB
;�B
;�B
;�B
<B
=�B
>�B
?�B
@�B
@�B
@�B
@�B
A B
A B
@�B
@B
@OB
@�B
A�B
B�B
DgB
C�B
C�B
DgB
F?B
GzB
G�B
HB
H�B
H�B
H�B
I�B
I�B
I�B
IB
J#B
I�B
J#B
J�B
K^B
K�B
MjB
MjB
MjB
NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�zB
��B
��B
ȴB
��B
�)B
ɺB
�^B
�)B
�XB
��B
��B
��B
��B
��B
��B
�^B
��B
��B
�#B
ɆB
��B
ʌB
�#B
�0B
��B
��B
�dB
��B
ɆB
�)B
��B
�RB
�)B
��B
�#B
��B
˒B
��B
ɆB
˒B
ʌB
ɆB
�)B
�)B
��B
��B
�BB
� B
�B
�[B
�NB
��B
��B
��B
�aB
ѷB
ӏB
�2B
� B
՛B
�mB
ںB
ܒB
�mB
�;B
��B
�WB
�B
�QB
�GB
��B
�ZB
�+B
��B
��B
�`B
��B
�lB
�`B
�8B
�fB
��B
�+B
�8B
�lB
��B
�2B
��B
�lB
��B
�rB
�DB
��B
�lB
��B
��B
�	B
��B
��B
��B
��B
�(B
�PB
��B
�]B iBBB�B{BB�B
�B+B>BBH�BK�BOvBR�BT,BU2B[WB[�B[�B_�B_�B^�B_�BbNB_pBa�B`vB^�BaBb�B`�B`Ba�Ba�B^�B`vB`�B^�BbB_�B\)B[#B^�B\)BYB^5BV�B[�BXBPBZ�BL�BMBG�BHBE9BE�BEBD3BB�B@BB�BB�B<�B<jB:*B8�B2aB4�B1'B6B,�B-B#:B�B�B1B�B�BqB�B�B!BCB!-B!bB �B 'B!�BOB-CB.B2-B-wB+6B*�B*0B(�B+kB,=B*0B)�B6FB+6B0�B4BB�BZ�Bd�Bi�Bm)Br�By�B��B�fB�fB�DB��B�.B��B��B�oB��B��B��B�kB�CB�}B�CB�=B��B�wB��B�kB�qB��B�CB�CB��B�tB�FB�B��B�$B��B��B��B�$B��B�*B��B��B��B��B��B��B�'B�nB��B�!B��B�*B�B�B�4B�zB��B�FB��B��B��B�hB��B�B��B��B�B�tB�B�_B��B��B��B�$B�UBȀB��B�<B�B˒B��B�XBȴB�B�jB��B�B��B�zB��B��B�$B��B�'B��B��B�B�[BخB�KB�|B�jB�KB��B�zB�#B��B�#B�XB�^B�jB��B�^B˒B�6B�pB�B�B�vB�B��BΥBуB��B�mB��B�WB�EB�B��B�B"4B�BMB!bB��B�B��B��B�ZB��B��B�mB��B��BޞB�B��B��B��B��B�MB�B�>B��B�B�rB�xB�B��B�B�VB  B	�B�B
�B�B�BB~B�B�B�B�B�BBuBbB�B�B�BB�B�BxB,B-wB6�B4BH�BS[BY�B\�Ba�Bg�BdZBc Bf2Be,Be,Bd�BbBd�Bd�BbNBd&Bd�Bh>Bl"Bc�Be,Bd�Bc�Bc�Bc�Bk�Bn/Bk�BlWBk�BiyBk�Bk�Bl"Bk�BjBjKBl�Bp�Bm�Bl"Bm�Bn�BrBrGBffBb�BcTBd�Bc�BffBh>Bm�Bq�Bq�Bt�Bt�BzB|PB|�B|PBz�B}�B~]B}"By�BxB{�Bw2Bv�Bw�Bv�Bu�Br�Bu�Bt�Bw�Bv`BpBoiBoiBr|Bo�B��B�MBm]Be�Bf2Bd�Be,Bd&BbNBc Bd&Bb�BbBaBcTBc�Bg�BiDBhsBa|B\)B\�B_�Bi�B`BBbNBbNB_pB^B\�B[�B\�B\)BZ�B\]Bb�BkBp;Bu%B� B}�B|�Bx8By	B�;B|�BzDBt�Bv`Bv�BtTBr�B�B��BuZBuZBxlBzDBw�Bo BrGBp�BrBrBncBo BpBoiBm�Bl�Bm�Bo�Bl�BncBn�Bm]Bl�Bl�Bm)Bm)Bl�Bi�Bk�Bj�Bm�Bk�Bi�Bi�Bi�Bj�Bl�BlWBh>Bg�BkBiDBh>Bk�Be�BcTBe�Bi�BsMB`�BXyBXBTaB]�BWsBQBP�BLdB]/BHBK�B=<B<�B=BDgB>�B<jB9�B:�B:�B8�B7�B3�B2aB1[B1'B2aB5?B4�B.}B5B1'B+6B0�B,=B,B*�B)*B&�B'B+kB!B%B$�B �B(�B�B \B�B�B�B B�B�B�B�BxBBJBJBPB�B	B	lB�B%B�B�BBB  BB�B �B��B��B��B�B�B�B��B��B��B��B�lB�xB�B�2B��B��B��B��B�B�B�|B��B��B��B�MB��B�oB��B�B�
B�DB�B��B�jB�HBޞB�B�5BܒB��B�)B�WBٴB��B�B�B��B�)B�HB�BȴB��B��B�'B��B�UB�UB��B��B�[B�3B��B�nB��B�qB��B��B�~B�$B�@B�VB�~B��B��B�DB�B�YB}�Bx�Bo�Bq�Bo�Bm]Bi�Bi�BgBffBd�Bc�BdZBaBa�B_pB]�B[�B[#BZ�BW�BW�BWsBW�BS�BTaBOBL�BHBF?BF�BC�BB�B@OB?}B@�BDgBB�BB�B5�B-�B,qB-�B.IB'�B,qB,�B&�B&LB'�B&�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                           444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                           444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022071609040320220716090403IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071610012320220716100123QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071610012320220716100123QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                