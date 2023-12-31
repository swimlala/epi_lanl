CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  <   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-07-27T07:09:49Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  V�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  ]`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  w@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  }�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  �x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 x ,`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 2�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   S   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   Y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T _   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   _l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   _t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   _|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   _�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � _�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   `   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   `(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    `0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        `P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        `X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ``   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    `hArgo profile    3.1 1.2 19500101000000  20220727070949  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_222                 6810_008521_222                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��L�J�@��L�J�11  @��LPH�@��LPH�@1��ʂ@1��ʂ�d�,�s-��d�,�s-�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@B�\@��\@�G�@�p�@޸RA   AG�A ��A,(�A@��A`  A\)A�  A�Q�A�  A�\)A�\)A߮A�Q�B   B�B�B  B (�B(  B/�B8  B@(�BH  BP  BW�
B`(�Bh  Bp  Bx(�B�  B�{B�{B��B��B�  B�  B�  B�  B�  B�  B��B��B�  B�  B�  B��
B��
B�  B�(�B�{B�  B�{B��B�  B�{B��B��B��B��B�{B�{C 
=C  C��C  C  C	��C  C
=C  C
=C��C�C��C  C��C�C��C"  C$
=C&  C'��C)��C+��C.
=C0
=C2
=C4
=C6  C7�C9��C<
=C>{C@
=CB
=CD  CF  CH
=CJ
=CK��CN  CP
=CQ��CS�CV
=CW��CY�HC[�C]��C_�Cb  Cd
=Cf
=Ch
=Cj
=Cl  Cn  Cp  Cr
=Ct  Cv  Cw��Cz
=C|  C}��C��C�C�  C�  C�  C���C�  C�  C�C�C�C�C���C���C���C���C�  C�  C�  C�C�  C���C���C�C�  C�  C�
=C�
=C�C���C���C���C�  C���C�C�C�  C�C�C�C�  C�C�  C�  C�
=C�
=C�
=C�  C���C���C�C�C�C�  C�  C�  C�
=C�C�C�C�C�
=C�C�
=C�
=C�C�C�C���C�  C�C�  C���C���C���C�  C�  C�C�C�C�
=C�
=C�C�C�  C�  C�  C�  C�  C�  C���C���C���C���C���C�  C���C�  C�
=C�  C�  C�  C���C���C���C���C�  C�C�  C�  C�C�
=C�C�  C�  C�  C�C�
=C�
=C�C�  C�  C���C�C�C�C���C���D   D ��D  D��D  D��D  D� D�D� D  D� D�qD}qD  D}qD  D� D	  D	� D
  D
� D
�qD}qD  D��DD�D�D� D�qD� D  D��D  D� D�D�D�D}qD�qD� D  D��D�D}qD�qD}qD�qD� D�Dz�D�qD��D  D� D  D� D�D��D�qD}qD�qD}qD�qD ��D!�D!}qD!��D"}qD#�D#��D#�qD$z�D%  D%}qD&  D&� D&�qD'}qD(�D(�D)D)�D*  D*z�D*��D+� D,�D,� D,�qD-}qD.�D.� D/  D/}qD/��D0� D1  D1� D1��D2}qD3  D3��D4�D4��D5�D5�D6�D6��D7D7� D8�D8�D9�D9}qD9�qD:}qD;  D;�D<�D<� D=  D=� D=�qD>z�D>�qD?��D@D@��DA  DA}qDB�DB��DC  DC}qDC��DD}qDE  DE� DF  DF� DG  DG��DH  DH}qDI  DI��DJ�DJ��DK  DK� DL  DL� DM  DM� DM�qDN}qDO  DO��DP  DP}qDP�qDQ� DQ�qDR}qDS  DS��DTDT�DUDU�DV  DV��DWDW��DX�DX��DY�DY� DZ  DZ}qD[  D[}qD\�D\��D]  D]�D^D^��D_  D_z�D_�qD`}qDa  Da��Db  Db��Dc  Dc}qDd  Dd}qDe  De�DfDf� Df�qDg}qDh  Dh� DiDi��Di��Dj}qDk  Dk��Dl�Dl� Dl�qDm��DnDn� Do  Do� Dp  Dp� Dq  Dq� Dq�qDr� Dr�qDs}qDt  Dt� Dt�qDu� Du�qDv� Dw  Dw}qDx  Dx��Dy  Dy}qDy�qDz��D{  D{� D|�D|��D}  D}� D}�qD~}qD  D��D��D�@ D�� D��qD���D�AHD�� D��qD���D�@ D�� D��HD�  D�>�D�}qD���D�HD�@ D�� D���D���D�>�D�~�D�� D�HD�@ D�� D�� D�HD�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�AHD��HD��qD���D�>�D�� D�� D�  D�>�D�~�D��HD��D�4{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>�?k�?�\)?�Q�?�@�@(�@5@J=q@Y��@s33@��@�\)@��H@��@�33@�(�@�=q@�
=@�  @�{@�Q�AG�A
=A{A�A�A�RA#33A(��A/\)A3�
A;�AA�AFffAL(�AR�\AVffA\(�Aa�AeAj�HAqG�AuAy��A\)A�=qA�(�A�ffA���A��
A�{A�Q�A��A�A��A��\A�A�  A��A���A��A���A�z�A��A���A�(�A��A��A��
A�
=A�G�A��
A�
=A�G�A˅A�
=A�G�AӅA�
=Aٙ�A��
A�
=AᙚA�(�A�
=A�=qA���A�RA�=qA�p�A�\)A��\A�p�A��BG�B�HB  BG�B
=BQ�B	p�B33B��BB33B��BffB�B��B�RB(�Bp�B�RBQ�BB�HB z�B"{B#\)B$z�B&=qB'�
B(��B*=qB,  B-�B.=qB0  B0��B2{B3�B4��B5��B7
=B8  B8��B:ffB;\)B<  B=�B>ffB?33B?�B@��BA��BBffBC\)BDz�BD��BE�BF�HBG�BHQ�BIp�BJ{BJ�RBK�
BL��BMp�BN{BO
=BP  BP��BQp�BRffBS�BT(�BT��BU�BV�HBW\)BXQ�BYG�BZ=qBZ�RB[�B\��B]p�B]�B_
=B`  B`��Bap�Bb�RBc33Bd  Bd��Bf{Bf�\Bg33BhQ�BiG�BiBj�HBk�
Blz�Bm�Bm�Bo
=Bo�
Bp��Bq�Br=qBs\)Bs�
Btz�BuG�BvffBv�HBw�Bx��Byp�By�B{
=B{�B|Q�B}G�B~=qB~�\B\)B�=qB�z�B���B�33B�B�(�B�ffB��HB�\)B��B�  B�ffB���B�\)B���B��B�z�B���B��B��B�{B�ffB��RB��B��B��
B�=qB���B��B�p�B�B�=qB��RB��HB�G�B�B�(�B�ffB��RB�33B���B�B�Q�B��RB���B�33B�B�(�B�Q�B���B�33B��B�B�{B��\B���B��B��B�  B�ffB���B��HB�\)B��
B�(�B�ffB��HB�\)B��
B�(�B�ffB��HB�G�B��B��B�ffB��HB�33B�p�B��B�ffB���B�
=B�p�B�  B�ffB���B���B��B�  B�=qB�z�B��B��B��
B�{B��\B��B�\)B���B�  B��\B���B�33B��B�  B�z�B��HB��B�p�B��B�z�B���B��B�p�B��
B�ffB��RB�
=B�p�B�  B�z�B���B��B���B�{B�ffB��RB�\)B�B�{B�ffB�
=B�p�B�B�(�B��\B��B��B��B�=qB���B��B��B�  B�=qB���B�G�B��
B�{B�ffB���B�p�B�B�(�B��\B�
=B���B��B�=qB���B��B�B�  B�z�B�
=B���B�{B�ffB��HBÅB�  B�Q�BĸRB�33B��
B�Q�B���B��BǅB�(�BȸRB��BɅB��Bʣ�B�
=B�p�B��
B�Q�B��HB�\)BͮB�{BΣ�B�G�BϮB�{B�ffB���BхB�  B�ffBҸRB��Bә�B�(�BԸRB��BՅB��B�z�B�
=BׅB��B�=qB��HB�p�B��B�Q�BڸRB�G�B��
B�ffBܸRB�33B��
B�ffB���B�33B߮B�=qB���B�G�BᙚB�(�B���B�G�B�B�{B�RB�G�B�B�{B�z�B�
=B癚B�{B�z�B���B�33B��
B�Q�B�RB�
=B�B�  B�\B�
=B�B��
B�=qB�RB�G�B�B�  B�z�B���B�B�{B�z�B���B�33B�B�=qB��RB�
=B�p�B��B�z�B��HB�G�B���B��B�ffB��HB�\)B�B�{B�ffB���B�G�B�B�=qB��RB�
=B�p�B��
B�=qB���B�33B�p�B��
C 33C z�C �C �
C
=CG�C�\CC�C(�C\)C�\C��C{CG�Cp�C��C��C
=CG�Cz�C�RC�C{CG�Cp�C��C�HC{CQ�Cp�C��CC��C(�CffC��C�
C��C{CQ�C�CC��C	(�C	G�C	ffC	��C	��C

=C
33C
\)C
z�C
��C
C
=C=qCffC�C�C�
C
=CG�C�C�C��C  C33Cp�C�C�
C  C33C\)C�CC��C=qC\)C�C�C�
C{C\)C�C�C�
C  C=qCz�C�RC�HC  C(�CffC��C�
C  C(�CG�C�\C��C��C�CG�Cp�C�C�C�CG�Cp�C��C�
C  C=qCp�C��C�HC(�C\)C�\CC��C(�C\)C�\C��C
=C=qC�C�RC  CG�C�C�RC�C{C\)C��C�
C{C\)C��C�
C
=CG�Cp�C�C�C33Cz�C��C{C=qC�C�RC   C G�C �\C �HC!�C!G�C!�C!C"{C"\)C"��C"�HC#{C#Q�C#�\C#�
C$(�C$ffC$�\C$��C%{C%p�C%�C%�HC&�C&Q�C&��C&��C'G�C'�C'C(  C(G�C(�C(�HC)33C)z�C)C*  C*G�C*�\C*�C+(�C+\)C+��C,  C,Q�C,�C,C-{C-ffC-�RC-��C.=qC.z�C.C/�C/p�C/�RC/��C0=qC0�C0��C1(�C1z�C1C2  C2G�C2�\C2�HC3=qC3�C3�
C4�C4ffC4��C4�C5=qC5�C5�
C6(�C6z�C6C7
=C7G�C7�C7��C8�C8z�C8C9{C9Q�C9�\C9�
C:�C:z�C:��C;{C;\)C;��C;�HC<(�C<p�C<�RC<��C=G�C=�\C=�C>33C>z�C>C>��C?33C?z�C?C@  C@Q�C@��C@�HCA�CAffCA��CA�HCB�CB\)CB��CB�
CC{CCQ�CC��CC�
CD�CDffCD��CD��CE33CEz�CE�RCE�CF33CFp�CF�CF�CG33CGp�CG�CG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                             111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�=q@�\@B�\@��\@�G�@�p�@޸RA   AG�A ��A,(�A@��A`  A\)A�  A�Q�A�  A�\)A�\)A߮A�Q�B   B�B�B  B (�B(  B/�B8  B@(�BH  BP  BW�
B`(�Bh  Bp  Bx(�B�  B�{B�{B��B��B�  B�  B�  B�  B�  B�  B��B��B�  B�  B�  B��
B��
B�  B�(�B�{B�  B�{B��B�  B�{B��B��B��B��B�{B�{C 
=C  C��C  C  C	��C  C
=C  C
=C��C�C��C  C��C�C��C"  C$
=C&  C'��C)��C+��C.
=C0
=C2
=C4
=C6  C7�C9��C<
=C>{C@
=CB
=CD  CF  CH
=CJ
=CK��CN  CP
=CQ��CS�CV
=CW��CY�HC[�C]��C_�Cb  Cd
=Cf
=Ch
=Cj
=Cl  Cn  Cp  Cr
=Ct  Cv  Cw��Cz
=C|  C}��C��C�C�  C�  C�  C���C�  C�  C�C�C�C�C���C���C���C���C�  C�  C�  C�C�  C���C���C�C�  C�  C�
=C�
=C�C���C���C���C�  C���C�C�C�  C�C�C�C�  C�C�  C�  C�
=C�
=C�
=C�  C���C���C�C�C�C�  C�  C�  C�
=C�C�C�C�C�
=C�C�
=C�
=C�C�C�C���C�  C�C�  C���C���C���C�  C�  C�C�C�C�
=C�
=C�C�C�  C�  C�  C�  C�  C�  C���C���C���C���C���C�  C���C�  C�
=C�  C�  C�  C���C���C���C���C�  C�C�  C�  C�C�
=C�C�  C�  C�  C�C�
=C�
=C�C�  C�  C���C�C�C�C���C���D   D ��D  D��D  D��D  D� D�D� D  D� D�qD}qD  D}qD  D� D	  D	� D
  D
� D
�qD}qD  D��DD�D�D� D�qD� D  D��D  D� D�D�D�D}qD�qD� D  D��D�D}qD�qD}qD�qD� D�Dz�D�qD��D  D� D  D� D�D��D�qD}qD�qD}qD�qD ��D!�D!}qD!��D"}qD#�D#��D#�qD$z�D%  D%}qD&  D&� D&�qD'}qD(�D(�D)D)�D*  D*z�D*��D+� D,�D,� D,�qD-}qD.�D.� D/  D/}qD/��D0� D1  D1� D1��D2}qD3  D3��D4�D4��D5�D5�D6�D6��D7D7� D8�D8�D9�D9}qD9�qD:}qD;  D;�D<�D<� D=  D=� D=�qD>z�D>�qD?��D@D@��DA  DA}qDB�DB��DC  DC}qDC��DD}qDE  DE� DF  DF� DG  DG��DH  DH}qDI  DI��DJ�DJ��DK  DK� DL  DL� DM  DM� DM�qDN}qDO  DO��DP  DP}qDP�qDQ� DQ�qDR}qDS  DS��DTDT�DUDU�DV  DV��DWDW��DX�DX��DY�DY� DZ  DZ}qD[  D[}qD\�D\��D]  D]�D^D^��D_  D_z�D_�qD`}qDa  Da��Db  Db��Dc  Dc}qDd  Dd}qDe  De�DfDf� Df�qDg}qDh  Dh� DiDi��Di��Dj}qDk  Dk��Dl�Dl� Dl�qDm��DnDn� Do  Do� Dp  Dp� Dq  Dq� Dq�qDr� Dr�qDs}qDt  Dt� Dt�qDu� Du�qDv� Dw  Dw}qDx  Dx��Dy  Dy}qDy�qDz��D{  D{� D|�D|��D}  D}� D}�qD~}qD  D��D��D�@ D�� D��qD���D�AHD�� D��qD���D�@ D�� D��HD�  D�>�D�}qD���D�HD�@ D�� D���D���D�>�D�~�D�� D�HD�@ D�� D�� D�HD�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�AHD��HD��qD���D�>�D�� D�� D�  D�>�D�~�D��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>�?k�?�\)?�Q�?�@�@(�@5@J=q@Y��@s33@��@�\)@��H@��@�33@�(�@�=q@�
=@�  @�{@�Q�AG�A
=A{A�A�A�RA#33A(��A/\)A3�
A;�AA�AFffAL(�AR�\AVffA\(�Aa�AeAj�HAqG�AuAy��A\)A�=qA�(�A�ffA���A��
A�{A�Q�A��A�A��A��\A�A�  A��A���A��A���A�z�A��A���A�(�A��A��A��
A�
=A�G�A��
A�
=A�G�A˅A�
=A�G�AӅA�
=Aٙ�A��
A�
=AᙚA�(�A�
=A�=qA���A�RA�=qA�p�A�\)A��\A�p�A��BG�B�HB  BG�B
=BQ�B	p�B33B��BB33B��BffB�B��B�RB(�Bp�B�RBQ�BB�HB z�B"{B#\)B$z�B&=qB'�
B(��B*=qB,  B-�B.=qB0  B0��B2{B3�B4��B5��B7
=B8  B8��B:ffB;\)B<  B=�B>ffB?33B?�B@��BA��BBffBC\)BDz�BD��BE�BF�HBG�BHQ�BIp�BJ{BJ�RBK�
BL��BMp�BN{BO
=BP  BP��BQp�BRffBS�BT(�BT��BU�BV�HBW\)BXQ�BYG�BZ=qBZ�RB[�B\��B]p�B]�B_
=B`  B`��Bap�Bb�RBc33Bd  Bd��Bf{Bf�\Bg33BhQ�BiG�BiBj�HBk�
Blz�Bm�Bm�Bo
=Bo�
Bp��Bq�Br=qBs\)Bs�
Btz�BuG�BvffBv�HBw�Bx��Byp�By�B{
=B{�B|Q�B}G�B~=qB~�\B\)B�=qB�z�B���B�33B�B�(�B�ffB��HB�\)B��B�  B�ffB���B�\)B���B��B�z�B���B��B��B�{B�ffB��RB��B��B��
B�=qB���B��B�p�B�B�=qB��RB��HB�G�B�B�(�B�ffB��RB�33B���B�B�Q�B��RB���B�33B�B�(�B�Q�B���B�33B��B�B�{B��\B���B��B��B�  B�ffB���B��HB�\)B��
B�(�B�ffB��HB�\)B��
B�(�B�ffB��HB�G�B��B��B�ffB��HB�33B�p�B��B�ffB���B�
=B�p�B�  B�ffB���B���B��B�  B�=qB�z�B��B��B��
B�{B��\B��B�\)B���B�  B��\B���B�33B��B�  B�z�B��HB��B�p�B��B�z�B���B��B�p�B��
B�ffB��RB�
=B�p�B�  B�z�B���B��B���B�{B�ffB��RB�\)B�B�{B�ffB�
=B�p�B�B�(�B��\B��B��B��B�=qB���B��B��B�  B�=qB���B�G�B��
B�{B�ffB���B�p�B�B�(�B��\B�
=B���B��B�=qB���B��B�B�  B�z�B�
=B���B�{B�ffB��HBÅB�  B�Q�BĸRB�33B��
B�Q�B���B��BǅB�(�BȸRB��BɅB��Bʣ�B�
=B�p�B��
B�Q�B��HB�\)BͮB�{BΣ�B�G�BϮB�{B�ffB���BхB�  B�ffBҸRB��Bә�B�(�BԸRB��BՅB��B�z�B�
=BׅB��B�=qB��HB�p�B��B�Q�BڸRB�G�B��
B�ffBܸRB�33B��
B�ffB���B�33B߮B�=qB���B�G�BᙚB�(�B���B�G�B�B�{B�RB�G�B�B�{B�z�B�
=B癚B�{B�z�B���B�33B��
B�Q�B�RB�
=B�B�  B�\B�
=B�B��
B�=qB�RB�G�B�B�  B�z�B���B�B�{B�z�B���B�33B�B�=qB��RB�
=B�p�B��B�z�B��HB�G�B���B��B�ffB��HB�\)B�B�{B�ffB���B�G�B�B�=qB��RB�
=B�p�B��
B�=qB���B�33B�p�B��
C 33C z�C �C �
C
=CG�C�\CC�C(�C\)C�\C��C{CG�Cp�C��C��C
=CG�Cz�C�RC�C{CG�Cp�C��C�HC{CQ�Cp�C��CC��C(�CffC��C�
C��C{CQ�C�CC��C	(�C	G�C	ffC	��C	��C

=C
33C
\)C
z�C
��C
C
=C=qCffC�C�C�
C
=CG�C�C�C��C  C33Cp�C�C�
C  C33C\)C�CC��C=qC\)C�C�C�
C{C\)C�C�C�
C  C=qCz�C�RC�HC  C(�CffC��C�
C  C(�CG�C�\C��C��C�CG�Cp�C�C�C�CG�Cp�C��C�
C  C=qCp�C��C�HC(�C\)C�\CC��C(�C\)C�\C��C
=C=qC�C�RC  CG�C�C�RC�C{C\)C��C�
C{C\)C��C�
C
=CG�Cp�C�C�C33Cz�C��C{C=qC�C�RC   C G�C �\C �HC!�C!G�C!�C!C"{C"\)C"��C"�HC#{C#Q�C#�\C#�
C$(�C$ffC$�\C$��C%{C%p�C%�C%�HC&�C&Q�C&��C&��C'G�C'�C'C(  C(G�C(�C(�HC)33C)z�C)C*  C*G�C*�\C*�C+(�C+\)C+��C,  C,Q�C,�C,C-{C-ffC-�RC-��C.=qC.z�C.C/�C/p�C/�RC/��C0=qC0�C0��C1(�C1z�C1C2  C2G�C2�\C2�HC3=qC3�C3�
C4�C4ffC4��C4�C5=qC5�C5�
C6(�C6z�C6C7
=C7G�C7�C7��C8�C8z�C8C9{C9Q�C9�\C9�
C:�C:z�C:��C;{C;\)C;��C;�HC<(�C<p�C<�RC<��C=G�C=�\C=�C>33C>z�C>C>��C?33C?z�C?C@  C@Q�C@��C@�HCA�CAffCA��CA�HCB�CB\)CB��CB�
CC{CCQ�CC��CC�
CD�CDffCD��CD��CE33CEz�CE�RCE�CF33CFp�CF�CF�CG33CGp�CG�CG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                             111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�{A�oA�oA�oA��A�VA�oA��A��A��A��A�
=A���A�ZA�C�A�=qA�9XA�1'A�+A�(�A�&�A�$�A�VA��A�ĜAדuA�`BA�E�A�(�A��A֬A֝�A։7A�~�A�v�A�ffA�A�A��A�ĜA�n�A�VA��;A�I�AҶFAџ�A��A��A�AάA���A�x�A�$�A�=qA�ƨA��A�l�A��Aɩ�A���AǋDA� �A�"�A�dZA���A��A�\)A��-A�5?A��yA��`A�&�A�oA���A���A�A��A�/A��A�-A���A�/A���A�9XA��9A���A���A��
A�7LA��7A�C�A��DA�-A��^A���A�VA��A�Q�A���A��A��A��
A�t�A�jA��\A�C�A���A�bA�ZA�/A���A��mA��jA�ƨA�x�Az�RAvI�Ap�Al�uAe/A]oAZ9XAZJAXjATĜAS33AQ"�AN�AJ  AF �AB�A?�A?�A> �A<��A<VA:E�A7?}A5�wA5�A4 �A2�A0�9A.��A.JA+A(ZA'�hA'%A&9XA%A%t�A$��A!hsA��AffA�A=qA�A�\A%A��AZA�mA1A��A �A��A�hAhsA�AA"�A�Ax�A��A�A
E�A	�#A��AbNA�A/A��A��A��A��An�A^5AƨA �D@��@���@�dZ@��H@�G�@�K�@�=q@���@�33@���@陚@���@�@�w@��@��@�z�@�o@��@�?}@܃@�+@ڏ\@�~�@�v�@ڏ\@�ȴ@�~�@�{@��T@�E�@�t�@�1'@�1'@�  @��m@��@� �@�9X@ۥ�@ڧ�@���@�bN@���@�
=@և+@��@ղ-@�O�@�?}@�&�@��@ԣ�@�I�@��m@�ȴ@��@�~�@��@��/@�I�@��m@϶F@�n�@͙�@��@�  @ˮ@�1@��y@�hs@��@�O�@�O�@�`B@�X@�?}@�&�@��/@ǥ�@��
@�1@Ǿw@���@�E�@�p�@���@��`@���@ģ�@ċD@�Q�@��@�b@�v�@��^@�p�@�7L@�%@��@��9@�1'@��m@���@�o@��+@�^5@�J@��-@�hs@�Ĝ@�z�@��m@�dZ@��@��R@�~�@��@�O�@��@�A�@�9X@�(�@�1@���@��
@��@��@�S�@��@���@�5?@�X@��@���@��9@���@���@�;d@��!@�~�@�E�@�@��@��@���@�X@���@���@���@���@�9X@��@��H@�ff@��^@��@��@�Q�@��m@��@��R@��\@��+@�n�@�5?@��@�%@��/@�Ĝ@���@�j@�1'@�  @���@��@��;@���@�;d@�ȴ@�^5@�=q@�5?@�5?@�-@�$�@��#@�X@���@���@�r�@�bN@�I�@�|�@�C�@�  @�;d@��@���@�^5@�5?@��T@�p�@�x�@��@�O�@���@���@��D@�1@�K�@���@�$�@�@��@���@���@�hs@�/@��D@�I�@�1'@��
@�\)@�"�@���@�~�@��^@�x�@�G�@�7L@�%@��/@���@��@���@�%@��@�%@���@��9@�j@�1'@�b@��
@��@�t�@�;d@���@���@�v�@�$�@��T@�`B@���@��j@���@�z�@�Q�@��w@�dZ@�K�@�33@�o@��H@�n�@�=q@��@��@��-@��@��@�V@���@��u@�j@�bN@�1'@�  @��;@��w@��@�dZ@��R@�^5@�J@���@���@��7@�hs@�G�@��@��9@��@�j@�(�@�(�@�b@��F@�C�@��!@�M�@�5?@�J@���@���@��7@�X@�%@��9@���@�I�@�9X@�1@���@��F@��F@�C�@�
=@��@��\@�$�@��#@�@���@��h@�O�@�&�@��@��@�V@�%@�%@���@���@���@�I�@�  @��m@��m@��w@���@�l�@�;d@�+@�"�@�o@�
=@��@��R@�V@�J@���@���@��^@��@�%@��9@��u@��@�bN@�A�@�b@��@��@~�@~v�@~E�@}�T@}��@}V@|�j@|Z@{��@z�@y�@y�7@y&�@x�9@xr�@x  @w�@w+@v�R@v5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A�
=A��A��A�{A�{A�VA�bA��A�oA�JA�oA�{A�VA��A��A�oA�JA�VA�bA�oA��A��A��A��A��A��A��A��A�{A��A��A�{A��A�oA�
=A��A�VA��HA�
=A�
=A���A���AجA�S�A�^5A�S�A�XA�M�A�E�A�K�A�A�A�?}A�?}A�A�A�?}A�;dA�=qA�A�A�=qA�;dA�=qA�?}A�9XA�9XA�;dA�7LA�7LA�9XA�7LA�1'A�5?A�/A�-A�1'A�-A�(�A�/A�+A�(�A�-A�+A�&�A�+A�+A�&�A�&�A�+A�(�A�$�A�(�A�(�A�"�A�&�A�(�A�"�A�$�A�$�A��A��A�oA�VA�A�A�A���A��A��A��mA��;A���A���A�ȴA׸RAװ!Aװ!Aק�Aו�AבhA׉7A�z�A�n�A�n�A�jA�S�A�O�A�K�A�E�A�E�A�I�A�E�A�C�A�9XA�5?A�/A�1'A�&�A�"�A�"�A��A�oA�bA�JA���A��A��A���A�ȴA�AּjAְ!A֮Aְ!A֧�A֥�A֩�A֧�A֡�A֥�A֥�A֣�A֝�A֝�A֟�A֛�A֕�A֕�A֗�A֓uA֍PA֏\A֏\Aև+Aև+A։7A։7AփAփAև+AփA�~�AցAփA�|�A�|�A�~�A�|�A�x�A�x�A�z�A�v�A�t�A�x�A�v�A�p�A�t�A�r�A�r�A�l�A�jA�l�A�jA�dZA�`BA�bNA�bNA�\)A�XA�VA�O�A�E�A�?}A�=qA�;dA�5?A�1'A�+A�$�A�$�A�$�A��A��A� �A��A�{A���A��A��A��TA��`A��mA��A�Aղ-AծAէ�A՟�AՕ�A՗�AՕ�AՉ7AՁA�z�A�n�A�O�A�G�A�I�A�;dA�33A�33A��A�{A�{A�A���A��A��A���A���A��A��A��A��HA��#A��#A���A���A�ƨA���A�ƨAԴ9AԲ-AԲ-AԅA�hsA�C�A��A���A��;AӾwAӑhA�l�A�K�A���A���AҺ^Aҝ�A҃A�jA�=qA�{A��A���A���AѸRAљ�AѓuAёhA�~�A�z�A�v�A�n�A�`BA�O�A�9XA��A�
=A���A��A��A��A��A��yA��mA��TA��TA��TA��A���A���A���AмjAЋDA�&�A�
=A��HAϴ9Aϩ�Aϥ�Aϝ�AϑhA�^5A�7LA�VA���A��`Aδ9AΡ�AΑhA�x�A�ffA�S�A�/A�
=A��mA���AͼjA;wAͶFAͩ�Aͣ�Aͣ�A͙�A͏\A͑hA͋DÁA�r�A�hsA�bNA�\)A�K�A�E�A�;dA�33A�33A�/A�&�A�$�A�VA��A�ƨA̲-Ả7A�G�A�E�A�%A���A���A���A���A��A��TA���A���A˲-A˝�A˗�A˅A�v�A��A�A��HA��#A���A���A�ȴA�ĜAʸRAʥ�AʅA�r�A�bNA�Q�A�C�A�9XA�-A�$�A��A��A�oA�oA�{A�{A�bA�
=A�
=A���A�Aɣ�A�v�A�jA�ZA�?}A�5?A�VA�  A��/A���Aȴ9Aȧ�Aȉ7A�Q�A��A���A���AǼjAǇ+A�;dA��TAƮA�~�A�K�A�/A�"�A�
=A���A��mA�ĜAũ�AŁA�^5A�33A�1A��A���Aĩ�Aģ�Ağ�AđhA�r�A�ZA�C�A�5?A�+A��A��A�oA�A���A��A��;A��#AþwA�p�A�E�A�/A�$�A� �A�{A���A��;Aº^A�AA�x�A�dZA�O�A�=qA�(�A�&�A�&�A�"�A�VA��/A���A��A�z�A�n�A�bNA�bNA�bNA�bNA�^5A�XA�Q�A�5?A��A���A��
A��9A��hA�M�A�C�A�(�A��TA��+A�n�A�^5A�E�A�33A�+A�"�A�oA�JA�A��HA�x�A�G�A�9XA�1'A�33A�33A�+A� �A��A��A��A��A��A��A�oA�bA�bA�oA�oA�bA�
=A�1A�A�  A�A���A��A��A��A��A��A��`A��;A��#A��/A���A�ȴA��9A��A��!A���A��hA�v�A�1'A�1A�A��^A��^A��FA���A�dZA�^5A�G�A���A��7A�+A���A���A��\A�O�A�"�A��A��mA��^A��\A�p�A�\)A�C�A�1'A�$�A��A��A�VA�
=A�A�A�A���A���A���A���A���A��A��HA��A���A��PA�\)A�/A��A��A�VA�JA�%A�A���A��`A��wA��A��uA��+A��A��A��DA��DA��7A���A��PA��A�t�A�hsA�ZA�9XA��mA��A���A�&�A��FA�`BA�-A���A��RA�v�A�33A���A��!A�|�A�M�A�E�A�&�A�{A�JA�A�  A���A���A��TA��
A��jA��PA�`BA�K�A�?}A�"�A�  A���A��uA�Q�A�5?A�-A��A���A�ffA��#A�C�A���A��!A���A��uA�v�A�S�A�&�A��HA��A�?}A�A���A�t�A�^5A�K�A�C�A��A���A��`A���A��FA��!A���A�~�A�jA�XA�E�A�1'A���A�-A�
=A��A���A��jA�(�A���A��A�XA�E�A�A�A�A�A�A�A�9XA�9XA�9XA�7LA�+A�A���A�dZA�M�A�;dA��jA��HA�|�A�VA�-A�%A��A��HA���A���A��FA��+A�-A���A���A��A��A�\)A�;dA�-A��A��A��HA���A���A��\A�dZA�7LA��A���A��A���A��wA���A��A�jA�\)A�I�A�33A��A���A��mA�ȴA��PA�M�A�33A��A��TA�C�A��mA�ĜA��DA�l�A�?}A���A��A��uA�|�A� �A�ĜA��PA��A�r�A�`BA�E�A�7LA�(�A�{A���A���A�v�A��A��A��+A�ZA�-A��A��A�`BA� �A���A��#A���A���A�v�A�I�A�"�A���A���A���A�r�A�+A���A���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                             111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�{A�oA�oA�oA��A�VA�oA��A��A��A��A�
=A���A�ZA�C�A�=qA�9XA�1'A�+A�(�A�&�A�$�A�VA��A�ĜAדuA�`BA�E�A�(�A��A֬A֝�A։7A�~�A�v�A�ffA�A�A��A�ĜA�n�A�VA��;A�I�AҶFAџ�A��A��A�AάA���A�x�A�$�A�=qA�ƨA��A�l�A��Aɩ�A���AǋDA� �A�"�A�dZA���A��A�\)A��-A�5?A��yA��`A�&�A�oA���A���A�A��A�/A��A�-A���A�/A���A�9XA��9A���A���A��
A�7LA��7A�C�A��DA�-A��^A���A�VA��A�Q�A���A��A��A��
A�t�A�jA��\A�C�A���A�bA�ZA�/A���A��mA��jA�ƨA�x�Az�RAvI�Ap�Al�uAe/A]oAZ9XAZJAXjATĜAS33AQ"�AN�AJ  AF �AB�A?�A?�A> �A<��A<VA:E�A7?}A5�wA5�A4 �A2�A0�9A.��A.JA+A(ZA'�hA'%A&9XA%A%t�A$��A!hsA��AffA�A=qA�A�\A%A��AZA�mA1A��A �A��A�hAhsA�AA"�A�Ax�A��A�A
E�A	�#A��AbNA�A/A��A��A��A��An�A^5AƨA �D@��@���@�dZ@��H@�G�@�K�@�=q@���@�33@���@陚@���@�@�w@��@��@�z�@�o@��@�?}@܃@�+@ڏ\@�~�@�v�@ڏ\@�ȴ@�~�@�{@��T@�E�@�t�@�1'@�1'@�  @��m@��@� �@�9X@ۥ�@ڧ�@���@�bN@���@�
=@և+@��@ղ-@�O�@�?}@�&�@��@ԣ�@�I�@��m@�ȴ@��@�~�@��@��/@�I�@��m@϶F@�n�@͙�@��@�  @ˮ@�1@��y@�hs@��@�O�@�O�@�`B@�X@�?}@�&�@��/@ǥ�@��
@�1@Ǿw@���@�E�@�p�@���@��`@���@ģ�@ċD@�Q�@��@�b@�v�@��^@�p�@�7L@�%@��@��9@�1'@��m@���@�o@��+@�^5@�J@��-@�hs@�Ĝ@�z�@��m@�dZ@��@��R@�~�@��@�O�@��@�A�@�9X@�(�@�1@���@��
@��@��@�S�@��@���@�5?@�X@��@���@��9@���@���@�;d@��!@�~�@�E�@�@��@��@���@�X@���@���@���@���@�9X@��@��H@�ff@��^@��@��@�Q�@��m@��@��R@��\@��+@�n�@�5?@��@�%@��/@�Ĝ@���@�j@�1'@�  @���@��@��;@���@�;d@�ȴ@�^5@�=q@�5?@�5?@�-@�$�@��#@�X@���@���@�r�@�bN@�I�@�|�@�C�@�  @�;d@��@���@�^5@�5?@��T@�p�@�x�@��@�O�@���@���@��D@�1@�K�@���@�$�@�@��@���@���@�hs@�/@��D@�I�@�1'@��
@�\)@�"�@���@�~�@��^@�x�@�G�@�7L@�%@��/@���@��@���@�%@��@�%@���@��9@�j@�1'@�b@��
@��@�t�@�;d@���@���@�v�@�$�@��T@�`B@���@��j@���@�z�@�Q�@��w@�dZ@�K�@�33@�o@��H@�n�@�=q@��@��@��-@��@��@�V@���@��u@�j@�bN@�1'@�  @��;@��w@��@�dZ@��R@�^5@�J@���@���@��7@�hs@�G�@��@��9@��@�j@�(�@�(�@�b@��F@�C�@��!@�M�@�5?@�J@���@���@��7@�X@�%@��9@���@�I�@�9X@�1@���@��F@��F@�C�@�
=@��@��\@�$�@��#@�@���@��h@�O�@�&�@��@��@�V@�%@�%@���@���@���@�I�@�  @��m@��m@��w@���@�l�@�;d@�+@�"�@�o@�
=@��@��R@�V@�J@���@���@��^@��@�%@��9@��u@��@�bN@�A�@�b@��@��@~�@~v�@~E�@}�T@}��@}V@|�j@|Z@{��@z�@y�@y�7@y&�@x�9@xr�@x  @w�@w+@v�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A�
=A��A��A�{A�{A�VA�bA��A�oA�JA�oA�{A�VA��A��A�oA�JA�VA�bA�oA��A��A��A��A��A��A��A��A�{A��A��A�{A��A�oA�
=A��A�VA��HA�
=A�
=A���A���AجA�S�A�^5A�S�A�XA�M�A�E�A�K�A�A�A�?}A�?}A�A�A�?}A�;dA�=qA�A�A�=qA�;dA�=qA�?}A�9XA�9XA�;dA�7LA�7LA�9XA�7LA�1'A�5?A�/A�-A�1'A�-A�(�A�/A�+A�(�A�-A�+A�&�A�+A�+A�&�A�&�A�+A�(�A�$�A�(�A�(�A�"�A�&�A�(�A�"�A�$�A�$�A��A��A�oA�VA�A�A�A���A��A��A��mA��;A���A���A�ȴA׸RAװ!Aװ!Aק�Aו�AבhA׉7A�z�A�n�A�n�A�jA�S�A�O�A�K�A�E�A�E�A�I�A�E�A�C�A�9XA�5?A�/A�1'A�&�A�"�A�"�A��A�oA�bA�JA���A��A��A���A�ȴA�AּjAְ!A֮Aְ!A֧�A֥�A֩�A֧�A֡�A֥�A֥�A֣�A֝�A֝�A֟�A֛�A֕�A֕�A֗�A֓uA֍PA֏\A֏\Aև+Aև+A։7A։7AփAփAև+AփA�~�AցAփA�|�A�|�A�~�A�|�A�x�A�x�A�z�A�v�A�t�A�x�A�v�A�p�A�t�A�r�A�r�A�l�A�jA�l�A�jA�dZA�`BA�bNA�bNA�\)A�XA�VA�O�A�E�A�?}A�=qA�;dA�5?A�1'A�+A�$�A�$�A�$�A��A��A� �A��A�{A���A��A��A��TA��`A��mA��A�Aղ-AծAէ�A՟�AՕ�A՗�AՕ�AՉ7AՁA�z�A�n�A�O�A�G�A�I�A�;dA�33A�33A��A�{A�{A�A���A��A��A���A���A��A��A��A��HA��#A��#A���A���A�ƨA���A�ƨAԴ9AԲ-AԲ-AԅA�hsA�C�A��A���A��;AӾwAӑhA�l�A�K�A���A���AҺ^Aҝ�A҃A�jA�=qA�{A��A���A���AѸRAљ�AѓuAёhA�~�A�z�A�v�A�n�A�`BA�O�A�9XA��A�
=A���A��A��A��A��A��yA��mA��TA��TA��TA��A���A���A���AмjAЋDA�&�A�
=A��HAϴ9Aϩ�Aϥ�Aϝ�AϑhA�^5A�7LA�VA���A��`Aδ9AΡ�AΑhA�x�A�ffA�S�A�/A�
=A��mA���AͼjA;wAͶFAͩ�Aͣ�Aͣ�A͙�A͏\A͑hA͋DÁA�r�A�hsA�bNA�\)A�K�A�E�A�;dA�33A�33A�/A�&�A�$�A�VA��A�ƨA̲-Ả7A�G�A�E�A�%A���A���A���A���A��A��TA���A���A˲-A˝�A˗�A˅A�v�A��A�A��HA��#A���A���A�ȴA�ĜAʸRAʥ�AʅA�r�A�bNA�Q�A�C�A�9XA�-A�$�A��A��A�oA�oA�{A�{A�bA�
=A�
=A���A�Aɣ�A�v�A�jA�ZA�?}A�5?A�VA�  A��/A���Aȴ9Aȧ�Aȉ7A�Q�A��A���A���AǼjAǇ+A�;dA��TAƮA�~�A�K�A�/A�"�A�
=A���A��mA�ĜAũ�AŁA�^5A�33A�1A��A���Aĩ�Aģ�Ağ�AđhA�r�A�ZA�C�A�5?A�+A��A��A�oA�A���A��A��;A��#AþwA�p�A�E�A�/A�$�A� �A�{A���A��;Aº^A�AA�x�A�dZA�O�A�=qA�(�A�&�A�&�A�"�A�VA��/A���A��A�z�A�n�A�bNA�bNA�bNA�bNA�^5A�XA�Q�A�5?A��A���A��
A��9A��hA�M�A�C�A�(�A��TA��+A�n�A�^5A�E�A�33A�+A�"�A�oA�JA�A��HA�x�A�G�A�9XA�1'A�33A�33A�+A� �A��A��A��A��A��A��A�oA�bA�bA�oA�oA�bA�
=A�1A�A�  A�A���A��A��A��A��A��A��`A��;A��#A��/A���A�ȴA��9A��A��!A���A��hA�v�A�1'A�1A�A��^A��^A��FA���A�dZA�^5A�G�A���A��7A�+A���A���A��\A�O�A�"�A��A��mA��^A��\A�p�A�\)A�C�A�1'A�$�A��A��A�VA�
=A�A�A�A���A���A���A���A���A��A��HA��A���A��PA�\)A�/A��A��A�VA�JA�%A�A���A��`A��wA��A��uA��+A��A��A��DA��DA��7A���A��PA��A�t�A�hsA�ZA�9XA��mA��A���A�&�A��FA�`BA�-A���A��RA�v�A�33A���A��!A�|�A�M�A�E�A�&�A�{A�JA�A�  A���A���A��TA��
A��jA��PA�`BA�K�A�?}A�"�A�  A���A��uA�Q�A�5?A�-A��A���A�ffA��#A�C�A���A��!A���A��uA�v�A�S�A�&�A��HA��A�?}A�A���A�t�A�^5A�K�A�C�A��A���A��`A���A��FA��!A���A�~�A�jA�XA�E�A�1'A���A�-A�
=A��A���A��jA�(�A���A��A�XA�E�A�A�A�A�A�A�A�9XA�9XA�9XA�7LA�+A�A���A�dZA�M�A�;dA��jA��HA�|�A�VA�-A�%A��A��HA���A���A��FA��+A�-A���A���A��A��A�\)A�;dA�-A��A��A��HA���A���A��\A�dZA�7LA��A���A��A���A��wA���A��A�jA�\)A�I�A�33A��A���A��mA�ȴA��PA�M�A�33A��A��TA�C�A��mA�ĜA��DA�l�A�?}A���A��A��uA�|�A� �A�ĜA��PA��A�r�A�`BA�E�A�7LA�(�A�{A���A���A�v�A��A��A��+A�ZA�-A��A��A�`BA� �A���A��#A���A���A�v�A�I�A�"�A���A���A���A�r�A�+A���A���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                             111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BR�BS&BR�BR�BR�BS&BR BR�BR�BR�BQ�BQ�BRTBK)BGEBGEBGEBF�BGBGEBGzBHBJ�BK�BN�BP�BS[BS�BT�BYB[WB[WB\)B[�B[�B[�BZ�BZ�B\�B^�B]�BZ�BYKBiDB�4B��B��B��B��B��BѷB՛B�B��B��B��B�2B��BB@B�B�B~B(B�BSB'B0�BD�BVB[WB\�B]�B]�Bm)Bs�Bk�Bg8Bg�Bm�B�iBj�BT,B[�BV�BK�B5B 'B�BB�B�B�B��B�ZB��B�#B��B��B�mB�9B�B��Be�BFB.�BB�B
�NB
��B
�oB
d�B
6B
 �B
GB	�BB	�zB	�_B	�B	m�B	R B	P}B	J�B	>�B	-wB	'RB	qB	�B	�B�B�&B�jB�vB��B�2B��B��B�jB��B�BƨB��B�aB�OB�-B��B��B�B��B�-B�'B��B��B��B�zB�B��B��B��B�OB��B�xB�B��B�4B��B��B��B�tB�tB��B��B�zB�:B�nB�FB��B��B��B�OB��B�IB��B��B��B�B�eB�$B�eB��B�uB��B��B�B��B�7B��B��B��B��B��B��B��B��B��B��B��B��B��B�eB��B��B�XB��B��B�}B�aB��B��B�3B��B��B�B�B��B�B��B��B�`B��B��B�.B��B��B	oB	oB	�B	�B	�B	�B	�B	hB	�B	�B	_B	$�B	/�B	9�B	B�B	I�B	K�B	L�B	K�B	PB	Q�B	S�B	T�B	W�B	^5B	a�B	`B	d�B	hsB	jB	k�B	l�B	m]B	m]B	o5B	o�B	p�B	r�B	t�B	uZB	v�B	y>B	y�B	zDB	z�B	{�B	|PB	}�B	��B	��B	��B	��B	�\B	�FB	��B	�B	�SB	�IB	��B	�'B	�nB	��B	�B	�zB	�B	�B	��B	��B	�eB	��B	��B	��B	��B	��B	��B	�$B	��B	��B	�B	��B	�B	�B	��B	��B	� B	��B	�-B	��B	��B	�BB	�vB	�BB	�vB	ѷB	҉B	��B	��B	��B	�2B	�2B	�gB	՛B	�B	��B	�EB	�EB	�yB	�B	�QB	�B	چB	یB	چB	�]B	�B	�B	��B	�|B	�|B	�HB	�B	�NB	�`B	��B	�B	�B	�B	�QB	�B	��B	� B	�5B	�B	��B	��B	��B	�	B	�rB	��B	��B	��B	��B	�B	��B	�B	�"B	��B	��B	�cB
 4B
�B
_B
	7B
	�B

	B
JB
VB
VB
"B
�B
�B
:B
�B
 B
.B
�B
"B
�B
�B
VB
�B
bB
4B
@B
�B
�B
{B
SB
YB
�B
�B
1B
�B
�B
	B
qB
=B
CB
�B
IB
�B
�B
 �B
"�B
#nB
$@B
$�B
%B
%FB
%zB
%�B
&B
&�B
&�B
'RB
'�B
($B
(�B
)*B
*�B
+B
+6B
+6B
+6B
+6B
-B
-B
,�B
,�B
-B
-B
.�B
/B
/�B
/�B
/�B
0!B
0UB
0UB
0�B
1[B
1�B
1�B
2-B
2�B
2�B
2�B
2�B
33B
4�B
4�B
4�B
4�B
4�B
4�B
5B
5tB
6B
6�B
7LB
7B
6�B
6�B
7�B
7�B
7�B
7LB
7LB
7LB
7�B
9$B
9XB
9XB
9�B
9�B
9�B
:*B
;�B
<B
<6B
;�B
<6B
>B
=�B
=<B
<�B
<�B
<jB
=B
=<B
=qB
=qB
>B
>BB
>BB
>�B
?HB
?}B
?}B
?HB
@B
@OB
?�B
?�B
?�B
@OB
@�B
@�B
AUB
A�B
A�B
A�B
A�B
A�B
B[B
B'B
B�B
C-B
CaB
DgB
D�B
D�B
FB
E�B
E�B
E�B
FB
F?B
F�B
F�B
F�B
G�B
G�B
G�B
HB
HB
IB
IB
I�B
I�B
JXB
K�B
K�B
K�B
K�B
K�B
M6B
MjB
M�B
NpB
OG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@V�BT�BP}BTaBS�BR�BS�BS�BO�BS[BTaBQ�BRTBT,BS�BP�BT�BT,BQNBQBS�BRTBQ�BS�BR�BQ�BTaBR�BQ�BS&BRTBQ�BS�BN�BS�BP�BK�BK)Ba|BS�BK�BQ�BdZBXBL0BB�BC�BGBJ#BFBEmBGEBG�BHBF�BFtBHBG�BF?BGBH�BF�BFBHKBG�BFtBGzBG�BFtBF�BH�BF?BGEBGEBE�BGBHKBE�BGBHBF?BGBHKBF?BF�BHBHBF�BF�BH�BGBFtBIBGEBFtBH�BIBH�BJ�BJ�BJ#BI�BK�BJ�BJXBL�BM6BK)BK^BMjBN�BMBM6BP}BP}BN<BPBQNBPBP�BR�BS[BRTBQ�BS�BT�BR�BTaBT,BR�BS�BT�BS&BT,BUgBS�BT�BU�BT�BV�BW�BV9BUgBW�BYKBXEB\�B[�B[�BY�B\)B[�BZQB\)B\)BZQBZ�B\]BZ�BZB[#B\�B[�BZB[WB\�B[�BZQB[�B\�B[�BZ�B\�B\]BZ�B[WB\�B\]B[#B[WB]/B\)B[#B]/B\�BZ�B[�B\�B\�BZ�B\)B\�B[#B[WB]�B[WBZ�B[#B\�B]dB[�B[�B[WB\�B[�BZ�B[�B\]B[�BZ�B[�B[�BY�BY�B[�BZQBZQB[#BY�BX�BZ�BZ�BXyBZB\�B_�BZ�BZ�B\]BZ�BY�B]�Ba�B^jB\]B\)B]/B^�B\)B]�B_;B_pB^5B^�Bb�B^5B\�B_;B^�B]/BbNB\)B]�Ba�B]dB\�B\]BZQBZQB\�B]dB\�B\�B[�B[WBYBZQBZBWsBXBYBWsBU2B\�BW�B[�BYBW�BZB[�B\�B[�B`vBh>Bc Bc Be�Bj�Bn�BxB{JB~�B�;B~�B�B.B�B� B��B�4B�B��B�GB�B�lB��B��B�bB��B��B�uB�@B��B�MB�FB�FB�B��B��B��B�MB��B��B�6B�LB�B�[B�}B�B�}B��B��B�XB��B��B�B�?B��BĜB��B�B�XB�jB��B͟BуB�BB̘B�BбB��B�B��B��B�B�pB��BӏB�[B�NB��B�2B�aB�gB՛B�[B��B՛B��B�?B�?B�BیB��BޞBޞB�B�B� B�pB�B��B�ZB�5B�B�fB�B�KB�>B��B��B��B��B�iB�oB�GB�GB�;B�AB��B�`B��B�+B��B�+B�2B�B�fB��B�8B�lB��B��B��B�2B�B��B��B �B��B{B�"B��B��B��BAB  BB�B �B;BSB(B
	B�B~B�B�B�B �B1B7B�B�B�BB�BB�B�B�BhB:B�BxB\BVBDB�BxB�B"BVBDBDBBJB�B�B�BbB�B B�B�BoB�BbB�B@B�B�B	B	B�B{B�BB�B�B�B�BBkB&B/OB.IB-wB/�B0UB/OB-�B,�B,�B-CB.}B2�B0�B5tB6�B9�B>wBB'B:�B?BF�BD3BIRBLdBS&BRTBP�BP�BR�BR�BQ�BXEB`BB^jB\�BZQBYBYKBYKB[�B\)B\�B]/B[#B[WB\�B^jB]�B]�B\]B]/B\]B\�B\�B^5B^B\�B^5B^�B\�B\�B^5B\�B]dB_�B\�B]/B\�B\�B]�B^5B[�BdZB\�BcTBjBiBs�BjBi�Bj�Bm�B}�Bo�BpBx�BtB{�Bw�Bm�Br�BsMBqBj�BqvBp;Bo Bm)Bk�BlWBkQBj�Bk�Bj�Bj�BiBiDBg8BffBh
Bg�Bh
BgmBd�Be,Bh�Ba|B_;Bl"BjKBg�Be`Bf2Bh>Bf�Bg�Bg8Bh�Bk�Bo Bj�Bk�Bk�BlWBn�Bo BrGBo Bs�Bx8BxlByrBz�B|�B�B�"B��B�kB�7Bs�Bn�Bg8Be�BbBc�B_�BXyBV�BW?BO�BQBX�BU2BR�BTaBT�BU2B\�B_pB\�B`B^�B\)BW�BV�BY�BXyB[#B\�BYBP�BM6BM6BOBBd&BRTBXyBC�B;dB7B7B8�B8RB6FB8RB6FB0!B-CB)�B#B#�B�B~B"�B�B=BVB�B�B_B�B�BuB4B:B&�B{B BhB�BB&�B:B@B�B�BB	7B�B�B�B�B�B�BPB�B�B��B�(B�B@B�B�B�B��B�lB��B��B�B��B�>B��B�B��B�;B�B��B�8B��B�`B�fB�B�TB��B�5B��B�QBԕB�B��BбB�B�,B�}B�0BʌB͟B�^B��B˒B�0B��BʌB�mB�B��B�^B�/B�aB�BB�wB�FB��B�B��B��B�*B�?B��B��B�CB�B��B��B��B�{B�B�uB�FB�@B�B�{Bl�BtTB|Bm�Bl�Bf�B]dBYBPHBO�BK^BM6BFtB@�B@�B=qB;�B;0B:^B0�B,44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                             444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                             444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022072707094920220727070949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022080605024220220806050242QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022080605024220220806050242QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                