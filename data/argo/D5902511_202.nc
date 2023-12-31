CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  &   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-02-25T06:06:24Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  V8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  \�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  u�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  |    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 L &   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 ,P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` E�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   E�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   K�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   Q�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T W�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   X4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   X<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   XD   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   XL   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � XT   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   X�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   X�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Y   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Y    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       Y(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    Y0Argo profile    3.1 1.2 19500101000000  20220225060624  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_202                 6810_008521_202                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ټK�o�/@ټK�o�/11  @ټK���@ټK���@0G�zcs@0G�zcs�d��v����d��v���11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@}p�@�p�@�p�@�p�@��RA  A ��A,��A@  A`  A�Q�A�  A�Q�A���A���AУ�A�Q�A�  B (�B  B�
B  B (�B'�
B/�B7�
B@  BH(�BP  BX  B`  Bh  Bp  Bw�
B�  B�{B�{B��B�  B�{B�  B�{B��B�  B�  B��B�  B��B�  B�  B��B�  B�{B�{B��B�  B�{B�(�B�{B�(�B�{B�{B�  B�  B�  B�  C   C  C{C  C��C	��C�C  C  C
=C��C��C
=C
=C  C  C 
=C"  C#��C%��C'��C*
=C+��C.
=C0
=C2  C4  C6  C8  C9�C;��C=��C@
=CB
=CD  CF  CH
=CJ{CK��CM�CP
=CR
=CT
=CV
=CX
=CZ
=C\  C^  C`
=Ca��Cd  Cf  Ch  Ci�Cl
=Cn
=Cp
=Cr
=Ct  Cv  Cw�Cz  C{��C~  C�
=C�
=C�C�  C�  C���C���C�C�  C���C���C���C�C���C���C�  C�  C���C�  C���C���C���C���C���C���C�  C�C�
=C���C���C�C�  C�  C�C�C�  C���C���C���C���C���C�  C�  C���C���C���C�
=C�C�C�C�C�C�
=C�  C�  C���C��C�  C�C�C�
=C�  C���C�  C�  C���C���C���C���C���C���C���C�  C�  C���C�C�C�  C���C���C���C�  C��C���C���C�  C�  C���C���C�  C�C�C�C�  C�C�C�C�C�C�C�  C�C�  C���C�  C�C�  C���C�C�C�  C�C�  C�C�C�C�  C���C�C�  C���C�  C�C�  C���C���C���C�  D �D ��D  D��D�qD}qD�qD� DD� D�qD}qD  D��D  D��D  D� D	  D	� D
�D
��D
�qDz�D�qD��DD��D  D}qD�qD��DD�DD��D�D��D  D}qD�qD� D  D��D  D}qD  D� D�qD}qD�D��D  D� D  D}qD  D� D�D}qD�qD�D  D� D   D � D!  D!��D"  D"}qD"�qD#z�D#�qD$�D%�D%z�D%�qD&� D'�D'��D(D(��D)  D)��D*�D*� D+�D+}qD+��D,}qD-  D-��D.  D.��D/  D/� D0  D0� D1�D1� D2  D2� D2�qD3� D4�D4� D4�qD5� D6  D6}qD7  D7� D8  D8� D9  D9� D:  D:�D;D;� D<  D<� D<�qD=}qD>  D>��D?  D?� D@D@��DA  DAz�DA��DB}qDC  DC}qDC��DD� DE�DEz�DE�qDF� DG  DG}qDG�qDH� DI  DI� DJ�DJ��DK  DK}qDL  DL� DM  DM� DN  DNz�DO  DO� DO�qDP� DP�qDQ� DRDR��DS�DS��DT�DT��DU�DU� DU�qDVz�DV�qDW}qDW�qDX� DY  DY��DZDZ��D[�D[�D\D\�D]  D]� D^�D^}qD_  D_� D_�qD`� Da�Da}qDb  Db� Dc�Dc��Dd  Dd� De  De��Df�Df� Df�qDg� Dh  Dh}qDi�Di��Di�qDj� Dk�Dk��Dl  Dl� Dm  Dm� Dn�Dn��Do�Do�Dp  Dp��DqDq� Dq�qDrz�Dr��Ds��Dt�Dt}qDt�qDu}qDu��Dv�Dw�Dw}qDw�qDx}qDy�Dy��Dy�qDz� Dz��D{� D|�D|��D}  D}}qD}�qD~��D�D� D�  D�@ D���D�D�HD�@ D�~�D���D��qD�@ D��HD�D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�HD�B�D���D�� D���D�@ D��HD�� D���D�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?B�\?W
=?�z�?���?���?�
=?�G�@   @��@\)@(�@!G�@+�@:�H@=p�@G�@W
=@Y��@c�
@u@�  @��
@�\)@���@�
=@�  @�G�@�=q@���@�@�(�@��
@Ǯ@У�@�
=@�p�@�@��@�z�@��RA33AffA(�A\)A�
A��A(�A   A%�A'�A-p�A1G�A4z�A:=qA>{AAG�AG
=AJ�HAN�RAS�
AW
=AZ�HA`��Ac33Ah��Al��Ap  AvffAz=qA|��A���A��HA�(�A�
=A���A�=qA���A��RA���A��A�p�A�\)A��A�33A�p�A�Q�A���A��
A�ffA��A��\A�(�A�{A���A��A�(�A�A�\)A��A��A��A�\)A���A\A�z�AƸRAǮA�=qA�(�A��A�\)Aљ�Aҏ\A���AָRA�\)A��AۅA���A�
=A߮A��A�33A�z�A�ffA�\)A�G�A��HA��
A�{A�
=A���A�33A��
A�A��A���A��HA��A�{A�\)B ��B�B{B�RB33BQ�B��Bp�BffB33B�B��B	G�B	B
�RB�B  B��B{B�\B�Bz�B��B�B
=B�B��Bp�B=qB�B(�B�B=qB�RB�
B�B��B�RB   B ��B!p�B"�RB#�B$Q�B%B&�RB'\)B(��B)��B*�\B,  B,��B-B/
=B0  B0��B2=qB3
=B4  B5p�B6=qB733B8z�B9�B:�\B;�B<Q�B=p�B>�HB?�B@��BB{BB�RBC�BE�BF{BF�HBHQ�BI�BJ{BK\)BLQ�BM�BN�\BO\)BPQ�BQBR�RBS�BT��BV{BV�HBW�
BY�BZ=qB[
=B\z�B]��B^ffB_�B`��Ba��Bb�RBd(�Bd��Bf{Bg\)Bh  Bip�BjffBk33Blz�Bm��Bn�RBo\)Bp��BqBr�RBt(�Bt��Bu�Bw\)BxQ�Bx��BzffB{\)B|(�B}��B~�\B\)B�Q�B��HB�33B��B�ffB���B�p�B�  B�Q�B�
=B��B��B���B�
=B�p�B�  B��RB�
=B��B�(�B��\B���B��B�(�B�z�B�
=B��B�  B�ffB��B���B��B��\B�33B�p�B�  B���B�
=B�p�B�{B���B���B���B�{B�ffB���B�p�B�  B�=qB���B�\)B��B�=qB���B�G�B���B�=qB���B�
=B���B�(�B�z�B�
=B��B��B�Q�B���B�G�B�B�Q�B���B�
=B���B�  B�Q�B���B�p�B�B�{B��RB�33B�p�B��B�z�B���B�G�B��
B�=qB�z�B��B���B��
B�Q�B��HB�G�B���B�=qB���B�
=B��B�  B�z�B��RB�G�B��
B�=qB��\B�
=B���B��
B�ffB���B�G�B���B�{B���B���B�G�B��B�ffB���B��B��B�(�B�z�B���B�\)B��
B�{B��\B��B���B��B�=qB��HB�G�B��B�  B��\B���B�33B�B�=qB\B��HB�\)B��B�=qBģ�B�33BŮB�  B�Q�B��HB�\)B�B�{Bȣ�B�33B�p�B��
B�ffB���B�G�B˙�B�(�B̸RB��B�p�B��
B�ffB��HB�
=Bϙ�B�{BЏ\B��HB�33B��
B�=qBҏ\B��Bә�B��B�Q�B���B�p�BծB�{B֏\B�33Bי�B��B�ffB���B�p�BٮB�=qB���B�\)BۮB�{B܏\B��BݮB�  B�Q�B��HB�p�B��
B�(�B�RB�G�BᙚB��B�z�B��B�p�B��
B�Q�B���B�\)B�B�=qB��HB�33B�B�{B��B��B�p�B��B�\B��B�p�B�{B�RB���B��B�=qB�\B��B�B�{B��B�G�B�B�{B�RB��B�B�(�B���B�
=B��B�=qB��RB�
=B���B�Q�B��RB�33B��
B�ffB���B��B�{B�z�B�
=B���B�=qB���B��B�C (�C ffC ��C �HC33Cz�C�RC�CG�C�\CC
=CffC�C�HC(�C�C�
C  C\)C�C��C(�Cz�C�
C(�CffC��C��CG�C�\C��C	
=C	\)C	�RC
  C
=qC
�C
�HC(�C\)C��C
=CQ�C�\C�
C(�C�C��C  CQ�C�RC  C=qC�C�HC=qCz�C�RC  C\)C�C�C=qC��C�
C{CffC��C
=CQ�C��C
=CQ�C�C�
C=qCz�C�RC
=Cp�C�C�C=qC��C�
C{CffC�RC��C33C�\C�C{C\)C�RC��C33C�\C��C  C\)C��C��C�Cz�C��C�HC=qCz�C��C��C G�C �C �C ��C!G�C!�\C!��C"
=C"Q�C"��C"�C#{C#Q�C#�\C#�HC$(�C$p�C$�C$�C%�C%p�C%C%��C&(�C&p�C&C'  C'(�C'z�C'C(
=C(=qC(p�C(�RC)  C)Q�C)�C)�RC)��C*=qC*�\C*�
C+
=C+=qC+z�C+C,  C,G�C,�\C,�
C-�C-\)C-�\C-C.�C.ffC.��C.�
C/
=C/Q�C/��C/�HC0(�C0ffC0��C0��C1
=C1\)C1��C1�HC2{C2G�C2�C2��C3{C3\)C3��C3��C4{C4\)C4�C4��C533C5p�C5��C5�HC6�C6ffC6�C7  C7G�C7�C7C8  C8G�C8��C8�C9(�C9ffC9��C9�C:(�C:p�C:�RC;
=C;Q�C;��C;�C<33C<z�C<C=  C=G�C=�\C=�HC>33C>z�C>C?
=C?\)C?�C@
=C@\)C@�C@��CAG�CA�\CA�HCB33CBz�CB��CC�CC\)CC�CC��CDG�CD��CD�HCE33CEz�CECF
=CF\)CF�CG  CGQ�CG�\CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @=p�@}p�@�p�@�p�@�p�@��RA  A ��A,��A@  A`  A�Q�A�  A�Q�A���A���AУ�A�Q�A�  B (�B  B�
B  B (�B'�
B/�B7�
B@  BH(�BP  BX  B`  Bh  Bp  Bw�
B�  B�{B�{B��B�  B�{B�  B�{B��B�  B�  B��B�  B��B�  B�  B��B�  B�{B�{B��B�  B�{B�(�B�{B�(�B�{B�{B�  B�  B�  B�  C   C  C{C  C��C	��C�C  C  C
=C��C��C
=C
=C  C  C 
=C"  C#��C%��C'��C*
=C+��C.
=C0
=C2  C4  C6  C8  C9�C;��C=��C@
=CB
=CD  CF  CH
=CJ{CK��CM�CP
=CR
=CT
=CV
=CX
=CZ
=C\  C^  C`
=Ca��Cd  Cf  Ch  Ci�Cl
=Cn
=Cp
=Cr
=Ct  Cv  Cw�Cz  C{��C~  C�
=C�
=C�C�  C�  C���C���C�C�  C���C���C���C�C���C���C�  C�  C���C�  C���C���C���C���C���C���C�  C�C�
=C���C���C�C�  C�  C�C�C�  C���C���C���C���C���C�  C�  C���C���C���C�
=C�C�C�C�C�C�
=C�  C�  C���C��C�  C�C�C�
=C�  C���C�  C�  C���C���C���C���C���C���C���C�  C�  C���C�C�C�  C���C���C���C�  C��C���C���C�  C�  C���C���C�  C�C�C�C�  C�C�C�C�C�C�C�  C�C�  C���C�  C�C�  C���C�C�C�  C�C�  C�C�C�C�  C���C�C�  C���C�  C�C�  C���C���C���C�  D �D ��D  D��D�qD}qD�qD� DD� D�qD}qD  D��D  D��D  D� D	  D	� D
�D
��D
�qDz�D�qD��DD��D  D}qD�qD��DD�DD��D�D��D  D}qD�qD� D  D��D  D}qD  D� D�qD}qD�D��D  D� D  D}qD  D� D�D}qD�qD�D  D� D   D � D!  D!��D"  D"}qD"�qD#z�D#�qD$�D%�D%z�D%�qD&� D'�D'��D(D(��D)  D)��D*�D*� D+�D+}qD+��D,}qD-  D-��D.  D.��D/  D/� D0  D0� D1�D1� D2  D2� D2�qD3� D4�D4� D4�qD5� D6  D6}qD7  D7� D8  D8� D9  D9� D:  D:�D;D;� D<  D<� D<�qD=}qD>  D>��D?  D?� D@D@��DA  DAz�DA��DB}qDC  DC}qDC��DD� DE�DEz�DE�qDF� DG  DG}qDG�qDH� DI  DI� DJ�DJ��DK  DK}qDL  DL� DM  DM� DN  DNz�DO  DO� DO�qDP� DP�qDQ� DRDR��DS�DS��DT�DT��DU�DU� DU�qDVz�DV�qDW}qDW�qDX� DY  DY��DZDZ��D[�D[�D\D\�D]  D]� D^�D^}qD_  D_� D_�qD`� Da�Da}qDb  Db� Dc�Dc��Dd  Dd� De  De��Df�Df� Df�qDg� Dh  Dh}qDi�Di��Di�qDj� Dk�Dk��Dl  Dl� Dm  Dm� Dn�Dn��Do�Do�Dp  Dp��DqDq� Dq�qDrz�Dr��Ds��Dt�Dt}qDt�qDu}qDu��Dv�Dw�Dw}qDw�qDx}qDy�Dy��Dy�qDz� Dz��D{� D|�D|��D}  D}}qD}�qD~��D�D� D�  D�@ D���D�D�HD�@ D�~�D���D��qD�@ D��HD�D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�HD�B�D���D�� D���D�@ D��HD�� D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?B�\?W
=?�z�?���?���?�
=?�G�@   @��@\)@(�@!G�@+�@:�H@=p�@G�@W
=@Y��@c�
@u@�  @��
@�\)@���@�
=@�  @�G�@�=q@���@�@�(�@��
@Ǯ@У�@�
=@�p�@�@��@�z�@��RA33AffA(�A\)A�
A��A(�A   A%�A'�A-p�A1G�A4z�A:=qA>{AAG�AG
=AJ�HAN�RAS�
AW
=AZ�HA`��Ac33Ah��Al��Ap  AvffAz=qA|��A���A��HA�(�A�
=A���A�=qA���A��RA���A��A�p�A�\)A��A�33A�p�A�Q�A���A��
A�ffA��A��\A�(�A�{A���A��A�(�A�A�\)A��A��A��A�\)A���A\A�z�AƸRAǮA�=qA�(�A��A�\)Aљ�Aҏ\A���AָRA�\)A��AۅA���A�
=A߮A��A�33A�z�A�ffA�\)A�G�A��HA��
A�{A�
=A���A�33A��
A�A��A���A��HA��A�{A�\)B ��B�B{B�RB33BQ�B��Bp�BffB33B�B��B	G�B	B
�RB�B  B��B{B�\B�Bz�B��B�B
=B�B��Bp�B=qB�B(�B�B=qB�RB�
B�B��B�RB   B ��B!p�B"�RB#�B$Q�B%B&�RB'\)B(��B)��B*�\B,  B,��B-B/
=B0  B0��B2=qB3
=B4  B5p�B6=qB733B8z�B9�B:�\B;�B<Q�B=p�B>�HB?�B@��BB{BB�RBC�BE�BF{BF�HBHQ�BI�BJ{BK\)BLQ�BM�BN�\BO\)BPQ�BQBR�RBS�BT��BV{BV�HBW�
BY�BZ=qB[
=B\z�B]��B^ffB_�B`��Ba��Bb�RBd(�Bd��Bf{Bg\)Bh  Bip�BjffBk33Blz�Bm��Bn�RBo\)Bp��BqBr�RBt(�Bt��Bu�Bw\)BxQ�Bx��BzffB{\)B|(�B}��B~�\B\)B�Q�B��HB�33B��B�ffB���B�p�B�  B�Q�B�
=B��B��B���B�
=B�p�B�  B��RB�
=B��B�(�B��\B���B��B�(�B�z�B�
=B��B�  B�ffB��B���B��B��\B�33B�p�B�  B���B�
=B�p�B�{B���B���B���B�{B�ffB���B�p�B�  B�=qB���B�\)B��B�=qB���B�G�B���B�=qB���B�
=B���B�(�B�z�B�
=B��B��B�Q�B���B�G�B�B�Q�B���B�
=B���B�  B�Q�B���B�p�B�B�{B��RB�33B�p�B��B�z�B���B�G�B��
B�=qB�z�B��B���B��
B�Q�B��HB�G�B���B�=qB���B�
=B��B�  B�z�B��RB�G�B��
B�=qB��\B�
=B���B��
B�ffB���B�G�B���B�{B���B���B�G�B��B�ffB���B��B��B�(�B�z�B���B�\)B��
B�{B��\B��B���B��B�=qB��HB�G�B��B�  B��\B���B�33B�B�=qB\B��HB�\)B��B�=qBģ�B�33BŮB�  B�Q�B��HB�\)B�B�{Bȣ�B�33B�p�B��
B�ffB���B�G�B˙�B�(�B̸RB��B�p�B��
B�ffB��HB�
=Bϙ�B�{BЏ\B��HB�33B��
B�=qBҏ\B��Bә�B��B�Q�B���B�p�BծB�{B֏\B�33Bי�B��B�ffB���B�p�BٮB�=qB���B�\)BۮB�{B܏\B��BݮB�  B�Q�B��HB�p�B��
B�(�B�RB�G�BᙚB��B�z�B��B�p�B��
B�Q�B���B�\)B�B�=qB��HB�33B�B�{B��B��B�p�B��B�\B��B�p�B�{B�RB���B��B�=qB�\B��B�B�{B��B�G�B�B�{B�RB��B�B�(�B���B�
=B��B�=qB��RB�
=B���B�Q�B��RB�33B��
B�ffB���B��B�{B�z�B�
=B���B�=qB���B��B�C (�C ffC ��C �HC33Cz�C�RC�CG�C�\CC
=CffC�C�HC(�C�C�
C  C\)C�C��C(�Cz�C�
C(�CffC��C��CG�C�\C��C	
=C	\)C	�RC
  C
=qC
�C
�HC(�C\)C��C
=CQ�C�\C�
C(�C�C��C  CQ�C�RC  C=qC�C�HC=qCz�C�RC  C\)C�C�C=qC��C�
C{CffC��C
=CQ�C��C
=CQ�C�C�
C=qCz�C�RC
=Cp�C�C�C=qC��C�
C{CffC�RC��C33C�\C�C{C\)C�RC��C33C�\C��C  C\)C��C��C�Cz�C��C�HC=qCz�C��C��C G�C �C �C ��C!G�C!�\C!��C"
=C"Q�C"��C"�C#{C#Q�C#�\C#�HC$(�C$p�C$�C$�C%�C%p�C%C%��C&(�C&p�C&C'  C'(�C'z�C'C(
=C(=qC(p�C(�RC)  C)Q�C)�C)�RC)��C*=qC*�\C*�
C+
=C+=qC+z�C+C,  C,G�C,�\C,�
C-�C-\)C-�\C-C.�C.ffC.��C.�
C/
=C/Q�C/��C/�HC0(�C0ffC0��C0��C1
=C1\)C1��C1�HC2{C2G�C2�C2��C3{C3\)C3��C3��C4{C4\)C4�C4��C533C5p�C5��C5�HC6�C6ffC6�C7  C7G�C7�C7C8  C8G�C8��C8�C9(�C9ffC9��C9�C:(�C:p�C:�RC;
=C;Q�C;��C;�C<33C<z�C<C=  C=G�C=�\C=�HC>33C>z�C>C?
=C?\)C?�C@
=C@\)C@�C@��CAG�CA�\CA�HCB33CBz�CB��CC�CC\)CC�CC��CDG�CD��CD�HCE33CEz�CECF
=CF\)CF�CG  CGQ�CG�\CG�H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�l�A�n�A�p�ÁA͇+A͇+AͅAͅAͅAͅA͉7A͋DA͍PA͋DA͍PA͋DA͉7A͉7A͉7A͉7A͇+A͇+A�\)A��A��;A�bA�7LA͋DAͶFA��A�9XAΧ�A��
A�ƨAθRAΰ!Aΰ!AΩ�AΥ�AΣ�AΣ�AήAΰ!AΥ�AΝ�A�S�A�AͬA�I�A̧�A�1'A˛�A�A�Q�A�/A�hsA��`Aȥ�Aȇ+A���A��A�K�A�~�A�VA�ƨA��A�&�A�%A�  A��A�`BA��^A���A��PA��A��^A��^A�x�A���A�x�A��A���A�p�A���A�?}A�\)A�  A��jA�XA�1'A��yA���A��DA���A�Q�A�?}A���A�+A�l�A�^5A�dZA���A��`A�jA��
A� �A�t�A���A�ZA�E�A�
=AoA}��A{��Awt�Ar �AlQ�Ae��AaƨA]��AY�AWp�AV�!AV~�AT^5AQ/AP��AO?}AMO�AJ=qAH�HAGoADr�A@��A=�
A=l�A<bNA;hsA9l�A5�7A2�uA1XA0��A.��A-�FA+�TA+\)A*  A(�A&�DA%%A#�A"�RA"bA!��A r�A�9A�-An�AA�wA�A�A��AhsAO�A��A=qA�
AA�A33A�A�A��AĜAK�A��AbAp�Al�A��A��A�wA�A�RA9XA��Av�A�A1'A-AƨAO�A�A��A��A
�A
1A	�A	�A	�TA	�;A	�#A	`BA�A5?A�A��At�AG�A;dA�yA�PA�\A�AdZA%A�A�HAȴA��A��A�+Av�A9XAƨA�7A ��@��;@�+@�v�@�J@�`B@�S�@��@���@��9@�bN@��w@�\)@���@���@�x�@���@�{@�V@�1'@@�v�@�@�X@�Ĝ@��@�9X@��@��;@�S�@�M�@�X@���@�w@�ƨ@�ƨ@�F@�P@�\)@�"�@��@�33@�33@柾@�@�b@�C�@��@�^5@�$�@�^@�Ĝ@�j@�ƨ@�v�@��T@ݩ�@ܣ�@۾w@��@��@��@�r�@�  @֟�@�{@պ^@�p�@��@ӍP@�n�@��#@�G�@�z�@�bN@�Z@�Q�@�Q�@�I�@�A�@�9X@�1'@� �@���@�33@�v�@Ͳ-@��@���@ʇ+@�E�@�J@���@Ǿw@�\)@��H@ư!@Ə\@�E�@ŉ7@�  @�K�@�@°!@�=q@��@���@��@§�@�^5@�E�@���@��/@�1'@���@�S�@�33@��!@�5?@�{@��T@��h@�p�@�G�@��u@���@��;@���@�dZ@��@���@�v�@���@�I�@��
@�t�@��@��@�-@�/@�Ĝ@��9@��@��u@�j@��;@�;d@�ȴ@���@��\@�x�@�7L@��@�Ĝ@�I�@�ƨ@��\@�$�@�J@�@��T@�G�@�j@�b@�ƨ@���@��@�E�@��@�&�@��@�V@���@��9@�j@�Z@���@�l�@���@�~�@��#@�`B@��@��9@�Q�@�b@��
@��@�\)@��H@�$�@�J@��@�p�@���@��u@��@�bN@��m@��F@�o@��+@�5?@��^@���@���@�x�@�hs@�`B@�7L@�Ĝ@�A�@��
@���@���@���@�j@���@��P@�o@��H@��!@��!@���@��+@�=q@��@��#@�hs@��/@��u@�b@��@�;d@���@��H@���@�ȴ@�V@�X@��@�?}@�&�@��/@��@���@��@�r�@�j@�b@��
@��F@�|�@�K�@�
=@���@��T@���@�X@�G�@�?}@�V@���@���@��u@�Q�@��@�|�@�33@���@���@�^5@�=q@�5?@��@���@�x�@���@���@��D@�b@���@�l�@�
=@�^5@�-@�J@��^@�hs@�O�@�&�@��@���@��@�z�@�Z@��@���@�|�@�S�@�K�@��@��@��@�l�@��+@�V@�V@�M�@�$�@�@��@��@�?}@�V@��@�r�@�1'@�  @�;@��@l�@\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�dZA�p�A�t�A�n�A�l�A�n�A�jA�n�A�l�A�p�A�r�A�l�A�p�A�x�A�z�A�n�A�p�A�l�A�n�A͉7AͅAͅA͉7AͅA͇+A͇+AͅA͉7A͇+AͅA͉7A͉7AͅA͉7AͅÁAͅA͇+A̓A͇+A͇+A̓A͇+A̓A̓AͅA̓A̓A͇+AͅA͋DA͋DA͉7A͍PA͍PA͉7A͏\A͍PA͋DA͏\A͋DA͋DA͏\A͉7A͍PA͋DA͉7A͍PA͋DA͉7A͏\A͍PA͉7A͏\A͍PA͋DA͑hA͍PA͋DA͏\A͍PA͋DA͋DA͇+A͇+A͍PA͉7A͋DA͏\A͉7A͏\A͉7A͉7A͍PA͇+A͉7A͉7A̓A͋DA͋DA͉7A͋DA͇+AͅA͋DA͋DA͉7A͏\A͋DA̓A͇+A͉7AͅA͇+A͉7AͅA͍PA͋DA͉7A͋DA̓A͉7A͇+A͇+A͋DAͅA͉7A͇+AͅA͉7AͅA͇+A͉7AͅA͇+A͉7A͇+A͇+A͉7A͉7A͇+A͋DAͅA͇+A̓ÁA̓A�z�A�t�A�r�A�dZA�VA�S�A�K�A�E�A�C�A�E�A�9XA� �A�bA���A��yA��TA��
A�ĜA�ĜA�A�ȴA���A���A���A���A��`A���A���A�  A�%A�A�1A�JA�JA�bA�"�A�&�A�(�A�1'A�/A�1'A�=qA�=qA�G�A�bNA�r�A�z�AͅA͇+A͙�Aͧ�Aͧ�Aͧ�AͬAͮAͲ-A͸RAʹ9A�A���A���A��#A��`A��mA��A���A���A�  A�VA�bA� �A�;dA�G�A�`BA�l�A�p�A΃AΝ�AΥ�AήAκ^A���A���A���A��/A��#A��#A��;A��
A���A���A���A�ĜA�ƨA�ƨA���AξwA�AμjAμjAκ^Aδ9AζFAζFAΰ!Aΰ!Aδ9Aΰ!AήAβ-AήAήAβ-AήAήAβ-Aΰ!AάAΰ!AήAάAήAάAΧ�AάAΩ�AΥ�AΩ�AΧ�AΣ�AΧ�AΧ�AΣ�AΧ�AΧ�AΣ�AΧ�AΥ�AΡ�AΣ�AΥ�AΡ�AΡ�AΣ�AΟ�AΟ�AΡ�AΥ�AΟ�AΡ�AάAάAΩ�Aΰ!AήAΩ�AήAΰ!AήAήAβ-Aΰ!AάAΰ!Aβ-AήAβ-Aΰ!AΧ�AΥ�AΧ�AΧ�AΡ�AΥ�AΥ�AΟ�AΟ�AΣ�AΡ�AΟ�AΣ�AΡ�AΕ�AΓuAΏ\A�~�A�t�A�hsA�O�A�5?A�33A�-A�/A�-A��A��A�oA�A���A���A��mA��A���A���A͸RAͩ�Aͥ�Aͥ�A͙�A͙�A͕�A͇+A�t�A�bNA�Q�A�E�A�?}A�5?A�$�A�VA���A���A���A̮A̝�ȂhȦ+ÃA�~�A�r�A�XA�C�A�7LA�&�A� �A��A�bA�A��A��HA�ƨA˥�A˓uAˇ+A�v�A�l�A�dZA�K�A�+A�oA�  A�ƨAʓuA�z�A�t�A�n�A�dZA�ZA�ZA�VA�O�A�O�A�O�A�I�A�K�A�K�A�M�A�I�A�E�A�E�A�E�A�33A� �A��A�  A��`A���Aɟ�A�z�A�`BA�M�A�A�A�C�A�?}A�;dA�(�A��A���A��yA��/A���AȺ^AȮAȩ�Aȧ�Aȣ�Aȟ�Aȟ�Aȥ�Aȥ�Aȣ�Aȧ�AȬAȩ�Aȧ�Aȧ�Aȟ�AȍPA�x�A�n�A�bNA�S�A�C�A�1'A�(�A��A�A��A��
A���AǙ�A�bNA�`BA�Q�A�?}A��A�{A��A�ƨAƓuAƉ7AƁA�z�A�bNA�A�A�33A�$�A�
=A��A��#AŰ!AœuA�n�A�dZA�`BA�XA�K�A�=qA�1'A�&�A�$�A��A�oA���A��/AĮAđhA�`BAÑhA�Q�A��A�p�A�E�A��A�ȴA���A��+A�ZA�Q�A�I�A�;dA�1A��
A��9A���A�O�A���A�ZA�9XA��A��A���A�E�A��TA��+A�  A���A�S�A�bA�
=A�A���A��TA���A�-A��
A��FA��hA�jA�A�A�-A�$�A�JA��TA�ƨA�v�A�(�A��wA���A�hsA��A�VA�A���A���A�{A�~�A��#A��7A�9XA���A���A�ƨA��A��uA��7A��7A��7A��A�|�A�dZA�Q�A�A�A�1'A��;A�K�A�C�A�1'A�JA��A��RA��+A�E�A�;dA��TA���A��jA���A���A���A��\A��DA��A�~�A�t�A�l�A�hsA�A�A�?}A�5?A�%A��/A��!A��\A��A��+A��PA��DA��A�O�A��HA��A���A���A��FA��FA���A�r�A�I�A�A��wA��DA�~�A�bNA�1'A�A��\A�ZA�?}A�/A��A�
=A�A��yA��^A���A�ZA� �A��`A���A�v�A�1'A�
=A�A��A��`A���A��9A��A�S�A�7LA�oA�A��mA���A�XA�%A��#A���A���A�`BA�9XA�(�A���A��HA���A�t�A�?}A��yA��DA�t�A�v�A�t�A�n�A�hsA���A��A��FA��PA�VA�A�A��A��HA��A�ZA��PA���A��;A���A�p�A��A�ĜA�Q�A��HA���A� �A��DA�-A��hA���A��7A�E�A���A�dZA�9XA�5?A�/A��A�oA�%A���A��/A��9A���A�x�A�x�A�x�A�XA�&�A���A�XA���A�O�A�  A��9A�;dA���A���A�n�A�G�A��A�  A��A��mA��TA��;A��A���A���A�XA�{A���A��+A��A���A�I�A�33A��A��A��A�I�A�"�A�A��A��FA��DA�~�A�n�A�M�A�1'A��A��mA��-A��DA�hsA�E�A�"�A�%A��A��HA���A���A��A�dZA�G�A��A��A��
A���A���A���A���A��jA��\A�VA�9XA�1'A�{A���A��mA��/A���A�A��wA��!A���A���A�x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�l�A�n�A�p�ÁA͇+A͇+AͅAͅAͅAͅA͉7A͋DA͍PA͋DA͍PA͋DA͉7A͉7A͉7A͉7A͇+A͇+A�\)A��A��;A�bA�7LA͋DAͶFA��A�9XAΧ�A��
A�ƨAθRAΰ!Aΰ!AΩ�AΥ�AΣ�AΣ�AήAΰ!AΥ�AΝ�A�S�A�AͬA�I�A̧�A�1'A˛�A�A�Q�A�/A�hsA��`Aȥ�Aȇ+A���A��A�K�A�~�A�VA�ƨA��A�&�A�%A�  A��A�`BA��^A���A��PA��A��^A��^A�x�A���A�x�A��A���A�p�A���A�?}A�\)A�  A��jA�XA�1'A��yA���A��DA���A�Q�A�?}A���A�+A�l�A�^5A�dZA���A��`A�jA��
A� �A�t�A���A�ZA�E�A�
=AoA}��A{��Awt�Ar �AlQ�Ae��AaƨA]��AY�AWp�AV�!AV~�AT^5AQ/AP��AO?}AMO�AJ=qAH�HAGoADr�A@��A=�
A=l�A<bNA;hsA9l�A5�7A2�uA1XA0��A.��A-�FA+�TA+\)A*  A(�A&�DA%%A#�A"�RA"bA!��A r�A�9A�-An�AA�wA�A�A��AhsAO�A��A=qA�
AA�A33A�A�A��AĜAK�A��AbAp�Al�A��A��A�wA�A�RA9XA��Av�A�A1'A-AƨAO�A�A��A��A
�A
1A	�A	�A	�TA	�;A	�#A	`BA�A5?A�A��At�AG�A;dA�yA�PA�\A�AdZA%A�A�HAȴA��A��A�+Av�A9XAƨA�7A ��@��;@�+@�v�@�J@�`B@�S�@��@���@��9@�bN@��w@�\)@���@���@�x�@���@�{@�V@�1'@@�v�@�@�X@�Ĝ@��@�9X@��@��;@�S�@�M�@�X@���@�w@�ƨ@�ƨ@�F@�P@�\)@�"�@��@�33@�33@柾@�@�b@�C�@��@�^5@�$�@�^@�Ĝ@�j@�ƨ@�v�@��T@ݩ�@ܣ�@۾w@��@��@��@�r�@�  @֟�@�{@պ^@�p�@��@ӍP@�n�@��#@�G�@�z�@�bN@�Z@�Q�@�Q�@�I�@�A�@�9X@�1'@� �@���@�33@�v�@Ͳ-@��@���@ʇ+@�E�@�J@���@Ǿw@�\)@��H@ư!@Ə\@�E�@ŉ7@�  @�K�@�@°!@�=q@��@���@��@§�@�^5@�E�@���@��/@�1'@���@�S�@�33@��!@�5?@�{@��T@��h@�p�@�G�@��u@���@��;@���@�dZ@��@���@�v�@���@�I�@��
@�t�@��@��@�-@�/@�Ĝ@��9@��@��u@�j@��;@�;d@�ȴ@���@��\@�x�@�7L@��@�Ĝ@�I�@�ƨ@��\@�$�@�J@�@��T@�G�@�j@�b@�ƨ@���@��@�E�@��@�&�@��@�V@���@��9@�j@�Z@���@�l�@���@�~�@��#@�`B@��@��9@�Q�@�b@��
@��@�\)@��H@�$�@�J@��@�p�@���@��u@��@�bN@��m@��F@�o@��+@�5?@��^@���@���@�x�@�hs@�`B@�7L@�Ĝ@�A�@��
@���@���@���@�j@���@��P@�o@��H@��!@��!@���@��+@�=q@��@��#@�hs@��/@��u@�b@��@�;d@���@��H@���@�ȴ@�V@�X@��@�?}@�&�@��/@��@���@��@�r�@�j@�b@��
@��F@�|�@�K�@�
=@���@��T@���@�X@�G�@�?}@�V@���@���@��u@�Q�@��@�|�@�33@���@���@�^5@�=q@�5?@��@���@�x�@���@���@��D@�b@���@�l�@�
=@�^5@�-@�J@��^@�hs@�O�@�&�@��@���@��@�z�@�Z@��@���@�|�@�S�@�K�@��@��@��@�l�@��+@�V@�V@�M�@�$�@�@��@��@�?}@�V@��@�r�@�1'@�  @�;@��@l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�dZA�p�A�t�A�n�A�l�A�n�A�jA�n�A�l�A�p�A�r�A�l�A�p�A�x�A�z�A�n�A�p�A�l�A�n�A͉7AͅAͅA͉7AͅA͇+A͇+AͅA͉7A͇+AͅA͉7A͉7AͅA͉7AͅÁAͅA͇+A̓A͇+A͇+A̓A͇+A̓A̓AͅA̓A̓A͇+AͅA͋DA͋DA͉7A͍PA͍PA͉7A͏\A͍PA͋DA͏\A͋DA͋DA͏\A͉7A͍PA͋DA͉7A͍PA͋DA͉7A͏\A͍PA͉7A͏\A͍PA͋DA͑hA͍PA͋DA͏\A͍PA͋DA͋DA͇+A͇+A͍PA͉7A͋DA͏\A͉7A͏\A͉7A͉7A͍PA͇+A͉7A͉7A̓A͋DA͋DA͉7A͋DA͇+AͅA͋DA͋DA͉7A͏\A͋DA̓A͇+A͉7AͅA͇+A͉7AͅA͍PA͋DA͉7A͋DA̓A͉7A͇+A͇+A͋DAͅA͉7A͇+AͅA͉7AͅA͇+A͉7AͅA͇+A͉7A͇+A͇+A͉7A͉7A͇+A͋DAͅA͇+A̓ÁA̓A�z�A�t�A�r�A�dZA�VA�S�A�K�A�E�A�C�A�E�A�9XA� �A�bA���A��yA��TA��
A�ĜA�ĜA�A�ȴA���A���A���A���A��`A���A���A�  A�%A�A�1A�JA�JA�bA�"�A�&�A�(�A�1'A�/A�1'A�=qA�=qA�G�A�bNA�r�A�z�AͅA͇+A͙�Aͧ�Aͧ�Aͧ�AͬAͮAͲ-A͸RAʹ9A�A���A���A��#A��`A��mA��A���A���A�  A�VA�bA� �A�;dA�G�A�`BA�l�A�p�A΃AΝ�AΥ�AήAκ^A���A���A���A��/A��#A��#A��;A��
A���A���A���A�ĜA�ƨA�ƨA���AξwA�AμjAμjAκ^Aδ9AζFAζFAΰ!Aΰ!Aδ9Aΰ!AήAβ-AήAήAβ-AήAήAβ-Aΰ!AάAΰ!AήAάAήAάAΧ�AάAΩ�AΥ�AΩ�AΧ�AΣ�AΧ�AΧ�AΣ�AΧ�AΧ�AΣ�AΧ�AΥ�AΡ�AΣ�AΥ�AΡ�AΡ�AΣ�AΟ�AΟ�AΡ�AΥ�AΟ�AΡ�AάAάAΩ�Aΰ!AήAΩ�AήAΰ!AήAήAβ-Aΰ!AάAΰ!Aβ-AήAβ-Aΰ!AΧ�AΥ�AΧ�AΧ�AΡ�AΥ�AΥ�AΟ�AΟ�AΣ�AΡ�AΟ�AΣ�AΡ�AΕ�AΓuAΏ\A�~�A�t�A�hsA�O�A�5?A�33A�-A�/A�-A��A��A�oA�A���A���A��mA��A���A���A͸RAͩ�Aͥ�Aͥ�A͙�A͙�A͕�A͇+A�t�A�bNA�Q�A�E�A�?}A�5?A�$�A�VA���A���A���A̮A̝�ȂhȦ+ÃA�~�A�r�A�XA�C�A�7LA�&�A� �A��A�bA�A��A��HA�ƨA˥�A˓uAˇ+A�v�A�l�A�dZA�K�A�+A�oA�  A�ƨAʓuA�z�A�t�A�n�A�dZA�ZA�ZA�VA�O�A�O�A�O�A�I�A�K�A�K�A�M�A�I�A�E�A�E�A�E�A�33A� �A��A�  A��`A���Aɟ�A�z�A�`BA�M�A�A�A�C�A�?}A�;dA�(�A��A���A��yA��/A���AȺ^AȮAȩ�Aȧ�Aȣ�Aȟ�Aȟ�Aȥ�Aȥ�Aȣ�Aȧ�AȬAȩ�Aȧ�Aȧ�Aȟ�AȍPA�x�A�n�A�bNA�S�A�C�A�1'A�(�A��A�A��A��
A���AǙ�A�bNA�`BA�Q�A�?}A��A�{A��A�ƨAƓuAƉ7AƁA�z�A�bNA�A�A�33A�$�A�
=A��A��#AŰ!AœuA�n�A�dZA�`BA�XA�K�A�=qA�1'A�&�A�$�A��A�oA���A��/AĮAđhA�`BAÑhA�Q�A��A�p�A�E�A��A�ȴA���A��+A�ZA�Q�A�I�A�;dA�1A��
A��9A���A�O�A���A�ZA�9XA��A��A���A�E�A��TA��+A�  A���A�S�A�bA�
=A�A���A��TA���A�-A��
A��FA��hA�jA�A�A�-A�$�A�JA��TA�ƨA�v�A�(�A��wA���A�hsA��A�VA�A���A���A�{A�~�A��#A��7A�9XA���A���A�ƨA��A��uA��7A��7A��7A��A�|�A�dZA�Q�A�A�A�1'A��;A�K�A�C�A�1'A�JA��A��RA��+A�E�A�;dA��TA���A��jA���A���A���A��\A��DA��A�~�A�t�A�l�A�hsA�A�A�?}A�5?A�%A��/A��!A��\A��A��+A��PA��DA��A�O�A��HA��A���A���A��FA��FA���A�r�A�I�A�A��wA��DA�~�A�bNA�1'A�A��\A�ZA�?}A�/A��A�
=A�A��yA��^A���A�ZA� �A��`A���A�v�A�1'A�
=A�A��A��`A���A��9A��A�S�A�7LA�oA�A��mA���A�XA�%A��#A���A���A�`BA�9XA�(�A���A��HA���A�t�A�?}A��yA��DA�t�A�v�A�t�A�n�A�hsA���A��A��FA��PA�VA�A�A��A��HA��A�ZA��PA���A��;A���A�p�A��A�ĜA�Q�A��HA���A� �A��DA�-A��hA���A��7A�E�A���A�dZA�9XA�5?A�/A��A�oA�%A���A��/A��9A���A�x�A�x�A�x�A�XA�&�A���A�XA���A�O�A�  A��9A�;dA���A���A�n�A�G�A��A�  A��A��mA��TA��;A��A���A���A�XA�{A���A��+A��A���A�I�A�33A��A��A��A�I�A�"�A�A��A��FA��DA�~�A�n�A�M�A�1'A��A��mA��-A��DA�hsA�E�A�"�A�%A��A��HA���A���A��A�dZA�G�A��A��A��
A���A���A���A���A��jA��\A�VA�9XA�1'A�{A���A��mA��/A���A�A��wA��!A���A���A�x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B	u�B	u�B	u�B	t�B	u%B	u%B	uZB	uZB	u%B	u%B	t�B	u%B	uZB	u�B	u�B	v�B	w2B	wfB	xB	w�B	xB	x8B	�bB
�B
�B
��B
�XB
��B iB�B6�Bc�B}�B��B�B��B�oB��B�AB��B�B�%B�+B�fB�B��B�9B�<B��B�(BuB%B�BxB�B \B�B$�B*0B(�B1�B1'B-�B&B9XB5�B3hB4B3hB:^B9�BFBa�B[#B\]BT�BT,BYBX�B\]BU�BI�BD�B=B0!B-B2�B)�B"4B�B�"B�XB�CB�B��BcTBB'B7LBB
�B
��B
�B
�`B
��B
ԕB
ѷB
��B
�B
��B
w�B
f�B
\�B
RTB
G�B
-�B
1B	��B	�mB	��B	��B	� B	�uB	|�B	xlB	{B	g�B	`�B	b�B	W?B	S[B	K�B	FB	:�B	:�B	-CB	+B	*eB	(XB	)*B	(�B	&LB	'B	'�B	(XB	&�B	$�B	$�B	)*B	,qB	)_B	.IB	2�B	6zB	A B	D�B	RTB	O�B	N�B	PHB	R�B	XEB	W?B	L�B	<�B	9$B	8B	9�B	<�B	<6B	J#B	a�B	tB	��B	�	B	u�B	qvB	�YB	�VB	�hB	��B	�\B	��B	�B	�FB	��B	�=B	��B	�_B	�1B	��B	��B	�_B	�hB	��B	��B	��B	��B	�?B	��B	��B	��B	�jB	��B	�B	�<B	��B	��B	�<B	��B	��B	�}B	�BB	�wB	ƨB	ĜB	��B	�EB	�zB	�B	�B	�zB	�KB	�zB	��B	�EB	��B	ɆB	�KB	˒B	ʌB	��B	ʌB	��B	��B	�B	�B	�pB	�B	ΥB	��B	�B	��B	��B	֡B	�B	�mB	�2B	֡B	�sB	�B	��B	��B	��B	�QB	��B	��B	یB	�#B	��B	��B	��B	�jB	�pB	�B	�B	�B	��B	��B	�mB	�"B	�5B	��B	�)B	�B	� B	�B	�oB	��B	�B	�B	�B	�TB	��B	�B	�TB	��B	��B	�ZB	��B	��B	�TB	�TB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�.B	��B	��B	��B	��B	��B	��B	��B	�.B	��B	��B	�]B	��B	��B	�B	�xB	��B	�>B	�	B	��B	�%B	��B	��B	��B	�%B	�B	�%B	�%B	��B	�B	�8B	��B	��B	��B
%B

=B

	B
	lB
	B
	7B

�B
	�B
	7B

=B
�B
PB
VB
�B
hB
�B
bB
"B
�B
\B
�B
�B
uB
@B
�B
PB
DB
DB
�B
�B
�B
�B
�B
 B
�B
�B
oB
B
�B
�B
�B
�B
�B
$B
�B
$B
�B
$B
�B
eB
�B
B
7B
�B
�B
B
�B
B
�B
VB
!-B
!-B
!-B
!-B
!bB
"�B
"hB
"4B
#:B
$B
$�B
$�B
&B
#�B
"�B
#�B
$@B
$@B
%�B
&B
&LB
'�B
(�B
)_B
)�B
*0B
+B
*�B
*�B
*�B
,qB
,=B
.IB
.}B
/�B
0�B
1'B
1�B
2aB
2aB
2-B
2-B
1�B
2�B
2aB
1�B
3�B
2�B
6B
5?B
5B
5?B
5B
5B
5tB
6B
6�B
7�B
7�B
7�B
8B
7�B
7�B
8�B
9$B
:*B
9�B
:^B
:*B
:*B
;�B
<�B
<6B
<�B
>�B
?B
>�B
?B
?B
?}B
?HB
?�B
>�B
>�B
>�B
>�B
?B
>�B
?HB
?HB
?HB
?HB
?�B
@OB
@�B
A�B
A�B
AUB
A�B
A�B
B'B
A�B
B�B
CaB
C�B
C�B
D3B
E�B
F�B
G�B
GzB
IB
IB
I�B
IRB
J�B
I�B
IRB
JXB
I�B
IRB
H�B
IB
IB
IB
J#B
I�B
J�B
I�B
I�B
I�B
JXB
J�B
L�B
NB
NpB
O�B
OvB
NpB
N<B
N<B
NpB
M�B
M�B
M�B
N�B
N�B
NpB
O�B
O�B
O�B
O�B
O�B
O�B
O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	u�B	w2B	sB	tTB	v�B	v�B	u%B	w2B	u�B	w2B	u�B	u�B	w�B	tB	t�B	u�B	x8B	t�B	u�B	v�B	q�B	u�B	u�B	tTB	u�B	t�B	t�B	v+B	tB	t�B	u�B	tB	t�B	u�B	tTB	u�B	v�B	uZB	t�B	u�B	tTB	uZB	u�B	s�B	uZB	v+B	t�B	u�B	u�B	tTB	v+B	sB	t�B	u�B	s�B	t�B	v+B	s�B	t�B	u�B	s�B	u�B	u�B	tB	v�B	uZB	u�B	v�B	t�B	v�B	v�B	u%B	u%B	w2B	t�B	u%B	u�B	s�B	u�B	v�B	uZB	v`B	v�B	u�B	w�B	w�B	u�B	v�B	v�B	u�B	xB	uZB	w�B	w2B	v+B	w�B	w2B	v�B	y	B	v�B	v�B	x8B	v�B	w2B	y	B	v`B	v�B	w�B	u�B	v�B	y�B	w�B	wfB	y�B	xB	w�B	y	B	v�B	w�B	w�B	v�B	y>B	v�B	xB	xlB	v�B	x�B	v�B	xB	xlB	v�B	y>B	xB	v�B	xlB	x8B	w2B	w�B	x�B	wfB	x8B	xlB	v�B	x�B	w�B	y>B	y�B	x8B	zB	{�B	|�B	�iB	�B	��B	�bB	�:B	�zB	��B	�B	�B	�vB	�B
�B
	B
+kB
Q�B
u�B
�{B
��B
�lB
�{B
�B
��B
�IB
��B
�B
�B
�!B
�B
�B
�nB
�B
��B
��B
�-B
�?B
�aB
�B
��B
�KB
�HB
�HB
�mB
�;B
�
B
�B
��B
�B
��B
��B
��B
��B
�"B iB;B{B�B�B�B�B�BBCB \B#�B#:B$tB'�B0UB7�B;dBI�BIRBP�BT�BZBd�BgmBhsBm�BzBy�BzxB~(B�BcB�iB�;B�oB��B�GB�oB��B�B��B� B�B�B��B��B�iB�4B��B�;B�B��B��B�B�B�;B�4B�oB�AB�;B�oB�AB��B�;B�uB��B��B��B��B�B��B��B��B��B��B��B�GB��B�AB�{B�oB�AB��B��B��B�{B�B�B�GB��B�B�uB�MB��B��B��B��B��B��B��B�SB��B��B�_B��B��B��B��B�_B�B��B��B�B�lB��B��B�7B�+B�+B�7B��B��B��B�lB�1B�B�B�rB��B��B��B�.B��B�hB��B��B��B�B��B��B��B��B��B�B��B��B� B�-B�KB�dB͟B�BB֡B�EB�)B�BB��B�B�WB��B�/B�/B�B�B��B��B��B��B�.B 4B;B �B 4BB�BBB�BoB 4BoB�BBoBSB�BYB�B�B�B
	B�B�BVB\BB�B�BB�BeB�B�B�BBxB�B�BBxB�B�B�BxB=B�B \B�B�B"�B'RB%B&�B!BVB�B�B�B�BxBB�B �B 'B �B!-B!-B!-B!bB!�B#nB#�B$B%zB&�B(�B($B($B)�B)�B)_B,qB-B+B*0B)�B)*B(�B&B%�B(XB'�B($B%zB0�B7�B-CB0�B0UB1[B/�B5B2�B4�B1�B1�B/OB33B2�B0�B/�B0UB1'B1[B1�B.�B2�B,�B)�B)�B)�B-CB'B'�B%zB$�B"�B%FB&LB%FB&LB%BU�BD�B=qB<�B8�B7B@B4�B9�B7�B1�B0UB/�B5?B/B.�B,�B8�B?HB4�B0�B/�B1�B-CB8�B.B7�BA�B-�B=qB+�B)_B+kB.�B7�BB'BD�B>�B:*B>BB8RB8B6�B4B3�B3hB2aB?�B>B<jB:^BE�BA�B5?B9�BC�BH�B`BBd&B`vBXB_�Bb�Bm]B[�B\]B\]B[�BZ�BYBY�BZ�B_B^BZ�BXEBl�BW�BOBBQNBQ�BYBS[BY�B`�BO�BZQBR BQ�BRTBQNBP�BT,BV9BYBYKBW�BXEB\�BZ�BV�BW
B_pB[�BW
BW
BT�BR�BY�B\�B\�Bg�Be,BR�BOBMBUgBZ�BZ�B\�BN�BYKBK�BD�B=<B<jBB�BB�BB�BF?BF?BGBA�B@�BAUB@�B>�B9XB>B5�B6zB2�B1�B7�B-wB*0B,B*�B*0B,qB/B1[B)�B1�B+B0�B8�B/OB2aB+�B2�B9$B4nB+�B-B,B(XB($B%FB%FB)�B&�BCB=B�BxB�BAUB�B(B1BMB��B��B��B��B��B��BB�B�/B�B��B��B�B�yB�}B��B�B�wBŢB��B�B�B�9B�hB�FB� B�hB�B�.B�:B��B�hB�PB�xB�DB��B�YB�lB�	B�+B��B��B��Bz�BpBz�Br�B`�BX�BM6BOBF�BC�BB�BA�BA B@�B>�B@�BB�B8RB=qB;dB8�B>B,qB!�B!�B#:B3�B�BBFB�B7B~B	�B	lB	lB�BB�B�B�B
��B
��B
��B
�2B
��B
�B
�B
��B
� B
�"B
�)B
��B
�KB
�B
�|B
�B
�HB
�B
��B
�B
�B
ޞB
چB
ܒB
چB
�?B
�B
��B
�TB
�BB
�}B
�)B
�XB
�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                               44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                               44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022022506062420220225060624IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022030701010720220307010107QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022030701010720220307010107QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                