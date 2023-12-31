CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-10-05T21:06:29Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     p  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ux   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     p  [�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     p  z    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ɍ   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  Ϩ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                     PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p %0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` =�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   >    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   D    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   J    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T P    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   PT   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   P\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Pd   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   Pl   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � Pt   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   P�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Q   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Q   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Q8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Q@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       QH   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    QPArgo profile    3.1 1.2 19500101000000  20221005210629  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_231                 6810_008521_231                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��򅇓�@��򅇓�11  @���}V@���}V@1����n@1����n�d��w[�0�d��w[�011  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�{?��H@B�\@�G�@�  @��R@�  AG�A��A\)A+�A?\)A`  A�Q�A�  A�  A�  A���A�  A�
=A�B Q�B(�B�
B�
B�
B'�B0  B8  B@  BG�
BP  BX(�B`  Bg�
Bo�
Bx  B�{B�{B�  B�(�B�  B��B��B�  B�  B��B��B�  B�  B�{B�{B�  B��B��B��
B��B�  B�  B�{B�  B��B��B�  B��B��B��
B��
B�  C   C��C
=C  C  C
  C�C�C��C��C��C  C
=C�C
=C  C��C!�C$  C&{C(  C)��C+��C-��C/��C1��C4  C6
=C8
=C:  C;��C=��C@  CB
=CD
=CF  CH
=CJ
=CK��CM��CO��CR  CT  CU�CX  CZ{C[��C^
=C`  Cb  Cc��Ce�Ch  Ci�Cl  Cn
=Cp  Cq��Cs��Cv  Cx  Cy��C{��C}��C�  C�  C���C���C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�C�C�  C���C�  C�  C�  C�C�C���C��C���C�C�  C���C���C���C���C���C���C�  C�
=C�C�  C�  C�C�  C���C�  C�  C���C�  C�C�
=C�C�  C�  C�  C�C�  C���C���C���C�  C���C�C�C�  C���C���C���C�C�C���C�  C�  C���C�  C���C�  C�C�  C���C�  C�C�C�C�C�C�C�  C���C��C���C�  C�C�  C�C�
=C�  C���C�  C�  C�  C�C�C�
=C�C�  C�  C�  C�  C���C�  C�C���C���C���C���C���C�  C�  C�  C�  C�C�  C���C�C�  C���C�  C�  C�  C�D D �D�D� D  D� D  D��D�D� D�qDz�D��D� D  D� D��D}qD	  D	�D
D
}qD  D� D�D��D�qD� D�D� D  D� D  D� D  D}qD�qD� D�D�D�D� D�D��D  D}qD�qD� D�qD}qD  D}qD�qD��DD��D�qD}qD�qDz�D��D� D  D� D �D � D!  D!��D"  D"}qD"�qD#��D$  D$� D%  D%��D&�D&� D&�qD'}qD(�D(}qD(�RD)z�D*  D*��D*�qD+z�D+�qD,}qD,��D-� D.  D.}qD.�qD/}qD/�qD0��D1  D1� D2�D2��D2�qD3}qD3��D4}qD5  D5}qD6  D6��D7  D7��D8�D8��D9�D9� D9�qD:� D;  D;� D<�D<}qD<�qD=� D>�D>��D?�D?��D@�D@��DA�DA�DB  DB� DC�DC� DD�DD}qDD��DE}qDF  DF��DG  DG� DH  DH� DI�DI� DJ�DJ� DJ�qDK� DL  DL��DM  DM}qDM�qDN� DO  DO}qDP  DP��DQ  DQ}qDR  DR� DS  DS� DT  DT}qDT�qDU}qDU�qDV� DW�DW��DW�qDX}qDY  DY� DZ�DZ�D[�D[�D\�D\�D]  D]z�D]��D^}qD^��D_}qD_�qD`� Da�Da�Db  Db}qDc  Dc� Dd  Dd�De  Dez�De�qDf��Dg  Dg� Dh  Dh� Di  Di� Di�qDj��Dk�Dk��Dl�Dl��Dm�Dm��Dn  Dn� Do�Do��Dp�Dp� Dq�Dq��DrDr��Ds  Ds��Dt�Dt��Du  Du��DvDv� Dw  Dw�Dx�Dx}qDy  Dy� Dy�qDz}qD{�D{� D|  D|� D}�D}� D~�D~� D  D�D��D�AHD�~�D���D�  D�@ D�� D���D�  D�@ D�~�D��qD�  D�B�D��HD��HD��D�AHD��HD�� D���D�>�D�~�D��qD���D�AHD��HD���D�HD�>�D�~�D�� D���D�@ D�� D�� D���D�AHD��HD��HD�HD�B�D��HD���D�  D�>�D�~�D�� D�  D�AHD���D��HD���D�@ D�~�D��qD�  D�@ D�� D�� D�  D�@ D�� D��HD�fD�@ D�~�D���D�HD�B�D��HD�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�� D�� D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?���?�33?�(�@   @
=q@#�
@5@B�\@\(�@c�
@z�H@�ff@�\)@�Q�@��R@���@���@���@\@�ff@��@�
=@�  @�@�{@�Q�@�(�A33AffA��A{A�Az�A��A�A ��A#�
A)��A,(�A0��A5�A8Q�A>{AAG�AFffAJ�HAMp�AS�
AW
=AZ�HA`  Ab�\Ag�Aj�HAn�RAs�
AvffA{�A�  A�G�A�(�A�A�  A��A��A�ffA�Q�A��\A�p�A��RA��A��A�{A�Q�A��\A�p�A�
=A��A��
A�{A���A��\A�A��A��\A�z�A��RA���A�33A�ffA�  A�33A���A�\)A��A��
A�
=Aأ�AۅA�A�Q�A��HA���A�  A陚A�z�A�
=A�G�A�(�A�A�G�A�33A�{B ��Bp�B
=B  B��B�\B  B	G�B
=qB�
Bz�B=qB
=B��B��B�RBQ�Bp�BffB  B��BffB�B��B=qB
=B ��B!��B"�HB$(�B$��B&�\B'\)B(��B)�B*�RB,Q�B,��B.ffB/�
B0��B2{B3�B4Q�B5�B6�HB8Q�B9��B:�\B<(�B=G�B>ffB@  B@��BB{BC�BDQ�BE��BG33BH(�BI��BJ�\BL(�BMG�BN=qBO�BP��BR=qBS33BTz�BV{BV�HBXQ�BY��BZffB[�
B]�B]�B_�B`��BaBc33Bd  Be��Bf�RBg�
BiG�Bj=qBk\)Bm�Bm�Bo33Bp��Bqp�Br�HBtQ�Bu�Bv�RBw�Bx��Bz{B{
=B|��B}G�B~�HB�  B�z�B�G�B��B�ffB���B��B�=qB��RB�p�B�(�B��\B�33B��
B�Q�B��B�B�(�B���B�p�B�{B��HB�\)B�  B��RB��B��B��\B���B�B�=qB���B���B�(�B��\B�\)B��
B�z�B��B��B�Q�B���B�p�B�(�B���B�33B��B�ffB�
=B��B�{B��RB�p�B��
B��\B��B��B�(�B��HB�33B��
B��\B���B�B�(�B��RB��B��B�ffB�33B���B�{B��RB�p�B��
B�ffB��B���B�{B��HB�G�B��
B��\B���B��B�=qB��HB�33B�  B�ffB�
=B�B�{B��RB�p�B��
B��\B�33B��B�=qB���B�33B��B�z�B��HB���B�{B��\B�33B��B�Q�B��HB��B�{B¸RB�\)B�B�=qB�
=B�p�B�  BƸRB�
=BǮB�ffB���B�p�B�{B�z�B��BˮB�(�B���B�G�B��
BΏ\B�33Bϙ�B�Q�B��HB�\)B�{BҸRB��B��
Bԏ\B���BՅB�Q�BָRB�\)B�  B�ffB�33BٮB�=qB���B�\)B�  Bܣ�B�
=BݮB�z�B��HB�p�B�(�B�z�B�33B��
B�=qB���B㙚B�  B�RB�\)B�B�Q�B��B癚B�{B�RB�p�B��
B�ffB�
=B�B�Q�B�RB�B�{B�\B�33B��
B�=qB��HB�B�{B�\B�G�B�  B�z�B�
=B�B�Q�B��RB�\)B�  B��\B���B�B�ffB��HB�\)B�  B���B�33B���B�Q�B��HB�G�B��
C G�C z�C �RC{C\)C��C�
C33C�C�RC{Cp�C��C��CG�C�CC(�CffC��C�CG�C�C�RC{CQ�C��C�C=qC�CC	
=C	ffC	�RC
  C
=qC
��C
�C(�CffC��C(�C\)C�RC{CG�C��C  C33C�C�C(�Cp�C�
C�CffCC{CG�C�C  C=qC�\C�C�Cp�C�
C{C\)C��C{CQ�C�RC
=CG�C�\C��C�Cp�C��C
=C=qC�\CC��CG�Cp�C��C�HC�C=qCp�C�C�HC��C�C\)C�\C��C��C  C=qC\)Cz�C�RC�C
=C33Cp�C�C�RC��C(�C=qC�\C�C��C
=CG�CffC�CC��C 33C ffC �C ��C �HC!�C!=qC!z�C!�RC!��C"
=C"G�C"ffC"�\C"C#
=C#=qC#ffC#�\C#�RC#�HC$�C$\)C$��C$C$�HC%{C%Q�C%��C%�RC%�HC&
=C&G�C&�C&C&�C'
=C'=qC'z�C'�C'��C(  C(G�C(ffC(�\C(�
C)
=C)(�C)ffC)��C)�HC*  C*(�C*ffC*��C*�HC+
=C+33C+ffC+�\C+��C,{C,=qC,\)C,��C,�
C-{C-G�C-p�C-��C-�RC-�C.(�C.\)C.�\C.�C.�
C/�C/Q�C/p�C/��C/��C/��C0�C0Q�C0�C0�RC0��C1�C1G�C1�C1�C1�
C2  C2(�C2Q�C2�C2�RC2�C3�C3Q�C3�C3�RC3�HC4
=C4(�C4\)C4�C4�RC4�HC5{C5G�C5z�C5�C5�
C6
=C6=qC6z�C6��C6C6�C7{C7G�C7�\C7�RC7�C8�C8G�C8p�C8��C8��C8�C9
=C9=qC9ffC9�\C9�RC9��C:�C:Q�C:z�C:�C:�
C:��C;{C;G�C;ffC;�\C;�RC;�C<�C<Q�C<�C<�C<�
C=  C=(�C=\)C=�C=�C=��C=��C>�C>Q�C>z�C>��C>C>��C?{C?G�C?p�C?��C?��C@  C@(�C@\)C@�C@�C@�HCA
=CA=qCAp�CA��CA��CA��CB�CBQ�CB�CB�CB�
CC  CC(�CC\)CC�CC�CC�
CD
=CD33CD\)CD�\CD�RCD�HCE
=CE33CEp�CE��CECE�CF{CFG�CFp�CF��CF��CF��CG�CGG�CGz�CG��CG��CH  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�{?��H@B�\@�G�@�  @��R@�  AG�A��A\)A+�A?\)A`  A�Q�A�  A�  A�  A���A�  A�
=A�B Q�B(�B�
B�
B�
B'�B0  B8  B@  BG�
BP  BX(�B`  Bg�
Bo�
Bx  B�{B�{B�  B�(�B�  B��B��B�  B�  B��B��B�  B�  B�{B�{B�  B��B��B��
B��B�  B�  B�{B�  B��B��B�  B��B��B��
B��
B�  C   C��C
=C  C  C
  C�C�C��C��C��C  C
=C�C
=C  C��C!�C$  C&{C(  C)��C+��C-��C/��C1��C4  C6
=C8
=C:  C;��C=��C@  CB
=CD
=CF  CH
=CJ
=CK��CM��CO��CR  CT  CU�CX  CZ{C[��C^
=C`  Cb  Cc��Ce�Ch  Ci�Cl  Cn
=Cp  Cq��Cs��Cv  Cx  Cy��C{��C}��C�  C�  C���C���C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�C�C�  C���C�  C�  C�  C�C�C���C��C���C�C�  C���C���C���C���C���C���C�  C�
=C�C�  C�  C�C�  C���C�  C�  C���C�  C�C�
=C�C�  C�  C�  C�C�  C���C���C���C�  C���C�C�C�  C���C���C���C�C�C���C�  C�  C���C�  C���C�  C�C�  C���C�  C�C�C�C�C�C�C�  C���C��C���C�  C�C�  C�C�
=C�  C���C�  C�  C�  C�C�C�
=C�C�  C�  C�  C�  C���C�  C�C���C���C���C���C���C�  C�  C�  C�  C�C�  C���C�C�  C���C�  C�  C�  C�D D �D�D� D  D� D  D��D�D� D�qDz�D��D� D  D� D��D}qD	  D	�D
D
}qD  D� D�D��D�qD� D�D� D  D� D  D� D  D}qD�qD� D�D�D�D� D�D��D  D}qD�qD� D�qD}qD  D}qD�qD��DD��D�qD}qD�qDz�D��D� D  D� D �D � D!  D!��D"  D"}qD"�qD#��D$  D$� D%  D%��D&�D&� D&�qD'}qD(�D(}qD(�RD)z�D*  D*��D*�qD+z�D+�qD,}qD,��D-� D.  D.}qD.�qD/}qD/�qD0��D1  D1� D2�D2��D2�qD3}qD3��D4}qD5  D5}qD6  D6��D7  D7��D8�D8��D9�D9� D9�qD:� D;  D;� D<�D<}qD<�qD=� D>�D>��D?�D?��D@�D@��DA�DA�DB  DB� DC�DC� DD�DD}qDD��DE}qDF  DF��DG  DG� DH  DH� DI�DI� DJ�DJ� DJ�qDK� DL  DL��DM  DM}qDM�qDN� DO  DO}qDP  DP��DQ  DQ}qDR  DR� DS  DS� DT  DT}qDT�qDU}qDU�qDV� DW�DW��DW�qDX}qDY  DY� DZ�DZ�D[�D[�D\�D\�D]  D]z�D]��D^}qD^��D_}qD_�qD`� Da�Da�Db  Db}qDc  Dc� Dd  Dd�De  Dez�De�qDf��Dg  Dg� Dh  Dh� Di  Di� Di�qDj��Dk�Dk��Dl�Dl��Dm�Dm��Dn  Dn� Do�Do��Dp�Dp� Dq�Dq��DrDr��Ds  Ds��Dt�Dt��Du  Du��DvDv� Dw  Dw�Dx�Dx}qDy  Dy� Dy�qDz}qD{�D{� D|  D|� D}�D}� D~�D~� D  D�D��D�AHD�~�D���D�  D�@ D�� D���D�  D�@ D�~�D��qD�  D�B�D��HD��HD��D�AHD��HD�� D���D�>�D�~�D��qD���D�AHD��HD���D�HD�>�D�~�D�� D���D�@ D�� D�� D���D�AHD��HD��HD�HD�B�D��HD���D�  D�>�D�~�D�� D�  D�AHD���D��HD���D�@ D�~�D��qD�  D�@ D�� D�� D�  D�@ D�� D��HD�fD�@ D�~�D���D�HD�B�D��HD�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�� D�� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?���?�33?�(�@   @
=q@#�
@5@B�\@\(�@c�
@z�H@�ff@�\)@�Q�@��R@���@���@���@\@�ff@��@�
=@�  @�@�{@�Q�@�(�A33AffA��A{A�Az�A��A�A ��A#�
A)��A,(�A0��A5�A8Q�A>{AAG�AFffAJ�HAMp�AS�
AW
=AZ�HA`  Ab�\Ag�Aj�HAn�RAs�
AvffA{�A�  A�G�A�(�A�A�  A��A��A�ffA�Q�A��\A�p�A��RA��A��A�{A�Q�A��\A�p�A�
=A��A��
A�{A���A��\A�A��A��\A�z�A��RA���A�33A�ffA�  A�33A���A�\)A��A��
A�
=Aأ�AۅA�A�Q�A��HA���A�  A陚A�z�A�
=A�G�A�(�A�A�G�A�33A�{B ��Bp�B
=B  B��B�\B  B	G�B
=qB�
Bz�B=qB
=B��B��B�RBQ�Bp�BffB  B��BffB�B��B=qB
=B ��B!��B"�HB$(�B$��B&�\B'\)B(��B)�B*�RB,Q�B,��B.ffB/�
B0��B2{B3�B4Q�B5�B6�HB8Q�B9��B:�\B<(�B=G�B>ffB@  B@��BB{BC�BDQ�BE��BG33BH(�BI��BJ�\BL(�BMG�BN=qBO�BP��BR=qBS33BTz�BV{BV�HBXQ�BY��BZffB[�
B]�B]�B_�B`��BaBc33Bd  Be��Bf�RBg�
BiG�Bj=qBk\)Bm�Bm�Bo33Bp��Bqp�Br�HBtQ�Bu�Bv�RBw�Bx��Bz{B{
=B|��B}G�B~�HB�  B�z�B�G�B��B�ffB���B��B�=qB��RB�p�B�(�B��\B�33B��
B�Q�B��B�B�(�B���B�p�B�{B��HB�\)B�  B��RB��B��B��\B���B�B�=qB���B���B�(�B��\B�\)B��
B�z�B��B��B�Q�B���B�p�B�(�B���B�33B��B�ffB�
=B��B�{B��RB�p�B��
B��\B��B��B�(�B��HB�33B��
B��\B���B�B�(�B��RB��B��B�ffB�33B���B�{B��RB�p�B��
B�ffB��B���B�{B��HB�G�B��
B��\B���B��B�=qB��HB�33B�  B�ffB�
=B�B�{B��RB�p�B��
B��\B�33B��B�=qB���B�33B��B�z�B��HB���B�{B��\B�33B��B�Q�B��HB��B�{B¸RB�\)B�B�=qB�
=B�p�B�  BƸRB�
=BǮB�ffB���B�p�B�{B�z�B��BˮB�(�B���B�G�B��
BΏ\B�33Bϙ�B�Q�B��HB�\)B�{BҸRB��B��
Bԏ\B���BՅB�Q�BָRB�\)B�  B�ffB�33BٮB�=qB���B�\)B�  Bܣ�B�
=BݮB�z�B��HB�p�B�(�B�z�B�33B��
B�=qB���B㙚B�  B�RB�\)B�B�Q�B��B癚B�{B�RB�p�B��
B�ffB�
=B�B�Q�B�RB�B�{B�\B�33B��
B�=qB��HB�B�{B�\B�G�B�  B�z�B�
=B�B�Q�B��RB�\)B�  B��\B���B�B�ffB��HB�\)B�  B���B�33B���B�Q�B��HB�G�B��
C G�C z�C �RC{C\)C��C�
C33C�C�RC{Cp�C��C��CG�C�CC(�CffC��C�CG�C�C�RC{CQ�C��C�C=qC�CC	
=C	ffC	�RC
  C
=qC
��C
�C(�CffC��C(�C\)C�RC{CG�C��C  C33C�C�C(�Cp�C�
C�CffCC{CG�C�C  C=qC�\C�C�Cp�C�
C{C\)C��C{CQ�C�RC
=CG�C�\C��C�Cp�C��C
=C=qC�\CC��CG�Cp�C��C�HC�C=qCp�C�C�HC��C�C\)C�\C��C��C  C=qC\)Cz�C�RC�C
=C33Cp�C�C�RC��C(�C=qC�\C�C��C
=CG�CffC�CC��C 33C ffC �C ��C �HC!�C!=qC!z�C!�RC!��C"
=C"G�C"ffC"�\C"C#
=C#=qC#ffC#�\C#�RC#�HC$�C$\)C$��C$C$�HC%{C%Q�C%��C%�RC%�HC&
=C&G�C&�C&C&�C'
=C'=qC'z�C'�C'��C(  C(G�C(ffC(�\C(�
C)
=C)(�C)ffC)��C)�HC*  C*(�C*ffC*��C*�HC+
=C+33C+ffC+�\C+��C,{C,=qC,\)C,��C,�
C-{C-G�C-p�C-��C-�RC-�C.(�C.\)C.�\C.�C.�
C/�C/Q�C/p�C/��C/��C/��C0�C0Q�C0�C0�RC0��C1�C1G�C1�C1�C1�
C2  C2(�C2Q�C2�C2�RC2�C3�C3Q�C3�C3�RC3�HC4
=C4(�C4\)C4�C4�RC4�HC5{C5G�C5z�C5�C5�
C6
=C6=qC6z�C6��C6C6�C7{C7G�C7�\C7�RC7�C8�C8G�C8p�C8��C8��C8�C9
=C9=qC9ffC9�\C9�RC9��C:�C:Q�C:z�C:�C:�
C:��C;{C;G�C;ffC;�\C;�RC;�C<�C<Q�C<�C<�C<�
C=  C=(�C=\)C=�C=�C=��C=��C>�C>Q�C>z�C>��C>C>��C?{C?G�C?p�C?��C?��C@  C@(�C@\)C@�C@�C@�HCA
=CA=qCAp�CA��CA��CA��CB�CBQ�CB�CB�CB�
CC  CC(�CC\)CC�CC�CC�
CD
=CD33CD\)CD�\CD�RCD�HCE
=CE33CEp�CE��CECE�CF{CFG�CFp�CF��CF��CF��CG�CGG�CGz�CG��CG��CH  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�^5A�\)A�\)A�\)A�\)A�ZA�ZA�ZA�\)A�^5A�`BA�`BA�`BA�`BA�bNA�^5A�^5A�`BA�`BA�bNA�`BA�bNA�ffA�dZA�ffA�hsA�hsA�jA�hsA�bNA�\)A�O�A�K�A�G�A�M�A�  Aޥ�A���Aۡ�A�E�A���Aڗ�A�n�A�$�AٸRAه+A�;dAؗ�A�(�A�I�A�^5A�v�AԅA�;dA���AЏ\A�~�A�E�AΧ�A���A��A�{A�
=A���A�1'AȲ-A���A�~�A�bAƩ�A���A��A®A���A�=qA��`A���A���A���A�O�A�33A���A��A���A�+A���A�5?A�x�A��+A�  A�z�A�jA�JA�&�A���A�ȴA�$�A��A���A��7A�A�A���A�x�A�/A��A��A�bNA�  A��
A�1'A�1'A���A��\A�hsA��
A�|�A���A�  A���A��\A�v�A��!A�A�%A��!A�-A�9XA�p�A~Q�A|�/Ay�Ax��AtffAo`BAm/Ai��Ag��Ac�AaG�A^�A\�yAZ��AWx�ATE�ARI�AQ`BAOS�AKK�AD�jACC�A@��A?�A=�;A<bNA8��A7"�A6��A6v�A4�`A2��A1�FA0�HA/�A.��A.�A-��A,E�A*��A)�
A(I�A&�A&-A$ȴA$5?A"~�A!�A!�A ��A�\A�\A�A�DA+A�wAA�A��A7LAffA��A~�A��A7LA��A�FAhsA
��A
ZA	ƨA	�wA	K�A�DAA/A-A�7A�9AjA��AƨAƨA��A��A�TA�^A�7A7LA�`A�HA��A9XA��A/A �9@�j@�-@���@�  @�ff@�@��\@���@�@�J@��A J@��w@���@�t�@�;d@�@�|�@�I�@�b@�E�@���@�z�@�5?@�C�@�ƨ@�@��@�~�@��@�`B@�hs@���@��m@��@�1'@�(�@���@�
=@݁@��;@�r�@�?}@�@���@ݲ-@�7L@��@�bN@��y@�p�@؃@׍P@֗�@��@���@�V@�@թ�@�`B@ԋD@��
@���@�Z@ӥ�@���@���@Ь@� �@�9X@��
@�@���@�v�@ͩ�@�p�@��`@̛�@��;@�v�@�x�@�7L@�%@��@ȴ9@ȣ�@�9X@�I�@��
@Ə\@�7L@���@ēu@�(�@���@�"�@�V@��T@��h@�@�$�@�J@�@�`B@�b@��@��y@�V@�ff@��!@���@���@�J@��@�x�@�?}@��/@��D@���@��@���@��
@�dZ@�K�@���@�=q@��@��#@�p�@��@��m@��@�K�@��!@�E�@�5?@�-@���@��-@��@���@���@��@��H@��T@���@��-@��#@�@��@�`B@��@��/@�9X@��@���@�t�@�+@�o@�ȴ@�v�@�-@��@��^@�X@��@�r�@���@�C�@��@��R@�v�@�V@�M�@�$�@���@�X@���@�r�@�1@�dZ@�;d@�"�@�o@�@���@���@�E�@�J@�@��@�@��9@�Q�@��m@��P@�33@�
=@���@��+@�ff@�M�@�-@��@���@�X@�%@��9@���@�(�@��m@�|�@��H@�=q@�E�@�ff@�M�@��@���@��@��@��D@�z�@�9X@�1@��
@�ƨ@��@�l�@��@��@��!@�ff@�V@�-@�{@��^@�p�@�7L@�V@�%@���@�Ĝ@��@��D@�1'@��;@�ƨ@��@�l�@��@�ff@�E�@��@��@��@��9@���@��@�A�@��m@�l�@��!@�v�@�J@�{@�M�@�=q@���@�p�@�7L@�O�@�X@�G�@�G�@�V@���@��@�I�@��@�(�@� �@��@��F@�dZ@�;d@��y@��\@�~�@�=q@��@��h@��/@���@��@��@��F@�l�@�S�@��@���@��+@�V@�E�@���@�x�@�`B@�G�@�?}@�7L@�/@��j@��@���@��@��@��m@��@�K�@�+@���@���@���@�G�@��`@��j@���@��u@�r�@� �@���@�ƨ@��P@�l�@�;d@��@���@�~�@�5?@�p�@�/@��@���@��`@��9@�bN@�  @+@~ȴ@~v�@~5?@}`B@}/@|��@|��@|(�@{C�@z��@{�@{dZ@z�H@z�@y�^@y��@yhs@x�9@x �@xb@xb@w�@wl�@v�y@vV@u�@u�@uV@tj@s�F@s"�@s@r��@rJ@q��@qG�@pĜ@p�@pA�@o�;@ol�@n��@n��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�\)A�`BA�ZA�^5A�\)A�XA�^5A�ZA�^5A�\)A�\)A�^5A�XA�^5A�\)A�ZA�\)A�XA�\)A�VA�^5A�\)A�ZA�\)A�VA�\)A�\)A�XA�^5A�ZA�ZA�^5A�ZA�bNA�ZA�bNA�^5A�bNA�bNA�^5A�bNA�^5A�bNA�`BA�^5A�dZA�^5A�^5A�`BA�\)A�bNA�^5A�`BA�bNA�\)A�bNA�bNA�\)A�bNA�^5A�bNA�bNA�^5A�bNA�`BA�`BA�dZA�`BA�`BA�\)A�^5A�\)A�\)A�dZA�\)A�bNA�\)A�\)A�`BA�\)A�`BA�^5A�bNA�`BA�`BA�bNA�\)A�bNA�^5A�bNA�^5A�`BA�bNA�^5A�dZA�^5A�dZA�bNA�bNA�bNA�\)A�dZA�^5A�`BA�^5A�^5A�bNA�^5A�dZA�`BA�bNA�bNA�`BA�dZA�`BA�ffA�bNA�ffA�ffA�bNA�hsA�bNA�ffA�`BA�ffA�bNA�`BA�ffA�ffA�dZA�jA�dZA�hsA�hsA�dZA�jA�ffA�jA�ffA�hsA�jA�ffA�l�A�ffA�jA�jA�hsA�l�A�ffA�jA�l�A�ffA�jA�jA�dZA�jA�ffA�hsA�jA�dZA�dZA�bNA�bNA�ffA�^5A�bNA�ffA�bNA�dZA�ffA�XA�S�A�VA�ZA�O�A�O�A�S�A�K�A�K�A�I�A�O�A�S�A�G�A�I�A�I�A�E�A�G�A�G�A�A�A�C�A�G�A�M�A�Q�A�M�A�S�A�Q�A�M�A�I�A�G�A�E�A� �A�+A�VA���A��`A���A���A�ĜA޴9A޲-A���A�n�A�ffA�x�A�C�A�l�A�p�A�-A��/A۶FA۴9A۰!A۝�Aۣ�A۝�AۓuA�v�A�jA�ffA�S�A�=qA�&�A�oA���A��`A���A���A���Aڲ-Aڧ�Aڥ�Aڙ�Aڗ�Aڗ�AړuAڇ+AځA�|�A�r�A�jA�hsA�dZA�VA�VA�O�A�=qA�"�A�A��TA�ƨAپwAٲ-Aٲ-Aٲ-Aٴ9AٸRAٰ!Aٗ�AٍPA�z�A�|�A�z�A�v�A�r�A�hsA�A�A�;dA�(�A� �A�AخAؗ�A؟�A؛�AؓuA؉7A�x�A�jA�XA�K�A�&�A�$�A�1A��`A��;A�ȴA׉7A�ZA��A��A�Aֲ-A֝�AցA�x�A�I�A�"�A�
=A���A�AՍPA�z�A�\)A�A�A�"�A���A��;Aԝ�A�v�A�`BA�K�A�;dA�{A���Aӡ�A�jA�5?A�x�A�l�A�E�A�9XA��A�AѴ9A�I�A�=qA��A��A���AБhA�^5A�A�A�1'A���A���AϬA�ffA�XA�O�A�G�A�?}A�C�A�M�A�Q�A�XA�E�A�/A��A�A��yA���AάA�9XA��A��A�A��A���A;wAͧ�A͍PA͉7A�z�A�VA���A̼jA̝�A�l�A�^5A�9XA� �A��A��A��;A�ƨAˬA�l�A��A���A���AʸRAʧ�A�v�A�?}A�1A��AɾwAɣ�A�p�A�K�A�;dA�1'A�(�A�$�A��A�bA���A���A�ĜAȬAȏ\A�O�A�A��A��/A���AǶFAǲ-Aǰ!Aǥ�Aǝ�AǛ�AǑhA�ffA�E�A�7LA��A�oA�VA�VA�JA�
=A�A�%A���A��A��;Aƺ^A�p�A���A���AŰ!A�ffA��AēuA�=qA���A�$�A�A��A��A��`A��#A���A���A���A�ĜA���A¶FA\A�t�A�VA�-A��A��hA�r�A�O�A�5?A��;A���A��A�XA�1'A�ȴA�E�A��A���A��mA��/A���A��-A��\A�{A��9A��DA�^5A��A��A��wA���A���A���A��7A�jA�XA�A�A�&�A���A���A��jA���A��\A�x�A�l�A�^5A�5?A��A��hA�/A��A�Q�A�%A�"�A���A�E�A��yA���A���A��A�K�A��A��A�ƨA��A���A���A��+A�t�A�=qA�  A�^5A�A��PA�K�A���A��yA���A�ĜA���A��+A�|�A�x�A�p�A�hsA�bNA�^5A�VA�M�A�C�A�$�A�JA�  A��A��#A���A�ȴA��wA��A���A���A���A���A��\A�~�A�p�A�dZA�S�A�M�A�M�A�C�A�?}A�7LA��A�  A��A��/A���A��RA���A��DA�|�A�r�A�jA�ZA�(�A���A��`A��
A���A���A��DA�x�A�jA�bNA�S�A�A�A�1'A�(�A�$�A�{A�%A��A��yA��HA��;A��;A��A�ƨA��FA���A���A��PA�z�A�bNA�/A���A��!A���A�v�A�n�A�p�A�n�A�ffA�Q�A�E�A�9XA�/A�&�A��A��A�oA�bA�oA�oA�A��`A�ƨA��9A���A��PA�~�A�n�A�I�A�
=A���A���A�t�A�9XA��A���A��#A���A��!A���A��hA�l�A�A�A�&�A�
=A���A���A��HA��
A���A���A�ĜA��jA���A���A��hA�|�A�G�A��A�x�A�(�A��A�ĜA��uA�jA�K�A�9XA�/A�(�A��A�oA�A��A��TA���A��A���A�z�A�r�A�hsA�C�A��A�
=A��mA���A�7LA��HA��wA���A�z�A�XA�5?A�
=A��TA���A���A�p�A�?}A��A��RA���A��uA��DA��A�z�A�n�A�^5A�O�A�;dA�$�A��A�A��;A���A�;dA��-A�jA�=qA��A���A��jA���A�|�A�Q�A�"�A��A��mA��HA��/A��
A���A�ƨA�ffA�A���A��!A���A�|�A�Q�A�?}A�9XA�-A� �A��A��A�oA�JA�JA�1A�A�A���A���A���A���A��A��TA��A���A���A�ƨA���A��^A��FA��9A���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�\)A�\)A�\)A�\)A�ZA�ZA�ZA�\)A�^5A�`BA�`BA�`BA�`BA�bNA�^5A�^5A�`BA�`BA�bNA�`BA�bNA�ffA�dZA�ffA�hsA�hsA�jA�hsA�bNA�\)A�O�A�K�A�G�A�M�A�  Aޥ�A���Aۡ�A�E�A���Aڗ�A�n�A�$�AٸRAه+A�;dAؗ�A�(�A�I�A�^5A�v�AԅA�;dA���AЏ\A�~�A�E�AΧ�A���A��A�{A�
=A���A�1'AȲ-A���A�~�A�bAƩ�A���A��A®A���A�=qA��`A���A���A���A�O�A�33A���A��A���A�+A���A�5?A�x�A��+A�  A�z�A�jA�JA�&�A���A�ȴA�$�A��A���A��7A�A�A���A�x�A�/A��A��A�bNA�  A��
A�1'A�1'A���A��\A�hsA��
A�|�A���A�  A���A��\A�v�A��!A�A�%A��!A�-A�9XA�p�A~Q�A|�/Ay�Ax��AtffAo`BAm/Ai��Ag��Ac�AaG�A^�A\�yAZ��AWx�ATE�ARI�AQ`BAOS�AKK�AD�jACC�A@��A?�A=�;A<bNA8��A7"�A6��A6v�A4�`A2��A1�FA0�HA/�A.��A.�A-��A,E�A*��A)�
A(I�A&�A&-A$ȴA$5?A"~�A!�A!�A ��A�\A�\A�A�DA+A�wAA�A��A7LAffA��A~�A��A7LA��A�FAhsA
��A
ZA	ƨA	�wA	K�A�DAA/A-A�7A�9AjA��AƨAƨA��A��A�TA�^A�7A7LA�`A�HA��A9XA��A/A �9@�j@�-@���@�  @�ff@�@��\@���@�@�J@��A J@��w@���@�t�@�;d@�@�|�@�I�@�b@�E�@���@�z�@�5?@�C�@�ƨ@�@��@�~�@��@�`B@�hs@���@��m@��@�1'@�(�@���@�
=@݁@��;@�r�@�?}@�@���@ݲ-@�7L@��@�bN@��y@�p�@؃@׍P@֗�@��@���@�V@�@թ�@�`B@ԋD@��
@���@�Z@ӥ�@���@���@Ь@� �@�9X@��
@�@���@�v�@ͩ�@�p�@��`@̛�@��;@�v�@�x�@�7L@�%@��@ȴ9@ȣ�@�9X@�I�@��
@Ə\@�7L@���@ēu@�(�@���@�"�@�V@��T@��h@�@�$�@�J@�@�`B@�b@��@��y@�V@�ff@��!@���@���@�J@��@�x�@�?}@��/@��D@���@��@���@��
@�dZ@�K�@���@�=q@��@��#@�p�@��@��m@��@�K�@��!@�E�@�5?@�-@���@��-@��@���@���@��@��H@��T@���@��-@��#@�@��@�`B@��@��/@�9X@��@���@�t�@�+@�o@�ȴ@�v�@�-@��@��^@�X@��@�r�@���@�C�@��@��R@�v�@�V@�M�@�$�@���@�X@���@�r�@�1@�dZ@�;d@�"�@�o@�@���@���@�E�@�J@�@��@�@��9@�Q�@��m@��P@�33@�
=@���@��+@�ff@�M�@�-@��@���@�X@�%@��9@���@�(�@��m@�|�@��H@�=q@�E�@�ff@�M�@��@���@��@��@��D@�z�@�9X@�1@��
@�ƨ@��@�l�@��@��@��!@�ff@�V@�-@�{@��^@�p�@�7L@�V@�%@���@�Ĝ@��@��D@�1'@��;@�ƨ@��@�l�@��@�ff@�E�@��@��@��@��9@���@��@�A�@��m@�l�@��!@�v�@�J@�{@�M�@�=q@���@�p�@�7L@�O�@�X@�G�@�G�@�V@���@��@�I�@��@�(�@� �@��@��F@�dZ@�;d@��y@��\@�~�@�=q@��@��h@��/@���@��@��@��F@�l�@�S�@��@���@��+@�V@�E�@���@�x�@�`B@�G�@�?}@�7L@�/@��j@��@���@��@��@��m@��@�K�@�+@���@���@���@�G�@��`@��j@���@��u@�r�@� �@���@�ƨ@��P@�l�@�;d@��@���@�~�@�5?@�p�@�/@��@���@��`@��9@�bN@�  @+@~ȴ@~v�@~5?@}`B@}/@|��@|��@|(�@{C�@z��@{�@{dZ@z�H@z�@y�^@y��@yhs@x�9@x �@xb@xb@w�@wl�@v�y@vV@u�@u�@uV@tj@s�F@s"�@s@r��@rJ@q��@qG�@pĜ@p�@pA�@o�;@ol�@n��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�\)A�\)A�`BA�ZA�^5A�\)A�XA�^5A�ZA�^5A�\)A�\)A�^5A�XA�^5A�\)A�ZA�\)A�XA�\)A�VA�^5A�\)A�ZA�\)A�VA�\)A�\)A�XA�^5A�ZA�ZA�^5A�ZA�bNA�ZA�bNA�^5A�bNA�bNA�^5A�bNA�^5A�bNA�`BA�^5A�dZA�^5A�^5A�`BA�\)A�bNA�^5A�`BA�bNA�\)A�bNA�bNA�\)A�bNA�^5A�bNA�bNA�^5A�bNA�`BA�`BA�dZA�`BA�`BA�\)A�^5A�\)A�\)A�dZA�\)A�bNA�\)A�\)A�`BA�\)A�`BA�^5A�bNA�`BA�`BA�bNA�\)A�bNA�^5A�bNA�^5A�`BA�bNA�^5A�dZA�^5A�dZA�bNA�bNA�bNA�\)A�dZA�^5A�`BA�^5A�^5A�bNA�^5A�dZA�`BA�bNA�bNA�`BA�dZA�`BA�ffA�bNA�ffA�ffA�bNA�hsA�bNA�ffA�`BA�ffA�bNA�`BA�ffA�ffA�dZA�jA�dZA�hsA�hsA�dZA�jA�ffA�jA�ffA�hsA�jA�ffA�l�A�ffA�jA�jA�hsA�l�A�ffA�jA�l�A�ffA�jA�jA�dZA�jA�ffA�hsA�jA�dZA�dZA�bNA�bNA�ffA�^5A�bNA�ffA�bNA�dZA�ffA�XA�S�A�VA�ZA�O�A�O�A�S�A�K�A�K�A�I�A�O�A�S�A�G�A�I�A�I�A�E�A�G�A�G�A�A�A�C�A�G�A�M�A�Q�A�M�A�S�A�Q�A�M�A�I�A�G�A�E�A� �A�+A�VA���A��`A���A���A�ĜA޴9A޲-A���A�n�A�ffA�x�A�C�A�l�A�p�A�-A��/A۶FA۴9A۰!A۝�Aۣ�A۝�AۓuA�v�A�jA�ffA�S�A�=qA�&�A�oA���A��`A���A���A���Aڲ-Aڧ�Aڥ�Aڙ�Aڗ�Aڗ�AړuAڇ+AځA�|�A�r�A�jA�hsA�dZA�VA�VA�O�A�=qA�"�A�A��TA�ƨAپwAٲ-Aٲ-Aٲ-Aٴ9AٸRAٰ!Aٗ�AٍPA�z�A�|�A�z�A�v�A�r�A�hsA�A�A�;dA�(�A� �A�AخAؗ�A؟�A؛�AؓuA؉7A�x�A�jA�XA�K�A�&�A�$�A�1A��`A��;A�ȴA׉7A�ZA��A��A�Aֲ-A֝�AցA�x�A�I�A�"�A�
=A���A�AՍPA�z�A�\)A�A�A�"�A���A��;Aԝ�A�v�A�`BA�K�A�;dA�{A���Aӡ�A�jA�5?A�x�A�l�A�E�A�9XA��A�AѴ9A�I�A�=qA��A��A���AБhA�^5A�A�A�1'A���A���AϬA�ffA�XA�O�A�G�A�?}A�C�A�M�A�Q�A�XA�E�A�/A��A�A��yA���AάA�9XA��A��A�A��A���A;wAͧ�A͍PA͉7A�z�A�VA���A̼jA̝�A�l�A�^5A�9XA� �A��A��A��;A�ƨAˬA�l�A��A���A���AʸRAʧ�A�v�A�?}A�1A��AɾwAɣ�A�p�A�K�A�;dA�1'A�(�A�$�A��A�bA���A���A�ĜAȬAȏ\A�O�A�A��A��/A���AǶFAǲ-Aǰ!Aǥ�Aǝ�AǛ�AǑhA�ffA�E�A�7LA��A�oA�VA�VA�JA�
=A�A�%A���A��A��;Aƺ^A�p�A���A���AŰ!A�ffA��AēuA�=qA���A�$�A�A��A��A��`A��#A���A���A���A�ĜA���A¶FA\A�t�A�VA�-A��A��hA�r�A�O�A�5?A��;A���A��A�XA�1'A�ȴA�E�A��A���A��mA��/A���A��-A��\A�{A��9A��DA�^5A��A��A��wA���A���A���A��7A�jA�XA�A�A�&�A���A���A��jA���A��\A�x�A�l�A�^5A�5?A��A��hA�/A��A�Q�A�%A�"�A���A�E�A��yA���A���A��A�K�A��A��A�ƨA��A���A���A��+A�t�A�=qA�  A�^5A�A��PA�K�A���A��yA���A�ĜA���A��+A�|�A�x�A�p�A�hsA�bNA�^5A�VA�M�A�C�A�$�A�JA�  A��A��#A���A�ȴA��wA��A���A���A���A���A��\A�~�A�p�A�dZA�S�A�M�A�M�A�C�A�?}A�7LA��A�  A��A��/A���A��RA���A��DA�|�A�r�A�jA�ZA�(�A���A��`A��
A���A���A��DA�x�A�jA�bNA�S�A�A�A�1'A�(�A�$�A�{A�%A��A��yA��HA��;A��;A��A�ƨA��FA���A���A��PA�z�A�bNA�/A���A��!A���A�v�A�n�A�p�A�n�A�ffA�Q�A�E�A�9XA�/A�&�A��A��A�oA�bA�oA�oA�A��`A�ƨA��9A���A��PA�~�A�n�A�I�A�
=A���A���A�t�A�9XA��A���A��#A���A��!A���A��hA�l�A�A�A�&�A�
=A���A���A��HA��
A���A���A�ĜA��jA���A���A��hA�|�A�G�A��A�x�A�(�A��A�ĜA��uA�jA�K�A�9XA�/A�(�A��A�oA�A��A��TA���A��A���A�z�A�r�A�hsA�C�A��A�
=A��mA���A�7LA��HA��wA���A�z�A�XA�5?A�
=A��TA���A���A�p�A�?}A��A��RA���A��uA��DA��A�z�A�n�A�^5A�O�A�;dA�$�A��A�A��;A���A�;dA��-A�jA�=qA��A���A��jA���A�|�A�Q�A�"�A��A��mA��HA��/A��
A���A�ƨA�ffA�A���A��!A���A�|�A�Q�A�?}A�9XA�-A� �A��A��A�oA�JA�JA�1A�A�A���A���A���A���A��A��TA��A���A���A�ƨA���A��^A��FA��9A���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BJ�BK)BJ�BK^BJ�BK^BK)BJ�BK)BJ�BJ�BJ�BJ�BJXBJ�BJXBJ#BJXBJ�BJ�BJ#BJ�BK)BK)BK)BK^BK^BK^BK^BJ#BI�BF�BFtBE9BDgB<�B.�B�B
�VB
��B
�"B
�+B
�iB
��B
��B�BxB-�B8�B5�B0UB7�B;�BA BG�BW
Bc�BwfB��B�B��B��B�B �B iB�B�BMBuB_B�B"�B!-B+B4�B0�B>B<jB<6B>�BN<BEmBK�BH�BEBB�BA�B?�B=�B:�B:^B6B2�B0�B-�B&�B.IB)�BIBB�cB�B�?B�BB�wB�$B�LB�4B�1BbNBQNB>B0�B#�B!�B+B%B
�B
�mB
چB
�aB
�tB
��B
�\B
��B
rGB
Y�B
FtB
>wB
,B
�B
hB	�B	�B	��B	��B	�-B	��B	�	B	�iB	r�B	e`B	O�B	>BB	6FB	,�B	$�B	oB��B�B�vB�cB�B��BٴBרB��B�&B��BȀB�tB�mB��B��B�B�B�LB��B�hB�!B��B��B��B��B��B��B�@B�bB��B�kB��B�7B�@B�bB��B�B�MB��B�SB��B��B�=B��B�xB�IB�B��B��B��B��B��B��B��B��B��B�RB��B�B�UB��B�B��B�[B�HB�B��B��B�B�B�BB�pB�B��B��B�HB�&B��B�[B�B�oB	�B	#�B	*0B	CaB	MjB	IRB	?HB	?}B	C-B	T�B	d�B	m�B	jKB	iB	i�B	n/B	jB	xB	{B	z�B	zB	x�B	wfB	{JB	}"B	}�B	n/B	b�B	kB	^5B	`vB	S�B	M6B	Q�B	f2B	h�B	y>B	}�B	cB	.B	}�B	|�B	v�B	v�B	t�B	tTB	u�B	.B	�B	� B	��B	��B	��B	�B	��B	��B	��B	�"B	�(B	��B	�@B	��B	��B	��B	�CB	�~B	�B	��B	��B	�VB	��B	�\B	��B	�~B	�IB	��B	�VB	�:B	��B	�@B	��B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	�eB	��B	�$B	��B	�0B	�dB	��B	��B	��B	��B	��B	�wB	�UB	�gB	�aB	��B	�mB	��B	��B	̘B	��B	�HB	ҽB	�sB	�B	�yB	�)B	�QB	�WB	�]B	ܒB	�dB	��B	�WB	�]B	ݘB	ܒB	�)B	��B	��B	یB	یB	�QB	چB	��B	یB	��B	��B	��B	�;B	�B	�B	��B	�B	�B	�|B	��B	�ZB	�fB	��B	�2B	��B	�mB	�mB	��B	��B	�>B	�DB	��B	�B	�WB	�)B	�B	� B	� B	��B	�5B	�B	�B	�B	�B	��B	�ZB	��B	�ZB	��B	��B	��B	�2B	��B	��B	�B	�VB	�(B	��B	�B	��B	�(B	��B
 4B
;B
AB
B
uB
�B
B
GB
MB
{B
B
MB
YB
�B
�B
�B
	lB
	lB

�B

�B

�B

=B
�B
DB

rB

�B
B
DB
B

�B
B
B
JB
�B
�B
(B
.B
bB
�B
�B
�B
�B
�B
�B
B
:B
�B
B
B
FB
FB
FB
FB
{B
�B
�B
�B
B
YB
_B
�B
�B
_B
1B
7B
�B
�B
�B
~B
 �B
"4B
#nB
$@B
$�B
&�B
)*B
)�B
*0B
*0B
)�B
*eB
)_B
)�B
*�B
,B
,qB
-B
-wB
-�B
-�B
,�B
,�B
-�B
-wB
/B
.IB
/B
0�B
1[B
2aB
2-B
2aB
2�B
2�B
3�B
3hB
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4nB
5�B
6zB
6FB
6B
6B
5�B
6�B
6zB
6FB
8B
8�B
8�B
9�B
9�B
9�B
9�B
:*B
:^B
;dB
;�B
;�B
<B
;�B
<�B
=qB
=�B
=�B
>wB
?HB
?}B
?HB
?�B
@�B
A B
A B
A B
@�B
A B
AUB
A B
A�B
A�B
B'B
A�B
B'B
A�B
A�B
B[B
B[B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
EmB
F?B
F�B
GB
F�B
F�B
GB
GEB
GEB
GzB
GEB
F�B
GB
GzB
HKB
HKB
HKB
H�B
H�B
IB
I�B
I�B
JXB
JXG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BJ�BK^BK�BI�BLdBI�BJ�BK�BI�BL0BJXBK�BK�BJXBL�BJ#BK^BK^BJ�BK�BJXBLdBI�BK)BK^BJ�BL�BI�BJ�BLdBJ#BK�BK�BJ�BK)BIBL�BI�BK^BJXBJXBK�BI�BJ�BJXBJ�BK)BI�BJ�BK)BI�BK�BI�BK�BJXBI�BJ�BI�BI�BK�BIRBJ�BJ#BI�BK�BI�BK�BK)BI�BL0BIRBJXBI�BJ�BJ�BIBL�BH�BJ�BJ�BI�BK)BI�BJ�BI�BJ�BJ�BIRBJ�BIBK)BI�BK^BJ�BJ#BK)BH�BK�BI�BK^BJ�BK)BJXBHKBK�BJXBJ�BJ#BH�BK)BI�BK^BJ�BJXBK�BJXBK)BJ#BL0BJ�BJ�BL�BJXBL�BJ�BK�BJ#BK)BK^BJXBI�BL�BJ�BK�BK)BJ�BL�BJXBL0BJXBK�BK)BJ#BL�BJXBL0BK)BJ�BK�BJ�BL0BK^BJXBL0BK^BK)BL�BJ#BK)BJ�BJ�BL�BJXBJ�BK)BHBK^BI�BH�BJ�BI�BI�BK�BHKBI�BHBH�BD3BF?BI�BDgBF�BE�BEmBIRBD�BD3BFtBF�BD�BFtBDgBC�BD3BD3BD�BC-BF?BE�BFBE9BA�BF�B4nB6�BEB;0B9�B.B5tB+�B/B/�BDgB#nB�B'�B/�B	lBMB�B
��B
��B
��B
��B
��B
��BB
��B
��B
��B
��B
��B
�PB
�2B
�2B
�2B
�.B
��B
��B
��B
�B
��B
��B
��B
�TB
�`B
�ZB
��B
�;B
��B
��B
�/B
�/B
��B
�yB
��B
�B
�B
�8B
��B
�B
�B
�TB
��B
�B
��B
��BB~B�B4B\B�B B�B@BkBB�B"4B,�B<�B'�B)�B,qB0!B-B1'B33B8�B9$B8�B8B:�B:*B5tB5B7�B8�B4�B3�B0�B.�B.�B/OB-�B5tB4B1�B2�B9$B6FB4�B:^B6B:�B9�B;�B@�B<�B:�B;�B8�B@�BC�B>B?BB'BL0B6�B@�B?}BC�BB[BO�BMjBLdBR�BR�BY�BX�BV�BW
BW
B[#BXBa|Bf�Be,Be�Bg�Bm�Bk�BncBrGBy	B}�B��B�fB��B�:B��B�:B�'B��B�B�}B��B��B��B��B�FB��B��B��B��B�B�wB��BǮB��B�TB��B�jB� B��B�"B�+B�PB�B�B�B�8B�B�B�.BB iB�(B�BAB �B��B  B 4B�cB��BAB�B��B�B�BB�BSB�B�B�B{BBBMBB�B�B�BMBB�BMB�B �B �BoB�cBB�BBSB�B�BDBDB�B�BCB�B)�B1�B"hB!�B"4B 'B!bB!bB�B �B �BVB�B"�B#�B$B'B/B/B&�B(�B-wB4�B/�B,qB1�B/�BB�B?HB2aB5B/�B/B.B0�B0UBB�B?�B;�BB[BEmB=BB'B;dB:*B<B<�B;�B9$B<6B:�B>wB<�B:�B>BB>B=�B9$B;�BA�BC-BHKBH�BO�BB�BHBsBB�BO�BC-BC�BB�BB�BC�BDgBB�B?�BK^BV�BIRBA�BC-BMjBC-Ba�BQ�BK�BH�BPHBH�BGBHBHBE�BGBE�BE�BEmBE�BC�BDgBE9BE�BFtBFBD3BD�BE9BB[BB'BB�BD�BB[BA�B@�B@�BC-BD�BB�BB'BC�BA�B@�BA�BA B@BB�BC�BA�B@B?}B@B@�B@�B?�B>wB=<B=�BE9B@�B?}B=<B>B?�B>B=�B=<B=B;�B<jB=<B;0B:�B;0B<�B<B9XB:^B9�B8B8�B:�B:*B8B8RB9�B9�B9�B>BB=�B9$B7�B9XB5tB4�B49B4�B6�B5�B4�B3�B2aB2�B2-B2�B1�B1'B0�B2aB4nB3�B/�B/B/�B-�B-CB3�B4nB33B/�B2-B1�B-CB.�B,�B-CB+�B+�B-CB.�B1'B+kB.B(XB&�B,=B&LB&LB$�B%zB&�B&LB&LB$�B$�B*0B,=B6FB0UB,�B-�B/OB/�B/OB-CB,�B,=B+kB*�B*eB)_B($B)*B&�B'RB%FB!-B!�B%�B �BB�B �B#�B�B�B�BB�BbB\B�BJB4B�BB�B+B�B�BB �B  B iB iB�.B iB��B�B��B 4B�(B  B  B��B��B��B�B��B��B�B�,B�TB�BٴB�BרB՛B��B�B�BٴB�&B�B��B�dB�pB�EB�BǮB��BĜB�B�9B�gB�aB�gB�aB��B��B�[BBB��B��B��B�UB��B��B�B��B�B��B��44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                        44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                        44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022100521062920221005210629IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022101515312420221015153124QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022101515312420221015153124QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      PRES                            D�� G�O�D���G�O�@@  G�O�Valu passes DMQC                SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      TEMP                            D�� G�O�D���G�O�@�  G�O�Valu passes DMQC                SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                