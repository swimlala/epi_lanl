CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-05-06T19:17:06Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  Rp   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  W�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  m4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  r�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \  �P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 \    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   $8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   *8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T 08   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   0�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   0�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   0�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   0�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 0�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   1,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   1H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    1P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        1p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        1x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       1�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    1�Argo profile    3.1 1.2 19500101000000  20220506191706  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_211                 6810_008521_211                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�����@�����11  @��� ѷ@��� ѷ@0��9C@0��9C�d��j��d��j�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�@@  @�  @�  @�G�@�  @��RA\)A   A+�A?\)A^�RA�  A��A��A�  A�  A�Q�A�  A�  B (�B�
B  B�
B�B(  B0(�B8  B@  BH(�BP(�BX  B_�Bg�
Bp  Bx(�B�{B��B�  B�{B��B��B�  B�  B�{B�  B�{B�=qB�{B�{B�{B��B��B��B��B�{B�{B�(�B�(�B�(�B�{B�  B��
B�  B�  B�  B�  B�  C   C  C
=C  C��C	�C  C{C{C{C
=C{C
=C��C  C
=C   C"  C$
=C%��C'�C*  C+��C-��C/��C2  C4
=C6  C8
=C:
=C<
=C>
=C?��CB  CD  CF
=CH{CJ  CL  CN  CP
=CR  CT  CV  CW��CZ  C\  C^
=C`
=Ca��Cd  Cf
=Cg��Cj
=Cl
=Cn  Co�Cq��Ct  Cv  Cx  Cy��C|  C~  C��C���C���C�  C�C�C�
=C�C�C�C�C�  C���C�  C�  C�  C�C�C�
=C�C���C�C�C�  C�  C���C���C�  C�  C�C���C���C���C�  C�  C���C�
=C�C�  C�C�  C�C�
=C�
=C�  C���C���C���C���C���C�  C�C�  C�C���C���C�C�C���C���C���C�  C�C�C�  C�  C���C���C���C�  C�
=C�
=C�  C�C�C�
=C�  C���C���C�  C�  C�C�C���C�  C�C�  C���C���C�C�  C�  C�  C�  C�  C���C���C���C���C���C��C���C�  C�  C���C�  C�C�
=C���C���C�  C���C���C�C�C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C���D �D ��D  D� D�qD� D  D��D�D}qD�qD�D�D��DD�D  D� D�qD	��D
�D
}qD
�qD� D�qD}qD  D� D�D��D  D� D�qD� D  D}qD  D��D  Dz�D�qD��D  D� D�qD}qD�D� D�qD� D�D��D�qD}qD�qD� DD��D�D�D�D}qD  D� D   D � D �qD!� D"�D"� D#  D#� D$  D$��D%  D%}qD%�qD&� D'�D'� D(�D(��D)D)��D)�qD*}qD+�D+��D,  D,}qD-  D-� D-�qD.� D.�qD/z�D0  D0}qD0�qD1}qD2  D2� D3  D3� D4  D4��D5  D5z�D5�qD6� D7  D7��D8  D8� D8�qD9z�D9��D:z�D:�qD;z�D;�qD<� D=  D=��D>D>��D>�qD?� D@�D@��D@�qDA}qDB  DBz�DB�qDC� DDDD��DE  DE� DE�qDF��DG�DG� DH  DH��DI  DI}qDJ  DJ� DK  DK}qDL�DL��DM  DM�DNDN��DO  DO}qDO�qDP� DQDQ��DR  DR}qDR�qDS� DT�DT�DUDU� DU�qDV� DW  DW}qDX  DX��DY  DY��DZDZ��D[  D[� D[�qD\}qD]  D]}qD^  D^� D^��D_}qD_�qD`��Da�Da��Db�Db� Dc  Dc�Dd�Dd��De  De��Df�Df� Dg�Dg� Dh  Dh� Dh�qDi� Dj�Dj� Dk  Dk� Dk��Dlz�Dm  Dm��Dn  Dn� DoDo� Dp�Dp��Dq  Dq� Dq�qDr}qDs  Ds}qDs�qDt}qDt��Du}qDv�Dv� Dw�Dw�Dx  Dx}qDy  Dy� Dz�Dz��D{  D{��D|�D|��D}  D}��D~�D~}qD~�qD��D�HD�@ D�~�D���D���D�>�D�� D���D�  D�@ D�~�D��qD���D�>�D�~�D���D�  D�AHD��HD�� D�HD�B�D��HD�� D�  D�AHD��HD���D���D�@ D�~�D�� D�  D�>�D�� D�� D���D�@ D���D��HD�HD�B�D���D�D�HD�>�D�~�D���D���D�>�D�~�D���D���D�AHD�c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?aG�?�  ?��R?�{?���?��@   @
=q@(�@#�
@333@B�\@J=q@^�R@s33@}p�@�ff@��@�
=@�p�@��@��@��@�(�@\@�ff@�\)@�
=@�(�@��
@�{@�@�p�A33AQ�A��A  Az�A��A(�A   A%A(Q�A*�HA0  A5�A7�A;�A@  AC�
AFffAK�AP  AQ�AW
=AZ�HA]p�A`��AfffAh��Ak�Ap��Atz�AvffAz�HA\)A���A�=qA�z�A�ffA��A�G�A��A�A�
=A���A��A���A��RA���A��HA���A��A���A��
A�{A�\)A���A�(�A�ffA�  A��A���A�{A�Q�A��\A��A�ffA�G�A��
A�p�AǮAʏ\A�(�A�{A���A�33A��A�  A�=qAۅA�ffA���A�\A�z�A�
=A�G�A�\A�p�A�A�G�A��
A�ffA�  A��\A��A�ffB Q�BB�\B�B��B�B�\B�
B��B	B33BQ�B��B{B�B��Bp�B�RB  B��BB
=BQ�B�B�B\)Bz�BG�BffB�B ��B!��B"�HB$Q�B%G�B&{B'\)B(��B)B*�\B+�B,��B.ffB/33B0  B1G�B2�\B3�B4Q�B5��B6�HB8  B8��B9B;33B<(�B<��B=B?
=B@Q�B@��BABC33BD  BD��BF{BF�HBG�BH��BJ{BJ�RBK�BL��BM�BN�\BO\)BPz�BQ��BR=qBR�HBT  BT��BUBV�\BW\)BXz�BY��BZ{BZ�HB\(�B\��B]��B^ffB_�B`��Ba�Ba�Bb�HBd(�BeG�Be�Bf�HBh  BiG�Bi�Bj�HBl(�BmG�BnffBo
=Bp  Bq�BrffBs\)Bt(�BuG�BvffBw�BxQ�ByG�Bz�\B{�B|Q�B}��B~�HB�B�=qB���B�p�B�  B�Q�B���B�p�B�  B��\B���B�p�B�(�B��RB��B��B�Q�B���B�p�B��B�ffB�
=B��B�Q�B���B�33B��
B�z�B��B���B�{B��RB�p�B�  B��\B�
=B��B�=qB���B�\)B��
B�z�B�33B��B�(�B���B��B�{B��\B�
=B���B�Q�B�
=B�p�B��B�z�B�33B��B�z�B���B��B�=qB��HB�\)B��B���B�\)B��
B�Q�B���B��B�=qB���B�\)B�  B���B�G�B�  B���B��B��B�Q�B�
=B�B�=qB���B�\)B�{B���B��B�{B���B�G�B�  B��RB�\)B��B�ffB��B��B���B��B��B�ffB��B�B�Q�B���B���B�Q�B���B�p�B�{B���BÅB�{Bģ�B�\)B�{B���B�G�B��
Bȏ\B�G�B�  B�z�B�
=BˮB�ffB��B͙�B�(�B���BυB�(�BиRB�G�B��BҸRB�p�B�{Bԣ�B�33B��BָRB�p�B�{BظRB�33B�  B���B�G�B��Bܣ�B�\)B��B�z�B��B��B��B�33B��
B�z�B��B��B�\B�33B�B�ffB�33B��
B�Q�B��HB�B�Q�B��HB�\)B�{B���B�p�B�  B�z�B��B��B��B��B�B�z�B��B��B�\B��B��B�z�B��B���B�(�B��HB��B�{B��\B��B��
B�z�B��B���B�{B��RB�p�C 
=C \)C ��C �C(�CffC��C�CffC��C�C33C��C�C=qCz�C�RC�Cp�C�RC��CG�C��C��CG�C�C��C(�C�\C�HC	(�C	ffC	C
{C
p�C
��C  CG�C��C��CQ�C�\C��C�CffC��C�CffC�C��CG�C��C�CG�C�\C�
C{CffCC�Cp�C�C��CQ�C�C
=CG�C�C�HC=qC��C�CG�C�C��C�Cz�C�
C(�Cz�C�RC�Cz�CC
=CQ�C�C  C\)C�C  CQ�C��C��CQ�C�C{CffC�RC�Cp�C�RC   C \)C �RC!�C!z�C!C"{C"\)C"�RC#(�C#�C#�C$=qC$��C$�HC%=qC%��C%��C&Q�C&�C'  C'\)C'�RC({C(p�C(�
C)=qC)��C)�C*=qC*��C*�C+Q�C+�C,
=C,p�C,�HC-33C-�\C-�HC.33C.�\C.�HC/33C/�C/�
C033C0�\C0�C1G�C1�C2
=C2\)C2�RC3{C3p�C3��C4�C4z�C4��C5(�C5z�C5�
C633C6�C6�C7G�C7��C7��C8Q�C8��C9  C9\)C9�RC:
=C:\)C:�RC;{C;p�C;��C<�C<z�C<��C=(�C=�C=�HC>33C>�\C>�C?=qC?��C@  C@Q�C@�CA
=CAp�CACB{CBffCB�RCC
=CC\)CC��CC�CD33CD�CDCE
=CEG�CE�CECE��CF(�CF\)CF�\CFCF��CG=qCGp�CG��CG�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�=q@�@@  @�  @�  @�G�@�  @��RA\)A   A+�A?\)A^�RA�  A��A��A�  A�  A�Q�A�  A�  B (�B�
B  B�
B�B(  B0(�B8  B@  BH(�BP(�BX  B_�Bg�
Bp  Bx(�B�{B��B�  B�{B��B��B�  B�  B�{B�  B�{B�=qB�{B�{B�{B��B��B��B��B�{B�{B�(�B�(�B�(�B�{B�  B��
B�  B�  B�  B�  B�  C   C  C
=C  C��C	�C  C{C{C{C
=C{C
=C��C  C
=C   C"  C$
=C%��C'�C*  C+��C-��C/��C2  C4
=C6  C8
=C:
=C<
=C>
=C?��CB  CD  CF
=CH{CJ  CL  CN  CP
=CR  CT  CV  CW��CZ  C\  C^
=C`
=Ca��Cd  Cf
=Cg��Cj
=Cl
=Cn  Co�Cq��Ct  Cv  Cx  Cy��C|  C~  C��C���C���C�  C�C�C�
=C�C�C�C�C�  C���C�  C�  C�  C�C�C�
=C�C���C�C�C�  C�  C���C���C�  C�  C�C���C���C���C�  C�  C���C�
=C�C�  C�C�  C�C�
=C�
=C�  C���C���C���C���C���C�  C�C�  C�C���C���C�C�C���C���C���C�  C�C�C�  C�  C���C���C���C�  C�
=C�
=C�  C�C�C�
=C�  C���C���C�  C�  C�C�C���C�  C�C�  C���C���C�C�  C�  C�  C�  C�  C���C���C���C���C���C��C���C�  C�  C���C�  C�C�
=C���C���C�  C���C���C�C�C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C���D �D ��D  D� D�qD� D  D��D�D}qD�qD�D�D��DD�D  D� D�qD	��D
�D
}qD
�qD� D�qD}qD  D� D�D��D  D� D�qD� D  D}qD  D��D  Dz�D�qD��D  D� D�qD}qD�D� D�qD� D�D��D�qD}qD�qD� DD��D�D�D�D}qD  D� D   D � D �qD!� D"�D"� D#  D#� D$  D$��D%  D%}qD%�qD&� D'�D'� D(�D(��D)D)��D)�qD*}qD+�D+��D,  D,}qD-  D-� D-�qD.� D.�qD/z�D0  D0}qD0�qD1}qD2  D2� D3  D3� D4  D4��D5  D5z�D5�qD6� D7  D7��D8  D8� D8�qD9z�D9��D:z�D:�qD;z�D;�qD<� D=  D=��D>D>��D>�qD?� D@�D@��D@�qDA}qDB  DBz�DB�qDC� DDDD��DE  DE� DE�qDF��DG�DG� DH  DH��DI  DI}qDJ  DJ� DK  DK}qDL�DL��DM  DM�DNDN��DO  DO}qDO�qDP� DQDQ��DR  DR}qDR�qDS� DT�DT�DUDU� DU�qDV� DW  DW}qDX  DX��DY  DY��DZDZ��D[  D[� D[�qD\}qD]  D]}qD^  D^� D^��D_}qD_�qD`��Da�Da��Db�Db� Dc  Dc�Dd�Dd��De  De��Df�Df� Dg�Dg� Dh  Dh� Dh�qDi� Dj�Dj� Dk  Dk� Dk��Dlz�Dm  Dm��Dn  Dn� DoDo� Dp�Dp��Dq  Dq� Dq�qDr}qDs  Ds}qDs�qDt}qDt��Du}qDv�Dv� Dw�Dw�Dx  Dx}qDy  Dy� Dz�Dz��D{  D{��D|�D|��D}  D}��D~�D~}qD~�qD��D�HD�@ D�~�D���D���D�>�D�� D���D�  D�@ D�~�D��qD���D�>�D�~�D���D�  D�AHD��HD�� D�HD�B�D��HD�� D�  D�AHD��HD���D���D�@ D�~�D�� D�  D�>�D�� D�� D���D�@ D���D��HD�HD�B�D���D�D�HD�>�D�~�D���D���D�>�D�~�D���D���D�AHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?aG�?�  ?��R?�{?���?��@   @
=q@(�@#�
@333@B�\@J=q@^�R@s33@}p�@�ff@��@�
=@�p�@��@��@��@�(�@\@�ff@�\)@�
=@�(�@��
@�{@�@�p�A33AQ�A��A  Az�A��A(�A   A%A(Q�A*�HA0  A5�A7�A;�A@  AC�
AFffAK�AP  AQ�AW
=AZ�HA]p�A`��AfffAh��Ak�Ap��Atz�AvffAz�HA\)A���A�=qA�z�A�ffA��A�G�A��A�A�
=A���A��A���A��RA���A��HA���A��A���A��
A�{A�\)A���A�(�A�ffA�  A��A���A�{A�Q�A��\A��A�ffA�G�A��
A�p�AǮAʏ\A�(�A�{A���A�33A��A�  A�=qAۅA�ffA���A�\A�z�A�
=A�G�A�\A�p�A�A�G�A��
A�ffA�  A��\A��A�ffB Q�BB�\B�B��B�B�\B�
B��B	B33BQ�B��B{B�B��Bp�B�RB  B��BB
=BQ�B�B�B\)Bz�BG�BffB�B ��B!��B"�HB$Q�B%G�B&{B'\)B(��B)B*�\B+�B,��B.ffB/33B0  B1G�B2�\B3�B4Q�B5��B6�HB8  B8��B9B;33B<(�B<��B=B?
=B@Q�B@��BABC33BD  BD��BF{BF�HBG�BH��BJ{BJ�RBK�BL��BM�BN�\BO\)BPz�BQ��BR=qBR�HBT  BT��BUBV�\BW\)BXz�BY��BZ{BZ�HB\(�B\��B]��B^ffB_�B`��Ba�Ba�Bb�HBd(�BeG�Be�Bf�HBh  BiG�Bi�Bj�HBl(�BmG�BnffBo
=Bp  Bq�BrffBs\)Bt(�BuG�BvffBw�BxQ�ByG�Bz�\B{�B|Q�B}��B~�HB�B�=qB���B�p�B�  B�Q�B���B�p�B�  B��\B���B�p�B�(�B��RB��B��B�Q�B���B�p�B��B�ffB�
=B��B�Q�B���B�33B��
B�z�B��B���B�{B��RB�p�B�  B��\B�
=B��B�=qB���B�\)B��
B�z�B�33B��B�(�B���B��B�{B��\B�
=B���B�Q�B�
=B�p�B��B�z�B�33B��B�z�B���B��B�=qB��HB�\)B��B���B�\)B��
B�Q�B���B��B�=qB���B�\)B�  B���B�G�B�  B���B��B��B�Q�B�
=B�B�=qB���B�\)B�{B���B��B�{B���B�G�B�  B��RB�\)B��B�ffB��B��B���B��B��B�ffB��B�B�Q�B���B���B�Q�B���B�p�B�{B���BÅB�{Bģ�B�\)B�{B���B�G�B��
Bȏ\B�G�B�  B�z�B�
=BˮB�ffB��B͙�B�(�B���BυB�(�BиRB�G�B��BҸRB�p�B�{Bԣ�B�33B��BָRB�p�B�{BظRB�33B�  B���B�G�B��Bܣ�B�\)B��B�z�B��B��B��B�33B��
B�z�B��B��B�\B�33B�B�ffB�33B��
B�Q�B��HB�B�Q�B��HB�\)B�{B���B�p�B�  B�z�B��B��B��B��B�B�z�B��B��B�\B��B��B�z�B��B���B�(�B��HB��B�{B��\B��B��
B�z�B��B���B�{B��RB�p�C 
=C \)C ��C �C(�CffC��C�CffC��C�C33C��C�C=qCz�C�RC�Cp�C�RC��CG�C��C��CG�C�C��C(�C�\C�HC	(�C	ffC	C
{C
p�C
��C  CG�C��C��CQ�C�\C��C�CffC��C�CffC�C��CG�C��C�CG�C�\C�
C{CffCC�Cp�C�C��CQ�C�C
=CG�C�C�HC=qC��C�CG�C�C��C�Cz�C�
C(�Cz�C�RC�Cz�CC
=CQ�C�C  C\)C�C  CQ�C��C��CQ�C�C{CffC�RC�Cp�C�RC   C \)C �RC!�C!z�C!C"{C"\)C"�RC#(�C#�C#�C$=qC$��C$�HC%=qC%��C%��C&Q�C&�C'  C'\)C'�RC({C(p�C(�
C)=qC)��C)�C*=qC*��C*�C+Q�C+�C,
=C,p�C,�HC-33C-�\C-�HC.33C.�\C.�HC/33C/�C/�
C033C0�\C0�C1G�C1�C2
=C2\)C2�RC3{C3p�C3��C4�C4z�C4��C5(�C5z�C5�
C633C6�C6�C7G�C7��C7��C8Q�C8��C9  C9\)C9�RC:
=C:\)C:�RC;{C;p�C;��C<�C<z�C<��C=(�C=�C=�HC>33C>�\C>�C?=qC?��C@  C@Q�C@�CA
=CAp�CACB{CBffCB�RCC
=CC\)CC��CC�CD33CD�CDCE
=CEG�CE�CECE��CF(�CF\)CF�\CFCF��CG=qCGp�CG��CG�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A҅A҇+A҇+AґhAґhAґhAғuAґhAғuAғuAґhAғuAҕ�Aҗ�Aҕ�Aҙ�Aҝ�Aҝ�Aҡ�Aҟ�Aҡ�Aҥ�Aҥ�Aҡ�Aҝ�Aҙ�A�r�A�jA�K�A�E�A�+A�$�A��;A�|�A��;A�ZA�A�A��A���A̸RA�G�A˸RA�-A�p�Aɰ!A�-A��A�1A�t�A�9XA�/A��yA���A�K�Aģ�Aá�AA��7A��A�1'A�jA��#A�Q�A�G�A�9XA�bA�jA�M�A��RA�Q�A�t�A�Q�A�t�A�
=A��A��A���A���A�7LA��RA���A�hsA��A���A�ȴA�C�A�A��uA�33A�JA���A�C�A���A�{A��A���A���A�;dA��A��A�33A���A�bNA�5?A�-A�^5A��^A�$�A�oA��A�r�A���AxffAs33Ap1'An��Al�AjVAd�Ab�RAat�A_l�A^bNA[�AV��ASdZAR  AQp�AP��ANjAK�AI�AG�AF�uAC�AB{A?C�A;��A:ffA9��A6z�A4�uA49XA3�-A3A/�
A-�wA-"�A+��A)�A'7LA&~�A$bA"$�A!;dA �A��AA�A�9A�+AQ�A�A+AI�A�/AJA(�A
=AVA��AdZA��AQ�A�A
1A	�
A	��A�9A�#A��A~�A�9A�yA�HAv�A��AhsAI�A��A��A��AƨA�PAt�A?}A �RA 5?@��@���@�5?@���@�bN@��;@�l�@��y@��#@���@�ff@��7@��j@�t�@��@�@���@�p�@�p�@��@��`@�u@�1'@��@���@���@��-@�V@�|�@띲@�9X@�A�@���@��@蛦@�+@�{@���@�@��@�"�@◍@�hs@�/@�dZ@��#@�/@�A�@���@�bN@�dZ@��@�V@мj@�?}@Ϯ@�ȴ@�{@͉7@�7L@���@�Ĝ@���@�M�@�/@���@���@�J@�@��@�@�x�@�Ĝ@��@Ƈ+@�5?@�@�@�/@��@�ƨ@��
@��m@��m@���@þw@�S�@�;d@�C�@�S�@�\)@�;d@�t�@öF@�ƨ@��
@���@�j@���@��@�n�@���@��@��`@��9@��D@��D@��@�r�@�I�@��@���@�dZ@�C�@���@�E�@�hs@�Ĝ@�Q�@�  @��m@��@�dZ@�@�^5@�^5@�~�@���@���@�~�@�{@�Q�@�dZ@��H@�-@�{@�$�@��@�^5@��+@�@�J@��T@��-@��h@��@�x�@�p�@�p�@�G�@��@��;@��@�+@��y@�~�@�E�@�5?@�-@�$�@�J@���@�&�@���@��u@�1'@��@���@�ƨ@��F@���@�|�@�K�@�+@��H@���@�ff@�E�@�{@��T@���@�`B@�7L@��@��`@��@���@�o@���@�-@�@���@��7@�X@�/@��9@���@���@�;d@���@��!@�v�@�V@�$�@��T@��h@�x�@��@���@��9@���@�r�@�9X@�1@��@��F@��H@��!@�ff@�$�@��#@�@��-@��7@���@��D@�r�@�r�@�r�@�bN@�(�@�\)@���@���@�ff@�=q@�-@��@��@�@��^@��7@�/@���@��/@���@�bN@�Q�@�b@�C�@�33@��y@���@�v�@�ff@�^5@�5?@��@���@�x�@�V@��@�1'@�dZ@�C�@�33@��y@���@���@��!@��+@�^5@�$�@���@�`B@��/@��9@��D@�j@�(�@�|�@�
=@���@�n�@�=q@�@�p�@�O�@��@��@��9@��u@�z�@�Q�@�1'@�  @��F@��@�\)@�@��R@�v�@�@��7@���@�1'@���@��F@���@�|�@�t�@�dZ@�o@��@��@��@��y@��H@��!@�v�@�ff@�E�@�x�@���@�Q�@�(�@� �@�1@��
@���@�\)@��@��H@��!@���@��+@�E�@�5?@�@���@�hs@���@��j@��@|�@;d@�@~ȴ@~{@}?}@|(�@{t�@{33@z��@z=q@y��@yhs@yG�@y%@x��@x�u@xQ�@x1'@x �@x  @w�;@w�@w��@w�P@w�@v�y@v�@v�R@v��@v��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A҃AҁA҅AҋDA҅A҅A҉7A҅A҅A҉7A҅A҇+AҍPA҇+A҉7AґhAҏ\Aҏ\AғuAҏ\Aҏ\Aҕ�Aҏ\Aҏ\Aҕ�AґhAҏ\Aҕ�Aҕ�AґhAґhAҕ�Aҗ�Aҏ\Aҏ\Aҕ�AғuAҏ\AґhAҕ�AґhAґhAҕ�AғuAҏ\AғuAҕ�AғuAґhAҗ�AғuAґhAҕ�Aҗ�AґhAғuAҗ�AґhAғuAҙ�Aҕ�AғuAҙ�Aҗ�AғuAҕ�Aҙ�Aҗ�Aҕ�Aҙ�Aқ�Aҗ�Aҕ�Aҗ�Aҗ�AґhAғuAҙ�AғuAғuAҙ�AғuAҕ�Aҙ�Aҗ�Aҙ�Aҝ�Aқ�Aқ�Aҝ�Aҡ�Aқ�Aҝ�Aҝ�Aқ�Aҗ�Aқ�Aҙ�Aҝ�Aҡ�Aң�Aҟ�Aҟ�Aҡ�Aҟ�Aқ�Aң�Aң�Aҡ�Aҧ�Aҥ�Aҟ�Aҝ�Aҝ�Aқ�Aқ�Aҡ�Aҟ�Aҝ�Aҟ�Aҡ�Aҟ�Aң�Aҥ�Aң�Aҥ�Aҧ�Aҡ�Aҥ�Aҩ�Aҥ�Aҡ�Aҧ�Aҧ�Aң�Aң�Aң�Aҡ�Aҥ�Aҧ�Aҟ�Aҟ�Aң�Aҥ�Aҟ�Aҡ�Aҟ�Aҝ�Aҟ�Aҟ�Aҟ�Aқ�Aҙ�Aҝ�Aҟ�Aҝ�Aҡ�Aҥ�AҍPAқ�A҃AґhAғuA�jA�l�A�jA�jA�p�A�bNA�z�A҅A�~�A�ffA�VA�XA�O�A�ZA�K�A�I�A�E�A�C�A�K�A�M�A�O�A�K�A�;dA�M�A�K�A�E�A�E�A�?}A�-A�/A�(�A�(�A�$�A�-A�/A�(�A�&�A�(�A�&�A�"�A� �A�$�A�&�A�"�A� �A�$�A�&�A��A�A��A�ȴAѾwAѮAѧ�Aѥ�Aџ�Aя\A�|�AсA�~�A�p�A�bNA�VA�K�A�1'A�VA��/A�ȴA�ĜAУ�AЏ\AЉ7A�~�A�p�A�bNA�Q�A�G�A�=qA�7LA�/A�-A�+A�7LA�K�A�S�A�M�A�M�A�K�A�9XA�"�A��mA�ȴAυA�z�A�r�A�hsA�`BA�VA�O�A�VA΅A���A�hsA�  A���A̙�A̍PA̋DÃA�z�A�v�A�v�A�t�A�S�A�"�A�A���A��A��HA���A˶FAˡ�A�dZA�ZA�VA�O�A�C�A�9XA� �A���A���AʬAʡ�Aʕ�A�p�A�;dA� �A�  A��TA���AɶFAɡ�AɍPA�~�A�x�A�l�A�I�A�7LA�{A�A���A�  A���A��A��A��AȰ!A�z�A�A�A�33A�+A�bA��`A���AǬAǙ�Aǉ7A�x�A�jA�^5A�VA�E�A�A�A�=qA�7LA�1'A�/A�/A�/A�33A�1'A�-A�+A�-A�&�A�
=A��#A�ȴA�ƨA�ȴA�ȴAƾwAƼjAƾwA���Aƺ^Aƥ�A�p�A�ZA�\)A�;dA��A�\)A�/A���AāA�?}A��A�bA�JA�A��HA�VA��A��yA¼jA�APAA�ffA�M�A�oA��\A�A��+A��wA��7A�VA��A���A���A�|�A�=qA�$�A�1A���A��hA�JA�l�A���A�x�A�r�A�l�A�l�A�`BA�I�A� �A�JA��#A��wA��A�|�A�\)A�VA�VA�M�A�I�A�E�A�I�A�I�A�I�A�E�A�E�A�I�A�G�A�E�A�9XA�;dA�7LA�-A�"�A�ȴA��mA��RA�1'A�1A���A���A�jA�ZA�S�A�O�A�O�A�Q�A�VA�M�A�I�A�I�A�I�A�?}A�1'A�A���A���A�dZA��A��^A��HA�`BA�
=A��TA���A���A��RA�z�A��A�G�A��FA�I�A�  A��PA��mA�I�A��mA���A�z�A�^5A�bA�ƨA�p�A���A��/A���A���A�XA�$�A���A���A�n�A�O�A�A��;A���A���A���A�ĜA���A�v�A��A���A���A��A�n�A�;dA�{A��A���A�Q�A�JA��A�A���A��A�r�A�hsA�`BA�VA�I�A�?}A�+A�oA�1A���A��HA�A��!A�|�A�`BA�A�A��A��`A��7A��A��A���A��A�r�A�^5A�K�A�{A��mA�ĜA���A�p�A�O�A�33A�"�A��A�JA�  A��A��;A��A�oA��A�ĜA��uA��7A��A�t�A�n�A�bNA� �A��/A��RA�n�A�(�A���A�O�A��-A�O�A�  A��A��A��A���A�\)A�7LA�+A��A��A�bA�JA�JA�
=A�JA�
=A���A��yA��`A�A��uA�v�A�`BA�ZA�E�A�$�A��A�oA�%A�A���A��TA�ȴA���A�?}A�33A�%A��A�O�A�/A�(�A�5?A�1'A�1A�x�A��A�t�A��\A�JA��PA���A��A��^A�M�A�E�A�G�A�G�A�G�A�A�A�7LA�9XA�{A��A��!A�/A���A�5?A�ȴA�p�A�A��RA���A�~�A�t�A�hsA�^5A�M�A�33A�"�A���A���A���A�z�A���A��^A��hA�p�A�VA�G�A�&�A��A�ȴA��+A�ZA���A��!A��PA�~�A�|�A�x�A�jA�C�A�&�A�A��mA���A��jA��A��uA�z�A�bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A҅A҇+A҇+AґhAґhAґhAғuAґhAғuAғuAґhAғuAҕ�Aҗ�Aҕ�Aҙ�Aҝ�Aҝ�Aҡ�Aҟ�Aҡ�Aҥ�Aҥ�Aҡ�Aҝ�Aҙ�A�r�A�jA�K�A�E�A�+A�$�A��;A�|�A��;A�ZA�A�A��A���A̸RA�G�A˸RA�-A�p�Aɰ!A�-A��A�1A�t�A�9XA�/A��yA���A�K�Aģ�Aá�AA��7A��A�1'A�jA��#A�Q�A�G�A�9XA�bA�jA�M�A��RA�Q�A�t�A�Q�A�t�A�
=A��A��A���A���A�7LA��RA���A�hsA��A���A�ȴA�C�A�A��uA�33A�JA���A�C�A���A�{A��A���A���A�;dA��A��A�33A���A�bNA�5?A�-A�^5A��^A�$�A�oA��A�r�A���AxffAs33Ap1'An��Al�AjVAd�Ab�RAat�A_l�A^bNA[�AV��ASdZAR  AQp�AP��ANjAK�AI�AG�AF�uAC�AB{A?C�A;��A:ffA9��A6z�A4�uA49XA3�-A3A/�
A-�wA-"�A+��A)�A'7LA&~�A$bA"$�A!;dA �A��AA�A�9A�+AQ�A�A+AI�A�/AJA(�A
=AVA��AdZA��AQ�A�A
1A	�
A	��A�9A�#A��A~�A�9A�yA�HAv�A��AhsAI�A��A��A��AƨA�PAt�A?}A �RA 5?@��@���@�5?@���@�bN@��;@�l�@��y@��#@���@�ff@��7@��j@�t�@��@�@���@�p�@�p�@��@��`@�u@�1'@��@���@���@��-@�V@�|�@띲@�9X@�A�@���@��@蛦@�+@�{@���@�@��@�"�@◍@�hs@�/@�dZ@��#@�/@�A�@���@�bN@�dZ@��@�V@мj@�?}@Ϯ@�ȴ@�{@͉7@�7L@���@�Ĝ@���@�M�@�/@���@���@�J@�@��@�@�x�@�Ĝ@��@Ƈ+@�5?@�@�@�/@��@�ƨ@��
@��m@��m@���@þw@�S�@�;d@�C�@�S�@�\)@�;d@�t�@öF@�ƨ@��
@���@�j@���@��@�n�@���@��@��`@��9@��D@��D@��@�r�@�I�@��@���@�dZ@�C�@���@�E�@�hs@�Ĝ@�Q�@�  @��m@��@�dZ@�@�^5@�^5@�~�@���@���@�~�@�{@�Q�@�dZ@��H@�-@�{@�$�@��@�^5@��+@�@�J@��T@��-@��h@��@�x�@�p�@�p�@�G�@��@��;@��@�+@��y@�~�@�E�@�5?@�-@�$�@�J@���@�&�@���@��u@�1'@��@���@�ƨ@��F@���@�|�@�K�@�+@��H@���@�ff@�E�@�{@��T@���@�`B@�7L@��@��`@��@���@�o@���@�-@�@���@��7@�X@�/@��9@���@���@�;d@���@��!@�v�@�V@�$�@��T@��h@�x�@��@���@��9@���@�r�@�9X@�1@��@��F@��H@��!@�ff@�$�@��#@�@��-@��7@���@��D@�r�@�r�@�r�@�bN@�(�@�\)@���@���@�ff@�=q@�-@��@��@�@��^@��7@�/@���@��/@���@�bN@�Q�@�b@�C�@�33@��y@���@�v�@�ff@�^5@�5?@��@���@�x�@�V@��@�1'@�dZ@�C�@�33@��y@���@���@��!@��+@�^5@�$�@���@�`B@��/@��9@��D@�j@�(�@�|�@�
=@���@�n�@�=q@�@�p�@�O�@��@��@��9@��u@�z�@�Q�@�1'@�  @��F@��@�\)@�@��R@�v�@�@��7@���@�1'@���@��F@���@�|�@�t�@�dZ@�o@��@��@��@��y@��H@��!@�v�@�ff@�E�@�x�@���@�Q�@�(�@� �@�1@��
@���@�\)@��@��H@��!@���@��+@�E�@�5?@�@���@�hs@���@��j@��@|�@;d@�@~ȴ@~{@}?}@|(�@{t�@{33@z��@z=q@y��@yhs@yG�@y%@x��@x�u@xQ�@x1'@x �@x  @w�;@w�@w��@w�P@w�@v�y@v�@v�R@v��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A҃AҁA҅AҋDA҅A҅A҉7A҅A҅A҉7A҅A҇+AҍPA҇+A҉7AґhAҏ\Aҏ\AғuAҏ\Aҏ\Aҕ�Aҏ\Aҏ\Aҕ�AґhAҏ\Aҕ�Aҕ�AґhAґhAҕ�Aҗ�Aҏ\Aҏ\Aҕ�AғuAҏ\AґhAҕ�AґhAґhAҕ�AғuAҏ\AғuAҕ�AғuAґhAҗ�AғuAґhAҕ�Aҗ�AґhAғuAҗ�AґhAғuAҙ�Aҕ�AғuAҙ�Aҗ�AғuAҕ�Aҙ�Aҗ�Aҕ�Aҙ�Aқ�Aҗ�Aҕ�Aҗ�Aҗ�AґhAғuAҙ�AғuAғuAҙ�AғuAҕ�Aҙ�Aҗ�Aҙ�Aҝ�Aқ�Aқ�Aҝ�Aҡ�Aқ�Aҝ�Aҝ�Aқ�Aҗ�Aқ�Aҙ�Aҝ�Aҡ�Aң�Aҟ�Aҟ�Aҡ�Aҟ�Aқ�Aң�Aң�Aҡ�Aҧ�Aҥ�Aҟ�Aҝ�Aҝ�Aқ�Aқ�Aҡ�Aҟ�Aҝ�Aҟ�Aҡ�Aҟ�Aң�Aҥ�Aң�Aҥ�Aҧ�Aҡ�Aҥ�Aҩ�Aҥ�Aҡ�Aҧ�Aҧ�Aң�Aң�Aң�Aҡ�Aҥ�Aҧ�Aҟ�Aҟ�Aң�Aҥ�Aҟ�Aҡ�Aҟ�Aҝ�Aҟ�Aҟ�Aҟ�Aқ�Aҙ�Aҝ�Aҟ�Aҝ�Aҡ�Aҥ�AҍPAқ�A҃AґhAғuA�jA�l�A�jA�jA�p�A�bNA�z�A҅A�~�A�ffA�VA�XA�O�A�ZA�K�A�I�A�E�A�C�A�K�A�M�A�O�A�K�A�;dA�M�A�K�A�E�A�E�A�?}A�-A�/A�(�A�(�A�$�A�-A�/A�(�A�&�A�(�A�&�A�"�A� �A�$�A�&�A�"�A� �A�$�A�&�A��A�A��A�ȴAѾwAѮAѧ�Aѥ�Aџ�Aя\A�|�AсA�~�A�p�A�bNA�VA�K�A�1'A�VA��/A�ȴA�ĜAУ�AЏ\AЉ7A�~�A�p�A�bNA�Q�A�G�A�=qA�7LA�/A�-A�+A�7LA�K�A�S�A�M�A�M�A�K�A�9XA�"�A��mA�ȴAυA�z�A�r�A�hsA�`BA�VA�O�A�VA΅A���A�hsA�  A���A̙�A̍PA̋DÃA�z�A�v�A�v�A�t�A�S�A�"�A�A���A��A��HA���A˶FAˡ�A�dZA�ZA�VA�O�A�C�A�9XA� �A���A���AʬAʡ�Aʕ�A�p�A�;dA� �A�  A��TA���AɶFAɡ�AɍPA�~�A�x�A�l�A�I�A�7LA�{A�A���A�  A���A��A��A��AȰ!A�z�A�A�A�33A�+A�bA��`A���AǬAǙ�Aǉ7A�x�A�jA�^5A�VA�E�A�A�A�=qA�7LA�1'A�/A�/A�/A�33A�1'A�-A�+A�-A�&�A�
=A��#A�ȴA�ƨA�ȴA�ȴAƾwAƼjAƾwA���Aƺ^Aƥ�A�p�A�ZA�\)A�;dA��A�\)A�/A���AāA�?}A��A�bA�JA�A��HA�VA��A��yA¼jA�APAA�ffA�M�A�oA��\A�A��+A��wA��7A�VA��A���A���A�|�A�=qA�$�A�1A���A��hA�JA�l�A���A�x�A�r�A�l�A�l�A�`BA�I�A� �A�JA��#A��wA��A�|�A�\)A�VA�VA�M�A�I�A�E�A�I�A�I�A�I�A�E�A�E�A�I�A�G�A�E�A�9XA�;dA�7LA�-A�"�A�ȴA��mA��RA�1'A�1A���A���A�jA�ZA�S�A�O�A�O�A�Q�A�VA�M�A�I�A�I�A�I�A�?}A�1'A�A���A���A�dZA��A��^A��HA�`BA�
=A��TA���A���A��RA�z�A��A�G�A��FA�I�A�  A��PA��mA�I�A��mA���A�z�A�^5A�bA�ƨA�p�A���A��/A���A���A�XA�$�A���A���A�n�A�O�A�A��;A���A���A���A�ĜA���A�v�A��A���A���A��A�n�A�;dA�{A��A���A�Q�A�JA��A�A���A��A�r�A�hsA�`BA�VA�I�A�?}A�+A�oA�1A���A��HA�A��!A�|�A�`BA�A�A��A��`A��7A��A��A���A��A�r�A�^5A�K�A�{A��mA�ĜA���A�p�A�O�A�33A�"�A��A�JA�  A��A��;A��A�oA��A�ĜA��uA��7A��A�t�A�n�A�bNA� �A��/A��RA�n�A�(�A���A�O�A��-A�O�A�  A��A��A��A���A�\)A�7LA�+A��A��A�bA�JA�JA�
=A�JA�
=A���A��yA��`A�A��uA�v�A�`BA�ZA�E�A�$�A��A�oA�%A�A���A��TA�ȴA���A�?}A�33A�%A��A�O�A�/A�(�A�5?A�1'A�1A�x�A��A�t�A��\A�JA��PA���A��A��^A�M�A�E�A�G�A�G�A�G�A�A�A�7LA�9XA�{A��A��!A�/A���A�5?A�ȴA�p�A�A��RA���A�~�A�t�A�hsA�^5A�M�A�33A�"�A���A���A���A�z�A���A��^A��hA�p�A�VA�G�A�&�A��A�ȴA��+A�ZA���A��!A��PA�~�A�|�A�x�A�jA�C�A�&�A�A��mA���A��jA��A��uA�z�A�bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
h
B
h
B
hsB
gmB
h
B
g�B
h>B
h
B
g8B
g8B
h>B
gmB
g�B
g�B
h
B
g�B
gB
f�B
e�B
e�B
e�B
f�B
ffB
e�B
e�B
d�B
^jB
]�B
X�B
XB
V9B
XyB
|B
��B>�Bx8B��B��B��B�+B~�B|BxB��B�B�}B�B�B��B�>B�
B��B�BGB!�B'�B+�BD�BAUBIRBEBK)BK�BIRBE�B_;BQ�BK^BM6BNpBP}B[�B^jBd�BdZBg�Bf�Bb�B_pB^�B^jBV9BQBGEB:�B.�B$tB{B�B�;B��B�HB��B�qB��B��BGB-wB&BB
�B
�B
��B
��B
t�B
lWB
iyB
d�B
d&B
a�B
_;B
V�B
:*B
B
B	��B	��B	��B	�dB	��B	��B	�$B	�(B	�B	p�B	bNB	Z�B	VmB	R B	P�B	@�B	:�B	4B	.�B	,=B	 'B	#:B	1B	�B	B	�B	JB	JB	
�B	1B	JB	B	�B	 �B	�B	 iB�.B	;B��B��B��B�B�PB�B�>B�B�B�,B�B��BںB��B�KB�WB�;B�#B��BیB�vB��B�B��B�,B��B�8B��B��B�B��B	 �B	�B	%B	�B	B	VB	"B	�B	�B	JB	B	
=B	
�B	
=B	~B	PB	�B	bB	�B	hB	�B	:B	�B	�B	FB	B	$B	$B	�B	%B	)_B	.}B	1�B	2�B	7�B	:*B	8RB	7�B	=�B	@�B	CaB	A�B	C�B	IB	R�B	T�B	R�B	S�B	Q�B	P}B	PB	PB	PHB	OB	K�B	K�B	EB	?HB	<�B	9$B	8�B	8�B	:�B	8�B	:*B	;�B	?HB	J�B	OBB	OBB	Q�B	R�B	S�B	T,B	T�B	XEB	_�B	gB	rGB	u�B	w2B	xB	x�B	x�B	zDB	~�B	�B	��B	��B	��B	�fB	�B	��B	�"B	�VB	��B	�.B	�:B	��B	��B	�=B	�B	��B	�B	�B	�_B	��B	��B	��B	��B	�aB	��B	�[B	��B	�*B	�B	��B	��B	�'B	�[B	��B	�-B	��B	ŢB	�B	�B	�KB	�XB	˒B	�pB	�}B	�TB	��B	�
B	چB	یB	��B	�BB	�B	�B	�B	� B	�B	��B	�mB	�B	��B	�B	�fB	�B	��B	��B	�B	��B	��B	��B
oB
�B
B
MB
�B
�B
%B
�B
	�B
	�B

�B

�B
JB
JB
JB
JB
B
B
B
�B
�B
�B
.B
.B
bB
�B
�B
 B
4B
hB
�B
oB
:B
B
B
B
�B
�B
�B
B
�B
FB
�B
$B
�B
�B
�B
�B
�B
_B
_B
+B
B
B
�B
	B
qB
�B
CB
CB
�B
~B
�B
�B
�B
!B
�B
�B
VB
�B
 �B
!�B
"hB
$tB
$B
$�B
$�B
%FB
%B
$�B
%�B
'�B
&�B
&�B
&�B
&LB
%�B
&�B
'�B
'�B
'�B
($B
($B
(�B
(�B
(XB
(�B
)*B
)*B
*�B
*�B
+B
+6B
+6B
*�B
,qB
-wB
-CB
.IB
.�B
.�B
.�B
.}B
/OB
/B
/OB
/OB
1�B
2�B
2aB
3�B
2�B
2�B
3hB
33B
33B
3�B
3hB
3�B
4B
4�B
5tB
5�B
5�B
6zB
6�B
7LB
8�B
8�B
9�B
9XB
9�B
:�B
:*B
:*B
:^B
:^B
:�B
:�B
:�B
;0B
:�B
;�B
<6B
<�B
=qB
=�B
>B
>B
>wB
=�B
?B
?�B
@B
?�B
?�B
?�B
?�B
@B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
@�B
@�B
B[B
C�B
D3B
D3B
D3B
DgB
D�B
E9B
FB
F�B
GB
GzB
GEB
GzB
G�B
G�B
HB
HKB
IB
I�B
I�B
K)B
J�B
J�B
J�B
K^B
K�B
MB
N<B
NB
NB
NpB
N�B
OB
OvB
OBB
O�B
O�B
PHB
P}B
P�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
h�B
f2B
i�B
f�B
iB
h�B
ffB
gmB
jB
gmB
h�B
h>B
e�B
iyB
hsB
f�B
h
B
h>B
f�B
h�B
h�B
f�B
h�B
h�B
gB
h>B
iDB
f�B
gB
h�B
iB
g8B
gB
h>B
h�B
f�B
gB
h�B
gmB
e�B
h>B
g�B
f2B
g�B
h�B
g�B
ffB
gmB
h�B
ffB
gB
h�B
gB
f�B
h�B
g�B
f�B
h�B
h>B
ffB
h
B
h�B
f�B
g8B
h�B
h>B
f2B
h>B
h�B
g�B
f�B
hsB
h�B
h>B
gB
h�B
h
B
ffB
iyB
hsB
f�B
h�B
h�B
gB
h�B
gmB
ffB
g�B
g�B
f�B
e�B
gB
f�B
f�B
g8B
hsB
f�B
g�B
ffB
ffB
e`B
g8B
f�B
e�B
f2B
g�B
d&B
e`B
gB
e�B
f2B
gB
e�B
d�B
e�B
gB
e`B
e,B
g8B
f2B
e�B
g�B
f2B
`vB
gmB
f2B
e`B
g�B
f�B
e�B
gB
g�B
e�B
e�B
g�B
ffB
f�B
g�B
e�B
e`B
g8B
gB
e�B
dZB
gmB
e�B
dZB
f�B
f2B
ffB
d�B
e�B
e�B
d�B
d&B
f�B
e�B
d�B
g�B
ZB
l�B
\�B
c�B
^�B
[#B
\�B
W�B
aHB
YB
]dB
_pB
`�B
`B
c�B
X�B
YKB
[#B
ZB
V9B
X�B
WsB
Y�B
XyB
VmB
YB
Z�B
YKB
U2B
ZQB
Y�B
T,B
Y�B
W
B
V�B
U2B
V�B
U�B
U�B
U�B
V�B
U�B
UgB
YKB
Z�B
XEB
W�B
YB
Z�B
Z�B
Y�B
^B
iyB
�B
�_B
��B
�{B
��B
��B
��B
�0B
�HB
�BB
��B
�B
ԕB
ߤB
�2B
�B	7B-�B_�Bm)Bv�BwfBv`Bx8Bv�Bx�BzDBzBv�Bx�BzxBzDBy>B|�B� B��B�\B�B�@B�B��B�-B��B�B�tB�9B�B�B��B�6B��B��B��B�B�B��B��B~�B|�B}�B~�B.B}�B}VB��B}�B�B|PB}"B{B|�B|�B{JBw�Bv`BtTBu�BxBv�BxlB{JB~]B}VBzxB{�B��B�SB�B��B��B�hB��B��B�_B�7B�xB�hB�_B��B��B��B��B��B��B��B��BƨB�)B��B��B�DB�>B��B�B��B�/B�B��B�B��B�>B��B�B��B�mB�B�yB�DB��B�>B�B�B�B�sB�8B�mB�]B�B��B�B�AB�B�TB�ZB��B�B�B�B;B��B��B�B�B�B�BB 'B(�B �B �B�B�B&�B6�B-�B-B,�B.IB)_B,B'�B,�B3�BCaBW?BM6BFtB@�B>wBE�BEB=�BA�BC-B:^B<B;�B@�BD�Bg8BQBE9BD�BEBDgBC�BC�BIBG�BM�BG�BLdBQBJ�BK�BI�BJ�BK�BK�BJXBIRBIRBI�BI�BGBGzBGEBF�BE�BEBC�BB�BW?BU�B�_Bj�BS[BQ�BX�BW
BO�BM�BN<BN<BL�BK^BMjBL�BK^BH�BJ�BI�BR BMjBH�BP�BT�BS�BaHBW�BK�BEmBC�B@�B@BI�BRTBVBX�BK^BR�Ba�B^B\]Bf�BU�BXBS�BZB[�Bd&Bf2B[�B[�BaBe`BbBqvB`vBe�Bf2Bo5Bd�Bc�B`�B`BaHBg�Be�Bv�Be�Bc�Bf�Ba�Bl"Bb�BcTBg�BlWBffBf�Bf�BbNB`B_;B_�B`BB`vB^�B^�B^�B_�B^�B\�B]�B]/B^5Ba�B\�B\�B]�B]/B^jB`�BYBUgBW�BXBQ�BQNB\)BT�BQ�BP�BU2BP}BK�BI�BGzBE�BD�BC�BD�BXyBD3B8�B=�B33B1[B0!B.�B,=B.B2�B.�B$�B)*B!�B%zB)�BOB�B1B��B�B��B��B�lB�&B�B�B�;B�B�BBޞB�;B��B��B�dB��BٴBߤB�pB�2B҉BϫB�&B�6B��B�B��BɺB�KBȴB��B��B�HB�B��B�^B��B��B�'B�!B��B�CB�B|PB�rBr�Bn�B`�Bq�BK�B@�B2-B0�B0�B0!B-�B/�B+�B,�B%B/�BqB-�B 'B"hB�B�BVBB
�+B
��B
�B
� B
�B
��B
��BB
�2B
�9B
��B�B
�MB
�B
��B
��B
�	B
��B
�~B
��B
��B
��B
�B
��B
�oB
wfB
v�B
s�B
s�B
v�B
w�B
v`B
u�B
sB
r�B
pB
p;B
qAB
m�B
t�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022050619170620220506191706IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022051614012020220516140120QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022051614012020220516140120QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                